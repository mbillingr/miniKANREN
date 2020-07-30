from collections import deque

import sympy as sy

from core import SUSPENSION, take


def make_goal(func):
    """Decorator that turns a function of the form f(Substitution, ...) into
    a goal-creating function.

    For example:
        @make_goal
        def same(s, u, v):
            ...

    is equivalent to
        def same(u, v):
            def goal(s):
                ...
            return goal
    """

    def wrap(*args, **kwargs):
        def goal(s):
            return func(s, *args, **kwargs)

        return goal

    if func.__doc__ is not None:
        wrap.__doc__ = "produce a " + func.__doc__
    return wrap


@make_goal
def same(s, u, v):
    """goal that succeeds if u and v are equivalent"""
    s = s.unify(u, v)
    if s.is_valid():
        yield s


def fail(s):
    """goal that never succeeds"""
    if False:
        yield s


def succeed(s):
    """goal that always succeeds"""
    yield s


def never(s):
    """goal that never produces a substitution"""
    while True:
        yield SUSPENSION


def always(s):
    """goal that always produces a substitution"""
    while True:
        yield s


@make_goal
def disj(s, *subgoals):
    """goal that succeeds if any of its subgoals succeeds"""
    splicer = IteratorSplicer(g(s) for g in subgoals)
    yield from splicer.drain()


@make_goal
def conj2(s, g1, g2):
    """goal that succeeds if g1 and g2 succeed"""
    stream_combiner = IteratorSplicer()
    for t in g1(s):
        stream_combiner.append(g2(t))
        yield from stream_combiner.yield_one_from_each()
    yield from stream_combiner.drain()


def conj(g1, *subgoals):
    """produce a goal that succeeds if all of its subgoals succeed"""

    def build(x, g, target):
        return lambda t: AppendLater(target, map(x, g(t)))

    def goal(s):
        """goal that succeeds if all of its subgoals succeed"""
        stream_combiner = IteratorSplicer()
        generator_combiner = IteratorSplicer()

        gg = lambda u: AppendLater(stream_combiner, g1(u))
        for g in subgoals:
            gg = build(gg, g, generator_combiner)
        gg = map(gg, [s])
        generator_combiner.append(gg)

        for gen in generator_combiner.drain():
            yield from stream_combiner.yield_one_from_each()
            gen.append_now()
        yield from stream_combiner.drain()

    return goal


class AppendLater:
    def __init__(self, target, item):
        self.item = item
        self.target = target

    def append_now(self):
        self.target.append(self.item)


class IteratorSplicer:
    def __init__(self, iterators=()):
        self.iterators = deque(iterators)

    def is_empty(self):
        return not self.iterators

    def append(self, iterable):
        self.iterators.append(iterable)

    def drain(self):
        while not self.is_empty():
            yield from self.yield_next()

    def yield_one_from_each(self):
        """Yield one item from each iterator and remove exhausted iterators"""
        for _ in range(len(self.iterators)):
            yield from self.yield_next()

    def yield_next(self):
        while not self.is_empty():
            try:
                yield next(self.iterators[0])
                self.iterators.rotate()
                return
            except StopIteration:
                self.iterators.popleft()


@make_goal
def ifte(s, g_cond, g_true, g_false):
    """goal that succeeds if g_cond and g_true succeed or g_cond fails and g_false succeeds"""
    s_inf = g_cond(s)

    try:
        first_cond = next(s_inf)
    except StopIteration:
        yield from g_false(s)
        return

    stream_combiner = IteratorSplicer()
    stream_combiner.append(g_true(first_cond))

    for t in s_inf:
        yield from stream_combiner.yield_one_from_each()
        stream_combiner.append(g_true(t))
    yield from stream_combiner.drain()


@make_goal
def once(s, g):
    """goal that succeeds at most once"""
    yield from take(1, g(s))


@make_goal
def symeq(s, u, v):
    """goal that succeeds if u and v are symbolically equivalent"""

    expr = u - v

    for var, sub in s.subs:
        expr = expr.subs(var, sub)

    syms = expr.free_symbols

    if len(syms) == 0:
        if expr == 0:
            yield s
    else:
        symbol = next(iter(syms))  # select any one symbol to substitute
        for solution in sy.solve(expr, symbol):
            t = s.unify(symbol, solution)
            if t.is_valid():
                yield t
