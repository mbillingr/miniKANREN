import sympy as sy

from core import gen_var
from stream import SuspendIteration, take, append_inf, append_map_inf


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
        raise SuspendIteration(never(s))
        yield  # turns function into generator, even if it's unreachable


def always(s):
    """goal that always produces a substitution"""
    while True:
        yield s


@make_goal
def disj(s, subgoal1, *subgoals):
    """goal that succeeds if any of its subgoals succeeds"""
    stream = subgoal1(s)
    for g in subgoals:
        stream = append_inf(stream, g(s))
    yield from stream


@make_goal
def conj(s, subgoal1, *subgoals):
    """goal that succeeds if all of its subgoals succeed"""
    stream = subgoal1(s)
    for g in subgoals:
        stream = append_map_inf(g, stream)
    yield from stream


@make_goal
def ifte(s, g_cond, g_true, g_false):
    """goal that succeeds if g_cond and g_true succeed or g_cond fails and g_false succeeds"""

    def loop(s_inf=g_cond(s)):
        try:
            first_cond = next(s_inf)
        except StopIteration:
            yield from g_false(s)
            return
        except SuspendIteration as suspension:
            raise SuspendIteration(loop(suspension.stream))

        yield from append_inf(g_true(first_cond),
                              append_map_inf(g_true, s_inf))

    return loop()


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


@make_goal
def suspend(s, goal):
    """suspend the goal once"""
    raise SuspendIteration(goal(s))
    yield


@make_goal
def listo(s, x):
    """goal that succeeds if x is a list"""
    if isinstance(x, list):
        yield s
        return

    seq = []
    while True:
        yield from same(x, seq)(s)
        seq.append(gen_var('__'))


@make_goal
def appendo(s, a, b, l):
    a = s.walk_var(a)
    b = s.walk_var(b)
    l = s.walk_var(l)

    if isinstance(a, list) and isinstance(b, list):
        yield from same(a + b, l)(s)
    elif isinstance(a, list) and isinstance(l, list):
        n = len(a)
        yield from conj(same(a, l[:n]),
                        same(b, l[n:]))(s)
    elif isinstance(b, list) and isinstance(l, list):
        n = len(l) - len(b)
        yield from conj(same(a, l[:n]),
                        same(b, l[n:]))(s)
    elif isinstance(l, list):
        for n in range(len(l) + 1):
            yield from conj(same(a, l[:n]),
                            same(b, l[n:]))(s)
    elif isinstance(a, list):
        seq = []
        while True:
            yield from conj(same(b, seq),
                            same(l, a + seq))(s)
            seq.append(gen_var('__'))
    elif isinstance(b, list):
        seq = []
        while True:
            yield from conj(same(a, seq),
                            same(l, seq + b))(s)
            seq.append(gen_var('__'))
    else:
        seq = []
        while True:
            for n in range(len(seq) + 1):
                yield from conj(same(a, seq[:n]),
                                same(b, seq[n:]),
                                same(l, seq))(s)
            seq.append(gen_var('__'))
