import sympy as sy

from functional_data_structures import Map, Singleton
from stream import take, take_inf

variables = sy.symbols
Variable = sy.Symbol
Variable.__unify__ = lambda self, other, s: s.extend(self, other)


def is_var(x):
    return isinstance(x, Variable)


def is_atom(x):
    return type(x) in {bool, int, float, str, type(None), type, ReifiedVariable,
                       sy.core.add.Add,
                       sy.core.numbers.Integer, sy.core.numbers.Zero, sy.core.numbers.One, sy.core.numbers.NegativeOne}


def is_empty(x):
    try:
        next(x)
    except StopIteration:
        return True
    return False


def as_iterable(x):
    if isinstance(x, dict):
        return x.items()
    try:
        return iter(x)
    except TypeError:
        return None


class ReifiedVariable:
    def __init__(self, n):
        self.name = reify_name(n)

    def __eq__(self, other):
        return self.name == str(other)

    def __repr__(self):
        return self.name


class InvalidSubstitution(Singleton):
    @staticmethod
    def is_valid():
        return False

    def unify(self, *_args, **_kwargs):
        return self


class Substitution:
    def __init__(self, subs=Map()):
        if isinstance(subs, dict):
            self.subs = Map()
            for k, v in subs.items():
                self.subs = self.subs.insert(k, v)
        else:
            self.subs = subs

    @staticmethod
    def is_valid():
        return True

    def walk(self, var):
        try:
            value = self.subs.lookup(var)
        except KeyError:
            return var
        return self.walk_var(value)

    def walk_var(self, value):
        if is_var(value):
            return self.walk(value)
        return value

    def walk_deep(self, x):
        x = self.walk_var(x)

        if is_var(x) or is_atom(x):
            return x

        if hasattr(x, '__map__'):
            return x.__map__(self.walk_deep)
        elif isinstance(x, (list, tuple, set)):
            return type(x)(self.walk_deep(item) for item in x)
        elif isinstance(x, dict):
            return {self.walk_deep(k): self.walk_deep(v) for k, v in x.items()}
        else:
            raise NotImplementedError("walk_deep: {}".format(type(x)))

    def extend(self, var, value):
        if self.occurs(var, value):
            return InvalidSubstitution()
        return Substitution(self.subs.insert(var, value))

    def occurs(self, var, value):
        value = self.walk_var(value)
        if is_var(value):
            return var == value
        elif is_atom(value):
            return False
        iterator = as_iterable(value)
        if iterator:
            return any(self.occurs(var, v) for v in iterator)
        return False

    def unify(self, x, y, strict=True):
        x = self.walk_var(x)
        y = self.walk_var(y)

        if x == y:
            return self
        elif hasattr(x, '__unify__'):
            return x.__unify__(y, self)
        elif hasattr(y, '__unify__'):
            return y.__unify__(x, self)
        elif is_atom(x) or is_atom(y):
            return InvalidSubstitution()

        if strict and type(x) != type(y):
            return InvalidSubstitution()

        if len(x) != len(y):
            return InvalidSubstitution()

        x_iter = as_iterable(x)
        y_iter = as_iterable(y)

        if x_iter and y_iter:
            for xi, yi in zip(x_iter, y_iter):
                self = self.unify(xi, yi)
            return self
        return InvalidSubstitution()

    def reify(self, v):
        v = self.walk_deep(v)
        r = Substitution()._reify(v)
        return r.walk_deep(v)

    def _reify(self, v):
        v = self.walk_var(v)
        if is_var(v):
            n = len(self.subs)
            rv = ReifiedVariable(n)
            return Substitution(self.subs.insert(v, rv))
        elif is_atom(v):
            return self

        r = self
        for item in as_iterable(v):
            r = r._reify(item)
        return r

    def __eq__(self, other):
        return isinstance(other, Substitution) and dict(self.subs) == dict(other.subs)

    def __hash__(self):
        return hash(self.subs)

    def __repr__(self):
        return 'Substitution({})'.format(self.subs)


def reify(v):
    return lambda s: s.reify(v)


def reify_name(n):
    return '_{}'.format(n)


def run_goal(n_or_goal, goal=None):
    if goal is None:
        goal = n_or_goal
        return take_inf(goal(Substitution()))
    else:
        n = n_or_goal
        return take(n, goal(Substitution()))


def gen_var(name):
    return variables(gen_name(name))


FRESH_COUNTER = 0


def gen_name(name):
    global FRESH_COUNTER
    FRESH_COUNTER += 1
    return name + str(FRESH_COUNTER)


def reset_names():
    global FRESH_COUNTER
    FRESH_COUNTER = 0
