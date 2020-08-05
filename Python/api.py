import inspect

from core import reify, run_goal, variables, gen_name
from goals import conj
from stream import SuspendIteration


def run(*args):
    if isinstance(args[0], int):
        n = args[0]
        args = args[1:]
    else:
        n = None
    var, goals = args[0], args[1:]

    if isinstance(var, tuple):
        raise NotImplementedError("run with multiple fresh variables")

    if n is None:
        return map(reify(var), run_goal(conj(*goals)))
    else:
        return map(reify(var), run_goal(n, conj(*goals)))


def fresh(body):
    spec = inspect.signature(body)
    var_names = spec.parameters.keys()
    fresh_vars = [variables(gen_name(name)) for name in var_names]
    subgoals = body(*fresh_vars)
    try:
        return conj(*subgoals)
    except TypeError:
        return subgoals


def defrel(func):
    # return func
    # return lambda *args: suspend(func(*args))
    # return lambda *args: lambda s: func(*args)(s)
    # tmp = lambda s, *args: func(*args, s)

    def wrapper(*args):
        def goal(s):
            raise SuspendIteration(func(*args)(s))
            yield

        return goal

    return wrapper
