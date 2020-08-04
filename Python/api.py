import inspect

from core import reify, run_goal, variables
from goals import conj


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
    fresh_vars = (variables(name) for name in var_names)
    return body(*fresh_vars)
