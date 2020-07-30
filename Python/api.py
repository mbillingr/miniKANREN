from core import reify, run_goal
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
