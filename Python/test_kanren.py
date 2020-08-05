import pytest

from core import variables, Substitution, InvalidSubstitution, reify, run_goal, reset_names
from functional_data_structures import Map
from goals import same, fail, succeed, conj, disj, make_goal, never, always, ifte, once, symeq, suspend, listo
from stream import SuspendIteration, take


def assert_suspended(stream):
    with pytest.raises(SuspendIteration) as e_info:
        next(stream)
    return e_info.value.stream


def test_walk1():
    a, w, x, y, z = variables("a, w, x, y, z")

    substitution = Substitution({z: a, x: w, y: z})

    assert substitution.walk(x) is w
    assert substitution.walk(y) is a
    assert substitution.walk(z) is a


def test_walk2():
    v, w, x, y = variables("v, w, x, y")

    substitution = Substitution({x: y, v: x, w: x})

    assert substitution.walk(x) is y
    assert substitution.walk(v) is y
    assert substitution.walk(w) is y


def test_walk3():
    b, v, w, x, y, z = variables("b, v, w, x, y, z")

    substitution = Substitution({x: b, z: y, w: (x, "e", z)})

    assert substitution.walk(w) == (x, "e", z)


def test_occurs():
    x, y = variables("x, y")
    assert Substitution().occurs(x, x)
    assert Substitution({y: x}).occurs(x, (y,))
    assert Substitution({y: x}).occurs(x, [y])
    assert Substitution({y: x}).occurs(x, {y})
    assert not Substitution({y: x}).occurs(x, "x")
    assert not Substitution({y: x}).occurs(x, {"x": "y"})
    assert Substitution({y: x}).occurs(x, {"y": y})
    assert Substitution({y: x}).occurs(x, {y: "y"})


def test_extend():
    x, y = variables("x, y")
    assert Substitution().extend(x, [x]) is InvalidSubstitution()
    assert Substitution({y: x}).extend(x, [y]) is InvalidSubstitution()


def test_extend_and_walk():
    x, y, z = variables("x, y, z")
    s = Substitution({z: x, y: z})
    s = s.extend(x, 'e')
    assert s.walk(y) == 'e'


def test_same():
    x, y = variables("x, y")
    assert same.__doc__ == "produce a goal that succeeds if u and v are equivalent"
    assert set(same(True, False)(Substitution())) == set()
    assert set(same(x, y)(Substitution())) == {Substitution({x: y})}


def test_fail():
    assert fail.__doc__ == "goal that never succeeds"
    assert set(fail(Substitution())) == set()


def test_succeed():
    assert succeed.__doc__ == "goal that always succeeds"
    assert set(succeed(Substitution())) == {Substitution()}


def test_disj():
    x = variables("x")
    goal = disj(same("olive", x),
                same("oil", x))
    assert set(goal(Substitution())) == {Substitution({x: "olive"}), Substitution({x: "oil"})}


def test_make_goal():
    x = variables("x")

    @make_goal
    def positive_integers(s, var):
        i = 0
        while True:
            yield s.unify(var, i)
            i += 1

    goal = positive_integers(x)

    for i, s in zip(range(10), goal(Substitution())):
        assert s == Substitution({x: i})


def test_never():
    goal = never
    s_inf = goal(Substitution())
    s_inf = assert_suspended(s_inf)
    s_inf = assert_suspended(s_inf)
    s_inf = assert_suspended(s_inf)


def test_disj_x_never():
    x = variables("x")
    goal = disj(same("olive", x),
                never)
    s_inf = goal(Substitution())
    assert next(s_inf) == Substitution({x: "olive"})
    s_inf = assert_suspended(s_inf)
    s_inf = assert_suspended(s_inf)


def test_disj_never_x():
    x = variables("x")
    goal = disj(never,
                same("olive", x))
    s_inf = goal(Substitution())
    s_inf = assert_suspended(s_inf)
    assert next(s_inf) == Substitution({x: "olive"})
    s_inf = assert_suspended(s_inf)
    s_inf = assert_suspended(s_inf)


def test_always():
    s_inf = always(Substitution())
    assert next(s_inf) == Substitution()
    assert next(s_inf) == Substitution()
    assert next(s_inf) == Substitution()


def test_take_always():
    s_inf = always(Substitution())
    results = take(3, s_inf)
    assert list(results) == [Substitution()] * 3


def test_conj():
    x = variables("x")
    goal = conj(same("olive", x),
                same("oil", x))
    assert set(goal(Substitution())) == set()


def test_conj_succeed():
    x = variables("x")
    goal = conj(same(42, x), succeed)
    assert list(goal(Substitution())) == [Substitution({x: 42})]


def test_walk_deep():
    w, x, y, z = variables("w, x, y, z")
    sub = Substitution({x: "b", z: y, w: (x, "e", z)})
    value = sub.walk_deep(w)
    assert value == ("b", "e", y)


def test_walk_deep_map():
    w, x, y, z = variables("w, x, y, z")
    sub = Substitution({x: "b", z: y, w: Map().insert(x, z).insert("a", "b")})
    value = sub.walk_deep(w)
    assert value == Map().insert("b", y).insert("a", "b")


def test_reify():
    u, v, w, x, y, z = variables("u, v, w, x, y, z")
    sub = Substitution({x: (u, w, y, z, (["ice"], z)),
                        y: "corn",
                        w: (v, u)})
    re = sub.reify(x)
    assert re == ('_0', ('_1', '_0'), 'corn', '_2', (['ice'], '_2'))


def test_reify_some():
    x = variables("x")
    goal = disj(same("olive", x),
                same("oil", x))
    subs = take(5, goal(Substitution()))
    results = map(reify(x), subs)
    assert list(results) == ['olive', 'oil']


def test_reify_run_goal():
    x = variables("x")
    goal = disj(same("olive", x),
                same("oil", x))
    results = map(reify(x), run_goal(5, goal))
    assert list(results) == ['olive', 'oil']


def test_reify_run_goal_inf():
    x = variables("x")
    goal = disj(same("olive", x),
                same("oil", x))
    results = map(reify(x), run_goal(goal))
    assert list(results) == ['olive', 'oil']


def test_ifte_succeed():
    y = variables("y")
    goal = ifte(succeed,
                same(False, y),
                same(True, y))
    assert list(goal(Substitution())) == [Substitution({y: False})]


def test_ifte_FAIL():
    y = variables("y")
    goal = ifte(fail,
                same(False, y),
                same(True, y))
    assert list(goal(Substitution())) == [Substitution({y: True})]


def test_ifte_combine():
    x, y = variables("x, y")
    goal = ifte(same(True, x),
                same(False, y),
                same(True, y))
    assert list(goal(Substitution())) == [Substitution({x: True, y: False})]


def test_ifte_disj():
    x, y = variables("x, y")
    goal = ifte(disj(same(True, x), same(False, x)),
                same(False, y),
                same(True, y))
    assert list(goal(Substitution())) == [Substitution({x: True, y: False}),
                                          Substitution({x: False, y: False})]


def test_ifte_once_disj():
    x, y = variables("x, y")
    goal = ifte(once(disj(same(True, x), same(False, x))),
                same(False, y),
                same(True, y))
    assert list(goal(Substitution())) == [Substitution({x: True, y: False})]


def test_ifte_cond_suspended():
    y = variables("y")
    goal = ifte(suspend(succeed),
                same(False, y),
                same(True, y))
    s_inf = goal(Substitution())
    s_inf = assert_suspended(s_inf)
    assert list(s_inf) == [Substitution({y: False})]


def test_ifte_g1_suspended():
    y = variables("y")
    goal = ifte(succeed,
                suspend(same(False, y)),
                same(True, y))
    s_inf = goal(Substitution())
    s_inf = assert_suspended(s_inf)
    assert list(s_inf) == [Substitution({y: False})]


def test_ifte_g2_suspended():
    y = variables("y")
    goal = ifte(fail,
                same(False, y),
                suspend(same(True, y)))
    s_inf = goal(Substitution())
    s_inf = assert_suspended(s_inf)
    assert list(s_inf) == [Substitution({y: True})]


def test_symeq_solve_easy():
    x = variables("x")
    goal = symeq(x ** 2, 9)
    assert set(goal(Substitution())) == {Substitution({x: -3}), Substitution({x: 3})}


def test_symeq_solve_hard():
    x = variables("x")
    goal = symeq(x ** 2, x)
    assert set(goal(Substitution())) == {Substitution({x: 0}), Substitution({x: 1})}


def test_listo():
    reset_names()
    x = variables("x")
    goal = listo(x)
    s_inf = goal(Substitution())
    v1 = variables('__1')
    v2 = variables('__2')
    v3 = variables('__3')
    assert next(s_inf) == Substitution({x: []})
    assert next(s_inf) == Substitution({x: [v1]})
    assert next(s_inf) == Substitution({x: [v1, v2]})
    assert next(s_inf) == Substitution({x: [v1, v2, v3]})
