from api import run, fresh, defrel
from core import variables, Substitution, reset_names
from goals import fail, same, succeed, disj


def test_run_fail():
    q = variables('q')
    assert list(run(q, fail)) == []


def test_run_same():
    q = variables('q')
    assert list(run(q, same(q, 'pea'))) == ['pea']


def test_run_same_order_invariant():
    q = variables('q')
    assert list(run(q, same(q, 'pea'))) == list(run(q, same('pea', q)))


def test_run_succeed():
    q = variables('q')
    assert list(run(q, succeed)) == ['_0']


def test_fresh_no_vars():
    goal = fresh(lambda: succeed)
    r = goal(Substitution())
    assert list(r) == [Substitution()]


def test_fresh_one_var():
    reset_names()
    goal = fresh(lambda x: same(x, 1))
    r = goal(Substitution())
    assert list(r) == [Substitution({variables('x1'): 1})]


def test_fresh_multiple_vars():
    reset_names()
    goal = fresh(lambda x, y, z: same(x, y))
    r = goal(Substitution())
    assert list(r) == [Substitution({variables('x1'): variables('y2')})]


def test_fresh_multiple_conditions():
    reset_names()
    goal = fresh(lambda x, y, z: (same(x, y), same(x, z)))
    r = goal(Substitution())
    x, y, z = variables('x1, y2, z3')
    allowed_results = [[Substitution({x: y, y: z})],
                       [Substitution({x: y, z: x})],
                       [Substitution({x: z, z: y})],
                       [Substitution({x: z, y: z})]]
    assert list(r) in allowed_results


def test_run_fresh():
    q = variables('q')
    r = run(q, fresh(lambda x: same([x], q)))
    assert list(r) == [['_0']]


def test_run_fresh_ignore_var():
    q = variables('q')
    r = run(q, fresh(lambda x: same(q, 'pea')))
    assert list(r) == ['pea']


def test_run_fresh_ignore_query_var():
    q = variables('q')
    r = run(q, fresh(lambda x: same(x, 'pea')))
    assert list(r) == ['_0']


def test_defrel():
    @defrel
    def teacupo(t):
        return disj(same('tea', t), same('cup', t))

    x = variables('x')
    r = run(x, teacupo(x))
    assert list(r) == ['tea', 'cup']


def test_local_fresh():
    @defrel
    def caro(l, a):
        return fresh(lambda d: (same(a, l[0]),
                                same(d, l[1:])))

    @defrel
    def cdro(l, d):
        return fresh(lambda a: (same(a, l[0]),
                                same(d, l[1:])))

    q = variables('q')
    l = ['a', 'c', 'o', 'r', 'n']

    assert list(run(q, caro(l, q))) == ['a']
    assert list(run(q, cdro(l, q))) == [['c', 'o', 'r', 'n']]


def test_recursion():
    @defrel
    def listo(l):
        return disj(nullo(l),
                    fresh(lambda d: (cdro(l, d),
                                     listo(d))))

    @defrel
    def nullo(x):
        return same(x, ())

    @defrel
    def cdro(l, d):
        return fresh(lambda a: same((a, d), l))

    q = variables('q')

    goal = listo(())
    r = run(3, q, goal)
    assert list(r) == ['_0']

    r = run(4, q, listo(q))
    assert list(r) == [(),
                       ('_0', ()),
                       ('_0', ('_1', ())),
                       ('_0', ('_1', ('_2', ())))]
