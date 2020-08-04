from api import run, fresh
from core import variables, Substitution
from goals import fail, same, succeed


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
    goal = fresh(lambda x: same(x, 1))
    r = goal(Substitution())
    assert list(r) == [Substitution({variables('x'): 1})]


def test_fresh_multiple_vars():
    goal = fresh(lambda x, y, z: same(x, y))
    r = goal(Substitution())
    assert list(r) == [Substitution({variables('x'): variables('y')})]


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
