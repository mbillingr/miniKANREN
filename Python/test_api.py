from api import run
from core import variables
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
