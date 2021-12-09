import pytest

from stream import SuspendIteration, take_inf, append_inf, append_map_inf


def suspended_numbers(n):
    if n == 0:
        return
    yield n
    raise SuspendIteration(suspended_numbers(n - 1))


def powers(n, p=4):
    if p == 0:
        return
    yield n ** p
    raise SuspendIteration(powers(n, p - 1))


class SuspendedNumbers:
    def __init__(self, n):
        self.n = n
        self.spent = False

    def __next__(self):
        if self.spent:
            raise SuspendIteration(SuspendedNumbers(self.n - 1))
        if self.n == 0:
            raise StopIteration()
        self.spent = True
        return self.n

    def __iter__(self):
        return self

    def __repr__(self):
        return 'SN({} {})'.format(self.n, self.spent)


class SuspendedPowers:
    def __init__(self, n, p=3):
        self.n = n
        self.p = p
        self.spent = False

    def __next__(self):
        if self.spent:
            raise SuspendIteration(SuspendedPowers(self.n, self.p - 1))
        if self.p == 0:
            raise StopIteration()
        self.spent = True
        return self.n ** self.p

    def __iter__(self):
        return self

    def __repr__(self):
        return 'SP({}**{} {})'.format(self.n, self.p, self.spent)


def test_suspension():
    stream = suspended_numbers(1)
    assert next(stream) == 1
    with pytest.raises(SuspendIteration):
        next(stream)


def test_take_inf():
    stream = suspended_numbers(3)
    numbers = take_inf(stream)
    assert list(numbers) == [3, 2, 1]


def test_append_inf_equal_length():
    stream_a = suspended_numbers(2)
    stream_b = suspended_numbers(2)
    numbers = append_inf(stream_a, stream_b)
    assert list(take_inf(numbers)) == [2, 2, 1, 1]


def test_append_inf_b_is_longer():
    stream_a = suspended_numbers(2)
    stream_b = suspended_numbers(4)
    numbers = append_inf(stream_a, stream_b)
    assert list(take_inf(numbers)) == [2, 4, 1, 3, 2, 1]


def test_append_inf_a_is_longer():
    stream_a = suspended_numbers(4)
    stream_b = suspended_numbers(2)
    numbers = append_inf(stream_a, stream_b)
    assert list(take_inf(numbers)) == [4, 2, 3, 1, 2, 1]


def test_append_map_inf():
    numbers = suspended_numbers(5)
    num_powers = append_map_inf(powers, numbers)
    result = list(take_inf(num_powers))
    print(result)
    assert set(result[:1]) == {625}
    assert set(result[1:3]) == {125, 256}
    assert set(result[3:6]) == {25, 64, 81}
    assert set(result[6:10]) == {5, 16, 27, 16}
    assert set(result[10:14]) == {4, 9, 8, 1}
    assert set(result[14:17]) == {3, 4, 1}
    assert set(result[17:19]) == {2, 1}
    assert set(result[19:]) == {1}
