from copy import deepcopy


class Singleton:
    """Class with a single instance"""

    def __new__(cls):
        obj = object.__new__(cls)
        cls.__new__ = lambda _: obj
        return obj


class NullMap(Singleton):
    """The empty Map"""

    @staticmethod
    def is_empty():
        return True

    def insert(self, key, value):
        return Map(key, value, self)

    @staticmethod
    def lookup(key):
        raise KeyError(key)

    @staticmethod
    def remove(key):
        raise KeyError(key)

    @staticmethod
    def __iter__():
        return iter([])

    @staticmethod
    def __len__():
        return 0

    @classmethod
    def __map__(cls, _func):
        return cls()


class Map(tuple):
    """Functional mapping of keys to values.

    This container type is immutable and persistent.
    """

    def __new__(cls, key=None, value=None, tail=None):
        if key is None:
            return NullMap()
        return super().__new__(cls, (key, value, tail))

    @staticmethod
    def is_empty():
        return False

    def insert(self, key, value):
        return Map(key, value, self)

    def lookup(self, key):
        if self._key == key:
            return self._value
        return self._next.lookup(key)

    def remove(self, key):
        if self._key == key:
            return self._next
        return Map(self._key, self._value, self._next.remove(key))

    @property
    def _key(self):
        return self[0]

    @property
    def _value(self):
        return self[1]

    @property
    def _next(self):
        return self[2]

    def __iter__(self):
        yield self._key, self._value
        yield from iter(self._next)

    def __len__(self):
        return 1 + len(self._next)

    def __setattr__(self, *args, **kwargs):
        raise TypeError("'{}' object does not support item assignment".format(type(self).__name__))

    def __deepcopy__(self, memodict={}):
        return Map(deepcopy(self._key, memo=memodict),
                   deepcopy(self._value, memo=memodict),
                   deepcopy(self._next, memo=memodict))

    def __repr__(self):
        return '{' + ', '.join('{}:= {}'.format(k, v) for k, v in self) + '}'

    def __map__(self, func):
        return Map(func(self._key),
                   func(self._value),
                   self._next.__map__(func))
