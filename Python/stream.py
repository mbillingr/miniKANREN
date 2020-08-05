"""
A stream is an iterator that may raise a suspension.
A suspension is an Exception that holds a stream.

The above definition is an implementation of the following abstract definition:
> A stream produces items or ends with a suspension.
> A suspension produces a stream.

Which is a generalization of the MiniKANREN stream:
> A stream is the empty list, a pair whose cdr is a stream, or a suspension.
> A suspension is a function that returns a stream when invoked without arguments.


"""


class SuspendIteration(Exception):
    def __init__(self, stream):
        self.stream = stream


def take(n, stream):
    for _, item in zip(range(n), take_inf(stream)):
        yield item


def take_inf(stream):
    while True:
        try:
            yield from stream
            return
        except SuspendIteration as suspension:
            stream = suspension.stream


def append_inf(stream_a, stream_b):
    while True:
        try:
            a = next(stream_a)
            yield a
        except StopIteration:
            yield from stream_b
            return
        except SuspendIteration as suspension:
            raise SuspendIteration(append_inf(stream_b, suspension.stream))
        stream_a, stream_b = stream_b, stream_a


def append_map_inf(func, stream):
    try:
        s = next(stream)
    except StopIteration:
        return
    except SuspendIteration as suspension:
        raise SuspendIteration(append_map_inf(func, suspension.stream))

    yield from append_inf(func(s), append_map_inf(func, stream))
