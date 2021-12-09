# https://adventofcode.com/2021/day/8

import itertools

from api import run, fresh, defrel
from core import variables, Substitution, reset_names, is_var, gen_var
from goals import make_goal, fail, same, succeed, disj, conj, listo


import sys
sys.setrecursionlimit(10000)

var = gen_var


def parse_line(line):
    patterns, outputs = line.split(" | ")
    return patterns.split(), outputs.split()


def normalize_pattern(pat):
    for p in pat:
        yield tuple(1 if x in p else 0 for x in "abcdefg")


@make_goal
def permute_wires(s, segment_patterns, wire_patterns):
    for perm in itertools.permutations(range(7)):
        permuted_patterns = [tuple(wp[i] for i in perm) for wp in wire_patterns]
        yield from same(segment_patterns, permuted_patterns)(s)



@make_goal
def membero(s, item, list):
    list = s.walk_var(list)

    if is_var(list):
        raise NotImplementedError()

    for x in list:
        yield from same(item, x)(s)


if __name__ == '__main__':
    INPUTS = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

    input = INPUTS.splitlines(keepends=False)
    lines = list(map(parse_line, input))
    input_patterns = map(lambda x: tuple(normalize_pattern(x[0])), lines)
    output_patterns = map(lambda x: tuple(normalize_pattern(x[1])), lines)

    input_pattern = next(input_patterns)

    segment_patterns = variables("segment_patterns")
    wire_patterns = variables("wire_patterns")
    perm_wires = variables("perm_wires")

    goals = []

    wire_patterns = [
            (1, 1, 1, 0, 1, 1, 1),
            (0, 0, 1, 0, 0, 1, 0),
            (1, 0, 1, 1, 1, 0, 1),
            (1, 0, 1, 1, 0, 1, 1),
            (0, 1, 1, 1, 0, 1, 0),
            (1, 1, 0, 1, 0, 1, 1),
            (1, 1, 0, 1, 1, 1, 1),
            (1, 0, 1, 0, 0, 1, 0),
            (1, 1, 1, 1, 1, 1, 1),
            (1, 1, 1, 1, 1, 0, 1),
        ]

    goals += [
        permute_wires(segment_patterns, wire_patterns)
    ]

    for p in input_pattern:
        goals += [membero(p, segment_patterns)]

    results = run(segment_patterns, *goals)

    # unfortunately, this overflows the stack :(

    for r in results:
        print(r)
