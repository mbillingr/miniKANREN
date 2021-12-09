from api import run, fresh, defrel
from core import variables, Substitution, reset_names, is_var, Variable, gen_var
from goals import make_goal, fail, same, succeed, disj, conj, listo

import sys
sys.setrecursionlimit(1000)


var = gen_var


def house(owner=None, smokes=None, drinks=None, pet=None, color=None):
    owner = owner or var()
    smokes = smokes or var()
    drinks = drinks or var()
    pet = pet or var()
    color = color or var()
    return owner, smokes, drinks, pet, color


@make_goal
def right(s, r, l, lst):
    lst = s.walk_var(lst)

    neighbors = list(zip(lst, lst[1:]))

    yield from membero((l, r), neighbors)(s)


def next_to(a, b, lst):
    return disj(right(a, b, lst), right(b, a, lst))


@make_goal
def membero(s, item, list):
    list = s.walk_var(list)

    if is_var(list):
        raise NotImplementedError()

    for x in list:
        yield from same(item, x)(s)


if __name__ == "__main__":
    houses = var()

    riddle = (
        # There are five houses.
        same([var(), var(), var(), var(), var()], houses),

        # The Englishman lives in the red house.
        membero(house(owner="Englishman", color="red"), houses),

        # The Spaniard owns the dog.
        membero(house(owner="Spaniard", pet="dog"), houses),

        # Coffee is drunk in the green house.
        membero(house(drinks="coffee", color="green"), houses),

        # The Ukrainian drinks tea.
        membero(house(owner="Ukrainian", drinks="tea"), houses),

        # The green house is immediately to the right of the ivory house.
        right(house(color="green"), house(color="ivory"), houses),

        # The Old Gold smoker owns snails.
        membero(house(smokes="Old Gold", pet="snails"), houses),

        # Kools are smoked in the yellow house.
        membero(house(smokes="Kools", color="yellow"), houses),

        # Milk is drunk in the middle house.
        same([var(), var(), house(drinks="milk"), var(), var()], houses),

        # The Norvegian lives in the first house.
        same([house(owner="Norwegian"), var(), var(), var(), var()], houses),

        # The person who smokes Chesterfields lives in the house next to the person with the fox.
        next_to(house(smokes="Chesterfields"), house(pet="fox"), houses),

        # Kools are smoked in the house next to the house where the horse is kept.
        next_to(house(smokes="Kools"), house(pet="horse"), houses),

        # The Lucky Strike smoker drinks orange juice
        membero(house(smokes="Lucky Strike", drinks="orange juice"), houses),

        # The Japanese smokes Parliaments
        membero(house(owner="Japanese", smokes="Parliaments"), houses),

        # The Norwegian lives next to the blue house
        next_to(house(owner="Norwegian"), house(color="blue"), houses),

        # Who drinks water?  Who owns the zebra?
        membero(house(drinks="water"), houses),
        membero(house(pet="zebra"), houses),
    )

    results = run(houses, *riddle)

    for r in results:
        print(r)