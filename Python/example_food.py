from api import run, fresh, defrel
from core import variables, Substitution, reset_names, is_var, Variable
from goals import make_goal, fail, same, succeed, disj, conj, listo


def food_type(food, type):
    return disj(
        same((food, type), ("gouda", "cheese")),
        same((food, type), ("ritz", "cracker")),
        same((food, type), ("steak", "meat")),
        same((food, type), ("sausage", "meat")),
        same((food, type), ("limonade", "juice")),
        same((food, type), ("cookie", "dessert")),
    )


def flavor(flav, type):
    return disj(
        same((flav, type), ("sweet", "dessert")),
        same((flav, type), ("savory", "meat")),
        same((flav, type), ("savory", "cheese")),
        same((flav, type), ("sweet", "juice")),
    )


def food_flavor(food, flav):
    type = Variable("-")
    return conj(food_type(food, type), flavor(flav, type))


if __name__ == "__main__":
    what = variables('what')

    results = run(what, food_flavor(what, "sweet"))

    for r in results:
        print(r)