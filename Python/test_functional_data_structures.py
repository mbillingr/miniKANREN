import copy

from pytest import raises

from functional_data_structures import Map


def test_empty_mapping():
    assert Map().is_empty()


def test_map_insert_returns_map():
    mapping = Map().insert("key", "value")
    assert isinstance(mapping, Map)


def test_empty_map_deepcopy_returns_same_object():
    map0 = Map()
    map1 = copy.deepcopy(map0)
    assert map0 is map1


def test_map_deepcopy_returns_different_object_with_same_items():
    map0 = Map().insert("key", "value")
    map1 = copy.deepcopy(map0)
    assert map0 is not map1

    items_a = set(map0)
    items_b = set(map1)
    assert items_a == items_b


def test_map_insert_does_not_modify_old_map():
    mapping1 = Map().insert("one", 1)
    mapping1_backup = copy.deepcopy(mapping1)

    mapping2 = mapping1.insert("two", 2)
    assert mapping2 is not mapping1

    items_a = set(mapping1)
    items_b = set(mapping1_backup)
    assert items_a == items_b


def test_looking_up_in_the_empty_map_raises():
    mapping = Map()
    with raises(KeyError):
        mapping.lookup("key")


def test_looking_up_existing_key_returns_value():
    mapping = Map().insert("key", "value")
    val = mapping.lookup("key")
    assert val == "value"


def test_map_with_elements_inserted_is_not_empty():
    mapping = Map().insert("key", "value")
    assert not mapping.is_empty()


def test_can_look_up_a_value_after_inserting_another():
    mapping = Map()
    mapping = mapping.insert("one", 1)
    mapping = mapping.insert("two", 2)
    assert mapping.lookup("two") == 2
    assert mapping.lookup("one") == 1


def test_map_is_immutable():
    mapping = Map().insert("one", 1)
    with raises(TypeError):
        mapping.key = 0
    with raises(TypeError):
        mapping[0] = 0


def test_empty_map_supports_iteration():
    assert list(Map()) == []


def test_map_iterates_over_all_association():
    items = {("one", 1), ("two", 2)}
    mapping = Map()
    for key, value in items:
        mapping = mapping.insert(key, value)
    mapping_iter = iter(mapping)
    assert set(mapping_iter) == items


def test_all_empty_maps_share_an_instance():
    assert Map() is Map()


def test_remove_nonexisting_key_raises():
    mapping = Map().insert("key", "value")
    with raises(KeyError):
        mapping.remove("foo")


def test_remove_item():
    mapping0 = Map()
    mapping1 = mapping0.insert("one", 1)
    mapping2 = mapping1.insert("two", 2)
    mapping3 = mapping2.insert("three", 3)

    mapping = mapping3.remove("two")

    assert set(mapping) == {("one", 1), ("three", 3)}
    assert set(mapping3) == {("one", 1), ("two", 2), ("three", 3)}
    assert set(mapping2) == {("one", 1), ("two", 2)}
    assert set(mapping1) == {("one", 1)}
    assert set(mapping0) == set()
