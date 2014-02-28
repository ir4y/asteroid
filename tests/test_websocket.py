import pytest
import json
import uuid
from websocket import create_connection


@pytest.fixture(scope="module")
def ws(request):
    ws = create_connection("ws://localhost:8008/bullet")

    def fin():
        ws.close()
    request.addfinalizer(fin)
    return ws


def call_box(ws, function, *args):
    call_uuid = uuid.uuid1().get_hex()
    data = {
        'uuid': call_uuid,
        'resource': 'box',
        'function': function,
        'arguments': args}
    ws.send(json.dumps(data))
    return call_uuid


def assert_valid_responce(result, task_uuid, data):
    assert result.startswith(task_uuid)
    assert result[33:] == data


def test_echo(ws):
    task_uuid = call_box(ws, 'echo', "Hello, World")
    assert_valid_responce(ws.recv(), task_uuid, "Hello, World")


def test_create(ws):
    task_uuid = call_box(ws, 'create', {'id': 1,
                                        'user': 'ir4y',
                                        'email': 'ir4y.ix@gmail.com'})
    assert_valid_responce(ws.recv(), task_uuid, "ok")

    task_uuid = call_box(ws, 'get', 1)
    result = ws.recv()
    assert result.startswith(task_uuid)
    result = json.loads(result[33:])
    assert result == {'id': 1,
                      'user': 'ir4y',
                      'email': 'ir4y.ix@gmail.com'}


def test_filter(ws):
    for index in range(10):
        username = "username_{0}".format(index)
        task_uuid = call_box(ws, 'create', {'id': index,
                                            'user': username,
                                            'email': 'mail@gmail.com'})
        assert_valid_responce(ws.recv(), task_uuid, "ok")

    for index in range(11, 20):
        username = "username_{0}".format(index)
        task_uuid = call_box(ws, 'create', {'id': index,
                                            'user': username,
                                            'email': 'othermail@gmail.com'})
        assert_valid_responce(ws.recv(), task_uuid, "ok")

    task_uuid = call_box(ws, 'filter', "email", "mail@gmail.com")

    result = ws.recv()
    assert result.startswith(task_uuid)
    result = json.loads(result[33:])
    assert len(result) == 10
    for index in range(10):
        assert result[index]['id'] == index
        assert result[index]['email'] == 'mail@gmail.com'


def call_celery(ws, function, *args):
    call_uuid = uuid.uuid1().get_hex()
    data = {
        'uuid': call_uuid,
        'resource': 'celery',
        'function': function,
        'arguments': args}
    ws.send(json.dumps(data))
    return call_uuid


def test_celery(ws):
    task_uuid_1 = call_celery(ws, 'add_delay', 1, 1)
    task_uuid_2 = call_celery(ws, 'add', 2, 2)

    result = ws.recv()
    assert result.startswith(task_uuid_2)
    result = json.loads(result[33:])
    assert result == {'result': 4,
                      'status': 'SUCCESS'}

    result = ws.recv()
    assert result.startswith(task_uuid_1)
    result = json.loads(result[33:])
    assert result == {'result': 2,
                      'status': 'SUCCESS'}
