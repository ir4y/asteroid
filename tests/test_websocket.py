import pytest
import json
from websocket import create_connection


@pytest.fixture(scope="module")
def ws(request):
    ws = create_connection("ws://localhost:8008/bullet")

    def fin():
        ws.close()
    request.addfinalizer(fin)
    return ws


def test_echo(ws):
    data = {
        'resource': 'box',
        'function': 'echo',
        'arguments': ["Hello, World"]}
    ws.send(json.dumps(data))
    result = ws.recv()
    assert result == "Hello, World"


def test_create(ws):
    data = {
        'resource': 'box',
        'function': 'create',
        'arguments': [{'id': 1,
                       'user': 'ir4y',
                       'email': 'ir4y.ix@gmail.com'}]}
    ws.send(json.dumps(data))
    result = ws.recv()
    assert result == "ok"

    data = {
        'resource': 'box',
        'function': 'get',
        'arguments': [1]}
    ws.send(json.dumps(data))
    result = json.loads(ws.recv())
    assert result == {'id': 1,
                      'user': 'ir4y',
                      'email': 'ir4y.ix@gmail.com'}

def test_filter(ws):
    for index in range(10):
        username = "username_{0}".format(index)
        data = {
            'resource': 'box',
            'function': 'create',
            'arguments': [{'id': index,
                           'user': username,
                           'email': 'mail@gmail.com'}]}
        ws.send(json.dumps(data))
        result = ws.recv()
        assert result == "ok"

    for index in range(11, 20):
        username = "username_{0}".format(index)
        data = {
            'resource': 'box',
            'function': 'create',
            'arguments': [{'id': index,
                           'user': username,
                           'email': 'othermail@gmail.com'}]}
        ws.send(json.dumps(data))
        result = ws.recv()
        assert result == "ok"

    data = {
        'resource': 'box',
        'function': 'filter',
        'arguments': ["email","mail@gmail.com"]}
    ws.send(json.dumps(data))
    result = json.loads(ws.recv())
    assert len(result) == 10
    for index in range(10):
        assert result[index]['id'] == index
        assert result[index]['email'] == 'mail@gmail.com'
