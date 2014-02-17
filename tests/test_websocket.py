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
        'resource': None,
        'function': 'echo',
        'arguments': ["Hello, World"]}
    ws.send(json.dumps(data))
    result = ws.recv()
    assert result == "Hello, World"


def test_create(ws):
    data = {
        'resource': 'kvs_box',
        'function': 'create',
        'arguments': [{'id': 1,
                       'user': 'ir4y',
                       'email': 'ir4y.ix@gmail.com'}]}
    ws.send(json.dumps(data))
    result = ws.recv()
    assert result == "ok"

    data = {
        'resource': 'kvs_box',
        'function': 'get',
        'arguments': [1]}
    ws.send(json.dumps(data))
    result = json.loads(ws.recv())
    assert result == {'id': 1,
                      'user': 'ir4y',
                      'email': 'ir4y.ix@gmail.com'}
