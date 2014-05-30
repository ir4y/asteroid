import pytest
import json
import uuid
import redis
from websocket import create_connection


@pytest.fixture(scope="module")
def rc(request):
    pool = redis.ConnectionPool(host='localhost', port=6379, db=0)
    rc = redis.Redis(connection_pool=pool)

    def fin():
        pool.disconnect()
    request.addfinalizer(fin)
    return rc


@pytest.fixture(scope="module")
def ws(request):
    ws = create_connection("ws://localhost:8008/bullet")

    def fin():
        ws.close()
    request.addfinalizer(fin)
    return ws


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
    task_uuid_1 = call_celery(ws, 'tasks.add_delay', 1, 1)
    task_uuid_2 = call_celery(ws, 'tasks.add', 2, 2)

    result = json.loads(ws.recv())
    assert result['uuid'] == task_uuid_2
    assert result['result'] == 4
    assert result['status'] == 'SUCCESS'

    result = json.loads(ws.recv())
    assert result['uuid'] == task_uuid_1
    assert result['result'] == 2
    assert result['status'] == 'SUCCESS'


def test_redis(ws, rc):
    ws.send('{"function":"subscribe_to","resource":"pubsub","uuid":"subscribe_uuid","arguments":["test_channel"]}')
    result = json.loads(ws.recv())
    assert result['uuid'] == 'subscribe_uuid'
    assert result['result'] == 'ok'
    assert result['status'] == 'SUCCESS'
    rc.publish('test_channel','{"foo":"bar"}')
    result = json.loads(ws.recv())
    assert result['uuid'] == 'subscribe_uuid'
    assert result['foo'] == 'bar'
