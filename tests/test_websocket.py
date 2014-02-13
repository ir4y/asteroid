import pytest
from websocket import create_connection


@pytest.fixture(scope="module")
def ws(request):
    ws = create_connection("ws://localhost:8008/bullet")

    def fin():
        ws.close()
    request.addfinalizer(fin)
    return ws


def test_echo(ws):
    ws.send("Hello, World")
    result = ws.recv()
    assert result == "Hello, World"
