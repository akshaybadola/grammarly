import pytest
import time


def test_grammarly_connects(app):
    assert app.cookie is not None
    assert app.ws is not None


def test_grammarly_checks(app):
    text = "Hellow world!!! Its me; Joe."
    while not app.ws and not app.ws.sock.connected:
        time.sleep(.5)
    time.sleep(.5)
    app.send_check_request(text)
    assert text in app.keys
    time.sleep(5)
    assert "finished" in app._messages[text][-1].values()
