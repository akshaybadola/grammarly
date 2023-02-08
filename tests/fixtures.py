import pytest

import time
from grammarly import GrammarlyApp


@pytest.fixture(scope="session")
def app():
    app = GrammarlyApp()
    time.sleep(1)
    yield app
    app.close()
