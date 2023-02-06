import json
from threading import Thread

import requests
import websocket


class GrammarlyApp:
    init_msg = {'type': 'initial',
                'token': None,
                'docid': 'dfad0927-7b35-e155-6de9-4a107053da35-43543554345',
                'client': 'extension_chrome',
                'protocolVersion': '1.0',
                'clientSupports': ['full_sentence_rewrite_card',
                                   'free_clarity_alerts',
                                   'text_info',
                                   'tone_cards',
                                   'vox_check'
                                   'readability_check',
                                   'filler_words_check',
                                   'sentence_variety_check',
                                   'free_occasional_premium_alerts'],
                'dialect': 'american',
                'clientVersion': '14.924.2437',
                'action': 'start',
                'id': 0}
    auth_headers = {"User-Agent": None,
                    "Accept": "text/html,application/xhtml+xml,application/xml;"
                    "q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3",
                    "Accept-Language": "en-GB,en-US;q=0.9,en;q=0.8",
                    "Cache-Control": "no-cache",
                    "Pragma": "no-cache",
                    "Sec-Fetch-Mode": "navigate",
                    "Sec-Fetch-Sit": "same-origin",
                    "Sec-Fetch-User": "?1",
                    "Upgrade-Insecure-Requests": "1",
                    "Referer": "https://www.grammarly.com/"}

    def __init__(self, access_token=None, opts=None):
        self._auth_url = "https://grammarly.com/signin"
        self.cookie = None
        self.ws = None
        self._id = 0
        self.accces_token = access_token
        self._messages = []
        self.init_msg = self.init_msg.copy()
        if opts:
            self.init_msg = {**self.init_msg, **opts}
        self.authenticate()
        self.start()
        self.closed = False

    @property
    def cookie_str(self):
        if self.cookie is None:
            self.fetch_cookie()
        return ";".join([f"{k}={v}" for k, v in self.cookie.items()])

    @property
    def ws_headers(self):
        return {"origin": "chrome-extension://kbfnbcaeplbcioakkpcpgfkobkghlhen",
                "Cookie": self.cookie_str,
                "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) "
                "Gecko/20100101 Firefox/68.0"}

    @property
    def id(self):
        self._id += 1
        return self._id

    def authenticate(self):
        print("Authenticating grammarly app")
        response = requests.get(self._auth_url, headers=self.auth_headers)
        if response.status_code == 200:
            print("Successfully authenticated")
        else:
            raise ValueError("Could not authenticate")
        self.cookie = response.cookies

    def start(self):
        self.closed = False
        print("Starting grammarly app")
        self.ws = websocket.WebSocketApp("wss://capi.grammarly.com/freews",
                                         header=self.ws_headers,
                                         on_open=self.send_init_msg,
                                         on_message=self.on_message,
                                         on_close=self.on_close)
        self.t = Thread(target=self.ws.run_forever)
        self.t.start()
        print("Started grammarly app")

    def on_close(self, app, status, message):
        print("App closed", status)
        if message:
            print(message)
        print("self.closed", self.closed)
        if not self.closed:
            self.start()

    def on_message(self, app, message):
        self._messages.append(json.loads(message))

    def send_init_msg(self, app):
        self.ws.send(json.dumps(self.init_msg))
        return self.ws.recv()

    def send_check_request(self, text):
        req_dict = {"ch": [f"+0:0:{text}:0"],
                    "rev": 0,
                    "action": "submit_ot",
                    "id": self.id}
        self.ws.send(json.dumps(req_dict))

    def close(self):
        self.closed = True
        self.ws.close()


if __name__ == '__main__':
    app = GrammarlyApp()
    check_text = "Hellow world!!!"
    app.send_check_request(check_text)
