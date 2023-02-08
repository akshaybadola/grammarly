import atexit
import json
from threading import Thread

import requests
import websocket

from flask import Flask, request
from werkzeug import run_simple


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
        self._current_text = "INIT"
        self._messages = {self._current_text: []}
        self.init_msg = self.init_msg.copy()
        if opts:
            self.init_msg = {**self.init_msg, **opts}
        self.authenticate()
        self.start_grammarly_ws()
        self.closed = False
        self._app = Flask("Grammarly")
        self._init_routes()
        atexit.register(self.close)

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

    @property
    def keys(self):
        return [*self._messages.keys()]

    def authenticate(self):
        print("Authenticating grammarly app")
        response = requests.get(self._auth_url, headers=self.auth_headers)
        if response.status_code == 200:
            print("Successfully authenticated")
        else:
            raise ValueError("Could not authenticate")
        self.cookie = response.cookies

    def start_grammarly_ws(self):
        self.closed = False
        print("Starting grammarly app")
        self._current_text = "INIT"
        self.ws = websocket.WebSocketApp("wss://capi.grammarly.com/freews",
                                         header=self.ws_headers,
                                         on_open=self.send_init_msg,
                                         on_message=self.on_message,
                                         on_close=self.on_close)
        self.ws_thread = Thread(target=self.ws.run_forever)
        self.ws_thread.start()
        print("Started grammarly app")

    def on_close(self, app, status, message):
        print("App closed", status)
        if message:
            print(message)
        print("self.closed", self.closed)
        if not self.closed:
            self.start_grammarly_ws()

    def on_message(self, app, message):
        self._messages[self._current_text].append(json.loads(message))

    def send_init_msg(self, app):
        self.ws.send(json.dumps(self.init_msg))

    def send_check_request(self, text):
        self._current_text = text
        self._messages[self._current_text] = []
        req_dict = {"ch": [f"+0:0:{text}:0"],
                    "rev": 0,
                    "action": "submit_ot",
                    "id": self.id}
        self.ws.send(json.dumps(req_dict))

    def get_alerts_for_check_number(self, num: int):
        submit_count = 0
        beg = None
        end = None
        check_beg = False
        check_end = False
        for i, msg in enumerate(self._messages):
            if msg["action"] == "start":
                submit_count += 1
                if submit_count == num:
                    check_beg = True
            if check_beg and msg["action"] == "alert":
                beg = i
                check_end = True
            else:
                if check_beg and check_end:
                    end = i
        return beg, end

    def close(self):
        self.closed = True
        self.ws.close()

    def get_check_text(self, request):
        if request.method == "GET":
            if "num" not in request.args:
                return json.dumps({"error": "Message num must be given"})
            else:
                try:
                    num = int(request.args.get("num"))
                except Exception as e:
                    return json.dumps({"error": f"{e}"})
            text = self.keys[num]
        else:
            data = request.json
            if "text" not in data:
                return json.dumps({"error": "No text given"})
            text = data["text"]
        return text

    def _init_routes(self):
        @self._app.route("/check", methods=["POST"])
        def __check():
            data = request.json
            text = data.get("text", None)
            if not text:
                return json.dumps({"error": "No text given"})
            self.send_check_request(text)
            return json.dumps({"success": "Sent request"})

        @self._app.route("/check_finished", methods=["GET"])
        def __messages():
            text = self.get_check_text(request)
            return json.dumps(any("finished" in x for x in self._messages[text]))

        @self._app.route("/results", methods=["GET", "POST"])
        def __last_check_results():
            text = self.get_check_text(request)
            return self._messages[text]

    def start(self):
        run_simple(self.hostname, self.port, self._app, threaded=True)


if __name__ == '__main__':
    app = GrammarlyApp()
    check_text = "Hellow world!!!"
    app.send_check_request(check_text)
