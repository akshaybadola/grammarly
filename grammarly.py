from typing import List, Dict, Union
import argparse
import atexit
import json
from threading import Thread

import requests
import websocket

from flask import Flask, request
from werkzeug import run_simple


class GrammarlyApp:
    user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:68.0) Gecko/20100101 Firefox/68.0"
    cookie_keys = ["gnar_containerId", "funnelType",
                   "browser_info", "csrf-token",
                   "grauth", "redirect_location"]
    init_msg = {'type': 'initial',
                'token': None,
                'docid': 'dfad0927-7b35-e155-6de9-4a107053da35-43543554345',
                'client': 'extension_chrome',
                'protocolVersion': '1.0',
                'clientSupports': ["free_clarity_alerts",
                                   "readability_check",
                                   "filler_words_check",
                                   "sentence_variety_check",
                                   "free_occasional_premium_alerts"],
                'dialect': 'american',
                'clientVersion': '14.924.2437',
                "extDomain": "editpad.org",
                'action': 'start',
                'id': 0}
    auth_headers = {"User-Agent": None,
                    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3",
                    "Accept-Language": "en-GB,en-US;q=0.9,en;q=0.8",
                    "Cache-Control": "no-cache",
                    "Pragma": "no-cache",
                    "Sec-Fetch-Mode": "navigate",
                    "Sec-Fetch-Sit": "same-origin",
                    "Sec-Fetch-User": "?1",
                    "Upgrade-Insecure-Requests": "1",
                    "Referer": "https://www.grammarly.com/"}

    def __init__(self, hostname: str, port: int, access_token=None, opts=None,
                 check_text=None, debug=False):
        self._auth_url = "https://grammarly.com/signin"
        self.cookie = None
        self.ws = None
        self._id = 0
        self.accces_token = access_token
        self._current_text = "INIT"
        self._messages: Dict[str, List[Dict]] = {self._current_text: []}
        self.hostname = hostname
        self.port = port
        self._debug = debug
        self._check_text = check_text
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
            self.authenticate()
        return "; ".join([f"{k}={self.cookie[k]}" for k in self.cookie_keys])

    @property
    def ws_headers(self) -> Dict[str, str]:
        headers = {"origin": "chrome-extension://kbfnbcaeplbcioakkpcpgfkobkghlhen",
                   "Cookie": self.cookie_str,
                   "User-Agent": self.user_agent}
        return headers

    @property
    def id(self):
        self._id += 1
        return self._id

    @property
    def keys(self):
        return [*self._messages.keys()]

    def debug(self, *msgs):
        if self._debug:
            for msg in msgs:
                print(msg)

    def authenticate(self):
        """Authenticating Grammarly App
        """
        print("Authenticating grammarly app")
        response = requests.get(self._auth_url, headers=self.auth_headers)
        if response.status_code == 200:
            print("Successfully authenticated")
        else:
            raise ValueError("Could not authenticate")
        self.cookie = response.cookies

    def start_grammarly_ws(self):
        """Start Grammarly WebsocketApp
        """
        self.closed = False
        print("Starting grammarly app")
        self._current_text = "INIT"
        self.ws = websocket.WebSocketApp("wss://capi.grammarly.com/freews",
                                         header=self.ws_headers,
                                         # on_open=self.send_init_msg,
                                         on_message=self.on_message,
                                         on_close=self.on_close)
        self.ws_thread = Thread(target=self.ws.run_forever)
        self.ws_thread.start()
        print("Started grammarly app")

    def on_close(self, app, status, message):
        """Close callback for Grammarly websocket

        Args:
            app: app context for Grammarly websocket
            status: Status
            message: Message received when closing


        """
        print("App closed", status)
        if message:
            print(message)
        print("self.closed", self.closed)
        if not self.closed:
            self.start_grammarly_ws()

    def on_message(self, app: Flask, message: str):
        """Message callback for Grammarly websocket

        Args:
            app: app context for Grammarly websocket
            message: Message received from Grammarly websocket


        """
        if self.ws is None:
            raise TypeError("Websocket not initialized")
        else:
            self.debug("Got Message", json.loads(message))
            self._messages[self._current_text].append(json.loads(message))

    def send_init_msg(self, app: Flask):
        """Send init message to Grammarly websocket

        Args:
            app: app context for Grammarly websocket

        """
        if self.ws is None:
            raise TypeError("Websocket not initialized")
        else:
            self.debug("Sending init_message", self.init_msg)
            self.ws.send(json.dumps(self.init_msg))

    def send_check_request(self, text: str):
        """Send `text` to Grammarly websocket

        Args:
            text: Text to check

        """
        self._current_text = text
        self._messages[self._current_text] = []
        req_dict = {"ch": [f"+0:0:{text}:0"],
                    "rev": 0,
                    "action": "submit_ot",
                    "id": 0}
                    # "sessionUuid": self._messages["INIT"][-1]["sessionUuid"]}
        self.debug("Requesting check", req_dict)
        if self.ws is None:
            raise TypeError("Websocket not initialized")
        else:
            self.ws.send(json.dumps(req_dict))

    def get_alerts_for_check_number(self, key: str, num: int):
        submit_count = 0
        beg = None
        end = None
        check_beg = False
        check_end = False
        for i, msg in enumerate(self._messages[key]):
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

    def _text(self, check_text_or_num: Union[str, int]) -> str:
        if isinstance(check_text_or_num, int):
            text = self.keys[check_text_or_num]
        else:
            text = check_text_or_num
        return text

    def reset_result(self, check_text_or_num: Union[str, int]):
        text = self._text(check_text_or_num)
        self._messages[text] = []

    def get_result(self, check_text_or_num: Union[str, int]) -> List[Dict]:
        text = self._text(check_text_or_num)
        return self._messages[text]

    def get_check_text(self, request):
        """Subroutine to get check text depending on the request type

        Args:
            request: an instance of :code:`request`


        """
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
        """Initialize Routes
        """
        @self._app.route("/check", methods=["POST"])
        def __check():
            data = request.json
            text = data.get("text", None)
            if not text:
                return json.dumps({"error": "No text given"})
            self.send_check_request(text)
            return json.dumps({"success": "Sent request"})

        @self._app.route("/results", methods=["GET", "POST"])
        def __last_check_results():
            text = self.get_check_text(request)
            return json.dumps(self._messages.get(text))

        @self._app.route("/check_finished", methods=["GET", "POST"])
        def __messages():
            text = self.get_check_text(request)
            finished = False
            if text in self._messages:
                finished = any([('action', 'finished') in x.items()
                                for x in self._messages[text]])
            return json.dumps(finished)

    def start(self):
        run_simple(self.hostname, self.port, self._app, threaded=True)


if __name__ == '__main__':
    # app = GrammarlyApp()
    # check_text = "Hellow world!!!"
    # app.send_check_request(check_text)
    parser = argparse.ArgumentParser()
    parser.add_argument("hostname")
    parser.add_argument("port", type=int)
    args = parser.parse_args()
    app = GrammarlyApp(args.hostname, args.port)
    app.start()
