"""Patch dpkt 1.9.8 for the direct tests of cispa/http-conformance.

The upstream script fix_dpkt.sh does the same 4 edits with BSD sed. It gives
incorrect results with GNU sed and GNU grep. This script does the edits in
Python. It makes sure that each original line is present before it changes
the line. Then it does a test of the patched module in a new interpreter.

Run it with the Python of the upstream virtual environment.
"""

import importlib.util
import os
import subprocess
import sys

# (line number, original line, new lines)
EDITS = [
    (
        103,
        "            self.unpack(args[0])\n",
        ["            self.unpack(args[0], **kwargs)\n"],
    ),
    (
        178,
        "    def unpack(self, buf):\n",
        ["    def unpack(self, buf, **kwargs):\n"],
    ),
    (
        232,
        "    def unpack(self, buf):\n",
        ["    def unpack(self, buf, head_response=False, **kwargs):\n"],
    ),
    (
        252,
        "        Message.unpack(self, f.read(), is_body_allowed)\n",
        [
            "        if head_response:\n",
            "            is_body_allowed = False\n",
            "        Message.unpack(self, f.read(), is_body_allowed)\n",
        ],
    ),
]

CHECK = """
import dpkt
raw = b'HTTP/1.1 200 OK\\r\\nContent-Length: 3\\r\\n\\r\\nabc'
head = dpkt.http.Response(raw, head_response=True)
assert head.body == b'' and head.data == b'abc', (head.body, head.data)
get = dpkt.http.Response(raw)
assert get.body == b'abc' and get.data == b'', (get.body, get.data)
"""


def main():
    spec = importlib.util.find_spec("dpkt")
    path = os.path.join(spec.submodule_search_locations[0], "http.py")
    with open(path) as f:
        lines = f.readlines()
    # Edit from the last line to the first, so that the line numbers of the
    # edits that remain do not change.
    for number, original, new in sorted(EDITS, reverse=True):
        actual = lines[number - 1]
        assert actual == original, f"{path}:{number}: {actual!r} != {original!r}"
        lines[number - 1 : number] = new
    with open(path, "w") as f:
        f.writelines(lines)
    subprocess.run([sys.executable, "-c", CHECK], check=True)
    print(f"patched {path}")


if __name__ == "__main__":
    main()
