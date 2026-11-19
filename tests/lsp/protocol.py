#!/usr/bin/env python3
"""Protocol tests for stackls. Usage: python3 tests/lsp/protocol.py PATH"""

import json
import subprocess
import sys
import threading
import time

SERVER = sys.argv[1]
TIMEOUT = 20
INIT_RESULT = {
    "capabilities": {},
    "serverInfo": {"name": "stackls", "version": "0.1.0"},
}
TESTS = []


def test(fn):
    TESTS.append(fn)
    return fn


def body(obj):
    return obj if isinstance(obj, bytes) else json.dumps(obj).encode()


def frame(obj, header=None):
    data = body(obj)
    head = header if header is not None else b"Content-Length: %d\r\n" % len(data)
    return head + b"\r\n" + data


def req(id, method, params=None):
    msg = {"jsonrpc": "2.0", "id": id, "method": method}
    if params is not None:
        msg["params"] = params
    return msg


def note(method, params=None):
    msg = {"jsonrpc": "2.0", "method": method}
    if params is not None:
        msg["params"] = params
    return msg


INIT = req(1, "initialize", {"processId": None, "rootUri": None, "capabilities": {}})


def ok(id, result):
    return {"jsonrpc": "2.0", "id": id, "result": result}


def err(id, code):
    return {"jsonrpc": "2.0", "id": id, "error": {"code": code}}


def frames(out):
    """Split stdout into messages. Anything that is not a valid frame fails."""
    msgs = []
    while out:
        head, sep, rest = out.partition(b"\r\n\r\n")
        assert sep and head.startswith(b"Content-Length: "), "bad frame: %r" % out[:80]
        n = int(head[len(b"Content-Length: "):])
        assert len(rest) >= n, "short frame: %r" % out[:80]
        msgs.append(json.loads(rest[:n].decode("utf-8")))
        out = rest[n:]
    return msgs


def run(*chunks, delay=0.0, close=True):
    """Send chunks to a new server. Returns exit code, messages and stderr.
    Without close, stdin stays open until the server exits by itself."""
    proc = subprocess.Popen([SERVER], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = {}

    def feed():
        try:
            for chunk in chunks:
                proc.stdin.write(chunk)
                proc.stdin.flush()
                time.sleep(delay)
            if close:
                proc.stdin.close()
        except (BrokenPipeError, ValueError):
            pass

    def drain(name, stream):
        result[name] = stream.read()

    threads = [threading.Thread(target=feed, daemon=True),
               threading.Thread(target=drain, args=("out", proc.stdout), daemon=True),
               threading.Thread(target=drain, args=("err", proc.stderr), daemon=True)]
    for th in threads:
        th.start()
    try:
        proc.wait(timeout=TIMEOUT)
    finally:
        if proc.poll() is None:
            proc.kill()
            proc.wait()
        for th in threads[1:]:
            th.join(TIMEOUT)
        if not proc.stdin.closed:
            try:
                proc.stdin.close()
            except BrokenPipeError:
                pass
    return proc.returncode, frames(result["out"]), result["err"]


def match(got, want):
    """Compare messages. An error matches on code; the message must be a string."""
    assert len(got) == len(want), "got %d messages, want %d: %r" % (len(got), len(want), got)
    for g, w in zip(got, want):
        if "error" in w:
            assert set(g) == {"jsonrpc", "id", "error"}, g
            assert g["jsonrpc"] == "2.0" and g["id"] == w["id"], g
            assert g["error"]["code"] == w["error"]["code"], g
            assert isinstance(g["error"]["message"], str), g
        else:
            assert g == w, "got %r, want %r" % (g, w)


def expect(chunks, want, code=0, **kw):
    status, got, errs = run(*chunks, **kw)
    match(got, want)
    assert status == code, "exit %d, want %d, stderr %r" % (status, code, errs)
    return got, errs


@test
def lifecycle():
    params = {"processId": 42, "rootUri": "file:///tmp", "capabilities": {"textDocument": {}},
              "clientInfo": {"name": "client", "version": "1.2"}, "trace": "off",
              "workspaceFolders": None, "locale": "en"}
    _, errs = expect([frame(req(1, "initialize", params)), frame(note("initialized", {})),
                      frame(req(2, "shutdown")), frame(note("exit"))],
                     [ok(1, INIT_RESULT), ok(2, None)])
    assert b"client 1.2" in errs


@test
def initialize_params():
    for params in [None, {}, {"clientInfo": {"name": "c"}}, {"processId": None, "initializationOptions": [1]}]:
        msg = {"jsonrpc": "2.0", "id": 0, "method": "initialize"}
        if params is not None:
            msg["params"] = params
        expect([frame(msg)], [ok(0, INIT_RESULT)])
    expect([frame(b'{"jsonrpc":"2.0","id":0,"method":"initialize","params":null}')], [ok(0, INIT_RESULT)])


@test
def invalid_params():
    bad = [[], {"clientInfo": 5}, {"clientInfo": {}}, {"clientInfo": {"name": 1}},
           {"clientInfo": {"name": "c", "version": 2}}]
    chunks = [frame(req(i, "initialize", p)) for i, p in enumerate(bad)]
    chunks += [frame(req(9, "shutdown")), frame(INIT)]
    expect(chunks, [err(i, -32602) for i in range(len(bad))] + [err(9, -32002), ok(1, INIT_RESULT)])


@test
def ids():
    ids = ["a\"b\\c/é\u0000", "", 0, -1, 9223372036854775807, -9223372036854775808]
    expect([frame(INIT)] + [frame(req(i, "unknown/method")) for i in ids],
           [ok(1, INIT_RESULT)] + [err(i, -32601) for i in ids])
    status, got, _ = run(frame(b'{"jsonrpc":"2.0","id":9007199254740993,"method":"initialize"}'))
    assert got[0]["id"] == 9007199254740993


@test
def invalid_ids():
    bad = [b"1.5", b"1e2", b"9223372036854775808", b"null", b"true", b"{}", b"[1]"]
    expect([frame(b'{"jsonrpc":"2.0","id":%s,"method":"initialize"}' % i) for i in bad] + [frame(INIT)],
           [err(None, -32600) for _ in bad] + [ok(1, INIT_RESULT)])


@test
def not_initialized():
    expect([frame(req(1, "textDocument/hover", {})), frame(req(2, "shutdown")),
            frame(note("textDocument/didOpen", {})), frame(note("initialized")), frame(INIT)],
           [err(1, -32002), err(2, -32002), ok(1, INIT_RESULT)])


@test
def unknown_methods():
    got, _ = expect([frame(INIT), frame(req(2, "textDocument/hover", {})), frame(req(3, "textDocument/definition", {})),
                     frame(req(4, "$/unknown")), frame(req(5, "a\"b\\")), frame(note("$/cancelRequest", {"id": 2})),
                     frame(note("workspace/didChangeConfiguration", {})), frame(note("unknown")),
                     frame(req(6, "initialize", {})), frame(req(7, "shutdown"))],
                    [ok(1, INIT_RESULT), err(2, -32601), err(3, -32601), err(4, -32601), err(5, -32601),
                     err(6, -32600), ok(7, None)])
    assert got[4]["error"]["message"] == "Method not found: a\"b\\"


@test
def escaped_methods():
    got, _ = expect([frame(INIT), frame(b'{"jsonrpc":"2.0","id":2,"method":"textDocument\\/hover"}'),
                     frame(b'{"jsonrpc":"2.0","id":3,"method":"\\u0073hutdown"}'),
                     frame(b'{"jsonrpc":"2.0","method":"\\u0065xit"}')],
                    [ok(1, INIT_RESULT), err(2, -32601), ok(3, None)])
    assert got[1]["error"]["message"] == "Method not found: textDocument/hover"


@test
def parse_errors():
    bad = [b"", b"{", b'{"a":}', b"[1,]", b'"\xff"', b"{} x", b'{"jsonrpc":"2.0","id":1,"method":"initialize"',
           b"[" * 600 + b"]" * 600, b"[" * 100000, b'"\\ud800"', b"\x00"]
    expect([frame(b) for b in bad] + [frame(INIT)], [err(None, -32700) for _ in bad] + [ok(1, INIT_RESULT)])


@test
def invalid_requests():
    bad = [(b"[]", None), (b"1", None), (b'"x"', None), (b"null", None), (b'{"jsonrpc":"2.0"}', None),
           (b'{"jsonrpc":"1.0","id":3,"method":"initialize"}', 3),
           (b'{"id":3,"method":"initialize"}', 3),
           (b'{"jsonrpc":2,"id":3,"method":"initialize"}', 3),
           (b'{"jsonrpc":"2.0","id":3,"method":5}', 3),
           (b'{"jsonrpc":"2.0","id":3,"method":"initialize","params":"x"}', 3),
           (b'{"jsonrpc":"2.0","id":3,"method":"initialize","params":5}', 3),
           (b'{"jsonrpc":"2.0","id":4}', 4),
           (b'{"jsonrpc":"2.0","method":5}', None),
           (b'[{"jsonrpc":"2.0","id":5,"method":"initialize"}]', None)]
    ignored = [b'{"jsonrpc":"2.0","id":9,"result":null}', b'{"jsonrpc":"2.0","id":null,"error":{"code":1}}']
    expect([frame(b) for b, _ in bad] + [frame(b) for b in ignored] + [frame(INIT)],
           [err(i, -32600) for _, i in bad] + [ok(1, INIT_RESULT)])


@test
def shutdown_and_exit():
    expect([frame(INIT), frame(req(2, "shutdown")), frame(req(3, "shutdown")), frame(req(4, "initialize")),
            frame(note("exit")), frame(req(5, "shutdown"))],
           [ok(1, INIT_RESULT), ok(2, None), err(3, -32600), err(4, -32600)])
    expect([frame(INIT), frame(note("exit")), frame(req(2, "shutdown"))], [ok(1, INIT_RESULT)], code=1)
    expect([frame(note("exit"))], [], code=1)


@test
def end_of_input():
    expect([], [])
    expect([frame(INIT)], [ok(1, INIT_RESULT)])
    expect([frame(INIT), frame(req(2, "shutdown"))], [ok(1, INIT_RESULT), ok(2, None)])
    for tail in [b"Content-Len", b"Content-Length: 5\r\n", b"Content-Length: 5\r\n\r\n{}",
                 b"Content-Length: 2\n\n{}", b"\r\n"]:
        _, errs = expect([frame(INIT), tail], [ok(1, INIT_RESULT)], code=1)
        assert errs, tail


@test
def fragments():
    msgs = frame(INIT) + frame(req(2, "shutdown"))
    expect([msgs[i:i + 1] for i in range(40)] + [msgs[40:70], msgs[70:]], [ok(1, INIT_RESULT), ok(2, None)],
           delay=0.002)
    expect([msgs[:5], msgs[5:21], msgs[21:30], msgs[30:]], [ok(1, INIT_RESULT), ok(2, None)], delay=0.05)


@test
def coalesced():
    expect([frame(INIT) + frame(note("initialized")) + frame(req(2, "x")) + frame(req(3, "shutdown")) + frame(note("exit"))],
           [ok(1, INIT_RESULT), err(2, -32601), ok(3, None)])


@test
def utf8_length():
    params = {"clientInfo": {"name": "é€\U0001F600", "version": "é"}}
    data = json.dumps(req(1, "initialize", params), ensure_ascii=False).encode()
    assert len(data) != len(data.decode())
    _, errs = expect([frame(data), frame(req(2, "é"))], [ok(1, INIT_RESULT), err(2, -32601)])
    assert "é€\U0001F600 é".encode() in errs


@test
def headers():
    data = body(INIT)
    n = b"%d" % len(data)
    for head in [b"Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: " + n + b"\r\n",
                 b"Content-Length: " + n + b"\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n",
                 b"content-length: " + n + b"\r\n", b"CONTENT-LENGTH:" + n + b"\r\n",
                 b"Content-Length: \t " + n + b" \t\r\n", b"Content-Length: 000" + n + b"\r\n",
                 b"X-Other: a:b\r\nContent-Length: " + n + b"\r\nX-Empty:\r\n"]:
        expect([frame(data, head)], [ok(1, INIT_RESULT)])
    expect([frame(b"", b"Content-Length: 0\r\n"), frame(INIT)], [err(None, -32700), ok(1, INIT_RESULT)])


@test
def bad_headers():
    for head in [b"Content-Type: x\r\n", b"Content-Length: 2\r\nContent-Length: 2\r\n",
                 b"Content-Length: -1\r\n", b"Content-Length: +2\r\n", b"Content-Length: abc\r\n",
                 b"Content-Length:\r\n", b"Content-Length: 1 1\r\n", b"Content-Length: 2a\r\n",
                 b"Content-Length: 99999999999999999999999\r\n", b"Content-Length: 67108865\r\n",
                 b"Garbage\r\nContent-Length: 2\r\n", b"Content Length: 2\r\n", b": 2\r\nContent-Length: 2\r\n",
                 b"Content-Length: 2\rX: y\r\n", b"X: \x01\r\nContent-Length: 2\r\n", b""]:
        _, errs = expect([frame(INIT), frame(b"{}", head), frame(INIT)], [ok(1, INIT_RESULT)], code=1)
        assert errs, head


@test
def header_limit():
    long = b"X-Long: " + b"a" * 9000
    expect([frame(INIT), long], [ok(1, INIT_RESULT)], code=1, close=False)
    expect([frame(INIT), frame(INIT, b"X-Long: " + b"a" * 9000 + b"\r\nContent-Length: %d\r\n" % len(body(INIT)))],
           [ok(1, INIT_RESULT)], code=1)


@test
def large_body():
    big = note("$/unknown", {"text": "x" * 200000 + "é" * 1000})
    expect([frame(INIT), frame(big), frame(req(2, "shutdown"))], [ok(1, INIT_RESULT), ok(2, None)])


def main():
    failed = 0
    for fn in TESTS:
        try:
            fn()
            print("ok", fn.__name__)
        except Exception as e:
            failed += 1
            print("FAIL %s: %s: %s" % (fn.__name__, type(e).__name__, e))
    print("%d/%d passed" % (len(TESTS) - failed, len(TESTS)))
    sys.exit(1 if failed else 0)


main()
