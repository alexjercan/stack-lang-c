#!/usr/bin/env python3
"""Protocol tests for stackls. Usage: python3 tests/lsp/protocol.py PATH"""

import json
import os
import re
import subprocess
import tempfile
import sys
import threading
import time

SERVER = sys.argv[1]
TIMEOUT = 20
INIT_RESULT = {
    "capabilities": {"hoverProvider": True, "definitionProvider": True,
                     "completionProvider": {"triggerCharacters": ["."]},
                     "textDocumentSync": {"openClose": True, "change": 1}},
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


def run(*chunks, delay=0.0, close=True, cwd=None, env=None):
    """Send chunks to a new server. Returns exit code, messages and stderr.
    Without close, stdin stays open until the server exits by itself."""
    proc = subprocess.Popen([os.path.abspath(SERVER)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, cwd=cwd, env=env)
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
    got, _ = expect([frame(INIT), frame(req(2, "textDocument/formatting", {})), frame(req(3, "textDocument/references", {})),
                     frame(req(4, "$/unknown")), frame(req(5, "a\"b\\")), frame(note("$/cancelRequest", {"id": 2})),
                     frame(note("workspace/didChangeConfiguration", {})), frame(note("unknown")),
                     frame(req(6, "initialize", {})), frame(req(7, "shutdown"))],
                    [ok(1, INIT_RESULT), err(2, -32601), err(3, -32601), err(4, -32601), err(5, -32601),
                     err(6, -32600), ok(7, None)])
    assert got[4]["error"]["message"] == "Method not found: a\"b\\"


@test
def escaped_methods():
    got, _ = expect([frame(INIT), frame(b'{"jsonrpc":"2.0","id":2,"method":"textDocument\\/references"}'),
                     frame(b'{"jsonrpc":"2.0","id":3,"method":"\\u0073hutdown"}'),
                     frame(b'{"jsonrpc":"2.0","method":"\\u0065xit"}')],
                    [ok(1, INIT_RESULT), err(2, -32601), ok(3, None)])
    assert got[1]["error"]["message"] == "Method not found: textDocument/references"


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


# HOVER

# The libraries are in ../lib from the directory of the server.
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(SERVER)))
URI = "file:///nonexistent/stackls-test/main.stack"
MARKDOWN = {"textDocument": {"hover": {"contentFormat": ["markdown", "plaintext"]}}}
PLAINTEXT = {"textDocument": {"hover": {"contentFormat": ["plaintext", "markdown"]}}}
SIGNATURES = [
    ("dup", "(a) (a, a)", "Duplicate the top item."), ("swp", "(a, b) (b, a)", "Swap the top two items."),
    ("rot", "(a, b, c) (b, c, a)", "Rotate (a, b, c) to (b, c, a)."),
    ("rot4", "(a, b, c, d) (b, c, d, a)", "Rotate (a, b, c, d) to (b, c, d, a)."),
    ("pop", "(a) ()", "Remove the top item."), ("pick", "(int) (ptr)", "duplicate item n on top of the stack as a pointer"),
    ("+", "(int, int) (int)", "Add two integers."), ("-", "(int, int) (int)", "Subtract two integers."),
    ("*", "(int, int) (int)", "Multiply two integers."), ("/", "(int, int) (int)", "Signed integer division."),
    ("%", "(int, int) (int)", "Signed integer remainder."), ("|", "(int, int) (int)", "Bitwise OR."),
    ("&", "(int, int) (int)", "Bitwise AND."), ("^", "(int, int) (int)", "Bitwise XOR."),
    (">>", "(int, int) (int)", "Shift right"), ("<<", "(int, int) (int)", "Shift left"),
    (">", "(int, int) (bool)", "Greater than."), ("<", "(int, int) (bool)", "Less than."),
    ("=", "(int, int) (bool)", "Equal."), ("ptr.alloc", "(int) (ptr)", "Allocate n bytes."),
    ("ptr.free", "(ptr) ()", "Free an allocated block."), ("ptr.+", "(ptr, int) (ptr)", "Add a byte offset."),
    ("ptr.@", "(ptr, ptr, int) (ptr)", "Copy n bytes from src to dst and return dst."),
    ("int.&", "(int) (ptr)", "Address of the value."), ("int.*", "(ptr) (int)", "Dereference as int."),
    ("ptr.&", "(ptr) (ptr)", "Address of the value."), ("ptr.*", "(ptr) (ptr)", "Dereference as ptr."),
    ("bool.&", "(bool) (ptr)", "Address of the value."), ("bool.*", "(ptr) (bool)", "Dereference as bool."),
    ("os.env", "() (ptr)", "Pointer to the environment variable array."),
    ("syscall1", "(a, int) (int)", "Linux syscall with one argument."),
    ("syscall3", "(a, b, c, int) (int)", "Linux syscall with three arguments."),
]


def builtin(name):
    sig, doc = next((s, d) for n, s, d in SIGNATURES if n == name)
    return "func %s %s" % (name, sig), doc


BUILTIN_DATA = "Builtin data type."
NEWLINE = re.compile(r"\r\n|\r|\n")
TOKEN = re.compile(r"[^\s(),]+")


def utf16(text):
    return len(text.encode("utf-16-le")) // 2


def position(text, i):
    """The LSP position of the character text[i]."""
    breaks = list(NEWLINE.finditer(text[:i]))
    start = breaks[-1].end() if breaks else 0
    return {"line": len(breaks), "character": utf16(text[start:i])}


def find(text, needle, n=0):
    i = -1
    for _ in range(n + 1):
        i = text.index(needle, i + 1)
    return i


def did_open(text, uri=URI, version=1):
    return note("textDocument/didOpen",
                {"textDocument": {"uri": uri, "languageId": "stack", "version": version, "text": text}})


def did_change(text, uri=URI, version=2):
    return note("textDocument/didChange",
                {"textDocument": {"uri": uri, "version": version}, "contentChanges": [{"text": text}]})


def did_close(uri=URI):
    return note("textDocument/didClose", {"textDocument": {"uri": uri}})


def hover(id, pos, uri=URI):
    return req(id, "textDocument/hover", {"textDocument": {"uri": uri}, "position": pos})


def pos(line, character):
    return {"line": line, "character": character}


def hover_result(code, text, start, stop, markdown=True):
    fence = "`" * max([3] + [len(run) + 1 for run in re.findall("`+", code)])
    value = fence + "stack\n" + code + "\n" + fence if markdown else code
    if text:
        value += "\n\n" + text
    return {"contents": {"kind": "markdown" if markdown else "plaintext", "value": value},
            "range": {"start": start, "end": stop}}


def init(caps=MARKDOWN):
    return frame(req(1, "initialize", {"capabilities": caps}))


def hovers(text, cases, uri=URI, caps=MARKDOWN, before=(), **kw):
    """Open text and hover each case (needle, n, offset, want) at offset
    characters into the n-th needle. want is None for a null result, else
    (code, note) for the range of the token at the needle."""
    chunks = [init(caps)] + [frame(m) for m in before] + [frame(did_open(text, uri))]
    want = [ok(1, INIT_RESULT)]
    for k, (needle, n, off, w) in enumerate(cases):
        i = find(text, needle, n)
        chunks.append(frame(hover(10 + k, position(text, i + off), uri)))
        res = None
        if w is not None:
            stop = i + len(TOKEN.match(text, i).group())
            res = hover_result(w[0], w[1], position(text, i), position(text, stop), caps is not PLAINTEXT)
        want.append(ok(10 + k, res))
    chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
    return expect(chunks, want + [ok(2, None)], **kw)


@test
def hover_builtins():
    text = ('func main () (int) in\n'
            '    1 2 + dup pop "dup" -- dup\n'
            '    true if 3 else 4 fi int.sizeof +\n'
            'end\n')
    hovers(text, [
        ("+", 0, 0, builtin("+")),
        ("dup", 0, 0, builtin("dup")),
        ("dup", 0, 2, builtin("dup")),
        ("dup", 0, 3, None),
        ("pop", 0, 1, builtin("pop")),
        ("dup", 1, 0, None),
        ("dup", 2, 0, None),
        ("1", 0, 0, None),
        ("true", 0, 0, None),
        ("if", 0, 0, None),
        ("fi", 0, 0, None),
        ("func", 0, 0, None),
        ("end", 0, 0, None),
        ("int", 0, 0, ("data int", BUILTIN_DATA)),
        ("main", 0, 0, ("func main () (int)", "")),
        ("int.sizeof", 0, 4, ("int.sizeof () (int)", "int size")),
        ("    1", 0, 1, None),
    ])


DECLS = ('data pair (int a, ptr b)\n'
         'const N 3\n'
         'func pair.first (pair) (int) in pair.a int.* end\n'
         'func swap2 (a, b) (b, a) in swp end\n'
         'func main () (int) in\n'
         '    N pair.sizeof + 0 pair.first pair.b.offset + 1 2 swap2 + +\n'
         '    0 pair.init pop pair.b.set pair.& pair.*\n'
         'end\n')


@test
def hover_declarations():
    hovers(DECLS, [
        ("pair", 0, 0, ("data pair (int a, ptr b)", "")),
        ("int", 0, 1, ("data int", BUILTIN_DATA)),
        ("a,", 0, 0, None),
        ("ptr", 0, 2, ("data ptr", BUILTIN_DATA)),
        ("N", 0, 0, ("const N 3", "")),
        ("N", 1, 0, ("const N 3", "")),
        ("pair.first", 0, 0, ("func pair.first (pair) (int)", "")),
        ("pair)", 0, 0, ("data pair (int a, ptr b)", "")),
        ("pair.first", 1, 3, ("func pair.first (pair) (int)", "")),
        ("swap2", 0, 0, ("func swap2 (a, b) (b, a)", "")),
        ("a, b)", 0, 0, None),
        ("swap2", 1, 0, ("func swap2 (a, b) (b, a)", "")),
        ("swp", 0, 0, builtin("swp")),
    ])


@test
def hover_generated():
    hovers(DECLS, [
        ("pair.a", 0, 0, ("pair.a (pair) (int)", "pair.a getter")),
        ("pair.sizeof", 0, 0, ("pair.sizeof () (int)", "pair size")),
        ("pair.b.offset", 0, 0, ("pair.b.offset () (int)", "pair.b offset")),
        ("pair.init", 0, 0, ("pair.init (int, ptr) (pair)",
                             "pair constructor")),
        ("pair.b.set", 0, 0, ("pair.b.set (pair, ptr) ()",
                              "pair.b setter")),
        ("pair.&", 0, 0, ("func pair.& (pair) (ptr)", "pair reference")),
        ("pair.*", 0, 0, ("func pair.* (ptr) (pair)", "pair dereference")),
    ])
    text = 'data inner (int x)\ndata outer (inner i, int n)\nfunc f (outer) (inner) in outer.i end\n'
    hovers(text, [
        ("outer.i", 0, 0, ("outer.i (outer) (inner)", "outer.i getter")),
        ("inner i", 0, 0, ("data inner (int x)", "")),
        ("inner)", 0, 0, ("data inner (int x)", "")),
    ])


@test
def hover_bindings():
    text = ('func main () (int) in\n'
            '    1 2 match dup x in\n'
            '        dup x +\n'
            '        3 match x in x end +\n'
            '        true while dup loop false pool\n'
            '    end\n'
            '    dup +\n'
            'end\n')
    hovers(text, [
        ("dup", 0, 0, None),
        ("x", 0, 0, None),
        ("dup", 1, 0, None),
        ("x", 1, 0, None),
        ("x", 2, 0, None),
        ("x", 3, 0, None),
        ("dup", 2, 0, None),
        ("+", 0, 0, builtin("+")),
        ("dup", 3, 0, builtin("dup")),
    ])


BINDS = ('data pair (int a, bool f)\n'
         'const K 7\n'
         'func two () (int, bool) in 1 true end\n'
         'func id (a) (a) in match v in v end end\n'
         'func f (pair, ptr) (int) in\n'
         '    match p q in\n'
         '        p pair.a "s" two match n s c b in\n'
         '            c dup swp match x y in x y + pop end\n'
         '            b if s else "t" fi match t in t pop end\n'
         '            K match k in k end\n'
         '            0 while dup 10 < loop match i in i 1 + end pool pop\n'
         '            q id match r in r pop end\n'
         '            n +\n'
         '        end\n'
         '    end\n'
         'end\n')


def bind(name, type):
    return ("%s: %s" % (name, type), "")


@test
def hover_binding_types():
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(BINDS, [
            ("p q", 0, 0, bind("p", "pair")),
            ("q in", 0, 0, bind("q", "ptr")),
            ("p pair.a", 0, 0, bind("p", "pair")),
            ("n s c b", 0, 0, bind("n", "int")),
            ("s c b", 0, 0, bind("s", "string")),
            ("c b", 0, 0, bind("c", "int")),
            ("b in", 0, 0, bind("b", "bool")),
            ("c dup", 0, 0, bind("c", "int")),
            ("x y in", 0, 0, bind("x", "int")),
            ("y in", 0, 0, bind("y", "int")),
            ("x y +", 0, 0, bind("x", "int")),
            ("y +", 0, 0, bind("y", "int")),
            ("b if", 0, 0, bind("b", "bool")),
            ("s else", 0, 0, bind("s", "string")),
            ("t in", 0, 0, bind("t", "string")),
            ("t pop", 0, 0, bind("t", "string")),
            ("K match", 0, 0, ("const K 7", "")),
            ("k in", 0, 0, bind("k", "int")),
            ("k end", 0, 0, bind("k", "int")),
            ("i in", 0, 0, bind("i", "int")),
            ("i 1", 0, 0, bind("i", "int")),
            ("r in", 0, 0, bind("r", "ptr")),
            ("q id", 0, 0, bind("q", "ptr")),
            ("n +", 0, 0, bind("n", "int")),
            ("v in", 0, 0, bind("v", "a")),
            ("v end", 0, 0, bind("v", "a")),
            ("pair.a", 0, 0, ("pair.a (pair) (int)", "pair.a getter")),
            ("id match", 0, 0, ("func id (a) (a)", "")),
            ("dup swp", 0, 0, builtin("dup")),
        ], caps=caps)


ERRS = ('const X 5\n'
        'const D match x2 in x2 end\n'
        'func g () () in 1 match a1 b1 in a1 b1 end end\n'
        'func h () () in 1 2 match a2 a2 in a2 end end\n'
        'func u () () in undefined match z in z pop end end\n'
        'func w () () in 1 match dup in end end\n'
        'func te () () in 1 "s" + match z2 in z2 pop end end\n'
        'func late () () in 1 match g1 in g1 "s" + pop end end\n'
        'func sh () (int) in 1 match q in 2 match q in q end pop end end\n'
        'func cx () (int) in 7 match X in X end end\n'
        'func re () () in 1 match o in o pop end "x" match o in o pop end end\n'
        'func dupf () (int) in 1 end\nfunc dupf () (bool) in true end\n'
        'func ud () () in dupf match dv in end end\n'
        'const Z1 1\nconst Z1 "s"\nfunc uz () () in Z1 match zv in end end\n'
        'const Y1 Y2\nconst Y2 Y1\nfunc cy () () in Y1 match yv in end end\n'
        'func good () (int) in 1 match e in e end end\n')


@test
def hover_binding_errors():
    """Bindings without a type checked type get null."""
    hovers(ERRS, [
        ("x2 in", 0, 0, None),
        ("x2 end", 0, 0, None),
        ("a1", 0, 0, None),
        ("b1", 0, 0, None),
        ("a1", 1, 0, None),
        ("a2", 0, 0, None),
        ("a2", 1, 0, None),
        ("a2", 2, 0, None),
        ("z in", 0, 0, None),
        ("undefined", 0, 0, None),
        ("dup in", 0, 0, None),
        ("z2", 0, 0, None),
        # An error after the binding keeps its type.
        ("g1", 0, 0, bind("g1", "int")),
        ("g1", 1, 0, bind("g1", "int")),
        # The inner q is defined again; the outer q keeps its type.
        ("q in 2", 0, 0, bind("q", "int")),
        ("q in q", 0, 0, None),
        ("q end", 0, 0, None),
        ("X in", 0, 0, bind("X", "int")),
        ("X end", 0, 0, ("const X 5", "")),
        ("o in", 0, 0, bind("o", "int")),
        ("o pop", 0, 0, bind("o", "int")),
        ("o in", 1, 0, bind("o", "string")),
        ("o pop", 1, 0, bind("o", "string")),
        ("dv", 0, 0, None),
        ("zv", 0, 0, None),
        ("yv", 0, 0, None),
        ("e in", 0, 0, bind("e", "int")),
        ("e end end", 0, 0, bind("e", "int")),
    ], caps=PLAINTEXT)
    # Incomplete code keeps the types of the funcs before it.
    text = 'func f () (int) in 1 match a in a end end\nfunc g () () in 2 match b in b\n'
    hovers(text, [("a in", 0, 0, bind("a", "int")), ("b in", 0, 0, None)], caps=PLAINTEXT)


@test
def hover_binding_consts():
    """Constants expand before the type checker, in any order and depth."""
    up = "const C0 \"s\"\n" + "".join("const C%d C%d\n" % (k, k - 1) for k in range(1, 21))
    down = "".join("const C%d C%d\n" % (k, k - 1) for k in range(20, 0, -1)) + "const C0 \"s\"\n"
    for consts in [up, down]:
        text = consts + "func f () () in C20 match v in v pop end end\n"
        hovers(text, [("v in", 0, 0, bind("v", "string")), ("v pop", 0, 0, bind("v", "string"))], caps=PLAINTEXT)
    # Builtin constants, data helpers and specials.
    text = ('data box (int n, bool f)\n'
            'func f () () in int.sizeof box.sizeof 1 true box.init __line__ __file__\n'
            '    match a b c d e in e pop d pop c box.f pop b a + pop end end\n')
    hovers(text, [("a b c", 0, 0, bind("a", "int")), ("b c", 0, 0, bind("b", "int")),
                  ("c d", 0, 0, bind("c", "box")), ("d e", 0, 0, bind("d", "int")),
                  ("e in", 0, 0, bind("e", "string"))], caps=PLAINTEXT)


@test
def hover_binding_specials():
    """The preprocessor replaces a special name in a body with a literal."""
    text = ('func f () () in true 1 "s" match __line__ __file__ __col__ in\n'
            '    __line__ __file__ __col__ pop pop pop end end\n')
    hovers(text, [("__line__", 0, 0, bind("__line__", "bool")), ("__file__", 0, 0, bind("__file__", "int")),
                  ("__col__", 0, 0, bind("__col__", "string")), ("__line__", 1, 0, None),
                  ("__file__", 1, 0, None), ("__col__", 1, 0, None)], caps=PLAINTEXT)


@test
def hover_binding_large():
    """A body with many calls of one func is checked in linear time."""
    text = "func noop () () in end\nfunc f () () in 1 match x in " + "noop " * 20000 + "x pop end end\n"
    hovers(text, [("x in", 0, 0, bind("x", "int")), ("x pop", 0, 0, bind("x", "int"))], caps=PLAINTEXT)


@test
def hover_binding_imports():
    with tempfile.TemporaryDirectory() as tmp:
        with open(os.path.join(tmp, "dep.stack"), "w") as f:
            f.write('data dep.d (int x)\nconst dep.K "k"\nfunc dep.two () (int, string) in 1 "a" end\n')
        uri = "file://" + os.path.join(tmp, "main.stack")
        text = ('@import "dep"\n'
                'func main () (int) in dep.two match a b in b pop a 0 dep.d.init dep.K match d k in k pop d dep.d.x end + end end\n')
        hovers(text, [("a b", 0, 0, bind("a", "int")), ("b in", 0, 0, bind("b", "string")),
                      ("d k", 0, 0, bind("d", "dep.d")), ("k in", 0, 0, bind("k", "string")),
                      ("dep.d.x", 0, 0, ("dep.d.x (dep.d) (int)", "dep.d.x getter"))], uri=uri, caps=PLAINTEXT, cwd=tmp)
        env = dict(os.environ, STACK_HOME=ROOT)
        text = '@import "stdlib"\nfunc f (string) (int) in "abc" string.len match s n in s pop n end end\n'
        hovers(text, [("s n", 0, 0, bind("s", "string")), ("n in", 0, 0, bind("n", "int"))],
               uri=uri, caps=PLAINTEXT, cwd=tmp, env=env)


@test
def hover_binding_changes():
    """Edits get new types. Positions are UTF-16."""
    uri = URI
    srv = Session()
    srv.send(req(1, "initialize", {"capabilities": {}}))
    assert srv.read() == ok(1, INIT_RESULT)
    for k, (value, type) in enumerate([("1", "int"), ("1", "int"), ("true", "bool"), ('"x"', "string")]):
        text = 'func f () () in "\u00e9\U0001f600" pop %s match a in a pop end end\n' % value
        srv.send(did_open(text, uri) if k == 0 else did_change(text, uri, version=k + 1))
        for n in range(2):
            i = find(text, "a", n + 1)
            srv.send(hover(10 + k, position(text, i), uri))
            assert srv.read() == ok(10 + k, hover_result("a: " + type, "", position(text, i), position(text, i + 1),
                                                         False))
    srv.close()


@test
def hover_imports():
    with tempfile.TemporaryDirectory() as tmp:
        os.mkdir(os.path.join(tmp, "lib"))
        with open(os.path.join(tmp, "local.stack"), "w") as f:
            f.write('data cell (int v)\nfunc local.f (int) (cell) in cell.init end\n')
        with open(os.path.join(tmp, "lib", "shared.stack"), "w") as f:
            f.write('@import "local"\nfunc shared.g () () in end\n')
        text = '@import "shared"\nfunc main () () in 1 local.f cell.v pop shared.g string.len end\n'
        env = dict(os.environ, STACK_HOME=tmp)
        hovers(text, [
            ("local.f", 0, 0, ("func local.f (int) (cell)", "")),
            ("cell.v", 0, 0, ("cell.v (cell) (int)", "cell.v getter")),
            ("shared.g", 0, 0, ("func shared.g () ()", "")),
            ("string.len", 0, 0, None),
            ('"shared"', 0, 1, None),
        ], cwd=tmp, env=env)
        # A missing import keeps the declarations of the open file.
        text = '@import "missing"\nfunc main () () in main dup end\n'
        hovers(text, [("main", 1, 0, ("func main () ()", "")), ("dup", 0, 0, builtin("dup"))],
               cwd=tmp, env=env)
        env = dict(os.environ, STACK_HOME=ROOT)
        text = '@import "stdlib"\nfunc f (string) (int) in string.len end\n'
        hovers(text, [
            ("string.len", 0, 0, ("string.len (string) (int)", "string.len getter")),
            ("string", 0, 1, ("data string (int len, ptr str)", "DATA STRING")),
        ], cwd=tmp, env=env)


@test
def hover_unsaved_imports():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = os.path.join(tmp, "a b")
        os.mkdir(tmp)
        path = os.path.join(tmp, "dep.stack")
        with open(path, "w") as f:
            f.write('func dep.old () () in end\n')
        dep = "file://" + path.replace(" ", "%20")
        uri = "file://" + os.path.join(tmp, "main.stack").replace(" ", "%20")
        text = '@import "dep"\nfunc main () () in dep.old dep.new end\n'
        unsaved = did_open('func dep.new (int) (int) in end\n', dep)
        hovers(text, [("dep.old", 0, 0, None), ("dep.new", 0, 0, ("func dep.new (int) (int)", ""))],
               uri=uri, before=[unsaved], cwd=tmp)
        hovers(text, [("dep.old", 0, 0, ("func dep.old () ()", "")), ("dep.new", 0, 0, None)],
               uri=uri, before=[unsaved, did_close(dep)], cwd=tmp)
        # The open file imports itself.
        with open(os.path.join(tmp, "main.stack"), "w") as f:
            f.write("data old (int x)\n")
        text = '@import "main"\nfunc main () () in main old.x end\n'
        hovers(text, [("main", 1, 0, ("func main () ()", "")), ("main", 2, 0, ("func main () ()", "")),
                      ("old.x", 0, 0, None)], uri=uri, cwd=tmp)


BROKEN = ["", "func", "func f (", "func f () () in match", "data d (int", "@import", '@import "x',
          "func f () () in \"abc", "func f () () in \x00 dup end", "ÿþ dup", "end end fi pool",
          "func f () () in " + "if " * 2000, "func f () () in " + "match x in " * 2000,
          "func f () () in " + "x " * 20000 + "end",
          "const c c\nconst d d\nfunc f () () in c end", "data d (d x)\nfunc f (d) () in d.x end",
          "data d (int x)\ndata d (int y)\nfunc f () () in d.x end", "func f () () in end\nfunc f () () in end",
          "@import \"stdlib\"\n@import \"stdlib\"\nfunc f () () in string.len end"]


@test
def hover_broken_source():
    text = ('func good () (int) in 1 end\n'
            'func bad () (int) in 1 2 + \n'
            'func later () (int) in good end\n')
    hovers(text, [
        ("good", 0, 0, ("func good () (int)", "")),
        ("int", 0, 0, ("data int", BUILTIN_DATA)),
        ("bad", 0, 0, None),
        ("+", 0, 0, None),
        ("later", 0, 0, None),
        ("good", 1, 0, None),
    ])
    text = 'func good () (int) in 1 end\nfunc bad () () in 1 \r end\n'
    hovers(text, [("good", 0, 0, ("func good () (int)", "")), ("bad", 0, 0, None)])
    env = dict(os.environ, STACK_HOME=ROOT)
    for text in BROKEN:
        first = text.split("\n")[0]
        chunks = [init(), frame(did_open(text))]
        chunks += [frame(hover(10 + k, pos(0, k))) for k in range(min(len(first), 60) + 1)]
        chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
        status, got, errs = run(*chunks, cwd=ROOT, env=env)
        assert status == 0, (text[:40], status, errs[-200:])
        assert len(got) == len(chunks) - 2 and got[-1] == ok(2, None), (text[:40], got[-3:])


@test
def hover_positions():
    # A carriage return is not valid in Stack code, but it ends a line in a comment.
    text = 'func main () () in\n    "é€\U0001F600" pop -- \U0001F600 \U0001F600\r\n    "\U0001F600" swp -- a\r-- b\n    dup\nend\n'
    hovers(text, [
        ("pop", 0, 0, builtin("pop")),
        ("pop", 0, 2, builtin("pop")),
        ("swp", 0, 1, builtin("swp")),
        ("dup", 0, 0, builtin("dup")),
    ])
    line = position(text, find(text, "pop"))["line"]
    expect([init(), frame(did_open(text)),
            frame(hover(3, pos(line, 1000))),
            frame(hover(4, pos(99, 0))),
            frame(hover(5, pos(line, 6))),
            frame(hover(6, pos(line, 7))),
            frame(hover(7, pos(0, 0), uri="file:///other.stack"))],
           [ok(1, INIT_RESULT)] + [ok(i, None) for i in range(3, 8)])
    # A position inside a surrogate pair.
    text = 'func main () () in "\U0001F600"dup end\n'
    expect([init(), frame(did_open(text)), frame(hover(2, pos(0, 21))), frame(hover(3, pos(0, 23)))],
           [ok(1, INIT_RESULT), ok(2, None), ok(3, hover_result(*builtin("dup"), pos(0, 23), pos(0, 26)))])


@test
def hover_formats():
    text = 'func main () () in dup end\n'
    for caps, markdown in [(MARKDOWN, True), (PLAINTEXT, False), ({}, False), ({"textDocument": {"hover": {}}}, False),
                           ({"textDocument": {"hover": {"contentFormat": []}}}, False),
                           ({"textDocument": {"hover": {"contentFormat": ["x", 1, "plaintext", "markdown"]}}}, False),
                           ({"textDocument": {"hover": {"contentFormat": [5, "markdown"]}}}, True),
                           ({"textDocument": {"hover": {"contentFormat": "markdown"}}}, False),
                           ({"textDocument": 5}, False)]:
        expect([init(caps), frame(did_open(text)), frame(hover(2, pos(0, 20)))],
               [ok(1, INIT_RESULT), ok(2, hover_result(*builtin("dup"), pos(0, 19), pos(0, 22), markdown))])


@test
def hover_documents():
    one = 'func f () () in dup end\n'
    two = 'func g () () in\n  swp end\n'
    p, q = pos(0, 16), pos(1, 2)
    dup = hover_result(*builtin("dup"), p, pos(0, 19))
    swp = hover_result(*builtin("swp"), q, pos(1, 5))
    untitled = "untitled:Untitled-1"
    expect([init(), frame(hover(2, p)), frame(did_open(one)), frame(hover(3, p)),
            frame(did_change(two)), frame(hover(4, p)), frame(hover(5, pos(0, 5))), frame(hover(6, q)),
            frame(note("textDocument/didChange", {"textDocument": {"uri": URI, "version": 3},
                                                  "contentChanges": [{"range": {}, "text": "x"}]})),
            frame(hover(7, q)),
            frame(did_change(one, version=4)), frame(did_change(two, version=5)), frame(hover(8, q)),
            frame(did_close()), frame(hover(9, q)), frame(did_close()), frame(did_change(one)),
            frame(did_open(one, version=7)), frame(did_open(two, version=8)), frame(hover(10, q)),
            frame(did_open(one, uri=untitled)), frame(hover(11, p, uri=untitled)), frame(hover(12, q))],
           [ok(1, INIT_RESULT), ok(2, None), ok(3, dup), ok(4, None),
            ok(5, hover_result("func g () ()", "", pos(0, 5), pos(0, 6))),
            ok(6, swp), ok(7, swp), ok(8, swp), ok(9, None), ok(10, swp), ok(11, dup), ok(12, swp)])


@test
def hover_params():
    bad = [None, {}, {"textDocument": {"uri": URI}}, {"position": pos(0, 0)},
           {"textDocument": {}, "position": pos(0, 0)},
           {"textDocument": {"uri": 5}, "position": pos(0, 0)},
           {"textDocument": {"uri": URI}, "position": pos(-1, 0)},
           {"textDocument": {"uri": URI}, "position": pos(0, -1)},
           {"textDocument": {"uri": URI}, "position": pos(0.5, 0)},
           {"textDocument": {"uri": URI}, "position": pos("0", 0)},
           {"textDocument": {"uri": URI}, "position": [0, 0]},
           {"textDocument": {"uri": URI}, "position": pos(2147483648, 0)},
           {"textDocument": {"uri": URI}, "position": pos(0, 2147483648)},
           {"textDocument": {"uri": URI}, "position": pos(9223372036854775807, 0)}]
    chunks = [init(), frame(did_open("func f () () in dup end\n"))]
    chunks += [frame(req(10 + i, "textDocument/hover", p)) for i, p in enumerate(bad)]
    chunks += [frame(hover(2, pos(0, 16))), frame(hover(3, pos(2147483647, 0))), frame(hover(4, pos(0, 2147483647)))]
    expect(chunks, [ok(1, INIT_RESULT)] + [err(10 + i, -32602) for i in range(len(bad))] +
           [ok(2, hover_result(*builtin("dup"), pos(0, 16), pos(0, 19))), ok(3, None), ok(4, None)])
    expect([init(), frame(note("textDocument/didOpen", {})), frame(note("textDocument/didOpen", {"textDocument": {}})),
            frame(note("textDocument/didChange", {})), frame(note("textDocument/didClose", {"textDocument": 5})),
            frame(hover(2, pos(0, 0)))], [ok(1, INIT_RESULT), ok(2, None)])


@test
def hover_cache():
    """Edits and hovers in one session with stdlib imported."""
    env = dict(os.environ, STACK_HOME=ROOT)
    chunks, want = [init()], [ok(1, INIT_RESULT)]
    for v in range(20):
        text = '@import "stdlib"\nfunc f%d () (int) in "x" string.len end\n' % v
        chunks.append(frame(did_open(text, version=v) if v == 0 else did_change(text, version=v)))
        for k in range(3):
            chunks.append(frame(hover(100 * v + k, pos(1, 5))))
            want.append(ok(100 * v + k, hover_result("func f%d () (int)" % v, "", pos(1, 5), pos(1, 7 if v < 10 else 8))))
    expect(chunks + [frame(req(2, "shutdown"))], want + [ok(2, None)], cwd=ROOT, env=env)


@test
def hover_deep_nesting():
    good = "func good () () in end\n"
    want = ("func good () ()", "")
    for block in ["if ", "match x in ", "true while ", "if match x in true while "]:
        text = good + "func bad () () in " + block * 10000
        hovers(text, [("good", 0, 0, want), ("bad", 0, 0, None)])
    # At most 2048 nested blocks.
    for n, inner in [(2048, builtin("dup")), (2049, None)]:
        text = good + "func bad () () in\n" + "true if " * n + "dup" + " fi" * n + "\nend\n"
        hovers(text, [("good", 0, 0, want), ("dup", 0, 1, inner)])
    # The server continues after a deep document.
    deep = good + "func bad () () in " + "if " * 10000
    expect([init(), frame(did_open(deep)), frame(hover(2, pos(0, 5))), frame(did_change("func f () () in dup end\n")),
            frame(hover(3, pos(0, 16)))],
           [ok(1, INIT_RESULT), ok(2, hover_result(*want, pos(0, 5), pos(0, 9))),
            ok(3, hover_result(*builtin("dup"), pos(0, 16), pos(0, 19)))])


@test
def hover_versions():
    old, new = "func old () () in end\n", "func new () () in end\n"
    p = pos(0, 5)
    hold = hover_result("func old () ()", "", p, pos(0, 8))
    hnew = hover_result("func new () ()", "", p, pos(0, 8))
    expect([init(), frame(did_open(new, version=10)), frame(did_change(old, version=9)), frame(hover(2, p)),
            frame(did_change(old, version=10)), frame(hover(3, p)), frame(did_open(old, version=10)), frame(hover(4, p)),
            frame(did_change(old, version=11)), frame(hover(5, p)), frame(did_open(new, version=12)), frame(hover(6, p)),
            frame(did_change(old, version=-1)), frame(hover(7, p))],
           [ok(1, INIT_RESULT), ok(2, hnew), ok(3, hnew), ok(4, hnew), ok(5, hold), ok(6, hnew), ok(7, hnew)])


class Session:
    """A server that answers one request at a time."""

    def __init__(self, **kw):
        self.proc = subprocess.Popen([os.path.abspath(SERVER)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=subprocess.DEVNULL, **kw)

    def send(self, msg):
        self.proc.stdin.write(frame(msg))
        self.proc.stdin.flush()

    def read(self):
        head = b""
        while not head.endswith(b"\r\n\r\n"):
            c = self.proc.stdout.read(1)
            assert c, "server closed stdout"
            head += c
        return json.loads(self.proc.stdout.read(int(head.split(b":")[1])))

    def close(self):
        self.send(req(2, "shutdown"))
        assert self.read() == ok(2, None)
        self.send(note("exit"))
        self.proc.stdin.close()
        assert self.proc.wait(TIMEOUT) == 0


@test
def hover_import_changes():
    with tempfile.TemporaryDirectory() as tmp:
        dep = os.path.join(tmp, "dep.stack")
        uri = "file://" + os.path.join(tmp, "main.stack")
        text = '@import "dep"\nfunc main () () in dep.f end\n'
        at = position(text, find(text, "dep.f"))
        srv = Session(cwd=tmp)
        srv.send(req(1, "initialize", {"capabilities": {}}))
        assert srv.read() == ok(1, INIT_RESULT)
        srv.send(did_open(text, uri))
        for k, sig in enumerate(["()", "(int)", "(int)", "(ptr)"]):
            if k != 2:
                with open(dep, "w") as f:
                    f.write("func dep.f () %s in end\n" % sig)
            srv.send(did_change(text + "-- %d\n" % k, uri, version=k + 2))
            srv.send(hover(10 + k, at, uri))
            want = hover_result("func dep.f () %s" % sig, "", at,
                                {"line": at["line"], "character": at["character"] + 5}, False)
            assert srv.read() == ok(10 + k, want)
        srv.close()


DOCS = ('-- Body of g.\nfunc g () () in\n  -- body\n  end -- trailing\n-- First line.\n--\n'
        '--   `code` *x* _y_ <b>t</b> \\ [a](b) 1. # & &amp; a  b\tc\nfunc -- between\n  -- inside\n  f () () in g end\n\n'
        '-- detached\n\nfunc h () () in f end\nconst K 1 -- k\nfunc i () () in end\n'
        '--no space\n---- dashes\n  -- indent\nfunc j () () in end\n')
DOC_F = "First line.\n\n  `code` *x* _y_ <b>t</b> \\ [a](b) 1. # & &amp; a  b\tc"


@test
def hover_docs():
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(DOCS, [
            ("g ()", 0, 0, ("func g () ()", "Body of g.")),
            ("f ()", 0, 0, ("func f () ()", DOC_F)),
            ("g end", 0, 0, ("func g () ()", "Body of g.")),
            ("f end", 0, 0, ("func f () ()", DOC_F)),
            ("h ()", 0, 0, ("func h () ()", "")),
            ("i ()", 0, 0, ("func i () ()", "")),
            ("j ()", 0, 0, ("func j () ()", "no space\n-- dashes\nindent")),
            ("K", 0, 0, ("const K 1", "")),
        ], caps=caps)


@test
def hover_builtin_docs():
    text = "func main () () in\n" + "".join("    %s\n" % name for name, _, _ in SIGNATURES) + "end\n"

    def at(name):
        """The needle and n of name on its own line."""
        i = text.index("\n    %s\n" % name) + 5
        return name, sum(text.startswith(name, k) for k in range(i)), 0

    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(text, [at(name) + (builtin(name),) for name, _, _ in SIGNATURES], caps=caps)
    hovers(text, [
        at("rot") + (("func rot (a, b, c) (b, c, a)", "Rotate (a, b, c) to (b, c, a)."),),
        at("ptr.@") + (("func ptr.@ (ptr, ptr, int) (ptr)", "Copy n bytes from src to dst and return dst."),),
        at("<<") + (("func << (int, int) (int)", "Shift left"),),
        at("&") + (("func & (int, int) (int)", "Bitwise AND."),),
        at("pick") + (("func pick (int) (ptr)", "duplicate item n on top of the stack as a pointer"),),
    ])


@test
def hover_raw_markdown():
    doc = ("Takes `array<int>` and *n*, see [docs](https://example.com/a_b).\n\n- one\n- two_three\n"
           "  indented \\ <b>\nlast  line")
    text = "".join("-- %s\n" % line if line else "--\n" for line in doc.split("\n")) + "func f () () in f end\n"
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(text, [("f ()", 0, 0, ("func f () ()", doc)), ("f end", 0, 0, ("func f () ()", doc))], caps=caps)
    at = position(text, find(text, "f ()"))
    got, _ = expect([init(), frame(did_open(text)), frame(hover(2, at))],
                    [ok(1, INIT_RESULT), ok(2, hover_result("func f () ()", doc, at, pos(at["line"], 6)))])
    value = got[1]["result"]["contents"]["value"]
    assert "&nbsp;" not in value and "\\`" not in value and "\\\n" not in value and "\\*" not in value, value


@test
def hover_doc_lines():
    text = ("func a () () in\n  -- body\nend\n-- x\n\n-- doc ü \U0001F600\r\n-- two\r-- three\rraw\n\t-- tab\r\n"
            "func b () () in a end\n")
    hovers(text, [("b ()", 0, 0, ("func b () ()", "doc ü \U0001F600\ntwo\nthree\nraw\ntab")),
                  ("a end", 0, 0, ("func a () ()", ""))], caps=PLAINTEXT)
    hovers(text, [("b ()", 0, 0, ("func b () ()", "doc ü \U0001F600\ntwo\nthree\nraw\ntab"))])
    # The lexer decides what is a comment: a continued string is not a doc.
    text = ('const X "continued\\\n-- NOT A DOC"\nfunc f () () in end\n'
            'const Y "a\\\nb" -- c\nfunc g () () in end\n'
            'const Z "-- no\\\n" -- end\n\n-- yes\nfunc h () () in end\n')
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(text, [("f ()", 0, 0, ("func f () ()", "")), ("g ()", 0, 0, ("func g () ()", "")),
                      ("h ()", 0, 0, ("func h () ()", "yes"))], caps=caps)
    # A broken header keeps the docs of the functions before it.
    text = "-- e doc\nfunc e () () in end\n-- d doc\nfunc d ( in e end\n"
    hovers(text, [("e ()", 0, 0, ("func e () ()", "e doc")), ("e end", 0, 0, None), ("d (", 0, 0, None)],
           caps=PLAINTEXT)
    text = "-- e doc\nfunc e () () in end\n-- f doc\nfunc\n"
    hovers(text, [("e ()", 0, 0, ("func e () ()", "e doc"))], caps=PLAINTEXT)
    # Edits change the docs.
    text = "-- old\nfunc e () () in e end\n"
    new = "-- new\n-- doc\nfunc e () () in e end\n"
    at = position(new, find(new, "e ("))
    expect([init(PLAINTEXT), frame(did_open(text)), frame(did_change(new)), frame(hover(2, at))],
           [ok(1, INIT_RESULT), ok(2, hover_result("func e () ()", "new\ndoc", at,
                                                   {"line": at["line"], "character": 6}, False))])


@test
def hover_doc_imports():
    with tempfile.TemporaryDirectory() as tmp:
        dep = os.path.join(tmp, "dep.stack")
        with open(dep, "w") as f:
            f.write("-- disk *doc*\nfunc dep.f () () in end\n")
        uri = "file://" + os.path.join(tmp, "main.stack")
        text = '@import "dep"\nfunc main () () in dep.f end\n'
        hovers(text, [("dep.f", 0, 0, ("func dep.f () ()", "disk *doc*"))], uri=uri, caps=PLAINTEXT, cwd=tmp)
        hovers(text, [("dep.f", 0, 0, ("func dep.f () ()", "disk *doc*"))], uri=uri, cwd=tmp)
        unsaved = did_open("data x (int y)\n\n-- unsaved\nfunc dep.f () () in end\n", "file://" + dep)
        hovers(text, [("dep.f", 0, 0, ("func dep.f () ()", "unsaved"))], uri=uri, caps=PLAINTEXT,
               before=[unsaved], cwd=tmp)
        at = position(text, find(text, "dep.f"))
        stop = {"line": at["line"], "character": at["character"] + 5}
        srv = Session(cwd=tmp)
        srv.send(req(1, "initialize", {"capabilities": {}}))
        assert srv.read() == ok(1, INIT_RESULT)
        srv.send(did_open(text, uri))
        for k, doc in enumerate(["one", "one", "two\nlines", ""]):
            with open(dep, "w") as f:
                f.write("".join("-- %s\n" % line for line in doc.split("\n") if doc) + "func dep.f () () in end\n")
            srv.send(did_change(text + "-- %d\n" % k, uri, version=k + 2))
            srv.send(hover(10 + k, at, uri))
            assert srv.read() == ok(10 + k, hover_result("func dep.f () ()", doc, at, stop, False))
        srv.close()


CONSTS = ('-- The answer.\nconst A 42\nconst S "q\\"`x` ```"\nconst B true\nconst C A 1 +\nconst D\n'
          '    -- inner\n    if 1 else match x in x end fi\nconst W while true loop pool -- trailing\nconst E\n'
          'const X 5\nfunc main () (int) in 7 match X y in X y C D W E S B pop2 pop2 pop2 pop end end\n')
CONST_D = "const D\n    -- inner\n    if 1 else match x in x end fi"


@test
def hover_consts():
    doc = "The answer."
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(CONSTS, [
            ("A", 0, 0, ("const A 42", doc)),
            ("A 1", 0, 0, ("const A 42", doc)),
            ("S", 0, 0, ('const S "q\\"`x` ```"', "")),
            ("B", 0, 0, ("const B true", "")),
            ("C", 0, 0, ("const C A 1 +", "")),
            ("D", 0, 0, (CONST_D, "")),
            ("W", 0, 0, ("const W while true loop pool", "")),
            ("E", 0, 0, ("const E", "")),
            ("x in", 0, 0, None),
            ("x end", 0, 0, None),
            # A constant shadows a binding in the body, as the preprocessor expands it.
            ("X 5", 0, 0, ("const X 5", "")),
            ("X y", 0, 0, None),
            ("y in", 0, 0, None),
            ("X y", 1, 0, ("const X 5", "")),
            ("y C", 0, 0, None),
            ("C D", 0, 0, ("const C A 1 +", "")),
            ("D W", 0, 0, (CONST_D, "")),
            ("S B", 0, 0, ('const S "q\\"`x` ```"', "")),
        ], caps=caps)
    # The fence is longer than the backticks in the code.
    i = find(CONSTS, "S B")
    at, stop = position(CONSTS, i), position(CONSTS, i + 1)
    expect([init(), frame(did_open(CONSTS)), frame(hover(2, at))],
           [ok(1, INIT_RESULT), ok(2, {"contents": {"kind": "markdown", "value": '````stack\nconst S "q\\"`x` ```"\n````'},
                                       "range": {"start": at, "end": stop}})])
    text = 'const T "a%sb"\n' % ("`" * 40)
    at, stop = position(text, 6), position(text, 7)
    value = "`" * 41 + "stack\n" + text[:-1] + "\n" + "`" * 41
    expect([init(), frame(did_open(text)), frame(hover(2, at))],
           [ok(1, INIT_RESULT), ok(2, {"contents": {"kind": "markdown", "value": value},
                                       "range": {"start": at, "end": stop}})])


@test
def hover_definition_docs():
    text = ('-- Pair doc.\ndata pair (int a, ptr b)\n-- K doc\nconst K 1 -- k\nconst L "a\\\n-- no" -- c\n'
            'const M 2\n\n-- no\n\ndata e ()\n-- f *doc*\nfunc f (pair) (pair) in pair.a pop pair.& pair.* K L M end\n')
    pair, k, f = "Pair doc.", "K doc", "f *doc*"
    for caps in [PLAINTEXT, MARKDOWN]:
        hovers(text, [
            ("pair (", 0, 0, ("data pair (int a, ptr b)", pair)),
            ("pair)", 0, 0, ("data pair (int a, ptr b)", pair)),
            ("pair) in", 0, 0, ("data pair (int a, ptr b)", pair)),
            ("pair.a", 0, 0, ("pair.a (pair) (int)", "pair.a getter")),
            ("pair.&", 0, 0, ("func pair.& (pair) (ptr)", "pair reference")),
            ("pair.*", 0, 0, ("func pair.* (ptr) (pair)", "pair dereference")),
            ("K 1", 0, 0, ("const K 1", k)),
            ("K L", 0, 0, ("const K 1", k)),
            ("L", 0, 0, ('const L "a\\\n-- no"', "")),
            ("M 2", 0, 0, ("const M 2", "")),
            ("e ()", 0, 0, ("data e ()", "")),
            ("f (", 0, 0, ("func f (pair) (pair)", f)),
        ], caps=caps)


@test
def hover_const_errors():
    text = 'const P 1\n-- Q doc\nconst Q P P\ndata d (int x)\nconst Z 3 )\nfunc f () () in Q P Z end\n'
    hovers(text, [("P", 0, 0, ("const P 1", "")), ("Q P P", 0, 0, ("const Q P P", "Q doc")),
                  ("P P", 0, 0, ("const P 1", "")), ("d (", 0, 0, ("data d (int x)", "")),
                  ("Z", 0, 0, ("const Z 3", "")), ("Q P Z", 0, 0, None)], caps=PLAINTEXT)
    # Incomplete declarations get null and keep the declarations before them.
    for tail, needle in [("const R if 1\n", "R"), ("const R match x in\n", "R"), ('const R "a\n', "R"),
                         ("data d (int\n", "d"), ("const\n", None)]:
        cases = [("P", 0, 0, ("const P 1", ""))] + ([(needle, 0, 0, None)] if needle else [])
        hovers("const P 1\n" + tail, cases, caps=PLAINTEXT)


@test
def hover_const_imports():
    with tempfile.TemporaryDirectory() as tmp:
        dep = os.path.join(tmp, "dep.stack")
        with open(dep, "w") as f:
            f.write("-- disk\nconst dep.K\n    1 2 +\n-- data doc\ndata dep.d (int x)\n")
        uri = "file://" + os.path.join(tmp, "main.stack")
        text = '@import "dep"\nfunc main (dep.d) (int) in pop dep.K end\n'
        hovers(text, [("dep.K", 0, 0, ("const dep.K\n    1 2 +", "disk")),
                      ("dep.d", 0, 0, ("data dep.d (int x)", "data doc"))], uri=uri, caps=PLAINTEXT, cwd=tmp)
        unsaved = did_open("-- unsaved\nconst dep.K 7\ndata dep.d (int y)\n", "file://" + dep)
        hovers(text, [("dep.K", 0, 0, ("const dep.K 7", "unsaved")), ("dep.d", 0, 0, ("data dep.d (int y)", ""))],
               uri=uri, caps=PLAINTEXT, before=[unsaved], cwd=tmp)
        at = position(text, find(text, "dep.K"))
        stop = {"line": at["line"], "character": at["character"] + 5}
        srv = Session(cwd=tmp)
        srv.send(req(1, "initialize", {"capabilities": {}}))
        assert srv.read() == ok(1, INIT_RESULT)
        srv.send(did_open(text, uri))
        for k, (doc, value) in enumerate([("one", "1"), ("one", "1"), ("", "if 1 else 2 fi"), ("two", '"x"')]):
            with open(dep, "w") as f:
                f.write(("-- %s\n" % doc if doc else "") + "const dep.K %s\ndata dep.d (int x)\n" % value)
            srv.send(did_change(text + "-- %d\n" % k, uri, version=k + 2))
            srv.send(hover(10 + k, at, uri))
            assert srv.read() == ok(10 + k, hover_result("const dep.K " + value, doc, at, stop, False))
        srv.close()


def warm_hovers(text, at, n, cwd, edit=False):
    """The seconds of the first hover and of n more hovers, after an edit
    each when edit is true."""
    uri = "file://" + os.path.join(cwd, "main.stack")
    srv = Session(cwd=cwd)
    srv.send(req(1, "initialize", {"capabilities": {}}))
    assert srv.read() == ok(1, INIT_RESULT)
    t = time.monotonic()
    srv.send(did_open(text, uri))
    srv.send(hover(2, at, uri))
    want = srv.read()
    assert want["result"] is not None, want
    first = time.monotonic() - t
    t = time.monotonic()
    for k in range(n):
        if edit:
            srv.send(did_change(text + "-- %d\n" % k, uri, version=k + 2))
        srv.send(hover(2, at, uri))
        assert srv.read() == want
    warm = time.monotonic() - t
    srv.close()
    return first, warm


@test
def hover_doc_cache():
    """A text is lexed for docs once, not at each hover."""
    big = "".join('const C%d "%s"\n' % (k, "x" * 1500) for k in range(20))
    with tempfile.TemporaryDirectory() as tmp:
        text = big + "-- doc\nfunc f () () in end\n"
        first, warm = warm_hovers(text, position(text, find(text, "f ()")), 20, tmp)
        assert warm < first, (first, warm)
        with open(os.path.join(tmp, "dep.stack"), "w") as f:
            f.write(big + "-- dep doc\nfunc dep.f () () in end\n")
        text = '@import "dep"\nfunc main () () in dep.f end\n'
        first, warm = warm_hovers(text, position(text, find(text, "dep.f")), 20, tmp, edit=True)
        assert warm < first, (first, warm)



# DEFINITIONS


def definition(id, pos, uri=URI):
    return req(id, "textDocument/definition", {"textDocument": {"uri": uri}, "position": pos})


def location(uri, text, i):
    stop = i + len(TOKEN.match(text, i).group())
    return {"uri": uri, "range": {"start": position(text, i), "end": position(text, stop)}}


def file_uri(path):
    return "file://" + "".join(c if c.isalnum() and c.isascii() or c in "-._~/" else
                               "".join("%%%02X" % b for b in c.encode()) for c in path)


def definitions(text, cases, uri=URI, before=(), after=(), **kw):
    """Open text and ask the definition of each case (needle, n, offset,
    want) at offset characters into the n-th needle. want is None for null,
    (needle, n) for a name in text, or (uri, text, needle, n) for a name in
    another file."""
    chunks = [init()] + [frame(m) for m in before] + [frame(did_open(text, uri))] + [frame(m) for m in after]
    want = [ok(1, INIT_RESULT)]
    for k, (needle, n, off, w) in enumerate(cases):
        i = find(text, needle, n)
        chunks.append(frame(definition(10 + k, position(text, i + off), uri)))
        if w is not None and len(w) == 2:
            w = (uri, text) + w
        want.append(ok(10 + k, None if w is None else location(w[0], w[1], find(w[1], w[2], w[3]))))
    chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
    return expect(chunks, want + [ok(2, None)], **kw)


DEFS = ('data pair (int a, bool f)\n'
        'const K 7\n'
        'func two () (int, bool) in 1 true end\n'
        'func id (a) (a) in match v in v end end\n'
        'func f (pair, ptr) (int) in\n'
        '    match p q in\n'
        '        p pair.a two match n b in\n'
        '            b if n else K fi match t in t pop end\n'
        '            n match t in t match u in u t + pop end end\n'
        '            q id match r in r pop end\n'
        '            0 while dup 10 < loop match i in i 1 + end pool pop\n'
        '            p pair.f pop p 1 pair.a.set pair.a.offset pair.sizeof + pair.init pair.& pair.* pop2\n'
        '            int.sizeof ptr.sizeof + __line__ __file__ __col__ pop3 "two" pop -- two\n'
        '            n f\n'
        '        end\n'
        '    end\n'
        'end\n')


@test
def definition_declarations():
    definitions(DEFS, [
        ("pair (", 0, 0, ("pair (", 0)),
        ("a, bool", 0, 0, ("a, bool", 0)),
        ("int a", 0, 0, None),
        ("K 7", 0, 0, ("K 7", 0)),
        ("7", 0, 0, None),
        ("two ()", 0, 1, ("two ()", 0)),
        ("id (a)", 0, 0, ("id (a)", 0)),
        ("(a)", 0, 1, None),
        ("pair, ptr", 0, 0, ("pair (", 0)),
        ("ptr) (int", 0, 0, None),
        ("func f", 0, 0, None),
        ("in\n", 0, 0, None),
        ("p q", 0, 0, ("p q", 0)),
        ("p pair.a", 0, 0, ("p q", 0)),
        ("pair.a two", 0, 3, ("a, bool", 0)),
        ("two match", 0, 0, ("two ()", 0)),
        ("n b in", 0, 0, ("n b in", 0)),
        ("b if", 0, 0, ("b in", 0)),
        ("n else", 0, 0, ("n b in", 0)),
        ("K fi", 0, 0, ("K 7", 0)),
        ("t in t pop", 0, 0, ("t in t pop", 0)),
        ("t pop", 0, 0, ("t in t pop", 0)),
        ("n match t", 0, 0, ("n b in", 0)),
        ("t in t match", 0, 5, ("t in t match", 0)),
        ("u t +", 0, 2, ("t in t match", 0)),
        ("u t +", 0, 0, ("u in u", 0)),
        ("q id", 0, 0, ("q in", 0)),
        ("id match r", 0, 0, ("id (a)", 0)),
        ("v end", 0, 0, ("v in v", 0)),
        ("i 1 +", 0, 0, ("i in i", 0)),
        ("while", 0, 0, None),
        ("dup 10", 0, 0, None),
        ("+ end", 0, 0, None),
        ("pair.f pop", 0, 0, ("f)", 0)),
        ("pair.a.set", 0, 0, ("a, bool", 0)),
        ("pair.a.offset", 0, 0, ("a, bool", 0)),
        ("pair.sizeof", 0, 0, ("pair (", 0)),
        ("pair.init", 0, 0, ("pair (", 0)),
        ("pair.&", 0, 0, ("pair (", 0)),
        ("pair.*", 0, 0, ("pair (", 0)),
        ("int.sizeof", 0, 0, None),
        ("ptr.sizeof", 0, 0, None),
        ("__line__", 0, 0, None),
        ("__file__", 0, 0, None),
        ("__col__", 0, 0, None),
        ('"two"', 0, 1, None),
        ("-- two", 0, 4, None),
        ("n f\n", 0, 2, ("f (pair", 0)),
        ("\nend\n", 0, 1, None),
    ])


@test
def definition_formats():
    """The result does not depend on the hover format of the client."""
    text = 'func g () () in end\nfunc f () () in g end\n'
    for caps in [PLAINTEXT, MARKDOWN, {}]:
        expect([init(caps), frame(did_open(text)), frame(definition(2, pos(1, 16)))],
               [ok(1, INIT_RESULT), ok(2, location(URI, text, 5))])


@test
def definition_precedence():
    text = ('data box (int v)\n'
            'const dup 1\n'
            'func swp () () in end\n'
            'func g () () in end\n'
            'func g (int) () in end\n'
            'const box.v 2\n'
            'const C 3\n'
            'func f (int) () in\n'
            '    dup swp g box.v pop2\n'
            '    1 match g in g pop end\n'
            '    1 match C in C pop end\n'
            '    1 match rot in rot pop end\n'
            '    1 match __line__ in __line__ pop end\n'
            '    1 match x in 2 match x in x end x pop2 end\n'
            'end\n')
    definitions(text, [
        ("dup swp", 0, 0, ("dup 1", 0)),
        ("swp g", 0, 0, None),
        ("swp ()", 0, 0, ("swp ()", 0)),
        ("g box", 0, 0, ("g ()", 0)),
        ("g (int)", 0, 0, ("g (int)", 0)),
        ("box.v pop2", 0, 0, ("v)", 0)),
        ("box.v 2", 0, 0, ("box.v 2", 0)),
        ("g in", 0, 0, ("g in", 0)),
        ("g pop", 0, 0, ("g in", 0)),
        ("C in", 0, 0, ("C in", 0)),
        ("C pop", 0, 0, ("C 3", 0)),
        ("rot in", 0, 0, ("rot in", 0)),
        ("rot pop", 0, 0, None),
        ("__line__ in", 0, 0, ("__line__ in", 0)),
        ("__line__ pop", 0, 0, None),
        ("x end", 0, 0, ("x in x", 0)),
        ("x pop2", 0, 0, ("x in 2", 0)),
    ])


@test
def definition_const_bodies():
    """A binding in a const has a definition but no type."""
    text = ('const swap2 match a b in b a end\n'
            'const twice dup swap2 +\n'
            'func f () (int) in 1 2 swap2 twice pop end\n')
    definitions(text, [
        ("a end", 0, 0, ("a b in", 0)),
        ("b a end", 0, 0, ("b in", 0)),
        ("swap2 +", 0, 0, ("swap2 match", 0)),
        ("twice pop", 0, 0, ("twice dup", 0)),
        ("dup swap2", 0, 0, None),
    ])
    hovers(text, [("a end", 0, 0, None), ("a b in", 0, 0, None)])


@test
def definition_positions():
    """UTF-16 positions in the request and in the result. Names are ASCII,
    as in slc; the text before them on the line is not."""
    with tempfile.TemporaryDirectory() as tmp:
        text = ('-- \U0001F600\u00e9\r\nconst K "\U0001F600\u00e9" func g () () in end -- c\r\n'
                'func f () () in "\U0001F600\u4e2d" pop g K end\n')
        definitions(text, [("g K", 0, 0, ("g ()", 0)), ("K end", 0, 0, ("K ", 0)), ("g ()", 0, 0, ("g ()", 0)),
                           ('"\U0001F600\u4e2d"', 0, 1, None), ("K end", 0, 1, None)])
        dep = os.path.join(os.path.realpath(tmp), "dep.stack")
        dtext = '-- \U0001F600 x\r\n\nconst dep.K "\U0001F600\U0001F600" func dep.f () () in end\n'
        with open(dep, "w", encoding="utf-8") as f:
            f.write(dtext)
        text = '@import "dep"\nfunc f () () in "\u4e2d\U0001F600" pop dep.f dep.K end\n'
        definitions(text, [("dep.f", 0, 0, (file_uri(dep), dtext, "dep.f", 0)),
                           ("dep.f", 0, 4, (file_uri(dep), dtext, "dep.f", 0)),
                           ("dep.K end", 0, 0, (file_uri(dep), dtext, "dep.K", 0))], cwd=tmp)


@test
def definition_imports():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = os.path.realpath(tmp)
        home = os.path.join(tmp, "h o#m%e+é")
        os.makedirs(os.path.join(home, "lib"))
        local = 'data cell (int v)\nfunc local.f (int) (cell) in cell.init end\nconst local.K 1\n'
        shared = '@import "local"\nfunc shared.g () () in end\n'
        with open(os.path.join(tmp, "local.stack"), "w") as f:
            f.write(local)
        with open(os.path.join(home, "lib", "shared.stack"), "w") as f:
            f.write(shared)
        text = '@import "shared"\nfunc main () () in 1 local.f cell.v pop shared.g local.K pop "x" pop end\n'
        luri = file_uri(os.path.join(tmp, "local.stack"))
        suri = file_uri(os.path.join(home, "lib", "shared.stack"))
        cases = [
            ("local.f", 0, 0, (luri, local, "local.f", 0)),
            ("cell.v", 0, 0, (luri, local, "v)", 0)),
            ("shared.g", 0, 0, (suri, shared, "shared.g", 0)),
            ("local.K", 0, 0, (luri, local, "local.K", 0)),
            ('"shared"', 0, 1, None),
        ]
        for h in [home, os.path.relpath(home, tmp), ".//" + os.path.relpath(home, tmp) + "/./"]:
            definitions(text, cases, cwd=tmp, env=dict(os.environ, STACK_HOME=h))
        # ".." stays after a symbolic link: ln/.. is deep, not tmp.
        os.makedirs(os.path.join(tmp, "deep", "sub"))
        os.makedirs(os.path.join(tmp, "deep", "h2", "lib"))
        os.symlink(os.path.join(tmp, "deep", "sub"), os.path.join(tmp, "ln"))
        with open(os.path.join(tmp, "deep", "h2", "lib", "shared.stack"), "w") as f:
            f.write(shared)
        spelled = os.path.join(tmp, "ln", "..", "h2", "lib", "shared.stack")
        assert os.path.exists(spelled) and not os.path.exists(os.path.join(tmp, "h2"))
        definitions(text, [("shared.g", 0, 0, (file_uri(spelled), shared, "shared.g", 0))],
                    cwd=tmp, env=dict(os.environ, STACK_HOME="./ln/../h2"))
        # stdlib from the root of the repository, with STACK_HOME "." and absolute.
        text = '@import "stdlib"\nfunc f (string) (int) in string.len end\n'
        with open(os.path.join(ROOT, "lib", "stdlib.stack"), encoding="utf-8") as f:
            std = f.read()
        want = (file_uri(os.path.join(os.path.realpath(ROOT), "lib", "stdlib.stack")), std, "len, ptr str)", 0)
        definitions(text, [("string.len", 0, 0, want), ("string)", 0, 0, want[:2] + ("string (int len", 0))],
                    cwd=ROOT, env=dict(os.environ, STACK_HOME="."))
        want = (file_uri(os.path.join(ROOT, "lib", "stdlib.stack")),) + want[1:]
        definitions(text, [("string.len", 0, 0, want)], cwd=tmp, env=dict(os.environ, STACK_HOME=ROOT))


@test
def definition_unsaved_imports():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = os.path.join(os.path.realpath(tmp), "a b")
        os.mkdir(tmp)
        path = os.path.join(tmp, "dep.stack")
        disk = 'func dep.old () () in end\nfunc dep.both () () in end\n'
        with open(path, "w") as f:
            f.write(disk)
        dep = "file://" + path.replace(" ", "%20")
        # The client spelling of the URI is kept for an open document.
        open_uri = "file://" + path.replace(" ", "%20").replace("dep.stack", "%64ep.stack")
        uri = "file://" + os.path.join(tmp, "main.stack").replace(" ", "%20")
        text = '@import "dep"\nfunc main () () in dep.old dep.new dep.both end\n'
        unsaved = '\n\nfunc dep.both () () in end\nfunc dep.new (int) (int) in end\n'
        definitions(text, [("dep.old", 0, 0, None), ("dep.new", 0, 0, (open_uri, unsaved, "dep.new", 0)),
                           ("dep.both", 0, 0, (open_uri, unsaved, "dep.both", 0))],
                    uri=uri, before=[did_open(unsaved, open_uri)], cwd=tmp)
        definitions(text, [("dep.old", 0, 0, (dep, disk, "dep.old", 0)), ("dep.new", 0, 0, None),
                           ("dep.both", 0, 0, (dep, disk, "dep.both", 0))],
                    uri=uri, before=[did_open(unsaved, open_uri), did_close(open_uri)], cwd=tmp)
        # A main document that is not a file and is not saved.
        definitions('func f () () in f end\n', [("f end", 0, 0, ("f ()", 0))], uri="untitled:Untitled-1")


@test
def definition_import_changes():
    """didChange of an open import moves the target, and of the main file
    changes its names."""
    with tempfile.TemporaryDirectory() as tmp:
        tmp = os.path.realpath(tmp)
        path = os.path.join(tmp, "dep.stack")
        with open(path, "w") as f:
            f.write("func dep.f () () in end\n")
        dep = file_uri(path)
        uri = file_uri(os.path.join(tmp, "main.stack"))
        text = '@import "dep"\nfunc main () () in dep.f end\n'
        at = position(text, find(text, "dep.f"))
        srv = Session(cwd=tmp)
        srv.send(req(1, "initialize", {"capabilities": {}}))
        assert srv.read() == ok(1, INIT_RESULT)
        srv.send(did_open(text, uri))
        srv.send(did_open("func dep.f () () in end\n", dep))
        for k in range(4):
            dtext = "\n" * k + "-- " + "\U0001F600" * k + "\nfunc  dep.f () () in end\n"
            srv.send(did_change(dtext, dep, version=k + 2))
            srv.send(definition(10 + k, at, uri))
            assert srv.read() == ok(10 + k, location(dep, dtext, find(dtext, "dep.f")))
        srv.send(did_change(dtext.replace("dep.f", "dep.g"), dep, version=9))
        srv.send(definition(20, at, uri))
        assert srv.read() == ok(20, None)
        text2 = "\n" + text.replace("dep.f end", "main end")
        srv.send(did_change(text2, uri, version=3))
        srv.send(definition(21, position(text2, find(text2, "main end")), uri))
        assert srv.read() == ok(21, location(uri, text2, find(text2, "main ()")))
        srv.close()


@test
def definition_errors():
    """Bad params, closed documents, positions out of the text and source
    with errors."""
    bad = [None, {}, {"textDocument": {"uri": URI}}, {"position": pos(0, 0)},
           {"textDocument": {"uri": 5}, "position": pos(0, 0)},
           {"textDocument": {"uri": URI}, "position": pos(-1, 0)},
           {"textDocument": {"uri": URI}, "position": pos(0.5, 0)},
           {"textDocument": {"uri": URI}, "position": pos(2147483648, 0)}]
    text = "func f () () in f end\n"
    chunks = [init(), frame(did_open(text))]
    chunks += [frame(req(10 + i, "textDocument/definition", p)) for i, p in enumerate(bad)]
    chunks += [frame(definition(2, pos(0, 16))), frame(definition(3, pos(5, 0))), frame(definition(4, pos(0, 99))),
               frame(definition(5, pos(0, 16), "file:///other.stack")), frame(did_close()),
               frame(definition(6, pos(0, 16)))]
    expect(chunks, [ok(1, INIT_RESULT)] + [err(10 + i, -32602) for i in range(len(bad))] +
           [ok(2, location(URI, text, 5)), ok(3, None), ok(4, None), ok(5, None), ok(6, None)])
    expect([frame(definition(1, pos(0, 0)))], [err(1, -32002)])
    # The names before a syntax error resolve; the names after it do not.
    text = ('func good () (int) in 1 end\n'
            'func user () (int) in good end\n'
            'func bad () (int) in good 1 match x in x \n'
            'func later () (int) in good end\n')
    definitions(text, [("good", 1, 0, ("good ()", 0)), ("user", 0, 0, ("user", 0)), ("good", 2, 0, None),
                       ("x \n", 0, 0, None), ("later", 0, 0, None), ("good", 3, 0, None)])
    # Type errors, unknown names, duplicates and errors of other functions.
    text = ('func a () () in 1 match y in y y + "s" + end end\n'
            'func b () () in missing dup dup b end\n'
            'data d (int x)\ndata d (bool y)\n'
            'func c () () in d.x d.y end\n')
    definitions(text, [("y y", 0, 0, ("y in", 0)), ("missing", 0, 0, None), ("b end", 0, 0, ("b ()", 0)),
                       ("d.x", 0, 0, ("x)", 0)), ("d.y", 0, 0, ("y)", 0))])
    env = dict(os.environ, STACK_HOME=ROOT)
    for text in BROKEN:
        first = text.split("\n")[0]
        chunks = [init(), frame(did_open(text))]
        chunks += [frame(definition(10 + k, pos(0, k))) for k in range(min(len(first), 60) + 1)]
        chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
        status, got, errs = run(*chunks, cwd=ROOT, env=env)
        assert status == 0, (text[:40], status, errs[-200:])
        assert len(got) == len(chunks) - 2 and got[-1] == ok(2, None), (text[:40], got[-3:])



# COMPLETION


def completion(id, pos, uri=URI):
    return req(id, "textDocument/completion", {"textDocument": {"uri": uri}, "position": pos})


def name_char(c):
    return not c.isspace() and c not in '(),"'


def token_at(text, i):
    """The name token that a completion at text[i] replaces, as in the server."""
    start, stop = i, i
    while start > 0 and name_char(text[start - 1]):
        start -= 1
    if start == i and text.startswith("--", i):
        return i, i
    while stop < len(text) and name_char(text[stop]):
        stop += 1
    return start, stop


def check_items(result, text, i, want):
    """want is [] for no items, else a dict: has {label: (kind, detail)} with
    detail None when there is none, absent [labels], only {labels}."""
    assert result is not None and result["isIncomplete"] is False, result
    items = result["items"]
    if want == []:
        assert items == [], [it["label"] for it in items][:20]
        return
    start, stop = token_at(text, i)
    prefix = text[start:i]
    rng = {"start": position(text, start), "end": position(text, stop)}
    labels = [it["label"] for it in items]
    assert len(labels) == len(set(labels)), "duplicate labels"
    for it in items:
        assert it["label"].startswith(prefix), (prefix, it)
        assert it["textEdit"] == {"range": rng, "newText": it["label"]}, it
        assert set(it) <= {"label", "kind", "detail", "textEdit"}, it
    byname = {it["label"]: it for it in items}
    for label, (kind, detail) in want.get("has", {}).items():
        assert label in byname, (label, prefix, labels[:40])
        assert byname[label]["kind"] == kind, byname[label]
        assert byname[label].get("detail") == detail, byname[label]
    for label in want.get("absent", []):
        assert label not in byname, (label, byname.get(label))
    if "only" in want:
        assert set(labels) == set(want["only"]), (prefix, sorted(labels))


def completions(text, cases, uri=URI, before=(), after=(), **kw):
    """Open text and complete at each case (needle, n, offset, want), at
    offset characters into the n-th needle. want is None for a null result."""
    chunks = [init()] + [frame(m) for m in before] + [frame(did_open(text, uri))] + [frame(m) for m in after]
    at = []
    for k, (needle, n, off, w) in enumerate(cases):
        i = find(text, needle, n) + off
        at.append(i)
        chunks.append(frame(completion(10 + k, position(text, i), uri)))
    chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
    status, got, errs = run(*chunks, **kw)
    assert status == 0, (status, errs[-300:])
    assert len(got) == len(cases) + 2 and got[0] == ok(1, INIT_RESULT) and got[-1] == ok(2, None), got[-2:]
    for k, ((needle, n, off, w), i) in enumerate(zip(cases, at)):
        g = got[k + 1]
        assert g.get("id") == 10 + k and "result" in g, g
        try:
            if w is None:
                assert g["result"] is None, g
            else:
                check_items(g["result"], text, i, w)
        except AssertionError as e:
            raise AssertionError("case %r: %s" % ((needle, n, off), e))
    return got


FUNC, CTOR, FIELD, VAR, CONST, STRUCT = 3, 4, 5, 6, 21, 22


def builtins_with(prefix):
    return {n for n, _, _ in SIGNATURES if n.startswith(prefix)}


COMP = ('data pair (int a, bool f)\n'
        'const K 7\n'
        'const K.two 2\n'
        'func two () (int, bool) in 1 true end\n'
        'func id (a) (a) in match v in v end end\n'
        'func f (pair, ptr) (int) in\n'
        '    match p q in\n'
        '        p pair.a two match n b in\n'
        '            b if n else K fi match t in t pop end\n'
        '            n match t in t match u in u t + pop end end\n'
        '            q id match r in r pop end\n'
        '            0 while dup 10 < loop match i in i 1 + end pool pop\n'
        '            "str two" pop 12 pop -- comment two\n'
        '            n\n'
        '        end\n'
        '    end\n'
        'end\n')
PAIR = {"pair.a", "pair.f", "pair.a.set", "pair.f.set", "pair.a.offset", "pair.f.offset", "pair.init",
        "pair.sizeof", "pair.&", "pair.*"}


@test
def completion_names():
    completions(COMP, [
        ("pair.a two", 0, 0, {"has": {
            "pair.a": (FIELD, "pair.a (pair) (int)"), "pair.f.set": (FIELD, "pair.f.set (pair, bool) ()"),
            "pair.a.offset": (CONST, "pair.a.offset () (int)"), "pair.init": (CTOR, "pair.init (int, bool) (pair)"),
            "pair.sizeof": (CONST, "pair.sizeof () (int)"), "pair.&": (FUNC, "func pair.& (pair) (ptr)"),
            "K": (CONST, None), "K.two": (CONST, None), "dup": (FUNC, "func dup (a) (a, a)"),
            "int.sizeof": (CONST, "int.sizeof () (int)"), "p": (VAR, "pair"), "q": (VAR, "ptr"),
            "two": (FUNC, "func two () (int, bool)"), "f": (FUNC, "func f (pair, ptr) (int)"), "id": (FUNC, "func id (a) (a)")},
            "absent": ["n", "b", "v", "pair", "int", "a", "func", "match", "__line__"]}),
        ("pair.a two", 0, 5, {"only": PAIR}),
        ("pair.a two", 0, 3, {"only": PAIR}),
        ("pair.a two", 0, 6, {"only": {"pair.a", "pair.a.set", "pair.a.offset"}}),
        ("two match", 0, 2, {"only": {"two"}}),
        ("K fi", 0, 1, {"only": {"K", "K.two"}}),
        ("b if", 0, 1, {"has": {"b": (VAR, "bool"), "bool.&": (FUNC, "func bool.& (bool) (ptr)")}, "absent": ["bool"]}),
        ("t pop", 0, 0, {"has": {"t": (VAR, "int"), "n": (VAR, "int"), "p": (VAR, "pair")}, "absent": ["u", "r"]}),
        ("u t +", 0, 0, {"has": {"u": (VAR, "int"), "t": (VAR, "int"), "n": (VAR, "int")}}),
        ("+ pop", 0, 1, {"only": {"+"}}),
        ("q id", 0, 1, {"only": {"q"}}),
        ("q id", 0, 0, {"has": {"q": (VAR, "ptr")}, "absent": ["t", "u", "r", "i", "v"]}),
        ("r pop", 0, 1, {"has": {"r": (VAR, "ptr"), "rot": (FUNC, "func rot (a, b, c) (b, c, a)")}}),
        ("i 1 +", 0, 1, {"has": {"i": (VAR, "int"), "id": (FUNC, "func id (a) (a)")}}),
        ("while dup", 0, 2, {"only": set()}),
        ("\n            n\n", 0, 13, {"has": {"n": (VAR, "int"), "b": (VAR, "bool")}, "absent": ["i", "t", "r"]}),
        ('"str two"', 0, 0, {"has": {"two": (FUNC, "func two () (int, bool)")}}),
        ('"str two"', 0, 3, []),
        ('"str two"', 0, 9, []),
        ("12 pop", 0, 1, []),
        ("12 pop", 0, 2, []),
        ("-- comment two", 0, 0, {"has": {"n": (VAR, "int")}}),
        ("-- comment two", 0, 1, []),
        ("-- comment two", 0, 13, []),
        ("two ()", 0, 2, []),
        ("K 7", 0, 1, []),
        ("K 7", 0, 3, []),
        ("pair (", 0, 2, []),
        ("a, bool", 0, 1, []),
        ("p q", 0, 1, []),
        ("n b in", 0, 1, []),
        ("(int) in\n", 0, 7, []),
        ("pair, ptr", 0, 2, {"only": {"pair"}, "has": {"pair": (STRUCT, "data pair (int a, bool f)")}}),
        ("ptr) (int", 0, 1, {"only": {"ptr", "pair"}, "has": {"ptr": (STRUCT, "data ptr")}}),
        ("(pair, ptr", 0, 1, {"only": {"int", "ptr", "bool", "pair"}}),
        ("(a)", 0, 1, {"only": {"int", "ptr", "bool", "pair"}}),
        ("(a) (a)", 0, 3, []),
        ("int a", 0, 1, {"only": {"int"}}),
        ("bool f", 0, 2, {"only": {"bool"}}),
        ("data", 0, 2, []),
        ("\nend\n", 0, 4, []),
    ])


@test
def completion_partial():
    """Code that the parser does not accept yet: the scan of the tokens still
    finds the bindings in scope and the definitions after the error."""
    text = ('func early () () in end\n'
            'func g (int) () in\n'
            '    1 match x in x ptr. 1 2 < \n'
            '    2 match y z\n'
            'func later () () in end\n'
            'const LATE 1\n'
            'data late (int v)\n'
            'func h (la) () in 1 match w in w end ')
    completions(text, [
        ("x ptr", 0, 1, {"has": {"x": (VAR, None)}}),
        ("x ptr", 0, 2, {"has": {"x": (VAR, None), "early": (FUNC, "func early () ()"), "later": (FUNC, None),
                                 "LATE": (CONST, None), "g": (FUNC, None), "h": (FUNC, None)},
                         "absent": ["late", "y", "z", "w", "int", "v"]}),
        ("ptr. ", 0, 4, {"only": builtins_with("ptr.") | {"ptr.sizeof"}}),
        ("< \n", 0, 1, {"only": {"<", "<<"}}),
        ("y z", 0, 1, []),
        ("y z", 0, 3, []),
        ("(la)", 0, 3, {"only": {"late"}, "has": {"late": (STRUCT, None)}}),
        ("w end", 0, 1, {"only": {"w"}}),
        ("w end ", 0, 6, {"absent": ["w", "x"], "has": {"later": (FUNC, None)}}),
    ])


@test
def completion_scopes():
    text = ('func g () () in\n'
            '    1 match x in x end x\n'
            '    2 match x in x 3 match x y in x y end y end\n'
            '    if 1 match a in a end fi a\n'
            '    while true loop 1 match c in c end pool c\n'
            'end\n'
            'const swap2 match sa sb in sb sa end\n'
            'func k () () in 1 match g in g end end\n')
    completions(text, [
        ("x end x", 0, 1, {"only": {"x"}, "has": {"x": (VAR, "int")}}),
        ("x end x", 0, 7, {"only": set()}),
        ("x 3", 0, 1, {"only": {"x"}}),
        # The inner x is a type error, so it has no type.
        ("x y end", 0, 1, {"only": {"x"}, "has": {"x": (VAR, None)}}),
        ("y end y", 0, 1, {"only": {"y"}}),
        ("y end y", 0, 7, {"only": set()}),
        ("a end fi", 0, 1, {"only": {"a"}}),
        ("a\n", 0, 1, {"only": set()}),
        ("c end pool", 0, 1, {"only": {"c"}}),
        ("c\n", 0, 1, {"only": set()}),
        ("sb sa end", 0, 1, {"has": {"sb": (VAR, None)}}),
        ("sa end", 0, 2, {"has": {"sa": (VAR, None)}}),
        # A binding with the name of a func is a type error but it is in scope.
        ("g end", 0, 1, {"only": {"g"}, "has": {"g": (VAR, None)}}),
    ])


@test
def completion_precedence():
    text = ('data box (int v)\n'
            'const dup 1\n'
            'func swp () () in end\n'
            'func gg () () in end\n'
            'func gg (int) () in end\n'
            'const box.v 2\n'
            'const C 3\n'
            'func f (int) () in\n'
            '    1 match d in 1 "s" + match e in 1 match gg in 1 match C in\n'
            '        du sw gg box.v C d e\n'
            '    end end end end\n'
            'end\n')
    completions(text, [
        ("du sw", 0, 2, {"has": {"dup": (CONST, None)}}),
        ("sw gg", 0, 2, {"only": {"swp"}, "has": {"swp": (FUNC, "func swp (a, b) (b, a)")}}),
        ("gg box", 0, 2, {"only": {"gg"}, "has": {"gg": (VAR, None)}}),
        ("box.v C", 0, 5, {"only": {"box.v", "box.v.set", "box.v.offset"}, "has": {"box.v": (FIELD, "box.v (box) (int)")}}),
        ("C d", 0, 1, {"only": {"C"}, "has": {"C": (CONST, None)}}),
        # f uses names with two definitions, so the checker gives no types.
        ("d e", 0, 1, {"has": {"d": (VAR, None), "dup": (CONST, None)}}),
        ("e\n", 0, 1, {"only": {"e"}, "has": {"e": (VAR, None)}}),
    ])
    # A special name is a literal unless a constant has its name. "--" in a
    # name is not a comment.
    text = ('const __col__ 4\n'
            'func __file__ () () in end\n'
            'func f () () in 1 match __line__ in 1 match a--b in __ a-- end end -- a--\n'
            'end\n')
    completions(text, [("__ a", 0, 2, {"only": {"__col__"}, "has": {"__col__": (CONST, None)}}),
                       ("a-- end", 0, 3, {"only": {"a--b"}, "has": {"a--b": (VAR, "int")}}),
                       ("-- a--", 0, 5, [])])
    # A constant that the parser did not get to is in the constant tier: it
    # hides a builtin and a special name. A func that the parser did not get
    # to is in the definition tier, after the builtins.
    text = ('func f () () in du __ sw\n'
            'const dup 1\n'
            'const __line__ 2\n'
            'func swp () () in end\n')
    completions(text, [("du __", 0, 2, {"only": {"dup"}, "has": {"dup": (CONST, None)}}),
                       ("__ sw", 0, 2, {"only": {"__line__"}, "has": {"__line__": (CONST, None)}}),
                       ("sw\n", 0, 2, {"only": {"swp"}, "has": {"swp": (FUNC, "func swp (a, b) (b, a)")}})])


@test
def completion_imports():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = os.path.realpath(tmp)
        path = os.path.join(tmp, "dep.stack")
        with open(path, "w") as f:
            f.write('func dep.old () () in end\n')
        dep = file_uri(path)
        uri = file_uri(os.path.join(tmp, "main.stack"))
        text = '@import "dep"\nfunc main (dc) () in dep. end\n'
        unsaved = 'data dcell (int v)\nfunc dep.new (int) (int) in end\n'
        cases = [("dep. end", 0, 4, {"only": {"dep.new"}, "has": {"dep.new": (FUNC, "func dep.new (int) (int)")}}),
                 ("dc)", 0, 2, {"only": {"dcell"}}), ('"dep"', 0, 2, [])]
        completions(text, cases, uri=uri, before=[did_open(unsaved, dep)], cwd=tmp)
        completions(text, [("dep. end", 0, 4, {"only": {"dep.old"}}), ("dc)", 0, 2, {"only": set()})],
                    uri=uri, before=[did_open(unsaved, dep), did_close(dep)], cwd=tmp)
        completions(text, [("dep. end", 0, 4, {"only": {"dep.old", "dep.newer"}})], uri=uri,
                    before=[did_open(unsaved, dep), did_change('func dep.old () () in end\nfunc dep.newer () () in end\n', dep)],
                    cwd=tmp)
        text = '@import "stdlib"\nfunc f (str) () in string.l end\n'
        completions(text, [("string.l", 0, 8, {"has": {"string.len": (FIELD, "string.len (string) (int)")},
                                                "absent": ["string.str"]}),
                           ("str)", 0, 3, {"has": {"string": (STRUCT, "data string (int len, ptr str)")}})],
                    cwd=ROOT, env=dict(os.environ, STACK_HOME=ROOT))


@test
def completion_changes():
    """didChange of the open file changes the names and the bindings."""
    srv = Session()
    srv.send(req(1, "initialize", {"capabilities": {}}))
    assert srv.read() == ok(1, INIT_RESULT)
    text = 'func f () () in 1 match aa in a'
    srv.send(did_open(text))
    for k, extra in enumerate(["", " end aa", " 2 match ab in a"]):
        t = text + extra
        if k:
            srv.send(did_change(t, version=k + 2))
        srv.send(completion(10 + k, position(t, len(t))))
        got = srv.read()
        assert got["id"] == 10 + k
        labels = {it["label"] for it in got["result"]["items"]}
        want = [{"aa"}, set(), {"aa", "ab"}][k]
        assert labels == want, (k, labels)
    srv.close()


@test
def completion_positions():
    """UTF-16 positions in the request and in the text edit."""
    text = ('-- \U0001F600\u00e9\r\nfunc gg () () in "\U0001F600\u00e9" pop g -- c\r\n'
            '"\u4e2d" pop g end -- \U0001F600 g\n')
    completions(text, [("g -- c", 0, 1, {"only": {"gg"}}), ("g -- c", 0, 0, {"has": {"gg": (FUNC, "func gg () ()")}}),
                       ("g end", 0, 1, {"only": {"gg"}}), ('"\U0001F600', 0, 1, []), ("\U0001F600 g", 0, 2, [])])


@test
def completion_errors():
    bad = [None, {}, {"textDocument": {"uri": URI}}, {"position": pos(0, 0)},
           {"textDocument": {"uri": 5}, "position": pos(0, 0)},
           {"textDocument": {"uri": URI}, "position": pos(-1, 0)},
           {"textDocument": {"uri": URI}, "position": pos(2147483648, 0)}]
    text = "func f () () in f"
    chunks = [init(), frame(did_open(text))]
    chunks += [frame(req(10 + i, "textDocument/completion", p)) for i, p in enumerate(bad)]
    chunks += [frame(completion(2, pos(5, 0))), frame(completion(3, pos(0, 0), "file:///other.stack")),
               frame(completion(4, pos(0, 99))), frame(completion(5, pos(0, 16)))]
    chunks += [frame(did_close()), frame(completion(6, pos(0, 16)))]
    status, got, errs = run(*chunks)
    assert status == 0, errs
    match(got[:-3], [ok(1, INIT_RESULT)] + [err(10 + i, -32602) for i in range(len(bad))] +
          [ok(2, None), ok(3, None)])
    assert got[-1] == ok(6, None) and [g["id"] for g in got[-3:-1]] == [4, 5], got[-3:]
    check_items(got[-3]["result"], text, len(text), {"only": {"f"}})
    # The func is not complete, so its name comes from the scan, without detail.
    check_items(got[-2]["result"], text, 16, {"has": {"f": (FUNC, None)}})
    expect([frame(completion(1, pos(0, 0)))], [err(1, -32002)])
    env = dict(os.environ, STACK_HOME=ROOT)
    for text in BROKEN + ["func f () () in " + "match x in " * 2000 + "x"]:
        first = text.split("\n")[0]
        chunks = [init(), frame(did_open(text))]
        chunks += [frame(completion(10 + k, pos(0, k))) for k in list(range(min(len(first), 60) + 1)) + [len(first)]]
        chunks += [frame(req(2, "shutdown")), frame(note("exit"))]
        status, got, errs = run(*chunks, cwd=ROOT, env=env)
        assert status == 0, (text[:40], status, errs[-200:])
        assert len(got) == len(chunks) - 2 and got[-1] == ok(2, None), (text[:40], got[-3:])
        for g in got[1:-1]:
            r = g["result"]
            assert r is not None and len({it["label"] for it in r["items"]}) == len(r["items"]), (text[:40], g)


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
