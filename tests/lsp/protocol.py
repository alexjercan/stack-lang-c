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
    "capabilities": {"hoverProvider": True, "textDocumentSync": {"openClose": True, "change": 1}},
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
    got, _ = expect([frame(INIT), frame(req(2, "textDocument/completion", {})), frame(req(3, "textDocument/definition", {})),
                     frame(req(4, "$/unknown")), frame(req(5, "a\"b\\")), frame(note("$/cancelRequest", {"id": 2})),
                     frame(note("workspace/didChangeConfiguration", {})), frame(note("unknown")),
                     frame(req(6, "initialize", {})), frame(req(7, "shutdown"))],
                    [ok(1, INIT_RESULT), err(2, -32601), err(3, -32601), err(4, -32601), err(5, -32601),
                     err(6, -32600), ok(7, None)])
    assert got[4]["error"]["message"] == "Method not found: a\"b\\"


@test
def escaped_methods():
    got, _ = expect([frame(INIT), frame(b'{"jsonrpc":"2.0","id":2,"method":"textDocument\\/definition"}'),
                     frame(b'{"jsonrpc":"2.0","id":3,"method":"\\u0073hutdown"}'),
                     frame(b'{"jsonrpc":"2.0","method":"\\u0065xit"}')],
                    [ok(1, INIT_RESULT), err(2, -32601), ok(3, None)])
    assert got[1]["error"]["message"] == "Method not found: textDocument/definition"


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


def literal(text):
    """Markdown that shows text as it is."""
    out, prev = [], 0
    for line in text.split("\n"):
        blank = line.strip(" \t") == ""
        out.append(("\\" if prev == 2 and not blank else "") + ("\n" if prev else ""))
        ws = True
        for c in "" if blank else line:
            if c == "\t":
                out.append("&nbsp;" * 4)
            elif c == " ":
                out.append("&nbsp;" if ws else " ")
            else:
                out.append("\\" + c if c.isascii() and not c.isalnum() and c.isprintable() else c)
            ws = c in " \t"
        prev = 1 if blank else 2
    return "".join(out)


def builtin(name, markdown=True):
    sig, doc = next((s, d) for n, s, d in SIGNATURES if n == name)
    return "func %s %s" % (name, sig), literal(doc) if markdown else doc


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
    value = "```stack\n" + code + "\n```" if markdown else code
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
        ("N", 0, 0, None),
        ("N", 1, 0, None),
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
            ("string", 0, 1, ("data string (int len, ptr str)", "")),
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
               [ok(1, INIT_RESULT), ok(2, hover_result(*builtin("dup", markdown), pos(0, 19), pos(0, 22), markdown))])


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
DOC_F_MD = ("First line\\.\n\n&nbsp;&nbsp;\\`code\\` \\*x\\* \\_y\\_ \\<b\\>t\\<\\/b\\> \\\\ \\[a\\]\\(b\\) 1\\. \\# \\& "
            "\\&amp\\; a &nbsp;b&nbsp;&nbsp;&nbsp;&nbsp;c")


@test
def hover_docs():
    for caps, doc_f, doc_g, doc_j in [(PLAINTEXT, DOC_F, "Body of g.", "no space\n-- dashes\nindent"),
                                      (MARKDOWN, DOC_F_MD, "Body of g\\.", "no space\\\n\\-\\- dashes\\\nindent")]:
        hovers(DOCS, [
            ("g ()", 0, 0, ("func g () ()", doc_g)),
            ("f ()", 0, 0, ("func f () ()", doc_f)),
            ("g end", 0, 0, ("func g () ()", doc_g)),
            ("f end", 0, 0, ("func f () ()", doc_f)),
            ("h ()", 0, 0, ("func h () ()", "")),
            ("i ()", 0, 0, ("func i () ()", "")),
            ("j ()", 0, 0, ("func j () ()", doc_j)),
            ("K", 0, 0, None),
        ], caps=caps)


@test
def hover_builtin_docs():
    text = "func main () () in\n" + "".join("    %s\n" % name for name, _, _ in SIGNATURES) + "end\n"

    def at(name):
        """The needle and n of name on its own line."""
        i = text.index("\n    %s\n" % name) + 5
        return name, sum(text.startswith(name, k) for k in range(i)), 0

    for caps, markdown in [(PLAINTEXT, False), (MARKDOWN, True)]:
        hovers(text, [at(name) + (builtin(name, markdown),) for name, _, _ in SIGNATURES], caps=caps)
    hovers(text, [
        at("rot") + (("func rot (a, b, c) (b, c, a)", "Rotate \\(a\\, b\\, c\\) to \\(b\\, c\\, a\\)\\."),),
        at("ptr.@") + (("func ptr.@ (ptr, ptr, int) (ptr)", "Copy n bytes from src to dst and return dst\\."),),
        at("<<") + (("func << (int, int) (int)", "Shift left"),),
        at("<") + (("func < (int, int) (bool)", "Less than\\."),),
        at("&") + (("func & (int, int) (int)", "Bitwise AND\\."),),
        at("int.&") + (("func int.& (int) (ptr)", "Address of the value\\."),),
        at("pick") + (("func pick (int) (ptr)", "duplicate item n on top of the stack as a pointer"),),
    ])
    assert literal(DOC_F) == DOC_F_MD


@test
def hover_doc_lines():
    text = ("func a () () in\n  -- body\nend\n-- x\n\n-- doc ü \U0001F600\r\n-- two\r-- three\rraw\n\t-- tab\r\n"
            "func b () () in a end\n")
    hovers(text, [("b ()", 0, 0, ("func b () ()", "doc ü \U0001F600\ntwo\nthree\nraw\ntab")),
                  ("a end", 0, 0, ("func a () ()", ""))], caps=PLAINTEXT)
    hovers(text, [("b ()", 0, 0, ("func b () ()", "doc ü \U0001F600\\\ntwo\\\nthree\\\nraw\\\ntab"))])
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
        hovers(text, [("dep.f", 0, 0, ("func dep.f () ()", "disk \\*doc\\*"))], uri=uri, cwd=tmp)
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
