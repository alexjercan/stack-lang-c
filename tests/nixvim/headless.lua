-- Run by `run.sh headless` in Neovim from nvim.nix, with the current
-- directory set to an empty temporary project. Exits with 0 when all checks
-- pass and with 1 at the first failed check.

local function fail(msg)
  io.stderr:write("FAIL: " .. msg .. "\n")
  vim.cmd("cquit 1")
end

local function check(ok, msg)
  if not ok then
    fail(msg)
  end
  io.stdout:write("ok: " .. msg .. "\n")
end

local root = vim.fn.getcwd()
vim.fn.mkdir(root .. "/.git", "p")
vim.fn.writefile({
  '@import "stdlib"',
  "",
  "-- TODO: hover",
  "data point (int x, int y)",
  "",
  "func generate.data (int) (int) in",
  '    dup -1 + true if "a\\n" string.stdout fi',
  "    while dup 0 > loop 1 - pool",
  "end",
  "",
  "func main () (int) in",
  "    0 generate.data",
  "end",
}, root .. "/main.stack")

vim.v.errmsg = ""
vim.cmd("edit main.stack")
local buf = vim.api.nvim_get_current_buf()

check(vim.v.errmsg == "", "no error when the file opens: " .. vim.v.errmsg)

check(vim.bo[buf].filetype == "stack", "filetype is stack")
check(vim.b[buf].current_syntax == "stack", "b:current_syntax is stack")
local files = vim.api.nvim_get_runtime_file("syntax/stack.vim", true)
check(#files == 1, "one syntax/stack.vim in runtimepath: " .. vim.inspect(files))

-- Syntax group at a 1-based line and column.
local function group(line, col)
  return vim.fn.synIDattr(vim.fn.synID(line, col, 1), "name")
end
local function expect(line, col, name, what)
  local got = group(line, col)
  check(got == name, ("%s at %d:%d is %q (got %q)"):format(what, line, col, name, got))
end

expect(1, 1, "stackInclude", "@import")
expect(1, 10, "stackString", "import path")
expect(3, 1, "stackComment", "comment")
expect(3, 4, "stackTodo", "TODO")
expect(4, 1, "stackStatement", "data")
expect(4, 6, "stackDataType", "data type name")
expect(4, 13, "stackType", "field type after (")
expect(4, 20, "stackType", "field type after ,")
expect(6, 1, "stackStatement", "func")
expect(6, 15, "", "data inside the name generate.data")
expect(6, 21, "stackType", "input type")
expect(6, 33, "stackStatement", "in")
expect(7, 9, "stackNumber", "-1")
expect(7, 15, "stackBoolean", "true")
expect(7, 20, "stackConditional", "if")
expect(7, 25, "stackSpecial", "string escape")
expect(8, 5, "stackRepeat", "while")
expect(8, 15, "stackNumber", "0")

local function clients()
  return vim.lsp.get_clients({ bufnr = buf })
end
check(vim.wait(10000, function()
  return #clients() > 0 and clients()[1].initialized
end, 50), "stackls attaches")
check(#clients() == 1, "one client attached")
local client = clients()[1]
check(client.name == "stackls", "client name is stackls")
check(client.config.cmd[1]:match("^/nix/store/.+/bin/stackls$") ~= nil,
  "cmd is an absolute store path: " .. client.config.cmd[1])
local home = client.config.cmd_env and client.config.cmd_env.STACK_HOME
check(home ~= nil and client.config.cmd[1] == home .. "/bin/stackls",
  "STACK_HOME is the package of cmd: " .. tostring(home))
check(client.root_dir == root, "root_dir from root_markers: " .. tostring(client.root_dir))
check(client.server_capabilities.hoverProvider == true, "server has hoverProvider")

local function hover(line, col)
  local params = {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    position = { line = line - 1, character = col - 1 },
  }
  local res, err = client:request_sync("textDocument/hover", params, 10000, buf)
  if res == nil or res.err ~= nil or res.result == nil then
    fail(("hover at %d:%d: %s"):format(line, col, vim.inspect(err or res)))
  end
  return res.result.contents.value
end

local dup = hover(7, 5)
check(dup:find("dup", 1, true) ~= nil, "hover on the builtin dup: " .. dup)
local stdout = hover(7, 30)
check(stdout:find("string.stdout", 1, true) ~= nil,
  "hover on string.stdout from STACK_HOME/lib/std/stdio.stack: " .. stdout)
local local_func = hover(12, 7)
check(local_func:find("func generate.data", 1, true) ~= nil,
  "hover on a function of the file: " .. local_func)

-- The module adds no keymaps. K and other LSP keys belong to the user.
for _, mode in ipairs({ "n", "x" }) do
  local global_k = vim.tbl_filter(function(map)
    return map.lhs == "K"
  end, vim.api.nvim_get_keymap(mode))
  check(#global_k == 0, "no global K mapping in mode " .. mode)
end

client:stop()
vim.cmd("qall!")
