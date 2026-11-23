-- Run by `run.sh syntax` in Neovim with `--clean`. The runtimepath has
-- editors/neovim. Checks only syntax/stack.vim. Exits with 0 when all
-- checks pass and with 1 at the first failed check.

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

vim.api.nvim_buf_set_lines(0, 0, -1, false, {
  "-- The C add.",
  'extern c.add (int, int) (int) "add"',
  'extern c.extern.f (float) () "f" -- extern',
  "func main () (int) in",
  "    40 2 c.add extern.x",
  "end",
})
vim.cmd("syntax on")
vim.cmd("setfiletype stack")
check(vim.b.current_syntax == "stack", "b:current_syntax is stack")

local function group(line, col)
  return vim.fn.synIDattr(vim.fn.synID(line, col, 1), "name")
end
local function expect(line, col, name, what)
  local got = group(line, col)
  check(got == name, ("%s at %d:%d is %q (got %q)"):format(what, line, col, name, got))
end

expect(1, 1, "stackComment", "doc comment")
expect(2, 1, "stackStatement", "extern")
expect(2, 6, "stackStatement", "end of extern")
expect(2, 8, "", "extern name")
expect(2, 15, "stackType", "argument type after (")
expect(2, 20, "stackType", "argument type after ,")
expect(2, 26, "stackType", "result type")
expect(2, 31, "stackString", "C symbol")
expect(3, 8, "", "extern inside the name c.extern.f")
expect(3, 10, "", "extern inside the name c.extern.f")
expect(3, 20, "stackType", "float type")
expect(3, 34, "stackComment", "comment after the symbol")
expect(3, 37, "stackComment", "extern in a comment")
expect(5, 17, "", "extern inside the name extern.x")
expect(4, 1, "stackStatement", "func")

vim.cmd("qall!")
