if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword     Statement       data nextgroup=DataType skipwhite
syn keyword     Statement       const func in end match
syn keyword     Conditional     if else fi while loop pool
syn keyword     Boolean         true false

syn match       Include "@import"

" anything after data is a type
syn match       DataType  "[^,() ]\+" contained
hi def link DataType            Type

" anything after ( or , is a type
syn match       Type  "(\zs\s*[^,() ]*\ze"
syn match       Type  ",\zs\s\+[^,() ]*\ze"

" C style integers ripped from c.vim
syn match       cNumbers        display transparent "\<\d\|\.\d" contains=cNumber
syn match       cNumber         display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
hi def link cNumber             Number

" C style strings ripped from c.vim
syn match       cSpecial    display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn region      String  start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial,@Spell
hi def link cSpecial            SpecialChar

" haskell style comment
syn match coolComment "--.*$" contains=coolTodo
hi def link coolComment         Comment

syn keyword  coolTodo contained TODO FIXME XXX
hi def link coolTodo            Todo
