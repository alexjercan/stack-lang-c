" Vim syntax file
" Language: Stack
" Filenames: *.stack

if exists("b:current_syntax")
  finish
endif

" A name is a sequence of printable ASCII characters other than space, '"',
" '(', ')' and ','. Keywords match only full names, so `generate.data` does
" not contain the keyword `data`.
syn iskeyword 33,35-39,42-43,45-126

syn keyword     stackStatement      data nextgroup=stackDataType skipwhite
syn keyword     stackStatement      const func in end match extern
syn keyword     stackConditional    if else fi
syn keyword     stackRepeat         while loop pool
syn keyword     stackBoolean        true false
syn keyword     stackInclude        @import import

" The name after data is a type.
syn match       stackDataType       "[^,() \t]\+" contained

" The first name after ( or , is a type.
syn match       stackType           "(\s*\zs[^,() \t]\+"
syn match       stackType           ",\s*\zs[^,() \t]\+"

" An integer is a name that starts with digits, with an optional '-'.
syn match       stackNumber         "\<-\=\d\+"

syn match       stackSpecial        "\\." contained
syn region      stackString         start=+"+ skip=+\\\\\|\\"+ end=+"+ end=+$+ contains=stackSpecial,@Spell

" A comment starts with -- outside of a name and ends at the end of the line.
syn match       stackComment        "\k\@1<!--.*$" contains=stackTodo,@Spell
" A match, not a keyword, so that `TODO:` is found.
syn match       stackTodo           "\<\%(TODO\|FIXME\|XXX\)" contained

hi def link stackStatement          Statement
hi def link stackConditional        Conditional
hi def link stackRepeat             Repeat
hi def link stackBoolean            Boolean
hi def link stackInclude            Include
hi def link stackDataType           Type
hi def link stackType               Type
hi def link stackNumber             Number
hi def link stackString             String
hi def link stackSpecial            SpecialChar
hi def link stackComment            Comment
hi def link stackTodo               Todo

let b:current_syntax = "stack"
