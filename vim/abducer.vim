if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "abducer"

" the language is case sensitive
syn case match

syn sync fromstart

" comments
syn region  aComment      start=/%/ end=/.*$/      contains=aNote
syn keyword aNote         XXX TODO NOTE FIXME

" variable
syn match   aVariable  /[A-Z_][a-zA-Z0-9_]*/

" name
syn match   aName  /[a-z][a-zA-Z0-9_]*\(\.[a-zA-Z0-9_][a-zA-Z0-9_]*\)*/  contains=NONE
syn region  aName  start=/'/ end=/'/

" float
syn match   aFloat  /[0-9][0-9]*\.\([0-9][0-9]*\)\?[eE][-+]\?[0-9][0-9]*\|[0-9][0-9]*\.[0-9][0-9]*\([eE][-+]\?[0-9][0-9]*\)\?/

" operators
syn match   aComma         /,/
syn match   aEnd           /\./
syn match   aColon         /:/
syn match   aEmbedImpl     /->/
syn match   aRevImpl       /<-/
syn match   aCostFunction  /\//
syn match   aAssert        /?/

" keywords
syn keyword aModality  understand generate event int i att bel
syn keyword aKeyword   disjoint

hi link aComment  Comment
hi link aNote     Todo

hi link aName      Normal
hi link aVariable  Identifier

hi link aFloat  Float

hi link aComma         Statement
hi link aEnd           Statement
hi link aRevImpl       Statement
hi link aEmbedImpl     Statement
hi link aCostFunction  Statement
hi link aKeyword       Statement
hi link aAssert        Special
hi link aModality      Type
