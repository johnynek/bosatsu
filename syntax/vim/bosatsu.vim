if exists("b:current_syntax")
  finish
endif

syn keyword bosatsuKeywords def match recur if elif else package import export struct enum external
syn match bosatsuComment "#.*$"

hi link bosatsuKeywords Keyword
hi link bosatsuComment Comment

let b:current_syntax = "bosatsu"
