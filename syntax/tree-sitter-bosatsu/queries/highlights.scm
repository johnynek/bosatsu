(comment) @comment

(package_declaration "package" @keyword)

(keyword) @keyword

(type_identifier) @type
(identifier) @variable
(backticked_identifier) @variable

(number) @number
(string) @string
(character) @string.special

(operator) @operator

["(" ")" "[" "]" "{" "}"] @punctuation.bracket
[":" "," "." ";"] @punctuation.delimiter
