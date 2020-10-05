# s7pp
S7 Scheme based text preprocessor.

A simple text preprocessor that's designed to process text, evaluating s-expressions prefixed with an `@` and run them via S7 and conver their output into text.

## Usage

`s7pp <input file> [output file]`

Takes `input file` and processes it, writing output to `output file` or stdout

## Examples

## Expansion Rules

### Prefixes

S-Expressions starting with `@` will be evaluated and their results expanded in place.

S-Expressions starting with `@;` will be consumed and ignored as a comment.

S-Expressions starting with `@@` will be evaulated and their results discarded, useful for defining variables, functions and loading other modules.

To emit a literal `@` You may prefix it like `;@` XXX, Maybe Broken!

S-Expressions inside of literal double quoted strings are not expanded. XXX Reconsider?

### S-Expression Expansion

When expanding an S-Expression after a `@`, it can be a symbol name, in which case the value is output, otherwise an S-Expression, with matching parenthesis `(` and `)` is consumed and passed to S7 to be evaluated, and then the output is written.

### Output stringification

When the returned value is a literal string or character, it is simply printed directly into an output. Lists are recursively evaluated using the same rules, with non-character, non-whitespace elements interspersed with space characters, and any other s7 object is converted to a string using standard s7 string conversion rules (s7_object_to_c_string)