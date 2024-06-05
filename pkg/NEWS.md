# book.of.utilities 0.1.6.7

## General Updates
- Updated `purrr` calls to use anonymous style functions

## Breaking Changes
- `unregex()`, `is.regex()`, `as.regex()`: Are no longer exported and will likely be removed in a future version.
- `%?%`: Removed 'cond_id' from the return object

## Enhancements
- `gen.pass()`: Expanded default glyphs
- `count.cycles()`: Argument 'reset' now accepts un-evaluated expressions which must evaluate to a logical vector of length-1 or \code{length(cond)}.

# book.of.utilities 0.1.6.6

- Minor documentation updates
- `radix()`: Added support for *hexadecimal* input: prefix values with `0x` to use.
- `%??%()`: Redesigned the function and updated documentation.
- `vlogical()`: Fixed a bug that did not correctly accept values for `vpattern` as indicated.
