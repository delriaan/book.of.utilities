# book.of.utilities Version 0.2
## 0.2.1220
### Bug Fixes
- `kr_key()`:
  - Fixed a runtime error preventing the key from being retrieved.

## 0.2.1210
- `ratio()`:
  - `NA` check when *'type == "cumulative"' will now error if `NA` is detected in the input.
  - Fixed a bug where argument 'sort.type' ordering was not being applied to names of the output.
  
## 0.2.1110
- `ratio()`:
  - Names will only return if the input had names.
  - Helper functions pulled out of the function and are now unexported library functions.
  - Added an `NA` check when *'type == "cumulative"'* since `cumsum()` does not have an *'na.rm'* argument.
  
## 0.2.1100
- `ratio()`:
  - Removed argument `decimals` 
  - Added argument `...`: This will allows existing code referring to `decimals` to continue to function without error, but rounding will not take place. This was implemented in version `0.2.1`; however, as an oversight, the argument `decimals` was not removed from the signature.

## 0.2.1
### Bug Fixes
- `ratio()`:
   - Corrected the logic for handling argument 'sort.type' to correctly sort the output.

# book.of.utilities Version 0.1
## 0.1.6.7020
### Enhancements
- `ratio()`: 
   - Added argument `sort.type` to allow for specifying how the input should be sorted just *before* it is transformed.

## 0.1.6.7011
### Enhancements
- `ratio()`: 
   - Improved method for returning the input as density.
   - Corrected handling of argument `decimals`

## 0.1.6.7001
### Enhancements
- `call.recursion()`: 
   - Added `...` to the signature to allow for passing arguments to `fun()`
   - Changed the order of the signature so that `...` immediately follows the last required argument.
- `calc.zero_mean()`: The function accepts a numeric value for argument `use.population` allowing the population standard deviation to be provided explicitly.

## 0.1.6.7
### General Updates
- Updated `purrr` calls to use anonymous style functions

### Breaking Changes
- `unregex()`, `is.regex()`, `as.regex()`: Are no longer exported and will likely be removed in a future version.
- `%?%`: Removed 'cond_id' from the return object

### Enhancements
- `gen.pass()`: Expanded default glyphs
- `count.cycles()`: Argument 'reset' now accepts un-evaluated expressions which must evaluate to a logical vector of length-1 or \code{length(cond)}.

## 0.1.6.6
- Minor documentation updates
- `radix()`: Added support for *hexadecimal* input: prefix values with `0x` to use.
- `%??%()`: Redesigned the function and updated documentation.
- `vlogical()`: Fixed a bug that did not correctly accept values for `vpattern` as indicated.
