---
engines:
- path: /home/delriaan/.positron-server/bin/ae29de6747f9f15cc38783410ec9a46a5b428393/quarto/share/extension-subtrees/julia-engine/\_extensions/julia-engine/julia-engine.js
toc-title: Table of contents
---

# ![book](book_small.png) Book of Utilities

**book.of.utilities** seeks to facilitate execution of those repetitive,
ad-hoc tasks often encountered during data processing.

## Installation

Use
`remotes::install_github("delriaan/book.of.utilities", subdir = "pkg")`
to install the latest version from GitHub.

## Functions

The following functional families are covered in `book.of.utilities`:

### Chapter 1: Calculators

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `calc.means()`                      Calculates one or more mean
                                      variants (arithmetic, zero,
                                      geometric, harmonic, RMS) for
                                      vectors and tabular inputs.
                                      Supports optional post-processing
                                      and z-score transformation.

  `calc.zero_mean()`                  Returns values centered at zero by
                                      subtracting the mean. Can
                                      optionally scale output as
                                      z-scores.

  `calc.rms()`                        Computes the root-mean-square of
                                      numeric input. This is a
                                      convenience wrapper around
                                      `calc.means()`.

  `calc.harmonic_mean()`              Computes the harmonic mean of
                                      numeric input. This is a
                                      convenience wrapper around
                                      `calc.means()`.

  `calc.geo_mean()`                   Computes the geometric mean of
                                      numeric input. This is a
                                      convenience wrapper around
                                      `calc.means()`.

  `ratio()`                           Computes ratio-based transforms
                                      such as relative-to-sum,
                                      relative-to-max, or cumulative
                                      ratio. Supports optional sorting
                                      and density-style transformation.

  `ranking.algorithm()`               Assigns rank values to scores using
                                      threshold mapping logic. Can return
                                      mapping metadata and optionally
                                      process via a configurable mapper.

  `range_diff()`                      Returns the inclusive range width,
                                      computed as max minus min. Supports
                                      single vectors or lists of vectors.

  `odds2probs()`                      Converts odds representations to
                                      probabilities. Accepts numeric
                                      vectors, log-odds, and
                                      colon-delimited string formats.
  -----------------------------------------------------------------------

### Chapter 2: Counters

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `count_cycles()`                    Generates a cycle counter driven by
                                      a logical condition vector.
                                      Supports reset rules and custom
                                      starting offsets.

  `factor_int()`                      Computes integer factors for one or
                                      more numeric inputs. Returns a
                                      vector or list depending on input
                                      length.

  `radix()`                           Converts numeric representations
                                      across radix bases (for example
                                      binary, decimal, hexadecimal).
                                      Supports named bases and arbitrary
                                      numeric bases.

  `factor.int()`                      Alias for `factor_int()`.

  `count.cycles()`                    Alias for `count_cycles()`.
  -----------------------------------------------------------------------

### Chapter 3: Miscellaneous

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `log_note()`                        Appends timestamped note text to a
                                      file or console. Can also display
                                      saved file contents.

  `vlogical()`                        Performs vectorized pattern
                                      matching across one or more
                                      patterns. Returns a logical matrix
                                      or simplified column-wise
                                      summaries.

  `polyname2orig()`                   Rewrites polynomial feature names
                                      back to source-variable form.
                                      Useful for making model matrix
                                      column names interpretable.

  `gen_primes()`                      Generates prime numbers within a
                                      supplied domain. Supports random
                                      sampling and distinct-only output.

  `call_recursion()`                  Repeatedly applies a function while
                                      a test condition is true, up to a
                                      maximum iteration count. Can return
                                      final output only or full iteration
                                      history.

  `checksum()`                        Compares an object digest to an
                                      expected hash value. Supports
                                      file-based and object-based checks.

  `gen.primes()`                      Alias for `gen_primes()`.

  `call.recursion()`                  Alias for `call_recursion()`.
  -----------------------------------------------------------------------

### Chapter 4: Operators

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `%bin%`                             Bins numeric values to fixed
                                      intervals using modulo subtraction.
                                      Stores binning ratio metadata as an
                                      attribute.

  `%><%`                              Operator wrapper for
                                      `test_between()` to evaluate
                                      interval overlap or containment
                                      logic.

  `%tf%`                              Creates paired true/false value
                                      choices for downstream conditional
                                      selection. Returns a two-column
                                      table structure.

  `%?%`                               Applies simple if-then-else
                                      selection using a logical condition
                                      vector and provided true/false
                                      result set.

  `%??%`                              Performs correlated row-wise
                                      conditional selection, returning
                                      matched true/false results for each
                                      row.

  `%if_na_empty%`                     Replaces `NA` or empty values in
                                      one object using values from
                                      another object.

  `test_between()`                    Tests whether ranges or values are
                                      between target ranges, with
                                      optional temporal labeling
                                      (`before`, `during`, `after`).
  -----------------------------------------------------------------------

### Chapter 5: Object Management

  -----------------------------------------------------------------------
  Function                            Synopsis
  ----------------------------------- -----------------------------------
  `distinct.list()`                   Returns first-level unique list
                                      elements using serialized hash
                                      signatures. Can return either a
                                      logical selector or filtered list.

  `enlist()`                          Converts vectors to named lists,
                                      using provided names or
                                      value-derived defaults. Preserves
                                      existing names when present.

  `get.object_sizes()`                Recursively inspects environments
                                      and reports object sizes with depth
                                      information. Useful for memory
                                      profiling nested objects.
  -----------------------------------------------------------------------
