# ![book](book_small.png) Book of Utilities

*`book.of.utilities`* seeks to facilitate execution of those repetitive, ad-hoc tasks often encountered during data processing. 

## Installation

Use `remotes::install_github("delriaan/book.of.utilities", subdir = "pkg")` to install the latest version from GitHub.

## Functions

The following functional families are covered in `book.of.utilities`:

<p>
  <b></b>
  <ul>
    <p>
      <li style="font-weight:bold" level="1">Chapter 1 - Authentication</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: gen.pass, keyring_export, keyring_import</span>
        </li>
      </ul>
    </p>
    <p>
      <li style="font-weight:bold" level="1">Chapter 2 - Calculators</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: calc.geo_mean, calc.harmonic_mean, calc.means</span>
        </li>
        <li level="2">
          <b></b>
          <span>: calc.rms, calc.zero_mean, odds2probs</span>
        </li>
        <li level="2">
          <b></b>
          <span>: radix, range_diff, ranking.algorithm</span>
        </li>
      </ul>
    </p>
    <p>
      <li style="font-weight:bold" level="1">Chapter 3 - Counters</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: count.cycles, factor.int</span>
        </li>
      </ul>
    </p>
    <p>
      <li style="font-weight:bold" level="1">Chapter 4 - Custom Operators</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: %?%, %??%, %&gt;&lt;%</span>
        </li>
        <li level="2">
          <b></b>
          <span>: %bin%, %tf%, test_between</span>
        </li>
      </ul>
    </p>
    <p>
      <li style="font-weight:bold" level="1">Chapter 5 - Object Management</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: distinct.list, enlist, get.object_sizes</span>
        </li>
      </ul>
    </p>
    <p>
      <li style="font-weight:bold" level="1">Chapter 6 - Miscellaneous</li>
      <ul>
        <li level="2">
          <b></b>
          <span>: as.regex, call.recursion, checksum</span>
        </li>
        <li level="2">
          <b></b>
          <span>: gen.primes, is.regex, log_note</span>
        </li>
        <li level="2">
          <b></b>
          <span>: polyname2orig, unregex, vlogical</span>
        </li>
      </ul>
    </p>
  </ul>
</p>

## Dependencies

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> type </th>
   <th style="text-align:left;"> package </th>
   <th style="text-align:left;"> version </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Depends </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> &gt;= 4.1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Depends </td>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> &gt;= 1.0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> rlang </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> purrr </td>
   <td style="text-align:left;"> &gt;= 1.0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> methods </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> foreach </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> iterators </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> stringi </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> readtext </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> sodium </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> digest </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> slider </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> magrittr </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> jsonlite </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> keyring </td>
   <td style="text-align:left;"> * </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> glue </td>
   <td style="text-align:left;"> * </td>
  </tr>
</tbody>
</table>
