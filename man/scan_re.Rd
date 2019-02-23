\name{scan_re}
\alias{scan_re}
\alias{scan_txt}
\title{
Scan a regular expression, or any other character string, from the console
}
\description{
The functions \code{scan_re} and \code{scan_txt}, which take
 no arguments, can be used to scan a text string from the console.
 After the function call, R will continue scanning your input
 until it encounters an empty input line, i.e. until it encounters
 two consecutive newline symbols (or until it encounters a line with
 nothing but whitespace characters). In other words, press ENTER
 twice in a row if you want to stop inputting characters.
 The function will then return your input as a character vector
 of length one. 
}
\usage{
scan_re()

scan_txt()
}
\details{
Both functions, \code{scan_re} and \code{scan_txt}, behave in exactly
 the same way. The first name, \code{scan_re}, emphasizes that the
 functions are specifically meant for inputting regular expressions.
 The second name, \code{scan_txt}, emphasizes that you can
 nevertheless also use the functions for scanning any other string.
 
 The specific reason why the functions are created, is to avoid
 the need for doubling backslashes when inputting regular expressions.
 When regular expressions are in a string literal, the special
 meaning of backslashes in R string literals forces one to
 double these backslashes, which is especially inconvenient because
 backslashes feature prominently in regular expression notation.
 The functions functions \code{scan_re} and \code{scan_txt} interpret
 their input as `raw strings'. Therefore, there is no need for
 doubling backslashes.
}
\value{
Returns a character vector of length one that contains the string that has
 been scanned.
}
\seealso{
See also \code{\link{cat_re}}.
}
