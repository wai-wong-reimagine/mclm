\name{conc_re}
\alias{conc_re}
\title{
Build concordance for the matches of a regular expression
}
\description{
The function \code{conc_re} builds a concordance for the matches of
 a regular expression. The result is a dataset that can be written
 to a file with the function \code{\link{write_dataset}}. The function
 mimics the behaviour of the concordance tool in the program
 AntConc.
}
\usage{
conc_re(pattern,
        x,
        c_left = 200,
        c_right = 200,
        perl = TRUE,
        after_line = "\n",
        file_encoding = "UTF-8",
        as_text = FALSE) 

}
\arguments{
  \item{pattern}{
  the argument \code{pattern} is a character string that contains the
   regular expression that serves as search term for the concordancer.
}
  \item{x}{
  the argument \code{x} is a character vector that determines which
  text is to be used as corpus.
  In case of the setting \code{as_text = TRUE}, the content of the
  argument \code{x} is treated
  as the actual text that is to be used as corpus. In case of the setting
  \code{as_text = FALSE},
  the argument \code{x} is treated as a
  vector of filenames, which are then interpreted as the names of the
  corpus files
  that contain the actual corpus data.
}
  \item{c_left}{
  the argument \code{c_left} is a number that specifies how many characters to
  the left of each match must be included in the result as the left co-text
  of the match.
}
  \item{c_right}{
  the argument \code{c_right} is a number that specifies how many characters to
  the right of each match must be included in the result as the right co-text
  of the match.
}

  \item{perl}{
  in case of the setting \code{perl = TRUE}, the argument \code{pattern} is
  treated
  as a PCRE flavor regular expression. Otherwise, the argument \code{pattern}
  is treated as a regular expression in R's default regular expression flavor.
}
  \item{after_line}{
  prior to the actual search operation, the lines from a corpus file are
  concatenated
  into one single character string,
  using, as separator between the lines of the file, the value given in
  \code{after_line}.
  In case of the setting \code{as_text = TRUE}, the argument
  \code{after_line} is ignored.
}
  \item{file_encoding}{
  each corpus file is interpreted as a text file the encoding of which is
  the one given in \code{file_encoding}. In case of the setting
  \code{as_text = TRUE}, the argument
  \code{file_encoding} is ignored. In case of the setting
  \code{as_text = FALSE}, the
  argument \code{file_encoding} can either be a character vector of length one,
  or
  a character vector with the same length as \code{x}. In the former case, all
  files in \code{x} are assumed to have the same encoding. In the latter
  case, different
  files can have different encodings.
}
  \item{as_text}{
  in case of the setting \code{as_text = TRUE}, the content of the argument
  \code{x} is treated
  as the actual text that is to be used as corpus. In case of the setting
  \code{as_text = FALSE},
  the argument \code{x} is treated as a
  vector of filenames, which are then interpreted as the names of the
  corpus files
  that contain the actual corpus data.
}
}
\details{
In order to make sure that the columns \code{left}, \code{match},
 and \code{right} in the
 output of \code{conc_re} do not contain any TAB or NEWLINE characters,
 whitespace in these items is
 being `normalized'. More particularly, each stretch of whitespace, i.e. each
 uninterrupted sequences of whitespace characters, is replaced by
 a single SPACE character.

The values in the items the \code{glob_id} and \code{id} in the output
 of \code{conc_re}
 are always identical in a
 dataset that is the output of the function \code{conc_re}. The item
 \code{glob_id}
 only becomes useful when later, for instance, one wants to merge two datasets.
}
\value{
Returns an object of the class \code{conc}, which is a kind of data frame
with as its rows the matches and with the following columns:

  \item{glob_id   }{Number indicating the position of the match in the
                    overall list of matches.}
  \item{id   }{Number indicating the position of the match in the
               list of matches for one specific query.}
  \item{source   }{Either the filename of the file in which the match was
                   found (in case of the setting
                   \code{as_text = FALSE}), or the string `-' (in case of
                   the setting \code{as_text = TRUE}).}
  \item{left   }{The lefthandside co-text of each match.}
  \item{match   }{The actual match.}
  \item{right   }{The righthandside co-text of each match.}

}
\examples{
(conc_data <- conc_re('\\\\w+', 'A very small corpus.', as_text = TRUE))
print_kwic(conc_data)
}
