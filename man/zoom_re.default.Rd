\name{zoom_re.default}
\alias{zoom_re.default}
\title{
Zoom in on the items that contain a regular expression match
}
\description{
Generic function that takes as its first argument \code{x}
  an object that represents
  a collection of linguistic units, for instance an object of the
  class \code{"freqlist"}, and extracts from it those units that
  contain a match for the regular expression \code{pattern}.
}
\usage{
\method{zoom_re}{default}(x, pattern, perl = TRUE)
}
\arguments{
  \item{x}{
  an object that represents a collection of linguistic units, for instance
  an object of the class \code{"freqlist"} 
  }
  \item{pattern}{
  a character vector (assumed to be of length 1) containing a regular expression
  }
  \item{perl}{
  a boolean vector (assumed to be of length 1) that determines whether or not
  the Perl flavor of
  regular expressions is assumed
  }
}
\value{
Returns an object containing the items that match the regular expression.
}
\examples{
x <- freqlist("The man and the mouse.", as_text = TRUE)
zoom_re(x, "a")
}
