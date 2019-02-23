\name{as.data.frame.zoom_re.freqlist}
\alias{as.data.frame.zoom_re.freqlist}
\title{
Coerce \code{"zoom_re.freqlist"} object to data frame
}
\description{
Coerces a  \code{"zoom_re.freqlist"} object to a data frame
}
\usage{
\method{as.data.frame}{zoom_re.freqlist}(x, ...)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class
   \code{"zoom_re.freqlist"}.
  }
  \item{...}{
   the argument \code{...} is not used at the moment.
  }
}
\value{
This function returns a data frame.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
(flist <- freqlist(toy_corpus, as_text = TRUE))
as.data.frame(zoom_re(flist, "e"))

}
