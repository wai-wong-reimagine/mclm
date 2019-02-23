\name{as_freqlist}
\alias{as_freqlist}
\title{
Coerce table to a frequency list
}
\description{
Coerces a table to a frequency list
}
\usage{
as_freqlist(x)
}
\arguments{
  \item{x}{
   the object \code{x} must be an oject of the class \code{"table"}.
  }
}
\value{
This function returns an object of the class \code{"freqlist"}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
tokens <- tokenize(toy_corpus)
as_freqlist(table(tokens))

}
