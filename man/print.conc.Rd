\name{print.conc}
\alias{print.conc}
\title{
Print a concordance-based data frame.
}
\description{
Print objects of the class \code{"conc"}. 
}
\usage{
\method{print}{conc}(x,
                     n = 6, 
                     ...)
}
\arguments{
  \item{x}{
  a concordance.
  }
  \item{n}{
  number of items to print in a KWIC display.
  }
  \item{...}{
  additional printing arguments
  }
}
\value{
Invisibly returns \code{x}.
}
\examples{
(conc_data <- conc_re('\\\\w+', 'A very small corpus.', as_text = TRUE))
print_kwic(conc_data)
print(conc_data, n = 3)
}

