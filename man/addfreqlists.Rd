\name{addfreqlists}
\alias{addfreqlists}
\title{
Add up two frequency lists
}
\description{
This function takes as its argument two frequency lists and returns a new
frequency list that is the sum of these two frequency lists.

}
\usage{
addfreqlists(x, y) 
}
\arguments{
  \item{x}{
   a first object of type \code{"freqlist"} (that can be created with the
   function \code{\link{freqlist}}) 
  }
  \item{y}{
   a second object of type \code{"freqlist"} (that can be created with the
   function \code{\link{freqlist}}) 
  }
}
\value{
   This function returns a new object of type \code{"freqlist"}, that is
   the sum of \code{x} and \code{y}. In other words, the resulting
   frequency list contains all items
   that are in either \code{x} or \code{y} (or both), and their frequency
   is the sum of their frequencies in \code{x} and \code{y} (with absence
   counting as a frequency of zero).
}
