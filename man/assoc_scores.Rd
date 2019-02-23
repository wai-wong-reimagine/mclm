\name{assoc_scores}
\alias{assoc_scores}
\alias{assoc_abcd}
\title{
Association scores used in collocation analysis and keyword analysis
}
\description{
The functions \code{assoc_scores} and \code{assoc_abcd} take as
 their arguments co-occurrence frequencies for a number of items, and
 return a range of association scores used in collocation analysis,
 collostruction analysis and keywords analysis.
}
\usage{
assoc_scores(x, 
             y = NULL, 
             min_freq = 3,
             measures = NULL,
             with_variants = FALSE,
             show_dots = FALSE,
             p_fisher_2 = FALSE,
             small_pos = 0.00001)

assoc_abcd(a, b, c, d,
           measures = NULL,
           with_variants = FALSE,
           show_dots = FALSE,
           p_fisher_2 = FALSE,
           small_pos = 0.00001) 
}
\arguments{
  \item{x}{
the argument \code{x} can either be an object of class \code{"freqlist"}
(i.e. the data type that is returns by the function \code{\link{freqlist}} or
an object of class \code{"cooc_info"} (i.e. the data type that is returns by
the functions \code{\link{surf_cooc}} and \code{\link{text_cooc}}.

If \code{x} is of class \code{"freqlist"}, it is interpreted as the
target frequency list (i.e. the list with the frequencies of items in the
target context).

If \code{x} is of class \code{"cooc_info"}, it is interpreted to contain target
frequency information, reference frequency information, and corpus size information.
}
  \item{y}{
if \code{x} is of class \code{"freqlist"}, then \code{y} is expected to also be
of class \code{"freqlist"}, and is interpreted as the
reference frequency list (i.e. the list with the frequencies of items in the
reference context).

If \code{x} is of class \code{"cooc_info"}, then \code{"y"} is ignored.
}
  \item{a}{
a vector of numbers that express how many times some target item occurs in
the target context. For instance, \code{a[i]} expresses how many times the
\code{i}-th target item occurs in the target context.
}
  \item{b}{
a vector of numbers that express how many times other items than some
target item occur in the target context. For instance, \code{b[i]} expresses
how many times other items than the \code{i}-th target item occur in the
target context.
}
  \item{c}{
a vector of numbers that express how many times some target item occurs in
the reference context. For instance, \code{c[i]} expresses how many times the
\code{i}-th target item occurs in the reference context.
}
  \item{d}{
a vector of numbers that express how many times other items than some
target item occur in the reference context. For instance, \code{d[i]} expresses
how many times other items than the  \code{i}-th target item occur in the
reference context.
}
  \item{min_freq}{
the minimum value for \code{a[i]} that is needed for item \code{i} to be
included in the output.
}
  \item{measures}{
a character vector containing the association measures (or related quantities)
 for which scores are
 requested. Supported measure names (and related quantities) are
 \code{"exp_a"}, \code{"exp_b"},
 \code{"exp_c"}, \code{"exp_d"}, \code{"DP_rows"}, \code{"DP_cols"}, and many others.
 The argument \code{measures}
 can also have the value \code{NULL}, which is interpreted as short for the default
 selection
 \code{c("exp_a", "DP_rows", "RR_rows",
         "OR", "MS", "PMI", "DICE",
         "G", "chi2", "t", "fisher")}. The argument \code{measures}
 can also have the value \code{"ALL"}, in which case all supported measures are
 calculated.
 }
  \item{with_variants}{
a boolean value that expresses whether for the requested \code{measures} all
 variants should be included in the output (\code{TRUE}) or just the
 main versions (\code{FALSE}).
}
  \item{show_dots}{
a boolean value that expresses whether or not a dot (\code{.}) should be
  output to the console each time calculations for a measure are finished.
}
  \item{p_fisher_2}{
a boolean value that expresses whether, in case \code{"fisher"} is one of
 the requested measures, the p-value for a two-sided test (testing for
 either attraction or repulsion) should also be
 calculated. By default, only the (computationally less demanding)
 p-value for a one-sided test (testing only for attraction) is calculating.
}
  \item{small_pos}{
Several of the association measures break down when
 one or more of the values \code{a}, \code{b}, \code{c}, and \code{d} are
 zero (for instance, because this would lead to division by zero or taking
 the log of zero). In order to avoid this, a small positive value is systematically
 added
 to all zero values for \code{a}, \code{b}, \code{c}, and \code{d}. The
 argument \code{small_pos} determines which small positive value is added
 in such cases. Its default value is \code{0.00001}.
 Adding these small positive values is done systematically, not only when
 measures are used that need this to be done. 
}

}
\details{
The function \code{assoc_scores} takes as its argument a
 target frequency list and a reference frequency list and
 returns a number of popular measure that express, for each
 item in either one of these lists, the extent to which
 the item is attracted to the target context (when compared
 to the reference context). 

The function \code{assoc_abcd} takes as its arguments four
 vectors \code{a}, \code{b}, \code{c} and
 \code{d} of equal length. Each tuple of values
 \code{(a[i],b[i],c[i],d[i])}, with \code{i} some integer
 number between one and the length of the vectors, is
 assumed to represent the four numbers \var{a}, \var{b},
 \var{c}, \var{d} in a
 contingency table of the type

\tabular{rccl}{
                      \tab target item  \tab other item  \tab \cr
    target context    \tab \var{a}      \tab \var{b}    \tab \var{m}\cr
    reference context \tab \var{c}      \tab \var{d}     \tab \var{n}\cr
                      \tab \var{k}      \tab \var{l}     \tab \var{N}\cr
  }
  
 In the above table \var{m}, \var{n}, \var{k}, \var{l}, and \var{N}
 are marginal frequencies. More specifically, \var{m} = \var{a} +
 \var{b}, \var{n} = \var{c} + \var{d}, \var{k} = \var{a} + \var{c},
 \var{l} = \var{b} + \var{d}, and \var{N} = \var{m} + \var{n}.
 
}
\value{
Returns a data frame with as its rows all items from either the target
 frequency list or the reference frequency list (or, in case the argument
 \code{min_freq} is non-zero, all items for its frequency in the
 target frequency list is a least
 \code{min_freq}), and with as its columns a range of measures that
 express the extent to which the items are attracted to the target
 context (when compared to the reference context). Some columns don't
 contain actual measures, but rather additional information that is useful
 for interpreting certain measures.

The following are (possible) columns in the output:

  \item{a   }{The frequency in cell \var{a}, possibly augmented by \code{small_pos}. This column is always present.}
  \item{b   }{The frequency in cell \var{b}, possibly augmented by \code{small_pos}. This column is always present.}
  \item{c   }{The frequency in cell \var{c}, possibly augmented by \code{small_pos}. This column is always present.}
  \item{d   }{The frequency in cell \var{d}, possibly augmented by \code{small_pos}. This column is always present.}
  \item{dir }{The direction of the association. It contains the value \code{1}
              in case of relative attraction between the target item and the target
              context (i.e. in case \eqn{a / m \ge c / n}), and it contains
              the value \code{-1} in case of relative repulsion between the
              target item and the target context (i.e. in case
              \eqn{a / m < c / n}). This column is always present. }
  \item{exp_a }{The expected value for the \var{a} cell, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(m * k)/N}. This column is present if \code{measures}
                includes either the value \code{"exp_a"} or the value \code{"expected"}.
                It is also present if \code{measures} is \code{NULL} or
                is equal to \code{"ALL"}.}
  \item{exp_b }{The expected value for the \var{b} cell, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(m * l)/N}. This column is present if \code{measures}
                includes either the value \code{"exp_b"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to  \code{"ALL"}.}
  \item{exp_c }{The expected value for the \var{c} cell, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(n * k)/N}. This column is present if \code{measures}
                includes either the value \code{"exp_c"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to  \code{"ALL"}.}
  \item{exp_d }{The expected value for the \var{d} cell, assuming no difference
                between the contexts. This value is calculated as
                \eqn{(n * l)/N}. This column is present if \code{measures}
                includes either the value \code{"exp_d"} or the value \code{"expected"}.
                It is also present if \code{measures} is equal to  \code{"ALL"}.}

}
\examples{
assoc_abcd(6, 100, 15, 1000)

a <- sample(0:100, 5, replace = TRUE)
b <- 100 - a
c <- sample(0:1000, 5, replace = TRUE)
d <- 1000 - c
scores <- assoc_abcd(a, b, c, d,
                     measures=c("PMI", "t", "fisher"),
                     with_variants = TRUE,
                     p_fisher_2 = TRUE)
round(scores, 3)

}
