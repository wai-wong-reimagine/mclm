\name{tokenize}
\alias{tokenize}
\title{
Split a text into a sequence of tokens
}
\description{
Splits a text into a sequence of tokens. Returns an
object of the class \code{"tokens"}.
}
\usage{
tokenize(x,
         re_drop_line = NA,
         line_glue = NA,
         re_cut_area = NA,
         re_token_splitter = "\\\\s+",
         re_token_extractor = "[^\\\\s]+",
         re_drop_token = NA,
         re_token_transf_in = NA,
         token_transf_out = NA,
         token_to_lower = TRUE,
         perl = TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   the object \code{x} either is a character vector or
   an object of the class \code{"TextDocument"} and contains the
   text that is to be tokenized.
   
   If \code{x} is a character vector and the length of \code{x}
   is higher than one, then each item in \code{x} is treated as a separate
   line (or a separate series of lines) in the text. Withing each
   item of \code{x}, the character \code{"\\\\n"} is also treated as
   a line separator.
  }
  \item{re_drop_line}{
   if \code{re_drop_line} is \code{NA}, then this argument is ignored.
   Otherwise, \code{re_drop_line} is a character vector (assumed to
   be of length 1) containing a regular expression. Lines in \code{x}
   that contain a match for \code{re_drop_line} are
   treated as not belonging to the corpus and are excluded from
   the results.
  }
  \item{line_glue}{
   if \code{line_glue} is \code{NA}, then this argument is ignored.
   Otherwise, all lines in a corpus file (or in \code{x}, if
   \code{as_text} is \code{TRUE}, are glued together in one
   character vector of length 1, with the string \code{line_glue}
   pasted in between consecutive lines.  The value of \code{line_glue}
   can also be equal to the empty string \code{""}.
   The `line glue' operation is conducted immediately after the `drop line'
   operation.
  }
  \item{re_cut_area}{
   if \code{re_cut_area} is \code{NA}, then this argument is ignored.
   Otherwise, all matches in a corpus file (or in \code{x}, if
   \code{as_text} is \code{TRUE}, are 'cut out' of the text prior
   to the identification of the tokens in the text (and are
   therefore not taken into account when identifying the tokens). 
   The `cut area' operation is conducted immediately after the
   `line glue'
   operation.

  }  
  \item{re_token_splitter}{
   the actual token identification is either based on
   \code{re_token_splitter}, a regular expression that identifies the
   areas between the tokens, or on \code{re_token_extractor}, a regular
   expressions that identifies the area that are the tokens. The first
   mechanism is the default mechanism: the 
   argument  \code{re_token_extractor} is only used
   if \code{re_token_splitter} is \code{NA}.
   
   more specifically, \code{re_token_splitter} is a regular expression
   that identifies the
   locations where lines in the
   corpus files are split into
   tokens. 
   The `token identification' operation is conducted immediately after the
   `cut area'
   operation.
   
  }
  \item{re_token_extractor}{
   a regular expression that identifies the locations of the
   actual tokens. This
   argument is only used
   if \code{re_token_splitter} is \code{NA}. Whereas matches for
   \code{re_token_splitter} are
   identified as the areas between the tokens, matches for
   \code{re_token_extractor} are
   identified as the areas of the actual tokens. Currently the
   implementation of
   \code{re_token_extractor} is a lot less time-efficient
   than that of \code{re_token_splitter}.
   The `token identification' operation is conducted immediately after the
   `cut area'
   operation.
  }
  \item{re_drop_token}{
   a regular expression that identifies tokens that are to be excluded
   from the results. Any token that contains a match for
   \code{re_drop_token} is removed from the results. If
   \code{re_drop_token} is \code{NA}, this argument is ignored. 
   The `drop token' operation is conducted immediately after the
   `token identification'
   operation.
   
  }
  \item{re_token_transf_in}{
   a regular expression that identifies areas in the tokens that are to be
   transformed. This argument works together with the argument
   \code{token_transf_out}.
   
   If both \code{re_token_transf_in} and \code{token_transf_out} differ
   from \code{NA}, then all matches, in the tokens, for the
   regular expression  \code{re_token_transf_in} are replaced with
   the replacement string \code{token_transf_out}.

   The `token transformation' operation is conducted immediately after the
   `drop token'
   operation.

  }
  \item{token_transf_out}{
   a `replacement string'. This argument works together with
   \code{re_token_transf_in} and is ignored if
   \code{re_token_transf_in} is \code{NA}.
   
  }
  \item{token_to_lower}{
    a boolean value that determines whether or not tokens must be converted
    to lowercase before returning the result.
    
   The `token to lower' operation is conducted immediately after the
   `token transformation'
   operation.
    
  }
  \item{perl}{
    a boolean value that determines whether or not the PCRE regular expression
    flavor is being used in the arguments that contain regular expressions.
  }
}
\value{
This function returns a token sequence, i.e. an object of the
  class \code{"tokens"}.
}
\examples{
toy_corpus <- "Once upon a time there was a tiny toy corpus.
It consisted of three sentence. And it lived happily ever after."
tokenize(toy_corpus)

}
