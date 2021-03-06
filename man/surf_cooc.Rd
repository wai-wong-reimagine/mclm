\name{surf_cooc}
\alias{surf_cooc}
\title{
Build surface collocation frequencies.
}
\description{
Builds surface collocation frequencies for a specific node.
}
\usage{
surf_cooc(x, 
          re_node,
          w_left = 3, 
          w_right = 3,
          re_boundary = NA,
          re_drop_line = NA,
          line_glue = NA,
          re_cut_area = NA,
          re_token_splitter = "\\\\s+",
          re_token_extractor = "[^\\\\s]+",
          re_drop_token = NA,
          re_token_transf_in = NA,
          token_transf_out = NA,
          token_to_lower = TRUE,
          perl = TRUE,
          blocksize = 300,
          verbose = FALSE,
          dot_blocksize = 10,
          file_encoding = "UTF-8")
}
\arguments{
  \item{x}{
   the object \code{x} contains the list of filenames of the
   corpus files.
  }
  \item{re_node}{
   regular expression used for identifying instances of the
   `node', i.e. the target item, for which surface collocation
   information is collected.
  }
  \item{w_left}{
   the number of words, or technically more correctly, the number of tokens,
   to the left of the `node' that are treated as belonging to the
   co-text of the `node'. (But also see \code{re_boundary}.)
  }
  \item{w_right}{
   the number of words, or technically more correctly, the number of tokens,
   to the right of the `node' that are treated as belonging to the
   co-text of the `node'. (But also see \code{re_boundary}.)
  }
  \item{re_boundary}{
   regular expression used for identifying tokens that `cut off' the
   co-text of a `node'. If not \code{is.na(re_boundary)}, then
   the left co-text of a `node' still contains at most \code{w_left} tokens,
   but moreover is futher `cut off' as soon as, starting from the `node' and
   travelling backward from the `node', a token is found that contains a
   match for \code{re_boundary}. This `boundary token' itself is also
   excluded from the co-text of the `node'. Likewise, to the right of the
   `node', the co-text is also `cut off' as soon as, starting from the
   `node' and
   travelling forward from the `node', a `boundary token' is found.
   Again, the `boundary token' itself is also excluded.
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
  \item{blocksize}{
    number that indicates how many corpus files are read to memory
    `at each individual step' during the steps in the procedure;
    normally the default value
    of \code{300} should not
    be changed, but when one works with exceptionally small corpus files,
    it may be worthwhile to use a higher number, and when one works with
    exceptionally large corpus files, ot may be worthwhile to use a
    lower number.
  }
  \item{verbose}{
    if \code{verbose} is \code{TRUE}, messages are printed to the console to
    indicate progress.
  }
  \item{dot_blocksize}{
    if \code{verbose} is \code{TRUE}, dots are printed to the console to
    indicate progress.
  }
  \item{file_encoding}{
  file encoding that is assumed in the corpus files.
  }
}
\details{
Two major steps can be distinguished in the procedure conducted by
  \code{surf_coor}. The first major step is the \emph{identification of
  the (sequence of) tokens} that, for the purpose of this analysis,
  will be considered to be the content of the corpus.
  The function arguments that jointly determine the details of
  this step are 
  \code{re_drop_line}, \code{line_glue}, \code{re_cut_area},
  \code{re_token_splitter}, \code{re_token_extractor},
  \code{re_drop_token}, \code{re_token_transf_in},
  \code{token_transf_out}, and \code{token_to_lower}.
  The sequence of tokens that is the ultimate outcome of this step
  is then handed over to the second major step of the procedure.
  
  The second major step is the \emph{establishment of the
  co-occurrence frequencies}. The function arguments
  that jointly determine the details of this step are
  \code{re_node}, \code{w_left}, \code{w_right}, and
  \code{re_boundary}. It is important to know that this
  second step is conducted after the tokens of the corpus have
  been identified, and that it is applied to a sequence of
  tokens, not to the original text. More specifically the
  regular expressions \code{re_node} and \code{re_boundary}
  are tested against individual tokens, as they are identified
  by the token identification procedure. Moreover, the numbers
  \code{w_left} and \code{w_right} also apply to tokens a
  they are identified by the token identification procedure.
}
\value{
The function \code{surf_coor} returns an object of the class
  \code{"cooc_info"}, containing information on co-occurrence
  frequencies.
}
