# -----------------------------------------------------------------------------
#
#    name: mclm.R  [short for: mastering corpus linguistics methods]
#    purpose: library of simple R functions in support of corpus linguistics
#    author: Dirk Speelman 
#    version: 2017-10-16
#
# -----------------------------------------------------------------------------

# ============================================================================
# yet to be put in proper location
# ============================================================================

find_xpath <- function(pattern,
                       x,
                       handlers = NULL,
                       trim = TRUE,
                       fun = NULL,
                       final_fun = NULL,
                       namespaces = NULL,
                       ...) {
   # ----------------------------------------------------------
   # - x can be any of the following:
   #      - a vector of filenames
   #      - a character vector of XML source
   #      - a list of parsed XML documents  
   #  - asText, trim, ignoreBlanks=TRUE
   #       - passed on the xmlParse
   #       if x contains parsed XML documents,
   #       then these arguments are ignored.
   #  - ...
   #       passed on to sapply
   # ----------------------------------------------------------
   if (!is.vector(x)) {
      x <- list(x)
   }
   res <- vector("list", length(x))
   for (i in seq_along(x)) {
      cur <- x[[i]]
      # -------------------------------------------------------
      # - if cur turns out to be XML source or a filename,
      #   which is tested with (is.character(cur)), we
      #   parse it with xmlParse()
      # - otherwise, we assume cur is a parse XML document
      #   this could be tested with something along the
      #   lines of ("XMLInternalDocument" %in% class(cur));
      #   however, currently this default case is not tested,
      #   just assumed.
      # case of cur.x is XML source or filename
      # -------------------------------------------------------
      if (is.character(cur)) { 
         cur <- XML::xmlParse(cur,
                              handlers = handlers,
                              trim = trim)
      }   
      # -------------------------------------------------------
      #res[[i]] <- cur[pattern]
      if (is.null(namespaces)) {
         namespaces = XML::xmlNamespaceDefinitions(cur, simplify = TRUE)
      }
      res[[i]] <- XML::xpathApply(cur, pattern,
                                  namespaces = namespaces, ...)
   }
   res <- unlist(res)
   if (!is.null(fun)) {
     res <- sapply(res, fun, ...)
   }
   if (!is.null(final_fun)) {
     res <- do.call(final_fun, list(res))
   }    
   res
}

print.conc <- function(x, n = 6, ...) {
  cat("Concordance-based data frame (number of observations: ",
      nrow(x),
      ")\n",
      sep = "")
  if (n > 0) {
    print_kwic(x, n = n, ...)
  }
  if (n < nrow(x)) cat("...\n")
  cat("\nThis data frame has ")
  cat(ncol(x), ifelse(ncol(x) > 1, "columns:\n", "column:\n"))
  df_names <- data.frame(column = names(x))
  print(df_names)
  invisible(x)
}

print_kwic <- function(x, 
                       min_c_left = NA,
                       max_c_left = NA,
                       min_c_match = NA,
                       max_c_match = NA,
                       min_c_right = NA,
                       max_c_right = NA,
                       from = 1,
                       n = 30,
                       drop_tags = TRUE) {
  if (length(x$left) == 0 ||
      length(x$match) == 0 ||
      length(x$right) == 0) {
    stop("x must have appropriate values for 'left', 'match', and 'right'")
  }
  if (from < 1 || from > nrow(x)) {
    stop("the argument 'from' does not have an appropriate value for 'x'")
  } 
  if (n < 1) {
    stop("the argument 'n' does not have a appropriate value")
  } 
  df <- data.frame(left = as.character(x$left),
                   match = as.character(x$match),
                   right = as.character(x$right),
                   stringsAsFactors = FALSE)
  to <- min(nrow(df), from + n - 1)
  df <- df[from:to, ]
  if (drop_tags) {
    df$left <- cleanup_spaces(drop_tags(df$left))
    df$match <- cleanup_spaces(drop_tags(df$match))
    df$right <- cleanup_spaces(drop_tags(df$right))
  }
  largest_left <- max(nchar(df$left), 0, na.rm = TRUE)
  largest_match <- max(nchar(df$match), 0, na.rm = TRUE)
  largest_right <- max(nchar(df$right), 0, na.rm = TRUE)  
  # calculate width for id_col, left_col, match_col, and right_col
  c_avail <- max(0, getOption("width") - 7) # 7 for spaces between columns
                                            # with some margin for scroll bar
  # -- id col
  c_id_col <- max(nchar(as.character(from:(from + n - 1))))
  c_avail <- max(0, c_avail - c_id_col)
  # -- match col
  c_match_col <- trunc(c_avail * 0.3)
  if (! is.na(min_c_match)) c_match_col <- max(0, min_c_match, c_match_col)
  if (! is.na(max_c_match)) c_match_col <- max(0, min(c_match_col, max_c_match))
  c_match_col <- min(c_match_col, largest_match)
  # -- right col
  c_avail <- max(0, c_avail - c_match_col)
  c_right_col <- trunc(c_avail * 0.5)
  if (! is.na(min_c_right)) c_right_col <- max(0, min_c_right, c_right_col)
  if (! is.na(max_c_right)) c_right_col <- max(0, min(c_right_col, max_c_right))
  c_right_col <- min(c_right_col, largest_right)
  # -- left col
    c_left_col <- max(0, c_avail - c_right_col)
  if (! is.na(min_c_left)) c_left_col <- max(0, min_c_left, c_left_col)
  if (! is.na(max_c_left)) c_left_col <- max(0, min(c_left_col, max_c_left))
  # --  
  df$left = stringr::str_trunc(df$left, c_left_col, side = "left")
  df$match = stringr::str_trunc(df$match, c_match_col, side = "center")
  df$right = stringr::str_trunc(df$right, c_right_col, side = "right")
  print(df)
  invisible(x)
}


# ============================================================================
# generics
# ============================================================================

details <- function(x, y) UseMethod("details")

details.default <- function(x, y) NULL

zoom_re <- function(x, pattern, perl = TRUE) UseMethod("zoom_re")

zoom_re.default <- function(x, pattern, perl = TRUE) x

unzoom <- function(x) UseMethod("unzoom")

unzoom.default <- function(x) x

# ============================================================================
# ad hoc functions used in section on slma()
# ============================================================================

adhoc_min <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    min(x, na.rm = TRUE)
  }
}

adhoc_max <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    max(x, na.rm = TRUE)
  }
}

adhoc_sum <- function(x) {
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    sum(x, na.rm = TRUE)
  }
}


# ============================================================================
# string manipulation functions
# ============================================================================

cleanup_spaces <- function(x,
                           remove_leading = TRUE,
                           remove_trailing = TRUE) {
  x <- gsub("\\s+", " ", x, perl = TRUE)
  if (remove_leading) {
    x <- gsub("^[ ]", "", x, perl = TRUE)
  }
  if (remove_trailing) {
    x <- gsub("[ ]$", "", x, perl = TRUE)
  }
  x
}

drop_tags <- function(x, half_tags_too = TRUE) {
   if (half_tags_too) {
      x <- gsub("^[^<]*>|<[^>]*>|<[^>]*$", "", x, perl = TRUE)
   } else {
      x <- gsub("<[^>]*>", "", x, perl = TRUE)
   }
   x
}

# ============================================================================
# tokenization
# ============================================================================

tokenize <- function(x, # character vector or TextDocument
                     re_drop_line = NA,
                     line_glue = NA, # e.g. "\n" or ""
                     re_cut_area = NA,
                     re_token_splitter = "\\s+",
                     re_token_extractor = "[^\\s]+",
                     re_drop_token = NA,
                     re_token_transf_in = NA,
                     token_transf_out = NA,
                     token_to_lower = TRUE,
                     perl = TRUE) {
  if ("TextDocument" %in% class(x)) x <- as.character(x)
  # (further) split into lines -----------------------------------
  x <- unlist(strsplit(x, split = "\n"))
  # drop lines if needed -----------------------------------------
  if (!is.na(re_drop_line)) {
    x <- x[grep(re_drop_line, x, perl = perl, invert = TRUE)]
  }
  # paste lines in long line if needed ---------------------------
  if (!is.na(line_glue)) {
    x <- paste(x, collapse = line_glue)
  }
  # drop uninterestion regions if needed -------------------------
  if (!is.na(re_cut_area)) {
    x <- gsub(re_cut_area, "", x, perl = perl)
  }
  # identify tokens ----------------------------------------------
  if (!is.na(re_token_splitter)) {
    tokens <- unlist(strsplit(x, re_token_splitter, perl = perl))
  } else {
    m <- gregexpr(re_token_extractor, x, perl = perl)
    tokens <- unlist(regmatches(x, m))
  }
  # -- drop tokens if needed -------------------------------------
  if (!is.na(re_drop_token)) {
    tokens <- tokens[grep(re_drop_token, tokens, perl = perl, invert = TRUE)]
  }
  # transform tokens if needed -----------------------------------
  if (! is.na(re_token_transf_in)) {
    tokens <- gsub(re_token_transf_in, token_transf_out, tokens)
  }
  # tokens to lower if needed ------------------------------------
  if (token_to_lower) {
    tokens <- tolower(tokens)
  }
  class(tokens) = "tokens"
  tokens
}

# =============================================================================
# the class "freqlist"
# =============================================================================

freqlist <- function(x,
                     re_drop_line = NA,
                     line_glue = NA, 
                     re_cut_area = NA,
                     re_token_splitter = "\\s+",
                     re_token_extractor = "[^\\s]+",
                     re_drop_token = NA,
                     re_token_transf_in = NA,
                     token_transf_out = NA,
                     token_to_lower = TRUE,
                     perl = TRUE,
                     blocksize = 300,
                     verbose = FALSE,
                     show_dots = FALSE,
                     dot_blocksize = 10,
                     file_encoding = "UTF-8",
                     as_text = FALSE) {
  if (as_text) {
    freqlist_char(x,
                  re_drop_line = re_drop_line,
                  line_glue = line_glue,
                  re_cut_area = re_cut_area,
                  re_token_splitter = re_token_splitter,
                  re_token_extractor = re_token_extractor,
                  re_drop_token = re_drop_token,
                  re_token_transf_in = re_token_transf_in,
                  token_transf_out = token_transf_out,
                  token_to_lower = token_to_lower,
                  perl = perl)
  } else {
    freqlist_corp(x,
                  re_drop_line = re_drop_line,
                  line_glue = line_glue,
                  re_cut_area = re_cut_area,
                  re_token_splitter = re_token_splitter,
                  re_token_extractor = re_token_extractor,
                  re_drop_token = re_drop_token,
                  re_token_transf_in = re_token_transf_in,
                  token_transf_out = token_transf_out,
                  token_to_lower = token_to_lower,
                  perl = perl,
                  blocksize = blocksize,
                  verbose = verbose,
                  show_dots = show_dots,
                  dot_blocksize = dot_blocksize,
                  file_encoding = file_encoding)
  }
}

freqlist_char <- function(x,
                          re_drop_line = NA,
                          line_glue = NA, 
                          re_cut_area = NA,
                          re_token_splitter = "\\s+",
                          re_token_extractor = "[^\\s]+",
                          re_drop_token = NA,
                          re_token_transf_in = NA,
                          token_transf_out = NA,
                          token_to_lower = TRUE,
                          perl = TRUE) {
  if ("tokens" %in% class(x)) {
    freqlist <- table(x)
  } else {
    freqlist <- table(tokenize(x,
                               re_drop_line = re_drop_line,
                               line_glue = line_glue,
                               re_cut_area = re_cut_area,
                               re_token_splitter = re_token_splitter,
                               re_token_extractor = re_token_extractor,
                               re_drop_token = re_drop_token,
                               re_token_transf_in = re_token_transf_in,
                               token_transf_out = token_transf_out,
                               token_to_lower = token_to_lower,
                               perl = TRUE))
  }  
  class(freqlist) <- "freqlist"
  freqlist
}


freqlist_corp <- function(x,
                          re_drop_line = NA,
                          line_glue = NA, # e.g. "\n" or ""
                          re_cut_area = NA,
                          re_token_splitter = "\\s+",
                          re_token_extractor = "[^\\s]+",
                          re_drop_token = NA,
                          re_token_transf_in = NA,
                          token_transf_out = NA,
                          token_to_lower = TRUE,
                          perl = TRUE,
                          blocksize = 300,
                          verbose = FALSE,
                          show_dots = FALSE,
                          dot_blocksize = 10,
                          file_encoding = "UTF-8") {
  first_pt <- proc.time(); new_pt <- first_pt
  globfreqlist <- vector()
  i = 1
  while (i <= length(x)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i + j) <= length(x))) {
      fname <- x[i + j]
      # -- read corpus file
      con <- file(fname, encoding = file_encoding)
      newlines <- readLines(con, warn = FALSE)
      close(con)    
      # -- drop lines if needed
      if (!is.na(re_drop_line)) {
        newlines <- newlines[grep(re_drop_line, newlines,
                                  perl = perl, invert = TRUE)]
      }
      # -- paste lines in long line if needed
      if (!is.na(line_glue)) {
        newlines <- paste(newlines, collapse = line_glue)
      }
      # -- drop uninterestion regions if needed
      if (!is.na(re_cut_area)) {
        newlines <- gsub(re_cut_area, "", newlines, perl = perl)
      }
      # -- identify tokens
      if (!is.na(re_token_splitter)) {
        newtokens <- unlist(strsplit(newlines,
                                     re_token_splitter,
                                     perl = perl))
      } else {
        m <- gregexpr(re_token_extractor,
                      newlines,
                      perl = perl)
        newtokens <- unlist(regmatches(newlines, m))
      }
      # -- drop tokens if needed
      if (!is.na(re_drop_token)) {
        newtokens <- newtokens[grep(re_drop_token,
                                    newtokens,
                                    perl = perl,
                                    invert = TRUE)]
      }
      # -- transform tokens if needed
      if (!is.na(re_token_transf_in)) {
        newtokens <- gsub(re_token_transf_in,
                          token_transf_out,
                          newtokens)
      }
      # -- tokens to lower if needed
      if (token_to_lower) {
        newtokens <- tolower(newtokens)
      }
      # ====================================================
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    blockfreqlist <- freqlist_from_text(blocktokens)
    globfreqlist <- addfreqlists(globfreqlist, blockfreqlist)
    prev_pt <- new_pt; new_pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new_pt[3]-first_pt[3], "|", 
           new_pt[3]-prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  if (verbose) cat("\n")
  class(globfreqlist) <- "freqlist"
  globfreqlist
}


as_freqlist <- function(x) {
  if (!"table" %in% class(x)) {
    stop("x must be of class \"table\"")
  }    
  class(x) <- c("freqlist", class(x))
  x
}

as.data.frame.freqlist <- function(x, ...) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class \"freqlist\"")
  }    
  d <- data.frame(frequency = as.numeric(x),
                  row.names = names(x))
  d <- d[order(x, decreasing = TRUE), , drop = FALSE]
  d
}

as.data.frame.conc <- function(x, ...) {
  if (! "conc" %in% class(x)) {
    stop("x must be of class \"x\"")
  }    
  d <- x
  class(d) <- setdiff(class(d), "conc")
  d
}

as_conc <- function(x,
                    left = NA,
                    match = NA,
                    right = NA,
                    keep_original = FALSE,
                    ...) {
  if (! "data.frame" %in% class(x)) {
    stop("x must be of class \"data.frame\"")
  }
  d <- x
  class(d) <- c("conc", setdiff(class(d), "conc"))
  # ==
  if (! is.na(left) && left != "left") {
    if (is.null(d[[left]])) {
      stop(paste0("the object 'x' does not have a column ", left, "'"))
    }
    d[["left"]] <- as.character(d[[left]])
    if (! keep_original) {
      d[[left]] <- NULL 
    }    
  }
  # --
  if (is.na(left) || left == "left") {
    if (is.null(d[["left"]])) {
      stop("the object 'x' does not have a column 'left'")
    }
    if (! is.character(d[["left"]])) {
      d[["left"]] <- as.character(d[["left"]])
    }  
  } 
  # ==
  if (! is.na(match) && match != "match") {
    if (is.null(d[[match]])) {
      stop(paste0("the object 'x' does not have a column ", match, "'"))
    }
    d[["match"]] <- as.character(d[[match]])
    if (! keep_original) {
      d[[match]] <- NULL 
    }    
  }
  # --
  if (is.na(match) || match == "match") {
    if (is.null(d[["match"]])) {
      stop("the object 'x' does not have a column 'match'")
    }
    if (! is.character(d[["match"]])) {
      d[["match"]] <- as.character(d[["match"]])
    }  
  } 
  # ==
  if (! is.na(right) && right != "right") {
    if (is.null(d[[right]])) {
      stop(paste0("the object 'x' does not have a column ", right, "'"))
    }
    d[["right"]] <- as.character(d[[right]])
    if (! keep_original) {
      d[[right]] <- NULL 
    }    
  }
  # --
  if (is.na(right) || right == "right") {
    if (is.null(d[["right"]])) {
      stop("the object 'x' does not have a column 'right'")
    }
    if (! is.character(d[["right"]])) {
      d[["right"]] <- as.character(d[["right"]])
    }  
  } 
  # ==
  d
}


print.freqlist <- function(x, n = 6, ...) {
  n_types <- length(x)
  n_tokens <- sum(x)
  sorted_x <- sort(x, decreasing = TRUE)
  n <- min(n, n_types)
  cat("Frequency list (number of types: ",
      n_types,
      ", number of tokens: ",
      n_tokens,
      ")\n",
      sep = "")
  if (n > 0) {
    d <- data.frame(word = names(sorted_x)[1:n],
                    frequency = as.numeric(sorted_x)[1:n])
    print(d)
    if (n_types > n) cat("...\n")
  }
  invisible(x)
}

details.freqlist <- function(x, y) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class \"freqlist\"")
  }
  if (!"character" %in% typeof(y)) {
    stop("y must be a character vector.")
  }
  result <- list(); class(result) <- "details.freqlist"
  result$item <- y
  result$freq <- as.numeric(x[y])
  result
}

print.details.freqlist <- function(x, ...) {
  if (!"details.freqlist" %in% class(x)) {
    stop("x must be of class \"details.freqlist\"")
  }
  cat("Word: ",
      x$item,
      "\nFrequency: ",
      x$freq,
      "\n",
      sep = "")  
  invisible(x)
}

zoom_re.freqlist <- function(x, pattern, perl = TRUE) {
  if (!"freqlist" %in% class(x)) {
    stop("x must be of class \"freqlist\"")
  }
  if (!"character" %in% typeof(pattern)) {
    stop("y must be a character vector containing a regular expression")
  }
  result <- list(
      zoom_pattern = pattern,
      zoom_selected = grep(pattern, names(x), perl = perl),
      full_list = x
    )
  class(result) <- "zoom_re.freqlist"
  result
}

print.zoom_re.freqlist <- function(x, n = 6, ...) {
  if (!"zoom_re.freqlist" %in% class(x)) {
    stop("x must be of class \"zoom_re.freqlist\"")
  }
  n_sel_types <- length(x$zoom_selected)
  cat("Pattern: ",
      x$zoom_pattern,
      " (number of matches: ",
      n_sel_types,
      ")\n",
      sep = "")
  sel_x <- x$full_list[x$zoom_selected]
  sorted_sel_x <- sort(sel_x, decreasing = TRUE)
  n <- min(n, n_sel_types)
  if (n > 0) {
    d <- data.frame(word = names(sorted_sel_x)[1:n],
                    frequency = as.numeric(sorted_sel_x)[1:n])
    print(d)
    if (n_sel_types > n) cat("...\n")
  }
  invisible(x)
}

as.data.frame.zoom_re.freqlist <- function(x, ...) {
  if (!"zoom_re.freqlist" %in% class(x)) {
    stop("x must be of class \"zoom_re.freqlist\"")
  }
  if (length(x$zoom_selected) == 0) {
    NULL
  } else {
    sel_x <- x$full_list[x$zoom_selected]
    sorted_sel_x <- sort(sel_x, decreasing = TRUE)
    d <- data.frame(word = names(sorted_sel_x),
                    frequency = as.numeric(sorted_sel_x))
    d
  }
}


# ============================================================================
# slma()
# ============================================================================


as.data.frame.slma <- function(x, ...) {
  x$scores
}

strsplit_space_tokenizer <- function(x) {
    unlist(strsplit(as.character(x), "\\s+", perl = TRUE))
}    

slma <- function(corp_a, corp_b,
                 sig_cutoff = qchisq(.95, df = 1),
                 small_pos = 0.00001,
                 keep_intermediate = TRUE) {
  #
  # a and b are expected to inherit from type Corpus
  #
  result <- list(intermediate = list()); class(result) <- "slma"  
  corp_ab <- c(corp_a, corp_b)
  tdm <- tm::TermDocumentMatrix(corp_ab,
    control = list(tokenize = strsplit_space_tokenizer,
                   wordLengths = c(1, Inf)))
  scores <- data.frame(row.names = rownames(tdm))  
  d_gsig <- data.frame(row.names = rownames(tdm))  
  d_logor <- data.frame(row.names = rownames(tdm))
  d_dir <- data.frame(row.names = rownames(tdm))
  n <- length(corp_a)
  m <- length(corp_b)
  for (i in 1:n) {
    for (j in (n+1):(n+m)) {
      colname <- paste0(i, "-", j)
      a <- as.vector(tdm[, i])
      b <- sum(a) - a
      c <- as.vector(tdm[, j])
      d <- sum(c) - c
      ascores <- assoc_abcd(a, b, c, d,
                            small_pos = small_pos)
      d_gsig[[colname]] <- ifelse(ascores$G > sig_cutoff, 1, NA) 
      d_logor[[colname]] <- log(ascores$OR)
      d_dir[[colname]] <- ascores$dir
    }  
  }
  scores$S_abs <- apply((d_dir * d_gsig), 1, sum, na.rm = TRUE)
  scores$S_nrm <- scores$S_abs / ncol(d_dir) 
  scores$S_att <- apply((d_dir == 1) * d_gsig, 1, sum, na.rm = TRUE)
  scores$S_rep <- apply((d_dir == -1) * d_gsig, 1, sum, na.rm = TRUE)
  scores$lor_min <- apply((d_logor * d_gsig), 1, adhoc_min) 
  scores$lor_max <- apply((d_logor * d_gsig), 1, adhoc_max) 
  scores$lor_sd <- apply((d_logor * d_gsig), 1, sd, na.rm = TRUE) 
  scores$S_lor <- apply((d_logor * d_gsig), 1, adhoc_sum) / ncol(d_dir)
  
  scores <- scores[order(scores$S_lor, decreasing = TRUE), ]
  # --                               
  result$scores <- scores
  result$sig_cutoff <- sig_cutoff
  result$small_pos <- small_pos
  if (keep_intermediate) {
    result$intermediate$corp_a <- corp_a
    result$intermediate$corp_b <- corp_b
    result$intermediate$tdm <- tdm
  }  
  result
}

print.slma <- function(x, n = 20, ...) {
  print(x$scores[1:n, ])
}


details.slma <- function(x, y) {
  if (!"slma" %in% class(x)) {
    stop("x must be of class \"slma\"")
  }
  if (length(x$intermediate) == 0) {
    stop("x was not created with keep_intermediate = TRUE.")    
  }
  if (!"character" %in% typeof(y)) {
    stop("y must be a character vector.")
  }
  result <- list(); class(result) <- "details.slma"
  result$summary <- x$scores[y, , drop = FALSE]
  freqs <- as.vector(x$intermediate$tdm[y, ])
  corp_sizes <- apply(x$intermediate$tdm, 2, function(x) sum(as.vector(x)))
  n <- length(x$intermediate$corp_a)
  m <- length(x$intermediate$corp_b)
  labs <- as.character(names(corp_sizes))
  idx <- 0
  res <- data.frame(comp = character(n * m),
                    a = numeric(n * m),
                    b = numeric(n * m),
                    c = numeric(n * m),
                    d = numeric(n * m),
                    G = numeric(n * m),
                    sig = numeric(n * m),
                    dir = numeric(n * m),                     
                    dir_sig = numeric(n * m),                     
                    #OR = numeric(n * m),
                    #OR_sig = numeric(n * m),
                    log_OR = numeric(n * m),
                    log_OR_sig = numeric(n * m),                    
                    stringsAsFactors = FALSE)
  for (i in 1:n) {
    for (j in (n+1):(n+m)) {
      idx <- idx + 1
      res[idx, "comp"] <- paste0(labs[i], "--", labs[j])
      res[idx, "a"]    <- freqs[i]
      res[idx, "b"] <- corp_sizes[i] - freqs[i]
      res[idx, "c"] <- freqs[j]
      res[idx, "d"] <- corp_sizes[j] - freqs[j]
      ascores <- assoc_abcd(res[idx, "a"],  res[idx, "b"],
                            res[idx, "c"],  res[idx, "d"],
                            small_pos = x$small_pos)
      res[idx, "G"] <- ascores$G
      res[idx, "sig"] <- ifelse(ascores$G > x$sig_cutoff, 1, 0)
      res[idx, "dir"] <- ascores$dir      
      res[idx, "dir_sig"]  <-  ifelse(ascores$G > x$sig_cutoff,
                                     ascores$dir,
                                     NA)      
      res[idx, "log_OR"]  <-  log(ascores$OR)
      res[idx, "log_OR_sig"]  <-  ifelse(ascores$G > x$sig_cutoff,
                                         log(ascores$OR),
                                         NA)
    }
  }
  row.names(res) <- res$comp
  res$comp <- NULL
  result$details <- res
  result$item <- y
  result$sig_cutoff <- x$sig_cutoff
  result$small_pos <- x$small_pos
  result    
}

print.details.slma <- function(x, ...) {
  if (!"details.slma" %in% class(x)) {
    stop("x must be of class \"details.slma\"")
  }  
  cat(paste0('SLMA details\n'))  
  cat(paste0('[item: "', x$item,'"]\n\n'))
  print(round(x$summary, 3))  
  cat(paste0('\n[cutoff for G: ', round(x$sig_cutoff, 3), ']\n'))
  cat(paste0('[small positive offset: ', x$small_pos, ']\n\n'))
  print(round(x$details, 3))      
}


tokenize <- function(x, # character vector or TextDocument
                     re_drop_line = NA,
                     line_glue = NA, # e.g. "\n" or ""
                     re_cut_area = NA,
                     re_token_splitter = "\\s+",
                     re_token_extractor = "[^\\s]+",
                     re_drop_token = NA,
                     re_token_transf_in = NA,
                     token_transf_out = NA,
                     token_to_lower = TRUE,
                     perl = TRUE) {
  if ("TextDocument" %in% class(x)) x <- as.character(x) 
  # drop lines if needed ----------------------------------------
  if (!is.na(re_drop_line)) {
    x <- x[grep(re_drop_line, x, perl = perl, invert = TRUE)]
  }
  # paste lines in long line if needed ---------------------------
  if (!is.na(line_glue)) {
    x <- paste(x, collapse = line_glue)
  }
  # drop uninterestion regions if needed -------------------------
  if (!is.na(re_cut_area)) {
    x <- gsub(re_cut_area, "", x, perl = perl)
  }
  # identify tokens ----------------------------------------------
  if (!is.na(re_token_splitter)) {
    tokens <- unlist(strsplit(x, re_token_splitter, perl = perl))
  } else {
    m <- gregexpr(re_token_extractor, x, perl = perl)
    tokens <- unlist(regmatches(x, m))
  }
  # -- drop tokens if needed -------------------------------------
  if (!is.na(re_drop_token)) {
    tokens <- tokens[grep(re_drop_token, tokens, perl = perl, invert = TRUE)]
  }
  # transform tokens if needed -----------------------------------
  if (! is.na(re_token_transf_in)) {
    tokens <- gsub(re_token_transf_in, token_transf_out, tokens)
  }
  # tokens to lower if needed ------------------------------------
  if (token_to_lower) {
    tokens <- tolower(tokens)
  }
  class(tokens) = "tokens"
  tokens
}


# ============================================================================
#
# ============================================================================
assoc_abcd <- function(a, b, c, d,
                       measures = NULL,
                       with_variants = FALSE,
                       show_dots = FALSE,
                       p_fisher_2 = FALSE,
                       small_pos = 0.00001) {
  assoc_scores_abcd(a, b, c, d,
                    measures = measures,
                    with_variants = with_variants,
                    show_dots = show_dots,
                    p_fisher_2 = p_fisher_2,
                    small_pos = small_pos)
}    



# ---
#  READ FROM CONSOLE
#

scan_re <- function() {
  result <- ""
  nlines <- 0
  while(grepl("\\S", newline <- readLines(n = 1), perl = TRUE)) {
    if (nchar(result) > 0) {
      result <- paste0(result, "\n")
    }
    result <- paste0(result, newline)
    nlines <- nlines + 1
  }
  if (nlines > 1) {
    result <- paste0(result, "\n")
  }
  result
}

scan_re2 <- function() {
  x <- scan(what = "character", sep = "\n")
  paste(x, collapse = "\n")
}


scan_txt <- function() {
  scan_re()
}

cat_re <- function(x,
                   format = c("plain", "R", "TeX"),
                   as_single_line = TRUE) {
  if (as_single_line) {
    x <- gsub("(?<![[\\\\])#[^\n]*[\r\n]*", "", x, perl = TRUE)
    x <- gsub("[\r\n]+", " ", x, perl = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- gsub("(^[ ]|[ ]$)", "", x, perl = TRUE)
  }
  if (format[1] == "R") {
    x <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
    x <- gsub("\"", "\\\"", x, perl = TRUE)
    x <- paste0("\"", x, "\"")
  }
  cat(x)
  cat("\n\n")
  invisible(x)
}


# ---
#  READ RAW CORPUS FILE
#

read.raw.corpfile.tokens <- function(file, fileEncoding="") {
# -- helper function of read.raw.corpus.freqinfo()
  # --
  con <- file(file, encoding = fileEncoding)
  lines <- readLines(con, warn=FALSE)
  close(con)    
  # --
  #lines <- scan(file, what="char", sep="\n", 
  #              fileEncoding=fileEncoding, blank.lines.skip = FALSE,
  #              quiet=TRUE)
  long.line <- paste(lines, collapse="")
  return(strsplit(long.line, "\\s+", perl=TRUE)[[1]])
}

make.freqlist <- function(tokens) {
# -- helper function of read.raw.corpus.freqinfo()
  return(table(as.factor(tokens)))
}

merge.freqlists <- function(flist1, flist2) {
# -- helper function of read.raw.corpus.freqinfo()
  names <- union(names(flist1), names(flist2))
  flist1 <- flist1[names]; names(flist1) <- names; flist1[is.na(flist1)] <- 0
  flist2 <- flist2[names]; names(flist2) <- names; flist2[is.na(flist2)] <- 0
  return(flist1 + flist2)
}

merge_freqlists <- function(x, y) {
  names <- union(names(x), names(y))
  x <- x[names]
  names(x) <- names
  x[is.na(x)] <- 0
  y <- y[names]
  names(y) <- names
  y[is.na(y)] <- 0
  x + y
}


# ---
#  FREQUENCY LISTS, KEYWORDS AND COLLOCATION RETRIEVAL FUNCTIONS
#

raw.corpus.freqlist <- function(fnames, 
                                fileEncoding="", 
                                drop.tokens=NULL) {
  freqlist <- numeric(0)
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     newtokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       newtokens <- newtokens[- grep(drop.tokens, newtokens, perl=TRUE)]
     }
     tokens    <- append(tokens, newtokens)
     cat("."); utils::flush.console()
     if ((i %% 20) == 0) {
       freqlist <- merge.freqlists(freqlist, make.freqlist(tokens))
       tokens <- character(0)
     }
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }
  }
  freqlist <- merge.freqlists(freqlist, make.freqlist(tokens))
  tokens <- numeric(0)
  return(freqlist)
}

raw.corpus.surface.cooccur <- function(fnames, 
                                       freqlist,
                                       L=3, 
                                       R=3,
                                       boundary.marker="<boundary />",
                                       fileEncoding="", 
                                       drop.tokens=NULL) {
  # ---
  itemlen <- length(freqlist)
  itempos <- 1:itemlen; names(itempos) <- names(freqlist)
  # ---
  pair.names <- outer(names(freqlist), names(freqlist), paste, sep=" | ")
  dim(pair.names) <- NULL
  pair.freqs <- rep(0, length(pair.names))
  names(pair.freqs) <- pair.names
  # ---
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     tokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       tokens <- tokens[- grep(drop.tokens, tokens, perl=TRUE)]
     }
     cat("."); utils::flush.console()
     # ---
     for (j in 1:length(tokens)) { # loop over collocates
       colloc <- tokens[j]
       if (! is.na(freqlist[colloc])) {
         nodes <- character(0)
         # --- look for nodes to left of collocate ---
         k <- 1
         while ((k <= R) && ((j - k) > 0)) {  # R from persp. of colloc is L from persp. of node
           node <- tokens[j - k]
           if (is.na(node)) {
             k <- R+1
           } else if (node == boundary.marker) {
             k <- R+1
           } else {
             if (! is.na(freqlist[node])) {
               nodes <- append(nodes, node)             
             }
             k <- k+1
           }
         }  # end of while (k <= R) {  }
         # --- look for nodes to right of collocate ---
         k <- 1
         while (k <= L) {  # L from persp. of colloc is R from persp. of node
           node <- tokens[j + k]
           if (is.na(node)) {
             k <- L+1
           } else if (node == boundary.marker) {
             k <- L+1
           } else {
             if (! is.na(freqlist[node])) {
               nodes <- append(nodes, node)
             }
             k <- k+1
           }
         }  # end of while (k <= L) {  }
         if (length(nodes) > 0) {
           for (node in levels(as.factor(nodes))) {
               pos <- itempos[colloc] + (itempos[node]-1)*itemlen
               pair.freqs[pos] = pair.freqs[pos] + 1
           }
         }
       }  # end of: if (! is.na(freqlist[colloc]))  {  }
     }  # end of: for (j in 1:length(tokens)) {  }
     # ---
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }  # end of: if (((i %% 60) == 0) | (i == length(fnames))) {  }
  }  # end of: for (i in 1:length(fnames)) { }
  return(pair.freqs)
}

surf_cooc <- function(x, 
                      re_node,
                      w_left = 3, 
                      w_right = 3,
                      re_boundary = NA,
                      re_drop_line = NA,
                      line_glue = NA, 
                      re_cut_area = NA,
                      re_token_splitter = "\\s+",
                      re_token_extractor = "[^\\s]+",
                      re_drop_token = NA,
                      re_token_transf_in = NA,
                      token_transf_out = NA,
                      token_to_lower = TRUE,
                      perl = TRUE,
                      blocksize = 300,
                      verbose = FALSE,
                      dot_blocksize = 10,
                      file_encoding = "UTF-8") {
  first_pt <- proc.time(); new_pt <- first_pt
  retval <- list()
  globfreqlist1 <- vector()
  globfreqlist2 <- vector()
  i = 1
  while (i <= length(x)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i + j) <= length(x))) {
      file <- x[i + j]
      # -- read file
      con <- file(file, encoding = file_encoding)
      newlines <- readLines(con, warn = FALSE)
      close(con)    
      # -- drop lines if needed
      if (! is.na(re_drop_line)) {
        newlines <- newlines[grep(re_drop_line, newlines,
                                  perl = perl, invert = TRUE)]
      }
      # -- paste lines in long line if needed
      if (! is.na(line_glue)) {
        newlines <- paste(newlines, collapse = line_glue)
      }
      # -- drop uninterestion regions if needed
      if (! is.na(re_cut_area)) {
        newlines <- gsub(re_cut_area, "", newlines, perl = perl)
      }
      # -- identify tokens
      if (! is.na(re_token_splitter)) {
        newtokens <- unlist(strsplit(newlines,
                                     re_token_splitter,
                                     perl = perl))
      } else {
        m <- gregexpr(re_token_extractor, newlines, perl = perl)
        newtokens <- unlist(regmatches(newlines, m))
      }
      # -- drop token if needed
      if (! is.na(re_drop_token)) {
        newtokens <- newtokens[grep(re_drop_token, newtokens,
                                    perl = perl, invert = TRUE)]
      }
      # -- transform tokens if needed
      if (! is.na(re_token_transf_in)) {
        newtokens <- gsub(re_token_transf_in, token_transf_out,
                          newtokens)
      }
      # -- tokens to lower if needed
      if (token_to_lower) {
        newtokens <- tolower(newtokens)
      }
      # ====================================================
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat(".")
        utils::flush.console() 
      }
      j <- j + 1
    }
    match_pos <- grep(re_node, blocktokens, perl = perl)
    bound_pos <- vector()
    if (!is.na(re_boundary)) {
      bound_pos <- grep(re_boundary, blocktokens, perl = perl)
    }
    target_pos <- vector()
    new_pos <- match_pos
    if (w_left > 0) {
      for (k in 1:w_left) {
        new_pos <- new_pos - 1
        new_pos <- setdiff(new_pos, bound_pos)
        target_pos <- union(target_pos, new_pos)
      }
    }
    new_pos <- match_pos
    if (w_right > 0) {
      for (k in 1:w_right) {
        new_pos <- new_pos + 1
        new_pos <- setdiff(new_pos, bound_pos)
        target_pos <- union(target_pos, new_pos)
      }
    }
    target_pos <- target_pos[target_pos > 0]
    target_pos <- target_pos[target_pos <= length(blocktokens)]
    target_pos <- setdiff(target_pos, match_pos)
    
    ref_pos <- setdiff(1:length(blocktokens), target_pos)
    ref_pos <- setdiff(ref_pos, match_pos)
    ref_pos <- setdiff(ref_pos, bound_pos)
    
    t1 <- table(blocktokens[target_pos])
    blockfreqlist1 <- as.vector(t1)
    names(blockfreqlist1) <- names(t1)
    globfreqlist1 <- addfreqlists(globfreqlist1, blockfreqlist1)
    
    t2 <- table(blocktokens[ref_pos])
    blockfreqlist2 <- as.vector(t2)
    names(blockfreqlist2) <- names(t2)
    globfreqlist2 <- addfreqlists(globfreqlist2, blockfreqlist2)

    prev_pt <- new_pt; new_pt <- proc.time()
    if (verbose) {
      cat((i + j) - 1,"(", new_pt[3] - first_pt[3], "|", 
           new_pt[3] - prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  cooc_info(target_freqlist = globfreqlist1,
            ref_freqlist = globfreqlist2,
            target_n = sum(globfreqlist1),
            ref_n = sum(globfreqlist2))
}

text_cooc <- function(x, 
                      re_node,
                      re_boundary = NA,
                      re_drop_line = NA,
                      line_glue = NA,
                      re_cut_area = NA,
                      re_token_splitter = "\\s+",
                      re_token_extractor = "[^\\s]+",
                      re_drop_token = NA,
                      re_token_transf_in = NA,
                      token_transf_out = NA,
                      token_to_lower = TRUE,
                      perl = TRUE,
                      blocksize = 300,
                      verbose = FALSE,
                      dot_blocksize = 10,
                      file_encoding = "UTF-8") {
  first_pt <- proc.time(); new_pt <- first_pt
  retval <- list()
  globfreqlist1 <- vector()
  corpsize1 <- 0
  globfreqlist2 <- vector()
  corpsize2 <- 0
  i = 1
  while (i <= length(x)) {
    j = 0
    blocktokens1 <- vector()
    blocktokens2 <- vector()
    while ((j < blocksize) && ((i + j) <= length(x))) {
      file <- x[i + j]
      # -- read file
      con <- file(file, encoding = file_encoding)
      newlines <- readLines(con, warn = FALSE)
      close(con)    
      # -- drop lines if needed
      if (! is.na(re_drop_line)) {
        newlines <- newlines[grep(re_drop_line, newlines,
                                  perl = perl, invert = TRUE)]
      }
      # -- paste lines in long line if needed
      if (! is.na(line_glue)) {
        newlines <- paste(newlines, collapse = line_glue)
      }
      # -- drop uninterestion regions if needed
      if (! is.na(re_cut_area)) {
        newlines <- gsub(re_cut_area, "", newlines, perl = perl)
      }
      # -- identify tokens
      if (! is.na(re_token_splitter)) {
        newtokens <- unlist(strsplit(newlines,
                                     re_token_splitter,
                                     perl = perl))
      } else {
        m <- gregexpr(re_token_extractor, newlines, perl = perl)
        newtokens <- unlist(regmatches(newlines, m))
      }
      # -- drop token if needed
      if (! is.na(re_drop_token)) {
        newtokens <- newtokens[grep(re_drop_token, newtokens,
                                    perl = perl, invert = TRUE)]
      }
      # -- transform tokens if needed
      if (! is.na(re_token_transf_in)) {
        newtokens <- gsub(re_token_transf_in,
                          token_transf_out,
                          newtokens)
      }
      # -- tokens to lower if needed
      if (token_to_lower) {
        newtokens <- tolower(newtokens)
      }
      # ====================================================
      if (! is.na(re_boundary)) {
        boundaries <- grep(re_boundary, newtokens, perl = perl)
      } else {
        boundaries <- numeric(0)
      }
      if (length(boundaries) > 0) {
        v <- newtokens[-boundaries]
        f <- rep(1:(length(boundaries) + 1),
                 c(boundaries, length(newtokens) + 1) -
                 c(1, boundaries + 1))
        txts <- split(v, f)
      } else {
        txts <- list('1' = newtokens)
      }
      for (item in txts) {
        hits <- grep(re_node, item, perl = perl)
        if (length(hits) > 0) {
          blocktokens1 <- c(blocktokens1, names(freqlist(item[-hits])))
          corpsize1 <-  corpsize1 + 1
        } else {
          blocktokens2 <- c(blocktokens2, names(freqlist(item)))
          corpsize2 <-  corpsize2 + 1
        }
      }
      # ====================================================
      if (verbose && (((i + j) %% dot_blocksize) == 0)) { 
        cat(".")
        utils::flush.console() 
      }
      j <- j + 1
    }
    # --
    t1 <- table(blocktokens1)
    blockfreqlist1 <- as.vector(t1)
    names(blockfreqlist1) <- names(t1)
    globfreqlist1 <- addfreqlists(globfreqlist1, blockfreqlist1)

    t2 <- table(blocktokens2)
    blockfreqlist2 <- as.vector(t2)
    names(blockfreqlist2) <- names(t2)
    globfreqlist2 <- addfreqlists(globfreqlist2, blockfreqlist2)

    # --
    prev_pt <- new_pt; new_pt <- proc.time()
    if (verbose) {
      cat((i + j) - 1,"(", new_pt[3] - first_pt[3], "|", 
           new_pt[3] - prev_pt[3], ")\n")
      utils::flush.console()
    }
    i <- i + j
  }
  cooc_info(target_freqlist = globfreqlist1,
            ref_freqlist = globfreqlist2,
            target_n = corpsize1,
            ref_n = corpsize2)
}


# testen: hoe verkrijgen dat leestekens niet meegeteld worden: drop.token ?

raw.getsurfcooc.node <- function(
          files, 
          re.node,
          L=3, 
          R=3,
          re.boundary="<boundary />",
          re.split="[.'!?;;,\\s]+",
          re.ok.line=".*",
          re.drop.token=NA,
          to.lower=TRUE,
          perl=TRUE,
          blocksize=300,
          verbose=FALSE,
          dot.blocksize=10,
          fileEncoding="latin1") {
  first.pt <- proc.time(); new.pt <- first.pt
  retval <- list()
  globfreqlist1 <- vector()
  globfreqlist2 <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = fileEncoding)
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding=fileEncoding, 
      #                 blank.lines.skip = FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- unlist(strsplit(newlines, re.split, perl=perl))
      if (to.lower) { newtokens <- tolower(newtokens) }
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    match.pos <- grep(re.node, blocktokens, perl=perl)
    bound.pos <- vector()
    if (! is.na(re.boundary)) {
      bound.pos <- grep(re.boundary, blocktokens, perl=perl)
    }
    target.pos <- vector()
    new.pos <- match.pos
    if (L > 0) {
      for (k in 1:L) {
        new.pos <- new.pos - 1
        new.pos <- setdiff(new.pos, bound.pos)
        target.pos <- union(target.pos, new.pos)
      }
    }
    new.pos <- match.pos
    if (R > 0) {
      for (k in 1:R) {
        new.pos <- new.pos + 1
        new.pos <- setdiff(new.pos, bound.pos)
        target.pos <- union(target.pos, new.pos)
      }
    }
    target.pos <- target.pos[target.pos > 0]
    target.pos <- target.pos[target.pos <= length(blocktokens)]
    target.pos <- setdiff(target.pos, match.pos)
    
    ref.pos <- setdiff(1:length(blocktokens), target.pos)
    ref.pos <- setdiff(ref.pos, match.pos)
    ref.pos <- setdiff(ref.pos, bound.pos)
    
    t1 <- table(blocktokens[target.pos])
    blockfreqlist1 <- as.vector(t1); names(blockfreqlist1) <- names(t1)
    globfreqlist1 <- addfreqlists(globfreqlist1, blockfreqlist1)
    
    t2 <- table(blocktokens[ref.pos])
    blockfreqlist2 <- as.vector(t2); names(blockfreqlist2) <- names(t2)
    globfreqlist2 <- addfreqlists(globfreqlist2, blockfreqlist2)

    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  retval$targetfreqs <- globfreqlist1
  retval$reffreqs <- globfreqlist2
  return(retval)
}


raw.corpus.surface.cooccur.node.old <- function(
                                       fnames, 
                                       freqlist,
                                       L=3, 
                                       R=3,
                                       node.regexp,
                                       boundary.marker="<boundary />",
                                       fileEncoding="", 
                                       drop.tokens=NULL) {
  # ---
  itemlen <- length(freqlist)
  itempos <- 1:itemlen; names(itempos) <- names(freqlist)
  # ---
  item.freqs <- rep(0, length(freqlist))
  names(item.freqs) <- names(freqlist)
  # ---
  tokens <- character(0)
  for (i in 1:length(fnames)) {
     fname     <- fnames[i]
     tokens <- read.raw.corpfile.tokens(fname, fileEncoding="UTF8")
     if (! is.null(drop.tokens)) {
       tokens <- tokens[- grep(drop.tokens, tokens, perl=TRUE)]
     }
     node.pos <- rep(FALSE, length(tokens))
     node.pos[grep(node.regexp, tokens, perl=TRUE)] <- TRUE
     cat("."); utils::flush.console()
     # ---
     for (j in 1:length(tokens)) { # loop over collocates
       colloc <- tokens[j]
       if (! is.na(freqlist[colloc])) {
         node.found <- FALSE
         # --- look for nodes to left of collocate ---
         k <- 1
         while ((k <= R) && ((j - k) > 0)) {  # R from persp. of colloc is L from persp. of node
           node <- tokens[j - k]
           if (is.na(node)) {
             k <- R+1
           } else if (node == boundary.marker) {
             k <- R+1
           } else {
             if (node.pos[j-k]) {
               node.found <- TRUE             
             }
             k <- k+1
           }
         }  # end of while (k <= R) {  }
         # --- look for nodes to right of collocate ---
         k <- 1
         while (k <= L) {  # L from persp. of colloc is R from persp. of node
           node <- tokens[j + k]
           if (is.na(node)) {
             k <- L+1
           } else if (node == boundary.marker) {
             k <- L+1
           } else {
             if (node.pos[j+k]) {
               node.found <- TRUE
             }
             k <- k+1
           }
         }  # end of while (k <= L) {  }
         if (node.found) {
               pos <- itempos[colloc]
               item.freqs[pos] = item.freqs[pos] + 1
         }
       }  # end of: if (! is.na(freqlist[colloc]))  {  }
     }  # end of: for (j in 1:length(tokens)) {  }
     # ---
     if (((i %% 60) == 0) | (i == length(fnames))) {
       cat("\n"); utils::flush.console()
     }  # end of: if (((i %% 60) == 0) | (i == length(fnames))) {  }
  }  # end of: for (i in 1:length(fnames)) { }
  return(item.freqs)
}


surface.cooccur.stats <- function(cooccur.freq, freqlist, corpus.size,
                                  min.freq=3) {
  cooccur.freq <- cooccur.freq[cooccur.freq >= min.freq]
  d <- data.frame(fullname = names(cooccur.freq), stringsAsFactors=FALSE)
  d$colloc = gsub("^(.*) \\| .*$", 
                  "\\1", names(cooccur.freq), perl=TRUE)
  d$node <- gsub("^.* \\| (.*)$", 
                 "\\1", names(cooccur.freq), perl=TRUE)
  d$a <- cooccur.freq
  d$b <- freqlist[d$node] - d$a
  d$c <- freqlist[d$colloc] - d$a
  d$d <- corpus.size - d$a - d$b - d$c
  d$DICE <- calc.DICE(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$OR <- calc.OR(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$log.OR <- calc.log.OR(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$PMI <- calc.PMI(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$Chi2 <- calc.Chi2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()
  d$Chi2.signed <- calc.Chi2.signed(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$p.Chi2 <- calc.p.Chi2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console() # two-sided
  d$G2 <- calc.G2(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
  d$G2.signed <- calc.G2.signed(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()
  d$p.G2 <- calc.p.G2(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console() # two-sided
  d$t <- calc.t(d$a, d$b, d$c, d$d);  cat("."); utils::flush.console()      
  d$p.t <- calc.p.t(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
                                         # use of this test not well motivated!!!
                                         # one-sided 
                                         # (only detects a too high)
  d$t.as.chisq1 <- p_to_chisq1(d$p.t); cat("."); utils::flush.console()
                                                   # bring p to scale of Chi2 and G2
  d$p.fisher <- calc.p.fisher(d$a, d$b, d$c, d$d); cat("."); utils::flush.console()
                                                   # one-sided 
                                                   # (only detects a too high)
  d$fisher.as.chisq1 <- p_to_chisq1(d$p.fisher); cat("."); utils::flush.console()
                                                   # bring p to scale of Chi2/G2
  cat("\n"); utils::flush.console()
  return(d)
}

# ---
# RUN CONCORDANCE IN R
#

conc_re <- function(pattern,
                    x,
                    c_left = 200,
                    c_right = 200,
                    perl = TRUE,
                    after_line = "\n",
                    file_encoding = "UTF-8",
                    as_text = FALSE) {
  n_texts <- length(x)
  list_source <- vector("list", n_texts)
  list_left <- vector("list", n_texts)
  list_hits <- vector("list", n_texts)
  list_right <- vector("list", n_texts)
  if (length(file_encoding) < n_texts) {
     file_encoding <- rep(file_encoding, length = n_texts)
  }   
  for (i in seq_along(x)) {
    if (as_text) {
      text <- x[i]
    } else {
      fname <- x[i]
      text <- read_txt(fname, paste_char = after_line,
                       file_encoding = file_encoding[i])
    }
    m <- gregexpr(pattern, text, perl = perl)[[1]]
    start <- as.numeric(m)
    stop <- start + attr(m, "match.length") - 1
    left <- vector("character", 0)
    hits <- vector("character", 0)
    right <- vector("character", 0)
    if (start[1] > 0) { # if there are matches
         left  <- substr(rep(text, length(start)), start - c_left, start - 1)
         hits  <- substr(rep(text, length(start)), start, stop)
         right <- substr(rep(text, length(start)), stop + 1, stop + c_right)
    }
    if (as_text) {
        list_source[[i]] <- rep("-", length(left))
    } else {
        list_source[[i]] <- rep(fname, length(left))        
    }
    list_left[[i]] <- left
    list_hits[[i]] <- hits
    list_right[[i]] <- right      
  }
  src <- unlist(list_source)
  lft <- unlist(list_left)
  hts <- unlist(list_hits)
  rgt <- unlist(list_right)
  if (length(src) > 0) {
     gid <- id <- 1:length(src)
     lft <- cleanup_spaces(lft, remove_leading = FALSE, remove_trailing = FALSE) 
     hts <- cleanup_spaces(hts, remove_leading = FALSE, remove_trailing = FALSE) 
     rgt <- cleanup_spaces(rgt, remove_leading = FALSE, remove_trailing = FALSE) 
  } else {
     gid <- id <- numeric(0)
  }
  df <- data.frame(glob_id = gid,
                   id = id,
                   source = src,
                   left = lft,
                   match = hts,
                   right = rgt,
                   stringsAsFactors = FALSE)
  class(df) <- c("conc", class(df))
  df
}


# ---
#  FROM CONCORDANCES TO DATASETS
#

read_conc_antconc <- function(file,
                              version = c("3.4.3", "3.2.4"),
                              file_encoding = "") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: preferably YES (which
#           is not the default, but the default value NO is also OK)
# - Also, you must make sure to specify the correct file encoding.
  # --
  con <- file(file, encoding = file_encoding)
  lines <- readLines(con, warn = FALSE)
  close(con)
  # --
  d <- NA
  if (length(lines > 0)) {
    if (version[1] == "3.2.4") {
        # ----------------------------------------------------------
        # TAB is assumed to surround hit (but this is not cheched)
        # ----------------------------------------------------------
        cells <- strsplit(lines, split = "\t", fixed = TRUE)
        d <- data.frame(x = 1:length(lines)); d$x <- NULL
        d$id <- unlist(lapply(cells, "[", 1))
        d$left <- unlist(lapply(cells, "[", 2))
        d$match <- unlist(lapply(cells, "[", 3))
        d$right <- unlist(lapply(cells, "[", 4))
        d$source <- unlist(lapply(cells, "[", 5))   
    } else if (version[1] == "3.4.3") {
        # ----------------------------------------------------------
        # tab is preferred to surround hit (but may not)
        # ----------------------------------------------------------
        cells <- strsplit(lines, split = "\t", fixed = TRUE)
        d <- data.frame(x = 1:length(lines)); d$x <- NULL
        d$id <- unlist(lapply(cells, "[", 1))
        d$left <- unlist(lapply(cells, "[", 2))
        col3 <- unlist(lapply(cells, "[", 3))
        max_n_col3 <- max(nchar(col3))
        if (max_n_col3 == 0) {
          # there are TABS around the hits in KWIC display
          d$match <- unlist(lapply(cells, "[", 4))
          d$right <- unlist(lapply(cells, "[", 5))
          d$source <- unlist(lapply(cells, "[", 6))
        } else {
          # there are no TABS around the hits in KWIC display
          # we can only guess what are the boudaries of the hit
          d$match <- NA
          d$right <- unlist(lapply(cells, "[", 3))
          d$match <- gsub("^(\\s*[^ ]*).*$", "\\1", d$right, perl = TRUE)
          d$right <- substr(d$eight, nchar(d$match) + 1, nchar(d$right))
          d$source <- unlist(lapply(cells, "[", 4))       
        }
    }
  }
  d
}

read.conc.antconc.prev <- function(file, version=c("3.4.3", "3.2.4"),
  fileEncoding="") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: preferably YES (which
#           is not the default, but the default value NO is also OK)
# - Also, you must make sure to specify the correct file encoding.
# ---
# To be tested: do I need to use readChar, as I did in
#               read.conc.antconc.old(...), or is it safe to
#               use scan(...)
# ---
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)  
  d <- NA
  if (length(lines > 0)) {
    if (version[1] == "3.2.4") {
        # ---------------------------------------------------------- 
        # TAB is assumed to surround hit (but this is not cheched) 
        # ---------------------------------------------------------- 
        cells <- strsplit(lines, split="\t", fixed=TRUE)
        d <- data.frame(x=1:length(lines)); d$x <- NULL
        d$Id <- unlist(lapply(cells, "[", 1))
        d$Left <- unlist(lapply(cells, "[", 2))
        d$Node <- unlist(lapply(cells, "[", 3))
        d$Right <- unlist(lapply(cells, "[", 4))
        d$Source <- unlist(lapply(cells, "[", 5))    
    } else if (version[1] == "3.4.3") {
        # ---------------------------------------------------------- 
        # tab is preferred to surround hit (but may not)
        # ---------------------------------------------------------- 
        cells <- strsplit(lines, split="\t", fixed=TRUE)
        d <- data.frame(x=1:length(lines)); d$x <- NULL
        d$Id <- unlist(lapply(cells, "[", 1))
        d$Left <- unlist(lapply(cells, "[", 2))
        col3 <- unlist(lapply(cells, "[", 3))
        max.n.col3 <- max(nchar(col3))
        if (max.n.col3 == 0) {
          # there are TABS around the hits in KWIC display
          d$Node <- unlist(lapply(cells, "[", 4))
          d$Right <- unlist(lapply(cells, "[", 5))
          d$Source <- unlist(lapply(cells, "[", 6))
        } else {
          # there are no TABS around the hits in KWIC display
          # we can only guess what are the boudaries of the hit
          d$Node <- NA
          d$Right <- unlist(lapply(cells, "[", 3))
          d$Node <- gsub("^(\\s*[^ ]*).*$", "\\1", d$Right, perl=TRUE)
          d$Right <- substr(d$Right, nchar(d$Node)+1, nchar(d$Right))
          d$Source <- unlist(lapply(cells, "[", 4))        
        }
    }
  }
  return(d)
}


read.conc.antconc.old <- function(file, window.size=50, fileEncoding="") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: preferably YES (which
#           is not the default, but the default value NO is also OK)
# - Also, the window.size argument must specify the CORRECT window size that
#   was used in AntConc when running the query.
# - Also, you must make sure to specify the correct file encoding.
# ---
  fileSize <- file.info(file)$size
  connection <- file(file, "rb")
  chars <- iconv(readChar(connection, fileSize, useBytes=TRUE),
                 from=fileEncoding)
  close(connection)
  chars <- gsub("\r", " ", chars)
  lines <- strsplit(chars, "\n")[[1]]
  Id <- gsub("^([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  lines <- substr(lines, nchar(Id)+2, nchar(lines))
  #KWIC <- gsub("^[^\t]*\t(.*)\t[^\t]*$", "\\1", lines, perl=TRUE)
  Source <- gsub("^.*\t([^\t]*)$", "\\1", lines, perl=TRUE)
  lines <- substr(lines, 1, nchar(lines)-nchar(Source)-1)
  Left <- substr(lines, 1, window.size)
  lines <- substr(lines, window.size+1, nchar(lines))
  Node <- rep("", length(lines))
  hasHitInTabs <- grep("^\t([^\t]*)\t.*$", lines, perl=TRUE)
  hitInTabs <- gsub("^\t([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  Node[hasHitInTabs] <- hitInTabs[hasHitInTabs]
  hasHitNoTabs <- grep("^([\\w]*).*$", lines, perl=TRUE)
  hitNoTabs <- gsub("^([\\w]*).*$", "\\1", lines, perl=TRUE)
  hasHitNoTabs <- setdiff(hasHitNoTabs, hasHitInTabs)
  Node[hasHitNoTabs] <- hitNoTabs[hasHitNoTabs]
  NodeSize <- nchar(Node)+1
  NodeSize[hasHitInTabs] <- NodeSize[hasHitInTabs] + 2 
  Right <- substr(lines, NodeSize, nchar(lines))
  Left <- gsub("\t", " ", Left, perl=TRUE)
  Node <- gsub("\t", " ", Node, perl=TRUE)
  Right <- gsub("\t", " ", Right, perl=TRUE)
  return(data.frame(Id=Id, Source=Source, Left=Left, Node=Node, Right=Right))
}

read.conc.antconc.veryold <- function(file, window.size=50, fileEncoding="") {
# ---
# - Assumes AntConc concordance results were saved with the default Concordance
#   Preferences. Especially important are:
#       Display Options
#           Hit number: YES
#           KWIC display: YES
#           File name: YES
#       Other options
#           Put tab spaces around hits in KWIC display: NO
# - Also, the window.size argument must specify the CORRECT window size that
#   was used in AntConc when running the query.
# - Also, you must make sure to specify the correct file encoding.
# ---
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)
  Id <- gsub("^([^\t]*)\t.*$", "\\1", lines, perl=TRUE)
  KWIC <- gsub("^[^\t]*\t(.*)\t[^\t]*$", "\\1", lines, perl=TRUE)
  Source <- gsub("^.*\t([^\t]*)$", "\\1", lines, perl=TRUE)
  possible.antconc.errors <- (nchar(KWIC) < window.size*2)
  KWIC[possible.antconc.errors] <- format(KWIC[possible.antconc.errors], 
    width=(window.size*2 + 20)) # mitigate occasional AntConc error
  Left <- substr(KWIC, 1, window.size)
  Node <- substr(KWIC, window.size+1, nchar(KWIC)-window.size)
  Right <- substr(KWIC, nchar(KWIC)-window.size+1,nchar(KWIC))
  Left <- gsub("\t", " ", Left, perl=TRUE)
  Node <- gsub("\t", " ", Node, perl=TRUE)
  Right <- gsub("\t", " ", Right, perl=TRUE)
  return(data.frame(Id=Id, Source=Source, Left=Left, Node=Node, Right=Right))
}

conc_to_dataset_antconc <- function(x,
                                    outfile,
                                    version=c("3.4.3", "3.2.4"),
                                    file_encoding="") {
  if (length(file_encoding) < length(x)) {
    file_encoding <- rep(file_encoding, length = length(x))
  }
  lines <- vector()
  for (i in 1:length(x)) {
      d <- read_conc_antconc(x[i],
                             version = version, 
                             file_encoding = file_encoding[i])
      if (! is.null(dim(d))) {
        lines <- append(lines,
                        paste(as.character(d$id),
                             as.character(d$source),
                              as.character(d$left),
                              as.character(d$match),
                              as.character(d$right), sep = "\t"))
      }
  }
  lines <- paste(1:length(lines),"\t", lines, sep = "")
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", lines)
  y <- paste0(paste(lines, collapse = "\n"), "\n")
  con <- file(outfile, "wb")
  writeBin(charToRaw(y), con, endian = "little")
  close(con)
  invisible(x)
}

conc_to_dataset_corpuseye <- function(x, outfile) {
# ---
# - infiles are one or several files to which the concordance search
#   results from the corpuseye website (http://corp.hum.sdu.dk/)
#   were exported (by clicking on export all and then saving as *.txt)
# - such files always have encoding UTF-8
# - we also save the results with encoding UTF-8
# ---
  corpdata <- vector()
  for (infile in x) {
    con <- file(infile, encoding = "UTF-8")
    lines <- readLines(con, warn = FALSE)
    close(con)
    corpdata <- append(corpdata, lines)
  }
  corp <- paste(corpdata, collapse = " \n")
  corp <- gsub("([^\n]) \n([^\n])", "\\1 \\2", corp, perl = TRUE)
  corp <- gsub("\n \n", "\n", corp, perl = TRUE)
  corp <- gsub("\\*", "\t", corp, perl = TRUE)
  corp <- gsub("\\|", "", corp, perl = TRUE)
  corp <- unlist(strsplit(corp, "\n"))
  corp <- corp[grep("[^\\s]", corp, perl = TRUE)]
  corp <- paste("corpuseye", corp, sep = "\t")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  # some more appending needs to be done here
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", corp)
  y <- paste0(paste(lines, collapse = "\n"), "\n")
  con <- file(outfile, "wb")
  writeBin(charToRaw(y), con, endian = "little")
  close(con)
  invisible(x)
}

import_conc <- function(x,
                        file_encoding = "UTF-8",
                        source_type = c("corpuseye"),
                        ...) {
  if (! is.character(source_type)) {
    stop("the argument 'source_type' must be a character string")
  }
  source_type <- tolower(source_type)
  switch(source_type,
         corpuseye = import_conc_corpuseye(x, ...))
}  

import_conc_corpuseye <- function(x, ...) {
  if (! is.character(x)) {
    stop("argument 'x' must be a character vector containing file names")
  } 
  corpdata <- character(0)
  for (in_file in x) {
    new_lines <- read_txt(in_file, encoding = "UTF-8") 
    corpdata <- append(corpdata, new_lines)
  }
  corp <- paste(corpdata, collapse = " \n")
  corp <- gsub("([^\n]) \n([^\n])", "\\1 \\2", corp, perl = TRUE)
  corp <- gsub("\n \n", "\n", corp, perl = TRUE)
  corp <- gsub("\\*", "\t", corp, perl = TRUE)
  corp <- gsub("\\|", "", corp, perl = TRUE)
  corp <- unlist(strsplit(corp, "\n"))
  corp <- corp[grep("[^\\s]", corp, perl = TRUE)]
  corp <- paste("corpuseye", corp, sep = "\t")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  corp <- paste(1:length(corp), "\t", corp, sep = "")
  lines <- append("glob_id\tid\tsource\tleft\tmatch\tright", corp)
  # --  
  cols <- unlist(strsplit(lines[1], "\t"))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, "\t")
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
    col_name <- cols[i]
    d[[col_name]] <- unlist(lapply(cells, "[", i))
    d[[col_name]] <- type.convert(d[[col_name]], as.is = TRUE)
  }
  class(d) <- c("conc", class(d))
  d
}

merge_conc <- function(..., show_warnings = TRUE) {
  if (show_warnings) { 
    df <- dplyr::bind_rows(...)
  } else {
    suppressWarnings(dplyr::bind_rows(...)) 
  } 
  df$glob_id <- 1:nrow(df)
  df
}

conc.to.dataset.corpuseye.prev <- function(infiles, outfile) {
# ---
# - infiles are one or several files to which the concordance search 
#   results from the corpuseye website (http://corp.hum.sdu.dk/)
#   were exported (by clicking on export all and then saving as *.txt)
# - such files always have encoding UTF-8
# - we also save the results with encoding UTF-8
# ---
  corpdata <- vector()
  for (infile in infiles) {
    lines <- scan(infile, what="char", sep="\n", 
                  fileEncoding="UTF-8", blank.lines.skip = FALSE,
                  quiet=TRUE)
    corpdata <- append(corpdata, lines)
  }
  corp <- paste(corpdata, collapse=" \n")
  corp <- gsub("([^\n]) \n([^\n])", "\\1 \\2", corp, perl=TRUE)
  corp <- gsub("\n \n", "\n", corp, perl=TRUE)
  corp <- gsub("\\*", "\t", corp, perl=TRUE)
  corp <- gsub("\\|", "", corp, perl=TRUE)
  corp <- unlist(strsplit(corp, "\n"))
  corp <- corp[grep("[^\\s]", corp, perl=TRUE)]
  corp <- paste("corpuseye", corp, sep="\t")
  corp <- paste(1:length(corp),"\t", corp, sep="")
  corp <- paste(1:length(corp),"\t", corp, sep="")
  # some more appending needs to be done here
  writeLines(iconv(append("GlobId\tId\tSource\tLeft\tNode\tRight",corp),
                   to="UTF-8", mark=TRUE), 
             outfile, useBytes=TRUE)
}

conc_to_dataset_bibleworks <- function(x, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(x)) {
    con <- file(x[i], encoding = "UTF-8")
    data_lines <- readLines(con, warn = FALSE)
    close(con)    
    data_lines <- data_lines[nchar(data_lines) > 0]
    id_lines <- 1:length(data_lines)
    con <- file(metafiles[i], encoding = "UTF-8")
    metadata_lines <- readLines(con, warn = FALSE)
    close(con)    
    metadata_lines <- metadata_lines[nchar(metadata_lines) > 0]
    source_lines <- rep(x[i], length(data_lines))
    id <- append(id, id_lines)
    data <- append(data, data_lines)
    metadata <- append(metadata, metadata_lines)
    source <- append(source, source_lines)
  }
  d <- data.frame(glob_id = 1:length(data),
                  id = id,
                  source = source,
                  data = data,
                  metadata = metadata)
  d <- d[-nrow(d), ] # in a future update, this should be done more elegantly
  write_dataset(d, outfile)
  invisible(x)
}


conc.to.dataset.bibleworks.prev <- function(datafiles, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(datafiles)) {
    data.lines <- scan(datafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = TRUE,
                       quiet=TRUE)
    id.lines <- 1:length(data.lines)
    metadata.lines <- scan(metafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = TRUE,
                       quiet=TRUE)
    source.lines <- rep(datafiles[i], length(data.lines))
    id <- append(id, id.lines)
    data <- append(data, data.lines)
    metadata <- append(metadata, metadata.lines)
    source <- append(source, source.lines)
  }
  d <- data.frame(GlobId=1:length(data),
                  Id=id,
                  Source=source,
                  Data=data,
                  Metadata=metadata)
  d <- d[-nrow(d),] # in a future update, this should be done more elegantly
  write_dataset(d, outfile)
}

conc.to.dataset.bibleworks2 <- function(datafiles, metafiles, outfile) {
  data <- vector()
  id <- vector()
  metadata <- vector()
  source <- vector()
  for (i in 1:length(datafiles)) {
    data.lines <- scan(datafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = FALSE,
                       quiet=TRUE)
    data.lines <- paste(data.lines, collapse=" \n")
    data.lines <- gsub("([^\n]) \n([^\n])", "\\1 \\2", data.lines, perl=TRUE)
    data.lines <- gsub("^ \n ", "", data.lines, perl=TRUE)
    data.lines <- gsub("\n \n", "\n", data.lines, perl=TRUE)
    data.lines <- unlist(strsplit(data.lines, " \n "))
    id.lines <- 1:length(data.lines)
    metadata.lines <- scan(metafiles[i], what="char", sep="\n", 
                       fileEncoding="UTF-8", blank.lines.skip = FALSE,
                       quiet=TRUE)
    metadata.lines <- paste(metadata.lines, collapse=" \n")
    metadata.lines <- gsub("([^\n]) \n([^\n])", "\\1 \\2", metadata.lines, perl=TRUE)
    metadata.lines <- gsub("^ \n ", "", metadata.lines, perl=TRUE)
    metadata.lines <- gsub("\n \n", "\n", metadata.lines, perl=TRUE)
    metadata.lines <- unlist(strsplit(metadata.lines, " \n "))
    source.lines <- rep(datafiles[i], length(data.lines))
    id <- append(id, id.lines)
    data <- append(data, data.lines)
    metadata <- append(metadata, metadata.lines)
    source <- append(source, source.lines)
  }
  d <- data.frame(GlobId=1:length(data),
                  Id=id,
                  Source=source,
                  Data=data,
                  Metadata=metadata)
  write_dataset(d, outfile)
}

# --
# reads a text file into a character vector
# - if paste.char is NA, each input line is a separate item in
#   the character vector
# - if paste.char is "\n", then the character vector contains
#   a single item, in which all input lines are concatenated,
#   using "\n" as line terminator.
# --
read_txt <- function(file,
                     file_encoding = "UTF-8",
                     paste_char = NA,
                     ...) {
  con <- file(file, encoding = file_encoding)
  lines <- readLines(con, warn = FALSE)
  close(con)
  if (! is.na(paste_char)) {
    lines <- paste0(paste(lines, collapse = paste_char), paste_char)
  }
  lines
}

write_txt <- function(x,
                      file = "",
                      file_encoding = "UTF-8",
                      paste_char = "\n") {
# --
# writes a character vector to a text file, using paste.char
# as line terminator.
# - if paste.char is NA, x is assumed to be a single line
#   that is written to file as is 
# --
  if (! is.character(x) || length(x) == 0) {
    stop("argument 'x' must be a character vector of at least length one")
  }
  if (! is.character(paste_char) || length(paste_char) != 1) {
    stop("argument 'paste_char' must be a character vector of length one")
  }
  x <- paste0(paste(x, collapse = paste_char), paste_char)
  write_txt_utf8(x, file = file)
  invisible(x)
}

write_txt_utf8 <- function(x,
                           file = "") {
  if (! is.character(x) || length(x) != 1) {
    stop("argument 'x' must be a character vector of length one")
  } 
  con <- file(file, "wb")
  writeBin(charToRaw(x), con, endian = "little")
  close(con)
  invisible(x)
}


# read a data frame (simpler, but potentially more robust than read.table)
# (header, quote and comment_char have not been implemented yet)
read_dataset <- function(file,
                         header = TRUE,
                         sep = "\t",
                         quote = "",
                         comment_char = "",
                         file_encoding="UTF-8",
                         stringsAsFactors = default.stringsAsFactors(),
                         ...) {
  con <- file(file, encoding = file_encoding)
  lines <- readLines(con, warn = FALSE)
  close(con)
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
      col_name <- cols[i]
      d[[col_name]] <- unlist(lapply(cells, "[", i))
      d[[col_name]] <- type.convert(d[[col_name]],
                                    as.is = ! stringsAsFactors)
  }
  d
}

# read concordance from file
read_conc <- function(file,
                      header = TRUE,
                      sep = "\t",
                      quote = "",
                      comment_char = "",
                      file_encoding="UTF-8",
                      stringsAsFactors = default.stringsAsFactors(),
                      ...) {
  lines <- read_txt(file, file_encoding = file_encoding) 
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(row.names = 1:length(lines))
  for (i in 1:length(cols)) {
      col_name <- cols[i]
      d[[col_name]] <- unlist(lapply(cells, "[", i))
      if (col_name %in% c("source", "left", "match", "right")) {
        d[[col_name]] <- type.convert(d[[col_name]], as.is = TRUE)
      } else {
        d[[col_name]] <- type.convert(d[[col_name]],
                                      as.is = ! stringsAsFactors) 
      }   
  }
  class(d) <- c("conc", class(d))
  d
}



read.dataset.prev <- function(file, header=TRUE, sep="\t", 
                              quote="", comment.char="", fileEncoding="UTF-8", ...) {
# --
# reads a data frame from the file format that is most often used in mcl
#   #   (header, quote and comment.char have not been implemented yet)
# --
  lines <- scan(file, what="char", sep="\n", 
                fileEncoding=fileEncoding, blank.lines.skip = FALSE,
                quiet=TRUE)
  cols <- unlist(strsplit(lines[1], sep))
  lines <- lines[2:length(lines)]
  cells <- strsplit(lines, sep)
  d <- data.frame(x=1:length(lines)); d$x <- NULL
  for (i in 1:length(cols)) {
      d[[cols[i]]] <- unlist(lapply(cells, "[", i))
      d[[cols[i]]] <- type.convert(d[[cols[i]]])
  }
  return(d)
}

write_dataset <- function(x,
                          file = "",
                          sep = "\t",
                          file_encoding = "UTF-8") {
  if (nrow(x) > 0) {
    lines <- as.character(x[, 1])
    for (i in 2:ncol(x)) {
      lines <- paste(lines, as.character(x[, i]), sep = sep)
    }
    names <- paste(names(x), collapse = sep)
    x <- paste0(paste(append(names, lines), collapse = "\n"), "\n")
    con <- file(file, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)
  }
  invisible(x)
}


# write a concordance
write_conc <- function(x,
                       file = "",
                       sep = "\t",
                       file_encoding = "UTF-8") {
  if (nrow(x) > 0) {
    lines <- as.character(x[, 1])
    for (i in 2:ncol(x)) {
      lines <- paste(lines, as.character(x[, i]), sep = sep)
    }
    names <- paste(names(x), collapse = sep)
    x <- paste0(paste(append(names, lines), collapse = "\n"), "\n")
    con <- file(file, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)
  }
  invisible(x)
}



write.dataset.prev <- function(x, file="", sep="\t", fileEncoding="UTF-8") {
# --
# writes a data frame to the file format that is most often used in mcl
# --
  if (nrow(x) > 0) {
    lines <- as.character(x[,1])
    for (i in 2:ncol(x)) {
      lines <- paste(lines, as.character(x[,i]), sep=sep)
    }
    names <- paste(names(x), collapse=sep)
    writeLines(iconv(append(names, lines), to=fileEncoding, mark=TRUE), 
               file, useBytes=TRUE)  
  }
}

# ---
#  FREQUENCY LISTS (represented as named numeric vectors)
#


freqlist_from_text <- function(x, all_freq_one = FALSE) { 
  t <- table(x)
  if (all_freq_one) {
    retval <- rep(1, length(t))
  } else {
    retval <- as.vector(t)
  }
  names(retval) <- names(t)
  class(retval) <- "freqlist"
  return(retval)
}

addfreqlists <- function(x, y) {
  sum.names <- union(names(x), names(y))
  sum.x <- x[sum.names]; sum.x[is.na(sum.x)] <- 0
  sum.y <- y[sum.names]; sum.y[is.na(sum.y)] <- 0
  sum.freq <- sum.x + sum.y
  names(sum.freq) <- sum.names
  class(sum.freq) <- "freqlist"
  return(sum.freq)
}

# the following function may eventually be turned into
# sort.freqlist(), i.e. a function that can be called
# with sort(x, ...), with x a freqlist
sortfreqlist <- function(x, criterion=c("word", "freq"), decreasing=FALSE) {
  if (criterion[1] == "freq") {
    return(sort(x, decreasing=decreasing))
  } else {
    return(x[sort(names(x), decreasing=decreasing)])
  }
}

writefreqlist <- function(x, file="", sep="\t", fileEncoding="UTF-8") {
# --
# writes a frequency list to file 
# --
  lines <- c("word\tfreq", paste(names(x), x, sep="\t"))
  writeLines(iconv(lines, to=fileEncoding, mark=TRUE), file, useBytes=TRUE)
}

readfreqlist <- function(file, header=TRUE, sep="\t", 
                         quote="", comment.char="", 
                         fileEncoding="UTF-8", ...) {
# --
# reads a frequency list from file 
#   (quote and comment.char have not been implemented yet)
# --
  # --
  con <- file(file, encoding = fileEncoding)
  lines <- readLines(con, warn=FALSE)
  close(con)    
  # --
  #lines <- scan(file, what="char", sep="\n", 
  #              fileEncoding=fileEncoding,
  #              blank.lines.skip = FALSE,
  #              quiet=TRUE, ...)
  if (header) { lines <- lines[2:length(lines)] }
  cells <- strsplit(lines, sep)
  x <- as.numeric(unlist(lapply(cells, "[", 2)))
  names(x) <- unlist(lapply(cells, "[", 1))
  return(x)
}

# the dependency on raw.getfreqlist in this function
# needs to be eliminated

read.wordlist <- function(file, header=TRUE, sep="\t", 
                          quote="", comment.char="", 
                          fileEncoding="UTF-8", ...) {
# --
# reads a word list from file 
#   (quote and comment.char have not been implemented yet)
  return(names(raw.getfreqlist(file, fileEncoding=fileEncoding)))
}



raw.getfreqlist <- function(files,
                            re.split="[.'!?;;,\\s]+",
                            re.ok.line=".*",
                            re.drop.token=NA,
                            to.lower=TRUE,
                            perl=TRUE,
                            blocksize=300,
                            verbose=FALSE,
                            dot.blocksize=10,
                            fileEncoding="latin1") {
  first.pt <- proc.time(); new.pt <- first.pt
  globfreqlist <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = fileEncoding)
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding=fileEncoding, 
      #                 blank.lines.skip=FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- unlist(strsplit(newlines, re.split, perl=perl))
      if (to.lower) { newtokens <- tolower(newtokens) }
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    t <- table(blocktokens)
    blockfreqlist <- as.vector(t); names(blockfreqlist) <- names(t)
    globfreqlist <- addfreqlists(globfreqlist, blockfreqlist)
    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  cat("\n")
  return(globfreqlist)
}

wpl.getfreqlist <- function(files, 
                            re.ok.line=".*",
                             re.token.match=".*",
                             re.token.replace="\\1",
                             re.drop.token=NA,
                             perl=TRUE,
                             blocksize=300,
                             verbose=FALSE,
                             dot.blocksize=10) {
  first.pt <- proc.time(); new.pt <- first.pt
  globfreqlist <- vector()
  i = 1
  while (i <= length(files)) {
    j = 0
    blocktokens <- vector()
    while ((j < blocksize) && ((i+j) <= length(files))) {
      file <- files[i+j]
      # --
      con <- file(file, encoding = "latin1")
      newlines <- readLines(con, warn=FALSE)
      close(con)    
      # --
      #newlines <- scan(file, what="char", sep="\n", 
      #                 fileEncoding="latin1", 
      #                 blank.lines.skip = FALSE, quiet=TRUE)
      newsellines <- newlines[grep(re.ok.line, newlines, perl=perl)]
      newtokens <- gsub(re.token.match, re.token.replace, 
                          newsellines, perl=TRUE)
      if (! is.na(re.drop.token)) {
        newtokens <- newtokens[- grep(re.drop.token, newtokens, perl=perl)]
      }
      blocktokens <- append(blocktokens, newtokens)
      if (verbose && (((i+j)%%dot.blocksize) == 0)) { 
        cat("."); utils::flush.console() 
      }
      j <- j + 1
    }
    t <- table(blocktokens)
    blockfreqlist <- as.vector(t); names(blockfreqlist) <- names(t)
    globfreqlist <- addfreqlists(globfreqlist, blockfreqlist)
    prev.pt <- new.pt; new.pt <- proc.time()
    if (verbose) {
      cat((i+j)-1,"(", new.pt[3]-first.pt[3], "|", 
           new.pt[3]-prev.pt[3], ")\n")
      utils::flush.console()
    }
    i <- i+j
  }
  cat("\n")
  return(globfreqlist)
}


show_dot <- function(show_dots = FALSE) {
  if (show_dots) {
    cat(".")
    utils::flush.console()
  }
  invisible(show_dots)
}

# ------------------------------------------------------------------------------
# keywords and collocation related functions
# ------------------------------------------------------------------------------
# All functions in this section assume the data stem from frequency tables of 
# the form:
#                           target item  other item
#         target context              a           b
#          other context              c           d 
#
# Moreover all functions accept a,b,c,d to be equal sized vectors of length
# 1 or higher. They take a[1],b[1],c[1] and d[1] to stem from frequency table 1, 
# a[2],b[2],c[2] and d[2], to stem from frequency table 2, etc.
# ------------------------------------------------------------------------------

# function that returns association scores on the basis of
# the frequencies a, b, c and d

assoc_scores_abcd <- function(a, b, c, d,
                              measures = NULL,
                              with_variants = FALSE,
                              show_dots = FALSE,
                              p_fisher_2 = FALSE,
                              small_pos = 0.00001) {
  if (is.null(measures)) {
    measures <- c("exp_a", "DP_rows", "RR_rows",
                  "OR", "MS", "PMI", "Dice",
                  "G", "chi2", "t", "fisher")
  }
  a <- zero_plus(a, small_pos = small_pos)
  b <- zero_plus(b, small_pos = small_pos)
  c <- zero_plus(c, small_pos = small_pos)
  d <- zero_plus(d, small_pos = small_pos)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; N <- m + n
  ea <- (m * k)/N; eb <- (m * l)/N 
  ec <- (n * k)/N; ed <- (n * l)/N
  # =========================================================================
  retval <- data.frame(a = a, b = b, c = c, d = d) # remove zeroes from output also
  retval$dir <- ifelse(a/m < c/n, -1, 1) # direction
  # =========================================================================
  # -- exp_a --
  if (is.element("exp_a", measures) ||
      is.element("expected", measures) ||
      is.element("ALL", measures)) {
    retval$exp_a <- ea
    show_dot(show_dots)
  }
  # -- exp_b --
  if (is.element("exp_b", measures) ||
      is.element("expected", measures) ||
      is.element("ALL", measures)) {
    retval$exp_b <- eb
    show_dot(show_dots)
  }
  # -- exp_c --
  if (is.element("exp_c", measures) ||
      is.element("expected", measures) ||
      is.element("ALL", measures)) {
    retval$exp_c <- ec
    show_dot(show_dots)
  }
  # -- exp_d --
  if (is.element("exp_d", measures) ||
      is.element("expected", measures) ||
      is.element("ALL", measures)) {
    retval$exp_d <- ed
    show_dot(show_dots)
  }
  # =========================================================================  
  # -- DP_rows, difference of proportions (aka delta p) --
  if (is.element("DP_rows", measures) ||
      is.element("DP", measures) ||
      is.element("ALL", measures)) {
    retval$DP_rows <- (a / m) - (c / n)
    show_dot(show_dots)
  }
  # -- DP_cols, difference of proportions (aka delta p) --
  if (is.element("DP_cols", measures) ||
      is.element("DP", measures) ||
      is.element("ALL", measures)) {
    retval$DP_cols <- (a / k) - (b / l)
    show_dot(show_dots)
  }
  # -- perc_DIFF_rows, %DIFF --
  if (is.element("perc_DIFF_rows", measures) ||
      is.element("perc_DIFF", measures) ||
      is.element("ALL", measures)) {
    retval$perc_DIFF_rows <- 100 * (a / m - c / n) / (c / n) 
    show_dot(show_dots)
  }  
  # -- perc_DIFF_cols, %DIFF --
  if (is.element("perc_DIFF_cols", measures) ||
      is.element("perc_DIFF", measures) ||
      is.element("ALL", measures)) {
    retval$perc_DIFF_cols <- 100 * (a / k - b / l) / (b / l) 
    show_dot(show_dots)
  }
  # -- DC_rows, difference coefficient --
  if (is.element("DC_rows", measures) ||
      is.element("DC", measures) ||
      is.element("ALL", measures)) {
    retval$DC_rows <- (a/m - c/n) / (a / m + c / n) 
    show_dot(show_dots)
  }
  # -- DC_cols, difference coefficient --
  if (is.element("DC_cols", measures) ||
      is.element("DC", measures) ||
      is.element("ALL", measures)) {
    retval$DC_cols <- (a/k - b/l) / (a / k + b / l)
    show_dot(show_dots)
  }    
  # =========================================================================   
  # -- RR_rows, relative risk --
  if (is.element("RR_rows", measures) ||
      is.element("RR", measures) ||
      is.element("ALL", measures)) {
    retval$RR_rows <- (a/m) / (c/n)
    show_dot(show_dots)
  }
  # -- RR_cols, relative risk --
  if (is.element("RR_cols", measures) ||
      is.element("RR", measures) ||
      is.element("ALL", measures)) {
    retval$RR_cols <- (a/k) / (b/l)
    show_dot(show_dots)
  }
  # -- LR_rows, Hardie's Log Ratio (rows) --
  if (is.element("LR_rows", measures) ||
      is.element("LR", measures) ||
      is.element("ALL", measures)) {
    retval$LR_rows <- log2((a / m) / (c / n))
    show_dot(show_dots)
  }
  # -- LR_cols, Hardie's Log Ratio (cols) --
  if (is.element("LR_cols", measures) ||
      is.element("LR", measures) ||
      is.element("ALL", measures)) {
    retval$LR_cols <- log2((a / k) / (b / l))
    show_dot(show_dots)
  }
  # =========================================================================     
  # -- OR, odds ratio --
  if (is.element("OR", measures) ||
      is.element("ALL", measures)) {
    retval$OR <- (a / b) / (c / d) # also equals (a/c) / (b/d)
    show_dot(show_dots)
  }
  # -- log_OR --
  if (is.element("log_OR", measures) ||
      is.element("ALL", measures)) {
    retval$log_OR <- log((a / b) / (c / d))
    show_dot(show_dots)
  }
  # =========================================================================     
  # -- MS, minimum sensitivity --
  if (is.element("MS", measures) ||
      is.element("ALL", measures)) {
    retval$MS <- pmin(a / m, a / k)
    show_dot(show_dots)
  }
  # -- Jaccard --
  if (is.element("Jaccard", measures) ||
      is.element("ALL", measures)) {
    retval$Jaccard <- a/(m + k -a)
    show_dot(show_dots)
  }  
  # -- Dice --
  if (is.element("Dice", measures) ||
      is.element("ALL", measures)) {
    retval$Dice <- (2 * a) / (m + k)
    show_dot(show_dots)
  }
  # -- logDICE --
  if (is.element("logDice", measures) ||
      is.element("ALL", measures)) {
    retval$logDice <- 14 + log2((2 * a)/(m + k))
    show_dot(show_dots)
  }  
  # =========================================================================       
  # -- Phi (Cramer's V) --
  if (is.element("phi", measures) ||
      is.element("ALL", measures)) {
    retval$phi <- (a * d - b * c) / sqrt((a + b) * (c + d) * (a + c) * (b + d))
    show_dot(show_dots)
  }
  # -- Q (Yule's Q) --
  if (is.element("Q", measures) ||
      is.element("ALL", measures)) {
    retval$Q <- (a * d - b * c) / (a * d + b * c)
    show_dot(show_dots)
  }
  # =========================================================================       
  # -- mu --
  if (is.element("mu", measures) ||
      is.element("ALL", measures)) {
    retval$mu <- a / ea
    show_dot(show_dots)
  }    
  # -- PMI --
  if (is.element("PMI", measures) ||
      is.element("ALL", measures)) {
    retval$PMI <- log2((a / N) / ((k / N) * (m / N))) # is log van mu
    show_dot(show_dots)
  }
  # -- pos.PMI --
  if (is.element("pos_PMI", measures) ||
      is.element("ALL", measures)) {
    retval$pos_PMI <- log2((a / N) / ((k / N) * (m / N)))
    retval$pos_PMI[retval$pos_PMI < 0] <- 0 # remove negs
    show_dot(show_dots)
  }
  # -- PMI2 --
  if (is.element("PMI2", measures) ||
      is.element("ALL", measures)) {
    retval$PMI2 <- log2(((a^2) / N) / ((k / N) * (m / N))) 
    show_dot(show_dots)
  }
  # -- PMI3 --
  if (is.element("PMI3", measures) ||
      is.element("ALL", measures)) {
    retval$PMI3 <- log2(((a^3) / N) / ((k / N) * (m / N))) 
    show_dot(show_dots)
  }  
  # =========================================================================
  # -- chi2 (4-term) --
  if (is.element("chi2", measures) ||
      is.element("ALL", measures)) {
    retval$chi2 <- (a - ea)^2 / ea + (b - eb)^2 / eb + (c - ec)^2 / ec + (d - ed)^2 / ed
    if (with_variants) {
      retval$p_chi2 <- 1 - pchisq(retval$chi2, 1)
      retval$chi2_signed <- retval$chi2 * retval$dir
    }
    show_dot(show_dots)
  }
  # -- chi2 (4-term) with Yates correction --
  if (is.element("chi2_Y", measures) ||
      is.element("ALL", measures)) {
    retval$chi2_Y <- (abs(a - ea) - .5)^2 / ea + (abs(b - eb) - .5)^2 / eb +
                     (abs(c - ec) - .5)^2 / ec + (abs(d - ed) - .5)^2 / ed
    if (with_variants) {
      retval$p_chi2_Y <- 1 - pchisq(retval$chi2_Y, 1)
      retval$chi2_Y_signed <- retval$chi2_Y * retval$dir
    }
    show_dot(show_dots)
  }
  # -- chi2 (2-term) --
  if (is.element("chi2_2T", measures) ||
      is.element("ALL", measures)) {
    retval$chi2_2T <- (a - ea)^2 / ea + (c - ec)^2 / ec
    if (with_variants) {
      retval$p_chi2_2T <- 1 - pchisq(retval$chi2_2T, 1)
      retval$chi2_2T_signed <- retval$chi2_2T * retval$dir
    }
    show_dot(show_dots)
  }
  # -- chi2 (2-term) with Yates correction --
  if (is.element("chi2_2T_Y", measures) ||
      is.element("ALL", measures)) {
    retval$chi2_2T_Y <- (abs(a - ea) - .5)^2 / ea + (abs(c - ec) - .5)^2 / ec
    if (with_variants) {
      retval$p_chi2_2T_Y <- 1 - pchisq(retval$chi2_2T_Y, 1)
      retval$chi2_2T_Y_signed <- retval$chi2_2T_Y * retval$dir
    }
    show_dot(show_dots)
  }
  # =========================================================================
  # note: I call this G, but often in linguistics the name G2 is used
  # =========================================================================
  # -- G (4-term) --
  if (is.element("G", measures) ||
      is.element("ALL", measures)) {
    retval$G <- 2 * (a * log(a / ea) + b * log(b / eb) +
                     c * log(c / ec) + d * log(d / ed))
    if (with_variants) {
      retval$p_G <- 1 - pchisq(retval$G, 1)
      retval$G_signed <- retval$G * retval$dir
    }
    show_dot(show_dots)
  }
  # -- G (2-term) --
  if (is.element("G_2T", measures) ||
      is.element("ALL", measures)) {
    retval$G_2T <- 2 * (a * log(a / ea) + c * log(c / ec))
    if (with_variants) {
      retval$p_G_2T <- 1 - pchisq(retval$G_2T, 1)
      retval$G_2T_signed <- retval$G_2T * retval$dir
    }
    show_dot(show_dots)
  }
  # =========================================================================  
  # -- t --
  if (is.element("t", measures) ||
      is.element("ALL", measures)) {
    retval$t <- ((a / N - k / N * m / N) /
                 sqrt(((a / N) * (1 - a / N)) / N))
    if (with_variants) {
      retval$p_t_1 <- 1 - pt(retval$t, N - 1) # one-sided!
      retval$t_1_as_chisq1 <- p_to_chisq1(retval$p_t_1)
      retval$p_t_2 <- 2 * retval$p_t_1
      sel <- which(retval$t < 0)
      if (length(sel) > 0) {
      # the following line does too much calculation,
      # but attempts to avoid this led to errors
        retval$p_t_2[sel] <- (2 * pt(retval$t, N - 1))[sel]
      }
      retval$t_2_as_chisq1 <- p_to_chisq1(retval$p_t_2)
    }
    show_dot(show_dots)
  }
  # =========================================================================  
  # -- fisher (one-sided!) --
  if (is.element("fisher", measures) ||
      is.element("ALL", measures)) {
    retval$p_fisher_1 <- 1 - phyper(a - 1, m, n, k)
    if (with_variants) {
      retval$fisher_1_as_chisq1 <- p_to_chisq1(retval$p_fisher_1)
      retval$p_fisher_1r <- phyper(a, m, n, k)
      retval$fisher_1r_as_chisq1 <- p_to_chisq1(retval$p_fisher_1r)
    }
    if (p_fisher_2) {
      pf2 <- vector()
      for (i in 1:length(a)) {
        m <- matrix(nrow = 2, byrow = TRUE,
                    c(round(a[i]),round(b[i]),
                      round(c[i]),round(d[i])))
        pf2[i] <- fisher.test(m)$p
      }
      retval$p_fisher_2 <- pf2
      retval$fisher_2_as_chisq1 <- p_to_chisq1(retval$p_fisher_2)
    }
    show_dot(show_dots)
  }
  #
  retval
}

# constructor for an object of the class "cooc_info"
cooc_info <- function(target_freqlist,
                      ref_freqlist,
                      target_n = NA,
                      ref_n = NA) {
  retval <- list(target_freqlist = target_freqlist,
                 ref_freqlist = ref_freqlist,
                 target_n = target_n,
                 ref_n = ref_n)
  if (is.na(retval$target_n)) {
    retval$target_n <- sum(target_freqlist)
  }
  if (is.na(retval$ref_n)) {
    retval$ref_n <- sum(ref_freqlist)
  }
  class(retval) = "cooc_info"
  retval
}


# function that returns association scores on the basis of
# a target frequency list and a reference frequency list;
# it internally calls assoc_scores_abcd()

# is x is of class cooc_info, then y is ignored
# otherwise x and y are assumed to be target.freqlist and
# ref.freqlist respectively.

assoc_scores <- function(x, 
                         y = NULL, 
                         min_freq = 3,
                         measures = NULL,
                         with_variants = FALSE,
                         show_dots = FALSE,
                         p_fisher_2 = FALSE,
                         small_pos = 0.00001) {
  # ---------------------------------------------------------------
  # -- min_freq is minimum frequency in target_freqlist
  #    for inclusion in the output
  # ---------------------------------------------------------------
  if (class(x) != "cooc_info") {
    x <- cooc_info(target_freqlist = x, ref_freqlist = y,
                   target_n = sum(x), ref_n = sum(y))
  }
  if (min_freq == 0) {
    union_names <- union(names(x$target_freqlist),
                         names(x$ref_freqlist))
    x$target_freqlist <- x$target_freqlist[union_names]
    x$target_freqlist[is.na(x$target_freqlist)] <- 0
  } else {
    x$target_freqlist <- x$target_freqlist[x$target_freqlist >= min_freq]
  }
  x$ref_freqlist <- x$ref_freqlist[names(x$target_freqlist)]
  x$ref_freqlist[is.na(x$ref_freqlist)] <- 0
  retval <- assoc_scores_abcd(
              a = x$target_freqlist,
              b = x$target_n - x$target_freqlist,
              c = x$ref_freqlist,
              d = x$ref_n - x$ref_freqlist,
              measures = measures,
              with_variants = with_variants,
              show_dots = show_dots,
              p_fisher_2 = p_fisher_2,
              small_pos = small_pos)
  rownames(retval) <- names(x$target_freqlist)
  retval
}


zero_plus <- function(x, small_pos = 0.00001) {
# auxiliary function that makes all values in numeric vector x strictly positive
# small.pos stands for 'small positive constant'
  x[x <= 0] <- small_pos
  x
}

p_to_chisq1 <- function(p) {
# returns the 'p right quantile' in the chi-square distribution with one df
  return(qchisq(1 - p, 1))
}

chisq1_to_p <- function(x) {
# returns the proportion of the chi-square distribution with one df
# that sits to the right of x
  1 - pchisq(x, 1)
}

# -- All calc.X() functions below in principle have become
# -- redundant, because their functionality has become subsumed by
# -- assoc_scores_abcd();
# -- they are kept nonetheless, because:
#      (i)  I'm still keeping open the option to (redundantly) also
#           export these functions (next to assoc_scores_abcd(),
#           simply because their source code is easier to grasp
#           for readers of MCLM
#      (ii) they can still be useful for debugging assoc_scores_abcd()
# -- However, for the moment, these functions are not exported.

calc.exp.a <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn
  return(ea)
}

calc.min.exp <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(apply(cbind(ea,eb,ec,ed), 1, min))
}

calc.G2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return(2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
}

calc.G2.is.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return(a/m > c/n)
}

calc.p.G2 <- function(a,b,c,d) {
  g2 <- calc.G2(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.pos <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.pos <- function(a,b,c,d) {
  g2 <- calc.G2.pos(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.neg <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[(a/m > c/n)] <- 0.0
  return(result)
}

calc.p.G2.neg <- function(a,b,c,d) {
  g2 <- calc.G2.neg(a,b,c,d)
  return(1 - pchisq(g2, 1))
}

calc.G2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- (2 * (a*log(a/ea) + b*log(b/eb) + c*log(c/ec) + d*log(d/ed)))
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)
}

calc.Chi2 <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  return((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
}

calc.Chi2.signed <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  ea <- (m * k)/mn; eb <- (m * l)/mn 
  ec <- (n * k)/mn; ed <- (n * l)/mn
  result <- ((a - ea)^2/ea + (b - eb)^2/eb + (c - ec)^2/ec + (d - ed)^2/ed)
  result[!(a/m > c/n)] <- - result[!(a/m > c/n)]
  return(result)  
}
calc.p.Chi2 <- function(a,b,c,d) {
  chi2 <- calc.Chi2(a,b,c,d)
  return(1 - pchisq(chi2, 1))
}

calc.PMI <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(log2( (a/mn) / ( (k/mn) * (m/mn) ) ))
}

calc.MS <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  return(min(a/m, a/k))
}

calc.t <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(t)
}

calc.p.t <- function(a,b,c,d) { # 1-sided
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  return(1 - pt(t, mn-1))
}

calc.p.t.two.sided <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c; l <- b + d; mn <- m + n
  t <- ( ( a/mn - k/mn * m/mn )   /
           sqrt( ( (a/mn) * (1 - a/mn) ) / mn ) )
  p <- 1 - pt(t, mn-1) # first assumption: positive deviation
  if (t < 0) {
    p <- pt(t, mn-1)   # correct previous assumption if needed
  }
  p <- 2*p             # two.sided
  return(p)
}


calc.DP <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) - (c/n))
}

calc.RR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d
  return((a/m) / (c/n))
}

calc.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return((a/b) / (c/d))
}

calc.log.OR <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  return(log((a/b) / (c/d)))
}

calc.DICE <- function(a,b,c,d) {
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; k <- a + c
  return( (2*a)/(m+k) )
}

calc.p.fisher <- function(a,b,c,d) {
  # the use of zero_plus() doesn't seem to hinder, so we keep it
  # for the sake of conceptual consistency across the
  # calc.X() functions
  a <- zero_plus(a); b <- zero_plus(b); c <- zero_plus(c); d <- zero_plus(d)
  m <- a + b; n <- c + d; k <- a + c
  return (1 - phyper(a-1,m,n,k))
}

calc.p.fisher.two.sided <- function(a,b,c,d) {
  # no zero_plus() here, to avoid warning about non-integer values
  result <- vector()
  for (i in 1:length(a)) {
    m <- matrix(c(a[i],b[i],c[i],d[i]), nrow=2, byrow=T)
    result[i] <- fisher.test(m)$p
  }
  return(result)
}


# ------------------------------------------------------------------------------
# CORRESPONDENCE ANALYSIS
# ------------------------------------------------------------------------------

ca.row.profiles <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the row profiles matrix R for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat), 1))
}

ca.col.profiles <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the col profiles matrix C for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat), 2))
}

ca.corresp.matrix <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the correspondence matrix P for the two way contingency table tdat
# ------------------------------------------------------------------------------
  return(prop.table(as.matrix(tdat)))
}

ca.row.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the row centroid for the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  return(data.c)  
}

ca.col.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the column centroid for the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  return(data.r)
}

ca.plot.profiles <- function(tdat, side=TRUE, vertical=TRUE) {
# ------------------------------------------------------------------------------
# Plots row and columns profiles for the two way contingency table tdat
# ------------------------------------------------------------------------------
# side=TRUE means plotting the two plots side by side
# side=FALSE means plotting one above the other
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  dir <- c("v","h"); if (!vertical) { dir <- c("h","v") }
  if (side) { par(mfrow=c(1,2)) } else { par(mfrow=c(2,1)) }
  graphics::mosaicplot(tdat, col=T, main="row profiles", dir=dir) 
  graphics::mosaicplot(t(tdat), col=T, main="column profiles",dir=dir) 
  par(mfrow=c(1,1))
}

ca.row.masses <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates masses for the rows in the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses
  return(data.r)
}

ca.col.masses <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates masses for the columns in the two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.c    <- apply(data.P, 2, sum)     # column masses
  return(data.c)
}

h.calc.chisq.dist <- function(x1, x2, m) {
# ------------------------------------------------------------------------------
# [auxiliary function used by row.chisq.dists() and col.chisq.dists()]
# calculated chi-square distance between two profiles x1 and x2, using m
# as the masses by means of which to weight the components in x1 and x2 
# ------------------------------------------------------------------------------
# if tdat is a matrix with absolute frequencies, then row distances between 
# the profiles of row 1 and row 2 are computed as follows:
#   data.P   <- tdat / sum(tdat)           # correspondence matrix  
#   row.prof <- prop.table(data.P, 1)      # row profiles
#   data.c   <- apply(data.P, 2, sum)      # col masses 
#   h.calc.chisq.dist(row.prof[1,], row.prof[2,], data.c)
# ------------------------------------------------------------------------------
# if tdat is a matrix with absolute frequencies, then column distances between 
# the profiles of column 1 and column 2 are computed as follows:
#   data.P   <- tdat / sum(tdat)           # correspondence matrix  
#   col.prof <- prop.table(data.P, 2)      # column profiles
#   data.r   <- apply(data.P, 1, sum)      # row masses 
#   h.calc.chisq.dist(col.prof[,1], col.prof[,2], data.r)
# ------------------------------------------------------------------------------
  result <- 0
  for (i in 1:length(x1)) {
    result <- result + (1/m[i] * (x1[i] - x2[i])^2)
  }
  return(sqrt(result))
}

row.chisq.dists <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all chi-square row distances between the rows in the 
# two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles
  data.c    <- apply(data.P, 2, sum)     # col masses 
  data.r    <- apply(data.P, 1, sum)     # row masses
  m <- outer(data.r, data.r) * 0         # initialize function output
  for (i in 1:nrow(data.P)) {
    for (j in 1:nrow(data.P)) {
      m[i,j] <- h.calc.chisq.dist(row.prof[i,], row.prof[j,], data.c)
    }
  }
  return(m)
}

col.chisq.dists <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all chi-square column distances between the columns in the 
# two way contingency table tdat
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof <- prop.table(data.P, 2)      # col profiles  
  data.c    <- apply(data.P, 2, sum)     # col masses 
  data.r    <- apply(data.P, 1, sum)     # row masses
  m <- outer(data.c, data.c) * 0         # initialize function output 
  for (i in 1:ncol(data.P)) {
    for (j in 1:ncol(data.P)) {
      m[i,j] <- h.calc.chisq.dist(col.prof[,i], col.prof[,j], data.r)
    }
  }
  return(m)
}

inertia.contrib <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates the contributions in all cells of the two way contingency table
# tdat to the overall inertia.
# ------------------------------------------------------------------------------
# This overall inertia is equal to
#      chisq.test(tdat, correct=F)$statistic / sum(tdat)
# and the contributions to the overall inertia are equal to
#      (chisq.test(tdat, correct=F)$residuals^2)/sum(tdat)
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  exp.mass  <- outer(data.r, data.c)     # expected masses (given independence)
  result <- outer(data.r, data.c) * 0
  for (i in 1:nrow(tdat)) {
    for (j in 1:ncol(tdat)) {
       result[i,j] <- 
         ((data.P[i,j] - exp.mass[i,j])^2 / exp.mass[i,j])
    }
  }
  return(result)
}

row.inertias <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- 0
    for (j in 1:ncol(tdat)) {
      result[i] <- result[i] + ((row.prof[i,j] - data.c[j])^2 / data.c[j])
    }
    result[i] <- result[i] * data.r[i]   # multiply by row mass
  }
  return(result)
}

col.inertias <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- 0
    for (i in 1:nrow(tdat)) {
      result[j] <- result[j] + ((col.prof[i,j] - data.r[i])^2 / data.r[i])
    }
    result[j] <- result[j] * data.c[j]   # multiply by column mass
  }
  return(result)
}


row.inertias2 <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- 0
    row.dist.to.centroid <- h.calc.chisq.dist(row.prof[i,], data.c, data.c) 
    row.mass <- data.r[i]
    result[i] <- row.dist.to.centroid^2 * row.mass
  }
  return(result)
}

col.inertias2 <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- 0
    col.dist.to.centroid <- h.calc.chisq.dist(col.prof[,j], data.r, data.r) 
    col.mass <- data.c[j]
    result[j] <- col.dist.to.centroid^2 * col.mass
  }
  return(result)
}

row.dists.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all row inertias of the two way contingency table tdat
# as m times the square of the row distance to the centroid of the rows 
# (i.e. to the average row profile), with m the mass of the row.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  row.prof  <- prop.table(data.P, 1)     # row profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.r * 0                # initialize the function output
  for (i in 1:nrow(tdat)) {
    result[i] <- h.calc.chisq.dist(row.prof[i,], data.c, data.c) 
  }
  return(result)
}

col.dists.centroid <- function(tdat) {
# ------------------------------------------------------------------------------
# Calculates all column inertias of the two way contingency table tdat
# as m times the square of the column distance to the centroid of the columns 
# (i.e. to the average column profile), with m the mass of the column.
# ------------------------------------------------------------------------------
  tdat <- as.matrix(tdat)
  data.P    <- tdat / sum(tdat)          # correspondence matrix
  col.prof  <- prop.table(data.P, 2)     # column profiles  
  data.r    <- apply(data.P, 1, sum)     # row masses (=average col profile)
  data.c    <- apply(data.P, 2, sum)     # col masses (=average row profile)
  result    <- data.c * 0                # initialize the function output
  for (j in 1:ncol(tdat)) {
    result[j] <- h.calc.chisq.dist(col.prof[,j], data.r, data.r) 
  }
  return(result)
}

fitted.row.dists <- function(tdat.ca) {
# ------------------------------------------------------------------------------
# Calculates all euclidean row distances in the first two dimensions of the
# ca solution tdat.ca using the principal row coordinates
# ------------------------------------------------------------------------------
  princ.coord <- tdat.ca$rowcoord %*% diag(tdat.ca$sv)
  x           <- rep(0, length(tdat.ca$rownames))
  names(x)    <- tdat.ca$rownames
  result      <- outer(x, x)                        # initialize function output
  for (i in 1:nrow(princ.coord)) {
    for (j in 1:nrow(princ.coord)) {
      y <- 0
      for (k in 1:2) {
        y = y + (princ.coord[i,k] - princ.coord[j,k])^2
      }
      result[i,j] <- sqrt(y)
    }
  }
  return(result)
}

fitted.col.dists <- function(tdat.ca) {
# ------------------------------------------------------------------------------
# Calculates all euclidean column distances in the first two dimensions of the
# ca solution tdat.ca using the principal column coordinates
# ------------------------------------------------------------------------------
  princ.coord <- tdat.ca$colcoord %*% diag(tdat.ca$sv)
  x           <- rep(0, length(tdat.ca$colnames))
  names(x)    <- tdat.ca$colnames
  result      <- outer(x, x)                        # initialize function output
  for (i in 1:nrow(princ.coord)) {
    for (j in 1:nrow(princ.coord)) {
      y <- 0
      for (k in 1:2) {
        y = y + (princ.coord[i,k] - princ.coord[j,k])^2
      }
      result[i,j] <- sqrt(y)
    }
  }
  return(result)
}

# -----------------------------------------------------------------------------
# "ca" related methods
# -----------------------------------------------------------------------------

row_pcoord <- function(x, ...) {
# ------------------------------------------------------------------------------
# Retrieves row principal coordinates for all dimensions of the
# ca solution object
# ------------------------------------------------------------------------------
  x$rowcoord %*% diag(x$sv)
}

col_pcoord <- function(x, ...) {
# -----------------------------------------------------------------------------
# Retrieves column principal coordinates for all dimensions of the
# ca solution object
# ------------------------------------------------------------------------------
  x$colcoord %*% diag(x$sv)
}

xlim4ca <- function(x, ...) {
# ------------------------------------------------------------------------------
# Assumes object is the output of a ca analysis.
# Returns the xlim value that should be used to reproduce a plot.ca(...)
# plot with the generic plot(...)
# ------------------------------------------------------------------------------
  r_pc <- row_pcoord(x, ...)
  c_pc <- col_pcoord(x, ...)
  range(r_pc[,1], c_pc[,1])
}

ylim4ca <- function(x, ...) {
# ------------------------------------------------------------------------------
# Assumes object is the output of a ca analysis.
# Returns the ylim value that should be used to reproduce a plot.ca(...)
# plot with the generic plot(...)
# ------------------------------------------------------------------------------
  r_pc <- row_pcoord(x, ...)
  c_pc <- col_pcoord(x, ...)
  xlim <- range(r_pc[,1], c_pc[,1])
  ylim <- range(r_pc[,2], c_pc[,2])
  xr <- xlim[2] - xlim[1]
  yr <- ylim[2] - ylim[1]
  r_diff <- xr - yr
  c(ylim[1] - r_diff/2, ylim[2] + r_diff/2) 
}

# -------------------------------------------------------------------------------
# DISTANCE MEASURES
#

cosine_dist <- function(x) {
  (1 - x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

