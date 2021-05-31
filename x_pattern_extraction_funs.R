
# utility functions to enable regex-based information extraction
# of key-value pairs.


#' @param x any R object
#' @param template any R object; `x` must have identical length and class(es)
#' as `template`, but not contents
stopifnot_is_like <- function(x, template) {
  EXP_LEN <- length(template)
  EXP_CLASS <- class(template)
  
  PF <- parent.frame(1)
  
  x_name <- substitute(x)
  eval(substitute(stopifnot(
    length(x) == EXP_LEN,
    identical(class(x), EXP_CLASS)
  ), list(EXP_LEN = EXP_LEN, EXP_CLASS = EXP_CLASS, x = x_name)),
  envir = PF)
}




#' @param text character string vector of texts
#' @param key.pattern pattern used to detect keys in text
#' @param value.pattern pattern used to detect values of keys in text
#' @details
#' This function attempts to find key-value pairs. The keys and values are 
#' searched separately, and are paired up after searching, if and only if the
#' same number of keys and values were found in the given element of `text`.
#' 
#' This has the advantage that the keys and values may be ordered whichever way
#' and still be paired up correctly. False positives only occur an extra
#' match is found for both the key and the value.
#' 
#' If the number of found keys and values differ for a given element of `x`,
#' `NULL` is returned in that case (the output of this function is a list of 
#' length `length(x)`, where each element of the list is itself a list with 
#' elements `keys` and `values`).
#' 
#' @examples
#' 
#' # list(list(keys = "KEY", values = "VALUE"))
#' extract_ordered_key_value_pairs("... KEY ... VALUE ... ", "KEY", "VALUE") 
#' 
#' # list(list(keys = "KEY", values = "VALUE"))
#' extract_ordered_key_value_pairs("... VALUE ... KEY ... ", "KEY", "VALUE") 
#' 
#' # list(list(keys = NULL, values = NULL))
#' extract_ordered_key_value_pairs("KEY ... VALUE VALUE", "KEY", "VALUE") 
#' 
extract_ordered_key_value_pairs <- function(
  text, 
  key.pattern = "((gleason)|(gleeson))",
  value.pattern = "(([1-6].{0,1}\\+.{0,1}[1-6])|(([^0-9\\+]{0,2})(\\d{1,2})([^0-9\\+]{0,2})))"
) {
  
  ## assumes only that the order of detected values is the same as 
  ## detected keys.
  
  
  stopifnot_is_like(value.pattern, character(1))
  stopifnot_is_like(key.pattern, character(1))
  stopifnot(
    is.character(text), length(text) > 0
  )
  requireNamespace("stringr")
  
  keys <- stringr::str_extract_all(text, pattern = key.pattern)
  n_keys <- vapply(keys, length, integer(1))
  values <- stringr::str_extract_all(text, pattern = value.pattern)
  n_values <- vapply(values, length, integer(1))
  
  same_n <- n_keys == n_values
  
  
  message("* extract_ordered_key_value_pairs: ",
          "There were ", sum(!same_n), " cases where n_keys != n_values. ", 
          "Returning NULL for those.")
  
  keys[!same_n] <- values[!same_n] <- list(NULL)
  
  
  list(keys = keys, values = values)
}





#' @param text character string vector of texts
#' @param key.pattern pattern used to detect keys in text
#' @param context.pattern pattern used to determine the allowed substring
#' between key and value
#' @param value.pattern pattern used to detect values of keys in text
#' @details
#' This function attempts to find key-value pairs where each key occurs before
#' it's own value in the text.
#' 
#' This approach has the advantage that, when the number of matched keys
#' and values in the text differs, one can disambiguate which value belongs to
#' which key (assuming the key-first-value-later order is correct).
#' 
#' If a key is found without a value or vice versa, the key/value is not 
#' retained. If an element
#' of `x` has not matched pairs, `NULL` is returned (the output of this function 
#' is a list of 
#' length `length(x)`, where each element of the list is itself a list with 
#' elements `keys` and `values`).
#' 
#' @examples
#' 
#' # list(list(keys = "KEY", values = "VALUE"))
#' extract_values_following_keys("... KEY ... VALUE ... ", "KEY", ".{1,30}", "VALUE") 
#' 
#' # list(list(keys = NULL, values = NULL))
#' extract_values_following_keys("... VALUE ... KEY ... ", "KEY", ".{1,30}", "VALUE") 
#' 
#' # list(list(keys = "KEY", values = "VALUE"))
#' extract_values_following_keys("KEY ... VALUE VALUE", "KEY", ".{1,30}", "VALUE") ") 
#' 
#' # list(list(keys = "KEY", values = "VALUE"))
#' extract_values_following_keys("KEY ... KEY ... VALUE ...", "KEY", ".{1,30}", "VALUE") 
#' 
extract_values_following_keys <- function(
  text = "gleason was 1 + 1", 
  key.pattern = "gleason",
  context.pattern = ".{1,30}",
  value.pattern = "\\d \\+ \\d"
) {
  ## assumes values follow keys
  requireNamespace("stringr")
  pattern <- paste0("(", key.pattern, ")(", context.pattern, ")(", 
                    value.pattern, ")")
  
  matches <- stringr::str_extract_all(text, pattern)
  
  n_matches <- vapply(matches, length, integer(1))
  n_no_matches <- sum(n_matches == 0)
  message("* extract_values_following_keys: ",
          "There were ", n_no_matches, " cases without any matches. ",
          "Returning NULL for those.")
  has_matches <- n_matches > 0
  matches[!has_matches] <- list(NULL)
  
  keys <- values <- matches
  keys[has_matches] <- lapply(matches[has_matches], function(string_vec) {
    stringr::str_extract(string_vec, pattern = key.pattern)
  })
  
  values[has_matches] <- lapply(matches[has_matches], function(string_vec) {
    stringr::str_extract(string_vec, pattern = value.pattern)
  })
  
  list(keys = keys, values = values)
}




#' @details
#' First calls extract_ordered_key_value_pairs, and where no pairs were 
#' extracted, extract_values_following_keys is called.
extract_all_key_value_pairs <- function(
  text = "gleason was 1 + 1", 
  key.pattern = "gleason",
  context.pattern = ".{1,30}",
  value.pattern = "\\d \\+ \\d"
) {
  message("* extract_all_key_value_pairs: ",
          "trying with 'extract_ordered_key_value_pairs'...")
  kv <- extract_ordered_key_value_pairs(
    text = text, key.pattern = key.pattern, value.pattern = value.pattern
  )
  has_no_match <- vapply(kv$values, length, integer(1)) == 0
  if (any(has_no_match)) {
    message("* extract_all_key_value_pairs: ",
            "trying with 'extract_values_following_keys'...")
    kv_fol <- extract_values_following_keys(
      text = text[has_no_match], 
      key.pattern = key.pattern, 
      value.pattern = value.pattern, 
      context.pattern = context.pattern
    )
    kv$values[has_no_match] <- kv_fol$values
    kv$keys[has_no_match] <- kv_fol$keys
  }
  return(kv)
}



#' @title Pattern Extraction
#' @description
#' Extract substrings (values) from text with context prefixes and suffixes.
#' @param text `[character]` (mandatory, no default)
#' 
#' text to extract values from
#' @param pattern_dt `[data.table]` (mandatory, no default)
#' 
#' a `data.table` with columns
#' 
#' - `pattern_name`: one name per pattern; these will be columns in output
#' - `prefix`: context prefix for value
#' - `value`: the value itself
#' - `suffix`: context suffix for value
#' @param mask_length `[integer]` (mandatory, default `53L`)
#' 
#' each time a match is found in an element of `text`, the match is replaced
#' by a mask to avoid matching the same thing multiple times; the mask looks
#' like e.g. `"%%%001%%%"` for the first match with `mask_length = 9L`;
#' `mask_length` must be larger than or equal to 5, and preferably something
#' like 53 for safety; additionally, `mask_length - 3L` must be divisible by 2
#' @param verbose `[logical]` (mandatory, default `TRUE`)
#' 
#' if `TRUE`, this function explains what it is doing at each phase
#' @details
#' If a value is found n times in a `text` element, there will be n values
#' for that element in output (all matches are extracted for each pattern).
#' 
#' The patterns are processed in the given order. Hence you may have even e.g.
#' a special case of another pattern and process that first to ensure more
#' exact matching. After extracting a pattern (defined by `prefix`, `value`,
#' and `suffix` pasted together), that match is replaced in `text` with
#' `mask` to ensure that once a match is found by a pattern,
#' consequent patterns cannot match to the same part of the text.
#' 
#' If an element of `text` has no matches in any of the given patterns,
#' there will be zero rows in output for that element.
#' 
#' @return
#' `data.table` with columns
#' 
#' - `pos`: integer column where each element identifies which element of `text` 
#'   the value was extracted from
#' - `pattern_name`: character string column identifying the pattern used
#'   to extract the corresponding value
#' - `value`: character string column of extracted values (with any context
#'   stripped)
#' 
#' the rows of the `data.table` are in the same order as `text` and, within
#' an element of `text`, in the order of appearance.
extract_context_affixed_values <- function(
  text,
  pattern_dt,
  mask_length = 53L,
  verbose = TRUE
) {
  t_start <- proc.time()
  requireNamespace("data.table")
  requireNamespace("stringr")
  stopifnot(
    is.character(text),
    
    data.table::is.data.table(pattern_dt),
    c("pattern_name", "prefix", "value", "suffix") %in% names(pattern_dt),
    !"id" %in% pattern_dt[["pattern_name"]],
    
    is.integer(mask_length), length(mask_length) == 1L,
    mask_length >= 5L, (mask_length - 3L) %% 2L == 0L,
    
    is.logical(verbose), length(verbose) == 1L, verbose %in% c(TRUE, FALSE)
  )
  
  full_patterns <- pattern_dt[, paste0(prefix, value, suffix)]
  if (verbose) {
    message("* extract_context_affixed_values: starting processing text elems")
  }
  mask_buffer <- paste0(rep("%", (mask_length - 3L) / 2L), collapse = "")
  
  extr_dt <- data.table::rbindlist(lapply(seq_along(text), function(i) {
    text_elem <- text[i]
    if (is.na(text_elem)) {
      dt <- data.table::setDT(list(
        pos = integer(0L),
        pattern_name = character(0L),
        value = character(0L)
      ))
      return(dt)
    }
    extracted <- pattern_names <- character(0L)
    for (j in 1:nrow(pattern_dt)) {
      pattern_name <- pattern_dt[["pattern_name"]][j]
      prefix <- pattern_dt[["prefix"]][j]
      suffix <- pattern_dt[["suffix"]][j]
      pattern <- full_patterns[j]
      while (stringr::str_detect(text_elem, pattern)) {
        if (grepl("^inspect this", text_elem)) {
          browser()
        }
        newly_extracted <- stringr::str_extract(text_elem, pattern)
        newly_extracted <- sub(
          paste0("^", prefix), "", newly_extracted, perl = TRUE
        )
        newly_extracted <- sub(
          paste0(suffix, "$"), "", newly_extracted, perl = TRUE
        )
        extracted <- c(extracted, newly_extracted)
        pattern_names <- c(pattern_names, pattern_name)
        if (length(extracted) > 999L) {
          stop("Looks like you had at least 1000 matches in string, ", i, 
               " which is not supported")
        }
        mask_num <- formatC(length(extracted), digits = 2L, flag = "0")
        mask <- paste0("_", mask_buffer, mask_num, mask_buffer, "_")
        text_elem <- sub(pattern, mask, text_elem, perl = TRUE)
      }
    }
    
    if (length(extracted) == 0L) {
      match_order <- 0L
    } else {
      match_order <- stringr::str_extract_all(
        text_elem,
        paste0("\\Q", mask_buffer, "\\E", "[0-9]{3}", "\\Q", mask_buffer, "\\E")
      )[[1L]]
      match_order <- as.integer(gsub("%", "", match_order, fixed = TRUE))
    }
    
    dt <- data.table::setDT(list(
      pos = rep(i, length(extracted)),
      pattern_name = pattern_names,
      value = extracted
    ))
    dt[match_order, ]
  }))
  
  if (verbose) {
    message("* extract_context_affixed_values: done processing everything; ", 
            data.table::timetaken(t_start))
  }
  
  extr_dt[]
}




# tests -------------------------------------------------------------------


suppressMessages(local({
  
  texts <- c(
    "gleason oli 1+2 mutta toisaalta 2+1 saattoi olla gleason",
    "gleason tuntematon",
    "gleason 1+1=2 gleason",
    "gleason oli ensin 1+1 mutta sitten 1+2",
    "gleason ja gleason olivat 5+2 ja 2+5",
    "havaittiin 3+4 ja 4+3 muuttujille gleason ja gleason"
  )
  
  ord_results <- extract_ordered_key_value_pairs(
    text = texts, 
    key.pattern = "(gleason)|(gleeson)", 
    value.pattern = "(\\d\\+\\d)"
  )
  
  ord_expected_values <- list(
    c("1+2", "2+1"),
    NULL,
    NULL,
    NULL,
    c("5+2","2+5"),
    c("3+4", "4+3")
  )
  
  fol_results <- extract_values_following_keys(
    text = texts, 
    key.pattern = "(gleason)|(gleeson)", 
    context.pattern = ".{1,30}",
    value.pattern = "(\\d\\+\\d)"
  )
  
  fol_expected_values <- list(
    "1+2",
    NULL,
    "1+1",
    "1+1",
    "5+2",
    NULL
  )
  
  stopifnot(identical(
    ord_results$values, ord_expected_values
  ))
  stopifnot(identical(
    fol_results$values, fol_expected_values
  ))
  
  
  
  # UNIT TEST: extract_context_affixed_values: only one match due to overwriting
  pat_dt <- data.table::data.table(
    pattern_name = c("secondary", "primary"),
    prefix = c("second most prevalent grade ", "most prevalent grade "),
    value = c("[3-5]", "[3-5]"),
    suffix = c("", "")
  )
  result_dt <- extract_context_affixed_values(
    text = "second most prevalent grade 3", 
    pat_dt,
    verbose = FALSE
  )
  stopifnot(
    identical(result_dt[["value"]], "3")
  )
  
  # UNIT TEST: extract_context_affixed_values: illustrative unit tests
  pat_dt <- data.table::data.table(
    pattern_name = c("a", "b", "c"), 
    prefix = c("primary grade[ ]*", "secondary grade[ ]*", "gleason score[ ]*"), 
    value = c("[3-5]", "[3-5]", "[6-90]"), 
    suffix = c("", "", "")
  )
  texts <- c(
    "primary grade 3",
    "secondary grade 4", 
    "primary grade 5 secondary grade 3", 
    "secondary grade 3 and primary grade 5, therefore gleason score 8",
    "primary grade 5 gleason score 8"
  )
  result_dt <- extract_context_affixed_values(
    text = texts, 
    pat_dt,
    verbose = FALSE
  )
  stopifnot(
    nrow(result_dt) == 9L,
    result_dt$value[result_dt$pos == 1L] == c("3"),
    result_dt$value[result_dt$pos == 2L] == c("4"),
    result_dt$value[result_dt$pos == 3L] == c("5", "3"),
    result_dt$value[result_dt$pos == 4L] == c("3", "5", "8"),
    result_dt$value[result_dt$pos == 5L] == c("5", "8")
  )
  
  
}))



















