





# utils -------------------------------------------------------------------
pe <- new.env()
source("x_pattern_extraction_funs.R", local = pe, encoding = "UTF-8")
ut <- new.env()
source("x_util_funs.R", local = ut, encoding = "UTF-8")
cf <- new.env()
source("x_confusion_funs.R", local = cf, encoding = "UTF-8")

# word elements ----------------------------------------------------------------
# `word_sep` defines what must separates words.
word_sep <- "[ ,-]{1,3}"

# `optional_word_sep` defines what may separate words.
optional_word_sep <- "[ ,-]{0,2}"

# `word_suffices` defines what characters words can use
# in inflections. E.g. "gradus" -> "gradusta", etc. The dot `"."` was included
# to allow for abbreviated forms, e.g. "yht.pist." meaning "yhteispistemäärä"
# meaning "total score".
word_suffices <- "[.a-zåäö]*"

# `one_arbitrary_natural_language_word` is an alias of `word_suffices` because
# both in effect define what characters a word is allowed to have (i.e.
# no difference in characters allowed in suffix vs. body of word).
one_arbitrary_natural_language_word <- word_suffices

# `zero_to_three_arbitrary_natural_language_words` allows
# `one_arbitrary_natural_language_word` to repeat zero, one, two, or three 
# times. The word separator is `optional_word_sep`.
zero_to_three_arbitrary_natural_language_words <- paste0(
  "(", one_arbitrary_natural_language_word, optional_word_sep, "){0,3}"
)
stopifnot(
  sub(
    zero_to_three_arbitrary_natural_language_words, 
    "_", 
    "1234 one two three four"
  ) == "1234 _four"
)

# other basic elements ---------------------------------------------------------

# `plus` defines what addition must look like.
plus <- "[ ]?[+][ ]?"

# `equals` defines how the equal sign is used in text.
equals <- "[ ]?[=][ ]?"

# `number_range` defines what ranges of single-digit numbers look like.
number_range <- "[0-9]+[ ]?[-][ ]?[0-9]+"
# `number_range_in_parenthesis` defines single-digit number ranges in 
# parenthesis, e.g. "( 0-9 )".
number_range_in_parenthesis <- paste0("\\([ ]?", number_range, "[ ]?\\)")

# `optional_nondigit_buffer_5` is intended to allow for arbitrary non-digit
# characters between two things (between zero and five).
optional_nondigit_buffer_5 <- "[^0-9]{0,5}"

# `optional_nondigit_buffer_20` is intended to allow for arbitrary non-digit
# characters between two things (between zero and twenty).
optional_nondigit_buffer_20 <- "[^0-9]{0,20}"

# `default_regex_suffix` defines a default ending for regular expression
# used to actually extract the Gleason value and its context 
# (i.e. it is the default RHS context).
default_regex_suffix <- "([^0-9]|$)"

# `arbitrary_expression_in_parenthesis` defines any expression in parenthesis.
arbitrary_expression_in_parenthesis <- "\\([^)]*\\)"

# funs --------------------------------------------------------------------
# Function `optional` turns input `regex` into an "optional regex" by 
# surrounding it with parentheses and appending `?` at the end.
# Even if `regex` has "+" or similar at the end, it becomes optional
# after passing through this function.
optional <- function(regex) {
  paste0("(", regex, ")?")
}
stopifnot(
  !grepl("a+", "b"),
  grepl(optional("a+"), "b")
)

whitelist_sep <- function() {
  # Function `whitelist_sep` always returns regex 
  # `"([ ,-]{0,2}| ja | tai | och | eller )"`.
  "([ ,-]{0,2}| ja | tai | och | eller )"
}

whitelist_to_whitelist_regex <- function(
  whitelist, 
  match_count = "+"
) {
  # Function `whitelist_to_whitelist_regex` turns a list of whitelist 
  # expressions into regex of those expressions. Each expression
  # may be separated by `whitelist_sep()` and repeat to the quantity specified
  # via argument `match_count`.
  stopifnot(
    is.character(whitelist),
    is.character(match_count),
    length(match_count) == 1
  )
  paste0(
    "(",
    "(",
    paste0(whitelist, collapse = "|"), 
    ")",
    whitelist_sep(),
    ")",
    match_count
  )
}
stopifnot(
  sub(
    whitelist_to_whitelist_regex(c("hi", "yo")),
    "",
    "hi yo hi hi yo"
  ) == "",
  sub(
    whitelist_to_whitelist_regex(c("hi", "yo")),
    "",
    "hi yo hiya yoman ho"
  ) == "ya yoman ho"
)


word_whitelist_to_word_whitelist_regex <- function(
  whitelist, 
  match_count = "+"
) {
  # Function `word_whitelist_to_word_whitelist_regex`
  # allows for a set of words to repeat the requested number of times
  # (defined via argument `match_count`)
  # in any order. The words may be separated by anything that matches
  # `whitelist_sep()`. The words are allowed to inflect
  # by appending regex `word_suffices` to each word in `whitelist`.
  stopifnot(
    is.character(whitelist),
    is.character(match_count),
    length(match_count) == 1
  )
  paste0(
    "(", 
    "(",
    paste0(whitelist, collapse = "|"), 
    ")",
    word_suffices, 
    whitelist_sep(),
    ")",
    match_count
  )
}
stopifnot(
  sub(
    word_whitelist_to_word_whitelist_regex(c("hi", "yo")),
    "",
    "hi yo hi hi yo"
  ) == "",
  sub(
    word_whitelist_to_word_whitelist_regex(c("hi", "yo")),
    "",
    "hi yo hiya yoman ho"
  ) == "ho"
)

# Function `multiple_alternative_value_matches` turns a regex capturing a value
# into one that can capture multiple ones occurring in sequence.
multiple_alternative_value_matches <- function(x) {
  stopifnot(
    is.character(x)
  )
  paste0(x, "(( | / |/| tai | ja | eller | och | and | or |[ ]?-[ ]?)", x, ")*")
}
stopifnot(
  stringr::str_extract( 
    "primääri gleason gradus (oikea, vasen): 4 5",
    multiple_alternative_value_matches("[0-9]")
  ) == "4 5",
  stringr::str_extract( 
    "primääri gleason gradus (oikea, vasen): 4 5 sana",
    multiple_alternative_value_matches("[0-9]")
  ) == "4 5"
)

# grade / score values ----------------------------------------------------
# `score_a_or_b` defines what kinds of grades (A and B in A + B = C) are 
# extracted.
score_a_or_b <- "[2-5]"
# `score_c` defines what kinds of scoresums (C in A + B = C) are extracted.
score_c <- "(10|[6-9])"

# whitelists and their derivatives ----------------------------------------
whitelist_scoreword <- c(
  "pist", "tyyp", "luok", "score", "gr", "lk", "kl", "mö", "kuvio",
  "arkkitehtuuri"
)
whitelist_scoreword_regex <- word_whitelist_to_word_whitelist_regex(
  whitelist_scoreword
)

whitelist_gleason_word <- "gl[aei]{1,2}s{1,2}[oi]n[a-zåäö]*"
whitelist_base_optional <- c(
  whitelist_scoreword_regex,
  paste0("n", word_suffices),
  number_range_in_parenthesis,
  arbitrary_expression_in_parenthesis
)
whitelist_base_optional_regex <- whitelist_to_whitelist_regex(
  whitelist_base_optional, match_count = "*"
)
base_gleason_regex <- paste0(
  whitelist_gleason_word,
  optional_word_sep,
  whitelist_base_optional_regex
)
optional_base_gleason_regex <- whitelist_to_whitelist_regex(
  c(whitelist_base_optional, whitelist_gleason_word), match_count = "*"
)

stopifnot(
  sub(base_gleason_regex, "", "gleason lk (1-5) (jotain muuta)") == "",
  sub(base_gleason_regex, "", "gleason gradus (2-5) (gleasongr2)") == ""
)

whitelist_primary <- c(
  "prim[aä]{1,2}", "pääluok", "hufvudkl", "valtaos", "enimm", 
  "tavalli", "vallits", "ylei", "hallits", "vanlig"
)
whitelist_primary_regex <- word_whitelist_to_word_whitelist_regex(whitelist_primary)
optional_or_aggressive_regex <- "([ ]?(/|tai|eller)[ ]?aggres[.a-zåäö]*)?"
whitelist_primary_regex <- paste0(
  whitelist_primary_regex,
  optional_or_aggressive_regex
)
stopifnot(
  sub(whitelist_primary_regex, "", "tavallisin/aggressiivisin") == "",
  sub(whitelist_primary_regex, "", "yleisin / aggressiivisin") == "",
  sub(whitelist_primary_regex, "", "yleisin") == "",
  sub(whitelist_primary_regex, "", "tavallisin") == "",
  sub(whitelist_primary_regex, "", "primääri") == ""
)

whitelist_secondary <- whitelist_primary[-(1:5)]
whitelist_secondary_regex <-  word_whitelist_to_word_whitelist_regex(whitelist_secondary)
whitelist_secondary_regex <- paste0(
  "((2[.])|toise|näst)[.a-zåäö]*[ ]?", whitelist_primary_regex
)
whitelist_secondary_regex <- paste0(
  "(", 
  whitelist_secondary_regex, 
  "|", 
  word_whitelist_to_word_whitelist_regex("sekund"),
  ")",
  optional_or_aggressive_regex
)
stopifnot(
  sub(whitelist_secondary_regex, "", "toiseksi tavallisin/aggressiivisin") == "",
  sub(whitelist_secondary_regex, "", "2. tavallisin/aggressiivisin") == "",
  sub(whitelist_secondary_regex, "", "2. yleisin / aggressiivisin") == "",
  sub(whitelist_secondary_regex, "", "2. yleisin") == "",
  sub(whitelist_secondary_regex, "", "2. yleisin") == "",
  sub(whitelist_secondary_regex, "", "toiseksi tavallisin") == "",
  sub(whitelist_secondary_regex, "", "sekundääri") == ""
)

whitelist_scoresumword <- c(
  "yh",
  "pist", 
  "poäng", 
  "sum", 
  "score", 
  "gradus"
)
whitelist_scoresumword_regex <- word_whitelist_to_word_whitelist_regex(
  whitelist_scoresumword
)
stopifnot(
  grepl(whitelist_scoresumword_regex, "yht.pist."),
  grepl(whitelist_scoresumword_regex, "pistesumma")
)

whitelist_total <- c("eli", "yht", "yhtä kuin", "pist", "sum", "total", "=", "sammanlag")
whitelist_total <- union(whitelist_total, whitelist_scoresumword)
whitelist_total <- sort(union(whitelist_total, whitelist_scoresumword))
whitelist_total_regex <- word_whitelist_to_word_whitelist_regex(
  whitelist_total
)

fcr_pattern_dt <- local({
  
  addition_dt <- local({
    a_plus_b <- paste0(score_a_or_b, plus, score_a_or_b)
    addition_values <- c(
      paste0(a_plus_b, optional_word_sep, optional_base_gleason_regex, whitelist_total_regex, optional_base_gleason_regex, optional_word_sep, score_c),
      paste0(score_c, equals, a_plus_b),
      paste0(score_c, "[ ]?\\(", a_plus_b, "[ ]?\\)"),
      paste0(score_c, "[ ]?\\(", score_a_or_b, ",[ ]?", score_a_or_b, "[ ]?\\)"),
      paste0(a_plus_b, "[ ]?\\(", score_c, "[ ]?\\)"),
      a_plus_b
    )
    addition_values <- paste0("(", addition_values, ")")
    addition_dt <- data.table::data.table(
      pattern_name = c(
        "a + b = c","c = a + b","c (a + b)","c (a, b)","a + b (c)", "a + b"
      ),
      match_type = c(rep("a + b = c", 5L), "a + b"),
      prefix = c(
        rep(paste0(base_gleason_regex, zero_to_three_arbitrary_natural_language_words), 5L), 
        paste0(base_gleason_regex, zero_to_three_arbitrary_natural_language_words)
      ),
      value = addition_values,
      suffix = default_regex_suffix
    )
    addition_dt[]
  })
  
  keyword_dt <- local({
    
    # kw_all_a --------------------------------------------------------------
    whitelist_only_one_kind <- c(
      "yksinom", "ainoas", "pelk", "endast", "enbart"
    )
    whitelist_only_one_kind_regex <- word_whitelist_to_word_whitelist_regex(
      whitelist_only_one_kind, match_count = "+"
    )
    kw_all_a_prefix <- paste0(
      whitelist_only_one_kind_regex,
      optional_word_sep, 
      base_gleason_regex, 
      optional_word_sep
    )
    kw_all_a_value <- score_a_or_b
    kw_all_a_suffix <- default_regex_suffix
    
    # kw_a ---------------------------------------------------------------------
    kw_a_prefix <- paste0(
      whitelist_primary_regex, 
      optional_word_sep, 
      optional_base_gleason_regex, 
      optional_word_sep,
      optional_nondigit_buffer_5
    )
    kw_a_value <- score_a_or_b
    kw_a_suffix <- default_regex_suffix
    
    # kw_b ---------------------------------------------------------------------
    kw_b_prefix <- paste0(
      whitelist_secondary_regex, 
      word_sep, 
      "((tai|/|eller) (pahin|korkein|högst)){0,1}",
      optional_word_sep, 
      optional_base_gleason_regex, 
      optional_word_sep,
      optional_nondigit_buffer_5
    )
    kw_b_value <- score_a_or_b
    kw_b_suffix <- default_regex_suffix
    
    # kw_c ---------------------------------------------------------------------
    whitelist_c_optional <- unique(c(
      whitelist_scoreword
    ))
    whitelist_c_optional <- paste0(
      whitelist_c_optional, word_suffices
    )
    
    addition_guide <- "\\(?[ ]?(a|x)[ ]?[+][ ]?(b|y)[ ]?\\)?"
    stopifnot(
      sub(addition_guide, "", "(a + b)") == "",
      sub(addition_guide, "", "x + y") == ""
    )
    
    whitelist_c_optional <- c(
      whitelist_c_optional, addition_guide, number_range_in_parenthesis,
      arbitrary_expression_in_parenthesis
    )
    whitelist_c_optional_base_regex <- whitelist_to_whitelist_regex(
      whitelist_c_optional, 
      match_count = "*"
    )
    
    kw_c_prefix <- paste0(
      whitelist_c_optional_base_regex,
      base_gleason_regex,
      whitelist_c_optional_base_regex,
      whitelist_scoresumword_regex,
      whitelist_c_optional_base_regex,
      optional_word_sep,
      optional_nondigit_buffer_5
    )
    stopifnot(
      grepl(kw_c_prefix, "gleason yht.pist."),
      grepl(kw_c_prefix, "gleason pistesumma"),
      !grepl(kw_c_prefix, "pelkästään gleason 1"),
      stringr::str_extract(
        "gleason score, summa a+b (2-10) 7", kw_c_prefix
      ) == "gleason score, summa a+b (2-10) "
    )
    
    kw_c_value <- score_c
    kw_c_suffix <- default_regex_suffix
    
    # kw_t ---------------------------------------------------------------------
    whitelist_tertiary <- c(
      "terti", paste0("((3\\.)|(kolmann)|(trädj))", whitelist_secondary)
    )
    whitelist_tertiary_regex <- word_whitelist_to_word_whitelist_regex(
      whitelist_tertiary
    )
    stopifnot(
      !grepl(whitelist_tertiary_regex, "3.tblyleisin gleason-gradus (1-5) 5")
    )
    kw_t_prefix <- paste0(
      whitelist_tertiary_regex, 
      optional_word_sep, 
      optional_base_gleason_regex, 
      optional_word_sep
    )
    kw_t_value <- score_a_or_b
    kw_t_suffix <- default_regex_suffix
    
    # a_kw ---------------------------------------------------------------------
    a_kw_prefix <- paste0(base_gleason_regex, optional_nondigit_buffer_5)
    a_kw_value <- score_a_or_b
    a_kw_suffix <- paste0(optional_word_sep, whitelist_primary_regex)
    
    # b_kw ---------------------------------------------------------------------
    b_kw_prefix <- paste0(base_gleason_regex, optional_nondigit_buffer_5)
    b_kw_value <- score_a_or_b
    b_kw_suffix <- paste0(optional_word_sep, whitelist_secondary_regex)
    
    # c_kw ---------------------------------------------------------------------
    c_kw_prefix <- paste0(
      base_gleason_regex, optional_word_sep, optional_nondigit_buffer_20
    )
    c_kw_value <- score_c
    whitelist_scoresum_suffix <- c(
      "tauti", "syö", "prostata", "karsino{1,2}ma", "eturauhassyö", "adeno"
    )
    whitelist_scoresum_suffix <- union(whitelist_scoresum_suffix, whitelist_scoreword)
    whitelist_scoresum_suffix_regex <- word_whitelist_to_word_whitelist_regex(
      whitelist_scoresum_suffix
    )
    c_kw_suffix <- paste0(word_sep, whitelist_scoresum_suffix_regex)
    
    # keyword pattern dt -------------------------------------------------------
    kw_names <- c("kw_t", "kw_b", "kw_a", "a_kw", "kw_c", "c_kw", "kw_all_a")
    elem_nms <- c("prefix", "value", "suffix")
    this_env <- environment()
    keyword_dt <- data.table::as.data.table(lapply(elem_nms, function(e_nm) {
      unlist(mget(paste0(kw_names, "_", e_nm), envir = this_env))
    }))
    data.table::setnames(keyword_dt, elem_nms)
    keyword_dt <- cbind(
      pattern_name = kw_names,
      match_type = c("t", "b", "a", "a", "c", "c", "kw_all_a"), 
      keyword_dt
    )
    keyword_dt[]
  })
  
  minor_dt <- local({
    minor_dt <- data.table::data.table(
      pattern_name = "sum_near_end",
      match_type = "c",
      prefix = paste0(base_gleason_regex, "[ ]?"),
      value = score_c,
      suffix = "[^0-9]{0,30}$"
    )
    minor_dt[]
  })
  
  
  pattern_dt <- rbind(addition_dt, minor_dt, keyword_dt, use.names = TRUE)
  pattern_dt[, "value" := multiple_alternative_value_matches(value)]
  pattern_dt[, "full_pattern" := paste0(prefix, value, suffix)]
  pattern_dt[]
})



# naive fcr -----------------------------------------------------------

fcr_add_pattern_dt <- fcr_pattern_dt[
  grepl("a.+b", pattern_name),
  ]

# extraction funs ---------------------------------------------------------
rm_false_positives <- function(x) {
  rm <- c(
    paste0(base_gleason_regex, "[ ]?4[ ](ja|tai|or|och|eller)[ ]5"),
    "fokaalinen syöpä \\([^)]*\\)",
    "\\(gleason score 6 tai alle\\)"
  )
  for (pat in rm) {
    x <- gsub(pat, "", x, perl = TRUE)
  }
  x
}

prepare_text <- function(x) {
  x <- rm_false_positives(ut$normalise_text(x))
  x <- gsub("\\([^0-9]+\\)", " ", x)
  gsub("[ ]+", " ", x)
}

parse_gleason_value_string_elements <- function(
  value_strings,
  match_types
) {
  # e.g. value_strings = c("3 + 4 = 7", "7"), match_types = c("a + b = c", "c")
  # -> data.table(a = c(3, NA), b = c(4, NA), c = c(7, 7))
  
  elem_parsers <- list(
    t = NULL,
    kw_all_a = list(
      a = "[0-9]+",
      b = "[0-9]+"
    ),
    `a + b = c` = list(
      a = c("[0-9]+[ ]?[+]", "[0-9]+"), 
      b = c("[+][ ]?[0-9]+", "[0-9]+"),
      c = c("(=[ ]?[0-9]+)|(^[0-9]+[ ]?[=(])", "[0-9]+")
    ),
    `a + b` = list(
      a = c("[0-9]+[ ]?[+]", "[0-9]+"), 
      b = c("[+][ ]?[0-9]+", "[0-9]+")
    ),
    `a...c` = list(a = "[0-9]+", c = "[0-9]+"),
    a = list(a = "[0-9]+"), 
    b = list(b = "[0-9]+"), 
    c = list(c = "[0-9]+")
  )
  
  stopifnot(
    is.character(value_strings),
    
    is.character(match_types),
    match_types %in% names(elem_parsers),
    
    length(value_strings) == length(match_types)
  )
  
  s <- seq_along(value_strings)
  parsed_dt <- data.table::rbindlist(lapply(s, function(i) {
    
    value_string <- value_strings[i]
    value_string <- gsub(whitelist_total_regex, "=", value_string)
    match_type <- match_types[i]
    int_parsers <- elem_parsers[[match_type]]
    if (is.null(int_parsers)) {
      return(NULL)
    }
    dt <- lapply(seq_along(int_parsers), function(j) {
      patterns <- int_parsers[[j]]
      values <- value_string
      for (pattern in patterns) {
        values <- unlist(stringr::str_extract_all(values, pattern))
      }
      as.integer(values)
    })
    if (any(vapply(dt, length, 1L) != length(dt[[1L]]))) {
      browser()
    }
    dt <- data.table::setDT(dt)
    data.table::setnames(dt, names(int_parsers))
    
    cbind(
      data.table::data.table(
        pos = rep(i, nrow(dt)),
        value_string = rep(value_string, nrow(dt)),
        match_type = rep(match_type, nrow(dt))
      ),
      dt
    )
  }), use.names = TRUE, fill = TRUE)
  
  parsed_dt <- rbind(
    data.table::data.table(
      pos = integer(0L), 
      value_string = character(0L),
      match_type = character(0L), 
      a = integer(0L),
      b = integer(0L), 
      c = integer(0L), 
      text_id = integer(0L),
      obs_id = integer(0L),
      text = character(0L), 
      src = character(0L)
    ),
    parsed_dt,
    use.names = TRUE, fill = TRUE
  )
  data.table::setkeyv(parsed_dt, c("text_id", "obs_id"))
  return(parsed_dt[])
}

extract_gleason_scores <- function(
  texts, 
  text_ids = seq_along(texts),
  format = c("standard", "typed")[1L],
  pattern_dt = pattern_dt
) {
  stopifnot(
    is.character(texts),
    is.vector(texts),
    is.integer(text_ids),
    is.vector(text_ids),
    length(format) == 1L,
    format %in% c("standard", "typed")
  )
  # "typed" format produced initially and converted to "standard" if requested.
  extr_dt <- pe$extract_context_affixed_values(
    text = texts,
    pattern_dt = pattern_dt, 
    verbose = TRUE
  )
  extr_dt[
    i = pattern_dt,
    on = "pattern_name",
    j = "match_type" := i.match_type
    ]
  extr_dt[, "text_id" := ..text_ids[pos]]
  extr_dt[, "obs_id" := text_id * 100L + 1:.N, by = "text_id"]
  data.table::setkeyv(extr_dt, c("pos", "obs_id"))
  
  parsed_dt <- parse_gleason_value_string_elements(
    value_strings = extr_dt$value, 
    match_types = extr_dt$match_type
  )
  parsed_dt[, "text_id" := extr_dt[["text_id"]][pos]]
  parsed_dt[, "obs_id" := extr_dt[["obs_id"]][pos]]
  
  parsed_dt <- parsed_dt[
    !(!a %in% c(2:5, NA) | !b %in% c(2:5, NA) | !c %in% c(4:10, NA))
    ]
  
  if (format == "standard") {
    parsed_dt[, c("text", "src") := value_string]
    parsed_dt[, "orig_obs_id" := obs_id]
    parsed_dt[, "obs_id" := text_id * 100L + 1:.N, by = "text_id"]
    data.table::setkeyv(parsed_dt, c("text_id", "obs_id"))
    id_dt <- parsed_dt[, .(orig_obs_id, obs_id)]
    
    parsed_dt <- ut$typed_format_dt_to_standard_format_dt(
      dt = parsed_dt
    )
    keep_col_nms <- c("text_id", "obs_id", "a", "b", "c")
    parsed_dt <- parsed_dt[j = .SD,.SDcols = keep_col_nms]
    parsed_dt[i = id_dt, j = "obs_id" := i.orig_obs_id]
  }
  
  data.table::setkeyv(parsed_dt, c("obs_id", "text_id"))
  data.table::setcolorder(parsed_dt, c("text_id", "obs_id"))
  parsed_dt[]
}

# confusion funs ----------------------------------------------------------
typed_value_strings <- function(match_type, a, b, c) {
  dt <- data.table::setDT(mget(names(formals(typed_value_strings))))
  dt[, "value" := NA_character_]
  dt[match_type == "a" & !is.na(a), "value" := paste0("a = ", a)]
  dt[match_type == "b" & !is.na(b), "value" := paste0("b = ", b)]
  dt[match_type == "c" & !is.na(c), "value" := paste0("c = ", c)]
  dt[match_type == "a + b" & !is.na(a + b), "value" := paste0(a, " + ", b)]
  dt[
    i = match_type == "a + b = c"  & !is.na(a + b + c), 
    j = "value" := paste0(a, " + ", b, " = ", c)
    ]
  dt[["value"]]
}

standard_value_strings <- function(a, b, c) {
  dt <- data.table::setDT(mget(names(formals(standard_value_strings))))
  dt[!is.na(a) & !is.na(b) & !is.na(c), "v" := paste0(a, " + ", b, " = ", c)]
  dt[!is.na(a) & !is.na(b) &  is.na(c), "v" := paste0(a, " + ", b)]
  dt[!is.na(a) &  is.na(b) &  is.na(c), "v" := paste0("a = ", a)]
  dt[ is.na(a) & !is.na(b) &  is.na(c), "v" := paste0("b = ", b)]
  dt[ is.na(a) &  is.na(b) & !is.na(c), "v" := paste0("c = ", c)]
  dt[is.na(v) & !is.na(c), paste0("c = ", c)]
  dt[["v"]]
}

minimum_sufficient_value_strings <- function(a, b, c) {
  dt <- data.table::setDT(mget(names(formals(standard_value_strings))))
  dt[!is.na(a) & !is.na(b), "v" := paste0(a, " + ", b)]
  dt[!is.na(a) &  is.na(b) &  is.na(c), "v" := paste0("a = ", a)]
  dt[ is.na(a) & !is.na(b) &  is.na(c), "v" := paste0("b = ", b)]
  dt[ is.na(a) &  is.na(b) & !is.na(c), "v" := paste0("c = ", c)]
  dt[is.na(v) & !is.na(c), paste0("c = ", c)]
  dt[["v"]]
}

value_string_vectors_by_text_id <- function(
  value_strings, 
  text_ids, 
  all_text_ids = text_ids
) {
  stopifnot(
    is.character(value_strings),
    is.integer(text_ids),
    is.integer(all_text_ids),
    text_ids %in% all_text_ids,
    !duplicated(all_text_ids)
  )
  splitter <- factor(as.character(text_ids), 
                     levels = as.character(all_text_ids))
  vsv_list <- split(value_strings, f = splitter)
  lapply(vsv_list, function(vsv) {
    as.character(vsv[!is.na(vsv)])
  })
}

value_string_groupdiff_dt <- function(estimated, known, text_ids) {
  stopifnot(
    inherits(estimated, "list"),
    inherits(estimated, "list"),
    vapply(estimated, is.character, logical(1L)),
    vapply(known, is.character, logical(1L)),
    length(estimated) == length(known),
    length(estimated) == length(text_ids),
    is.integer(text_ids),
    is.vector(text_ids)
  )
  estimated <- lapply(estimated, sort)
  known <- lapply(known, sort)
  is_identical <- vapply(
    seq_along(estimated), 
    function(i) identical(known[[i]], estimated[[i]]), 
    logical(1)
  )
  diff_dt <- data.table::data.table(
    text_id = text_ids[!is_identical], 
    known = known[!is_identical], 
    estimated = estimated[!is_identical]
  )
  
  base <- rep(list(character(0L)), nrow(diff_dt))
  diff_dt[, "known_not_estimated" := ..base]
  diff_dt[, "estimated_not_known" := ..base]
  if (nrow(diff_dt) > 0L) {
    for (i in 1:nrow(diff_dt)) {
      data.table::set(
        x = diff_dt,
        i = i,
        j = "known_not_estimated",
        value = list(ut$groupdiff(diff_dt[["known"]][[i]], 
                                  diff_dt[["estimated"]][[i]]))
      )
      data.table::set(
        x = diff_dt,
        i = i,
        j = "estimated_not_known",
        value = list(ut$groupdiff(diff_dt[["estimated"]][[i]], 
                                  diff_dt[["known"]][[i]]))
      )
    }
  }
  diff_dt[]
}

# other evaluation funs ---------------------------------------------------
match_number_dt <- function(match_list_list) {
  stopifnot(
    inherits(match_list_list, "list"),
    vapply(match_list_list, inherits, logical(1L), "list"),
    !is.null(names(match_list_list))
  )
  match_number_dt <- data.table::rbindlist(
    lapply(names(match_list_list), function(nm) {
      match_list <- match_list_list[[nm]]
      dt <- data.table::data.table(
        n_matches = vapply(match_list, length, integer(1L))
      )
      dt <- dt[, .N, keyby = "n_matches"]
      dt <- cbind(set = nm, dt)
      dt[]
    })
  )
  match_number_dt[
    j = "n_matches" := data.table::fifelse(
      n_matches >= 5L, ">=5", as.character(n_matches)
    )
    ]
  match_number_dt <- match_number_dt[
    j = .(N = sum(N)), keyby = c("set", "n_matches")
    ]
  match_number_dt[, "p" := round(100L * N / sum(N)), by = "set"]
  match_number_dt[, "n_p" := paste0(N, " (", p, " %)")]
  match_number_dt <- data.table::dcast(
    data = match_number_dt, set ~ n_matches, 
    value.var = "n_p", fill = "0 (0 %)"
  )
  match_number_dt[, "set" := factor(set, levels = names(match_list_list))]
  data.table::setkeyv(match_number_dt, "set")
  match_number_dt[]
}


match_type_number_dt <- function(match_list_list) {
  stopifnot(
    inherits(match_list_list, "list"),
    vapply(match_list_list, inherits, logical(1L), "list"),
    !is.null(names(match_list_list))
  )
  
  results <- lapply(names(match_list_list), function(nm) {
    ml <- match_list_list[[nm]]
    t <- table(unlist(lapply(ml, gsub, pattern = "[0-9]+", replacement = "n")))
    structure(as.integer(t), names = names(t))
  })
  match_names <- sort(unique(unlist(lapply(results, names))))
  results <- lapply(results, function(result) {
    result <- result[match_names]
    names(result) <- match_names
    result[is.na(result)] <- 0L
    result
  })
  match_type_dist_dt <- do.call(rbind, results)
  match_type_dist_dt <- cbind(
    set = names(match_list_list), 
    data.table::as.data.table(match_type_dist_dt)
  )
  match_type_dist_dt[]
}



# example data ------------------------------------------------------------

example_typed_format_dataset <- data.table::data.table(
  text_id = c(1L,1L, 2L,2L, 3L),
  text = rep(c("yleisin gleason oli 4 ja toiseksi tavallisin gleason oli 3",
               "primaari gleason on täten 5 ja toiseksi yleisin gleason on 4",
               "gleason 4 + 4 = 8"), times = c(2, 2, 1)),
  a = c(4L,NA, 5L,NA, 4L),
  b = c(NA,3L, NA,4L, 4L),
  c = c(NA,NA, NA,NA, 8L)
)




