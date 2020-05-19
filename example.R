





# funs --------------------------------------------------------------------
library("data.table")
pe <- new.env()
source("x_pattern_extraction_funs.R", local = pe, encoding = "UTF-8")
ut <- new.env()
source("x_util_funs.R", local = ut, encoding = "UTF-8")
ge <- new.env()
source("x_gleason_extraction_funs.R", local = ge, encoding = "UTF-8")
cf <- new.env()
source("x_confusion_funs.R", local = cf, encoding = "UTF-8")


# manually extracted Gleason scores ---------------------------------------
man_dt <- ge$example_typed_format_dataset
stopifnot(
  c("text_id", "text", "a", "b", "c") %in% names(man_dt)
)

# extraction --------------------------------------------------------------
fcr_dt <- man_dt[
  i = !duplicated(text_id),
  j = ge$extract_gleason_scores(texts = ge$prepare_text(text), 
                                text_ids = text_id, 
                                format = "standard",
                                pattern_dt = ge$fcr_pattern_dt)
  ]
fcr_add_dt <- man_dt[
  i = !duplicated(text_id),
  j = ge$extract_gleason_scores(texts = ge$prepare_text(text), 
                                text_ids = text_id, 
                                format = "standard",
                                pattern_dt = ge$fcr_add_pattern_dt)
  ]


# confusion ---------------------------------------------------------------
all_text_ids <- sort(unique(man_dt[["text_id"]]))

man_dt[, "obs_id" := 1:.N]
man_dt[, "match_type" := ut$infer_match_type(a, b, c)]
man_dt <- ut$typed_format_dt_to_standard_format_dt(man_dt)
man_dt[, "implied_c" := data.table::fifelse(!is.na(a) & !is.na(b), a + b, c)]
man_dt <- man_dt[
  (a %in% c(3:5, NA)) & (b %in% c(3:5, NA)) & (c %in% c(6:10, NA)) & (implied_c %in% c(6:10, NA))
  ]
man_dt[, "value" := ge$minimum_sufficient_value_strings(a, b, c)]
man_dt <- man_dt[!is.na(implied_c), ]


fcr_dt[, "implied_c" := data.table::fifelse(!is.na(a) & !is.na(b), a + b, c)]
fcr_dt <- fcr_dt[
  (a %in% c(3:5, NA)) & (b %in% c(3:5, NA)) & (c %in% c(6:10, NA)) & (implied_c %in% c(6:10, NA))
  ]
fcr_dt[, "value" := ge$minimum_sufficient_value_strings(a, b, c)]
fcr_dt <- fcr_dt[!is.na(implied_c), ]


fcr_add_dt[, "implied_c" := data.table::fifelse(!is.na(a) & !is.na(b), a + b, c)]
fcr_add_dt <- fcr_add_dt[
  (a %in% c(3:5, NA)) & (b %in% c(3:5, NA)) & (c %in% c(6:10, NA)) & (implied_c %in% c(6:10, NA))
  ]
fcr_add_dt[, "value" := ge$minimum_sufficient_value_strings(a, b, c)]
fcr_add_dt <- fcr_add_dt[!is.na(implied_c), ]


manual <- ge$value_string_vectors_by_text_id(
  value_strings = man_dt[["value"]], 
  text_ids = man_dt[["text_id"]], 
  all_text_ids = all_text_ids
)
fcr <- ge$value_string_vectors_by_text_id(
  value_strings = fcr_dt[["value"]], 
  text_ids = fcr_dt[["text_id"]], 
  all_text_ids = all_text_ids
)
fcr_add <- ge$value_string_vectors_by_text_id(
  value_strings = fcr_add_dt[["value"]], 
  text_ids = fcr_add_dt[["text_id"]], 
  all_text_ids = all_text_ids
)

match_number_dt <- ge$match_number_dt(
  match_list_list = mget(c("manual", "fcr", "fcr_add"))
)
print(match_number_dt)

match_type_number_dt <- ge$match_type_number_dt(
  match_list_list = mget(c("manual", "fcr", "fcr_add"))
)
print(match_type_number_dt)

diff_dt <- ge$value_string_groupdiff_dt(
  estimated = fcr, known = manual, text_ids = all_text_ids
)
message("found by fcr not found manually")
print(diff_dt[vapply(estimated_not_known, length, integer(1L)) > 0L, .(text_id, known, estimated, estimated_not_known)])
message("found manually not found by fcr")
print(diff_dt[vapply(known_not_estimated, length, integer(1L)) > 0L, .(text_id, known, estimated, known_not_estimated)])

confusion <- ut$bootstrap(
  x = data.table::as.data.table(mget(c("manual", "fcr", "fcr_add"))),
  statistics_fun = function(x, i) {
    stat_nms <- c("sensitivity", "specificity", "accuracy", "F1")
    fcr_add_conf <- cf$hit_confusion(
      estimated = x[["fcr_add"]][i],
      known = x[["manual"]][i]
    )[stat_nms]
    fcr_conf <- cf$hit_confusion(
      estimated = x[["fcr"]][i],
      known = x[["manual"]][i]
    )[stat_nms]
    100L * c(fcr_conf, fcr_add_conf, fcr_conf - fcr_add_conf)
  },
  statistics_id_dt = data.table::CJ(
    set = c("fcr", "fcr_add", "fcr - fcr_add"),
    statistic = c("sensitivity", "specificity", "accuracy", "F1"),
    sorted = FALSE, unique = FALSE
  ), 
  n_bootstrap_samples = 1e2L, 
  n_threads = 4L
)
confusion_r2 <- data.table::copy(confusion)
confusion_r2[, names(confusion_r2) := lapply(.SD, function(col) {
  if (is.numeric(col)) {
    round(col)
  } else {
    col
  }
})]
print(confusion_r2)



