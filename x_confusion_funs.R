

# utilities for comparing algorithm versus manual results




#' @param estimated `list` object containg a vector of estimated values for each 
#' subject; see **Details**
#' @param known  `list` object containg a vector of known values for each 
#' subject; see **Details**
#' @details
#' 
#' `estimated` is compared to `known` with regard to subject-specific values
#' whilst taking into the possibility of different numbers of
#' values in `estimated` and `known` for a given subject; e.g. an algorithm may 
#' think that a text mentions
#' something twice, whereas it is actually mentioned thrice. Here's how this
#' works:
#' 
#' - the order of the values for each subject is ignored;
#' - if a subject-value appears in both datasets, it counts towards 
#'   **true positives**;
#' - if a subject-value appears in `estimated` but not in `known`, it counts 
#'   towards **false positives**;
#' - if a subject-value appears in `known` but not in `estimated`, it counts 
#'   towards **false negatives**;
#' - if a subject has zero values in both datasets, this counts (once) towards
#'   **true negatives**
#' 
#' Subjects with no values should be supplied as a length-zero vectors of the
#' same class as the other subjects' values.
#' 

hit_confusion <- function(
  estimated,
  known
) {
  stopifnot(
    inherits(estimated, "list"),
    inherits(known, "list"),
    length(estimated) == length(known)
  )
  lapply(seq_along(estimated), function(i) {
    eval(substitute(stopifnot(
      identical(class(estimated[[1L]]), class(estimated[[i]])),
      identical(class(known[[1L]]), class(known[[i]])),
      identical(class(estimated[[i]]), class(known[[i]])),
      !anyNA(known[[i]]), 
      !anyNA(estimated[[i]])
    ), list(i = i)))
    NULL
  })
  
  n_false_negative <- n_true_negative <- 0L
  n_false_positive <- n_true_positive <- 0L
  for (i in seq_along(estimated)) {
    known_values <- known[[i]]
    estimated_values <- estimated[[i]]
    
    if (length(known_values) == 0L && length(estimated_values) == 0L) {
      n_true_negative <- n_true_negative + 1L
    } else {
      while (length(known_values) > 0) {
        
        m <- match(known_values[1L], estimated_values)
        if (!is.na(m)) {
          n_true_positive <- n_true_positive + 1L
          estimated_values <- estimated_values[-m]
        } else {
          n_false_negative <- n_false_negative + 1L
        }
        
        known_values <- known_values[-1L]
      }
      n_false_positive <- n_false_positive + length(estimated_values)
    }
    
  }
  
  do.call(confusion_statistics, mget(names(formals(confusion_statistics))))
}



confusion_statistics <- function(
  n_true_positive,
  n_false_positive,
  n_true_negative,
  n_false_negative
) {
  
  n_positive <- n_true_positive + n_false_positive
  n_true <- n_true_positive + n_true_negative
  n_negative <- n_true_negative + n_false_negative
  n_false <- n_false_positive + n_false_negative
  
  sensitivity <- n_true_positive / (n_true_positive + n_false_negative)
  recall <- sensitivity
  specificity <- n_true_negative / (n_true_negative + n_false_positive)
  if ((n_true_negative + n_false_positive) == 0L) {
    specificity <- 1.00
  }
  F1 <- 2L * n_true_positive / (
    2L * n_true_positive + n_false_positive + n_false_negative
  )
  accuracy <- (n_true_positive + n_true_negative) / (n_positive + n_negative)
  precision <- n_true_positive / (n_true_positive + n_false_positive)
  if (n_true_positive == 0) {
    precision <- 0.0
  }
  
  out_var_nms <- c(
    "sensitivity", "precision", "specificity", "accuracy", "F1",
    "n_true_positive", "n_false_positive", "n_positive", "n_true",
    "n_true_negative", "n_false_negative", "n_negative", "n_false"
  )
  out <- unlist(mget(out_var_nms))
  is_na <- is.na(out)
  if (any(is_na)) {
    message("NA values in output: ",
            deparse(round(out[is_na], 4)))
    browser()
  }
  return(out)
}



bootstrapped_confusion <- function(
  estimated,
  known,
  confusion_fun, 
  n_bootstrap_samples = 1e3L,
  n_threads = 4L,
  verbose = TRUE
) {
  requireNamespace("boot")
  requireNamespace("data.table")
  
  if (verbose) {
    message("* bootstrapped_confusion: bootstrapping...")
    t <- proc.time()
  }
  b <- boot::boot(
    data = data.table::data.table(estimated, known),
    statistic = function(x, i) {
      confusion_fun(estimated = x[["estimated"]][i],
                    known = x[["known"]][i])
    },
    R = n_bootstrap_samples,
    sim = "ordinary",
    stype = "i",
    ncpus = n_threads
  )
  
  ci <- lapply(seq_along(b[["t0"]]), function(i) {
    utils::tail(as.vector(
      boot::boot.ci(boot.out = b, index = i, type = "perc")[["percent"]]
    ), 2L)
  })
  
  ci[] <- lapply(ci, function(elem) {
    if (is.null(elem)) {
      rep(1.0, 2)
    } else {
      elem
    }
  })
  
  ci <- do.call(what = rbind, args = ci)
  ci <- cbind(b[["t0"]], 
              as.vector(apply(b[["t"]], 2L, mean)), 
              as.vector(apply(b[["t"]], 2L, median)),
              ci)
  rownames(ci) <- names(b[["t0"]])
  colnames(ci) <- c("grand_estimate", "mean", "median", "lo", "hi")
  ci <- round(ci, 2)
  
  if (verbose) {
    message("* bootstrapped_confusion: done; ", data.table::timetaken(t))
    t <- proc.time()
  }
  
  ci
}







