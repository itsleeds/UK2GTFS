#' What a conversion did: the choices made and what they removed
#'
#' @description A conversion makes many decisions the caller never sees in
#'   the output feed: which files were set aside as superseded or duplicate
#'   and in favour of which, which operating periods were cut short, which
#'   files failed to import, where each stop's position came from, and what
#'   later steps such as [gtfs_clean()], [gtfs_deduplicate()] and
#'   [gtfs_deduplicate_copies()] removed. [transxchange2gtfs()] records them
#'   in a `"conversion_report"` attribute on the feed it returns, and those
#'   functions add to it.
#'
#'   `conversion_report()` returns that record; printing it gives a summary.
#'   `write_conversion_report()` saves it as CSV tables with a Markdown
#'   summary, because the attribute does not survive [gtfs_write()].
#'
#' @param gtfs a gtfs object returned by [transxchange2gtfs()] (or merged from
#'   several with [gtfs_merge()], which combines their reports)
#' @return `conversion_report()`: an object of class
#'   `uk2gtfs_conversion_report`, a list of
#'   \describe{
#'     \item{settings}{the arguments the conversion ran with}
#'     \item{files}{one row per input file: `decision` (kept, dropped,
#'       truncated, added), the filter `rule` that decided it, the `reason`,
#'       and the file it gave way to (`by`); see [txc_filter_files()]}
#'     \item{failures}{files that failed to import or to convert}
#'     \item{stop_locations}{where each stop's position came from (see the
#'       `stop_locations` argument of [transxchange2gtfs()])}
#'     \item{steps}{one row per later processing step and what it removed}
#'     \item{copies}{the pairs of copies [gtfs_deduplicate_copies()] found}
#'   }
#'   or `NULL` if the feed carries no report.
#' @examples
#' \dontrun{
#' gtfs <- transxchange2gtfs("region.zip", filter_duplicate_files = TRUE)
#' gtfs <- gtfs_deduplicate_copies(gtfs)
#' conversion_report(gtfs)
#' write_conversion_report(gtfs, "region_report")
#' }
#' @export
conversion_report <- function(gtfs) {
  r <- attr(gtfs, "conversion_report")
  if (is.null(r)) return(NULL)
  structure(r, class = "uk2gtfs_conversion_report")
}

#' @param x a gtfs object carrying a report, or a report from
#'   `conversion_report()`
#' @param dir directory to write to; created if needed
#' @rdname conversion_report
#' @return `write_conversion_report()`: the paths written, invisibly
#' @export
write_conversion_report <- function(x, dir) {
  r <- if (inherits(x, "uk2gtfs_conversion_report")) x else conversion_report(x)
  if (is.null(r)) stop("No conversion report: x was not made by transxchange2gtfs()")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  out <- character(0)
  for (nm in c("settings", "files", "failures", "stop_locations", "steps",
               "copies")) {
    tab <- r[[nm]]
    if (is.null(tab) || !is.data.frame(tab)) next
    p <- file.path(dir, paste0(nm, ".csv"))
    utils::write.csv(tab, p, row.names = FALSE, na = "")
    out <- c(out, p)
  }
  md <- file.path(dir, "README.md")
  writeLines(conversion_report_markdown(r), md, useBytes = TRUE)
  invisible(c(md, out))
}

#' @param ... ignored
#' @rdname conversion_report
#' @export
print.uk2gtfs_conversion_report <- function(x, ...) {
  cat(conversion_report_markdown(x, max_rows = 10), sep = "\n")
  invisible(x)
}


# ---- building the report --------------------------------------------------

#' Start a report
#' @param settings named list of the conversion's settings
#' @noRd
conversion_report_new <- function(settings) {
  val <- vapply(settings, function(v) {
    if (is.null(v)) return("")
    paste(format(v), collapse = ", ")
  }, character(1))
  list(settings = data.frame(setting = names(settings), value = unname(val),
                             stringsAsFactors = FALSE),
       files = NULL, failures = NULL, stop_locations = NULL,
       steps = data.frame(step = character(0), detail = character(0),
                          n = numeric(0), stringsAsFactors = FALSE),
       copies = NULL)
}

#' Append a processing step to a feed's report, if it has one
#'
#' Steps are recorded only on a feed that already carries a report, so
#' functions used on feeds from other sources are unaffected.
#' @param gtfs a gtfs object
#' @param step the function or stage
#' @param detail what was done
#' @param n how many (trips, stops, ...) it affected
#' @noRd
conversion_report_step <- function(gtfs, step, detail, n = NA_real_) {
  r <- attr(gtfs, "conversion_report")
  if (is.null(r)) return(gtfs)
  r$steps <- rbind(r$steps, data.frame(step = step, detail = detail,
                                       n = as.numeric(n),
                                       stringsAsFactors = FALSE))
  attr(gtfs, "conversion_report") <- r
  gtfs
}

#' Combine the reports of feeds being merged
#'
#' Tables are stacked with a `part` column naming the feed they came from
#' (its list name, or its position). NULL if no feed carries a report.
#' @param gtfs_list list of gtfs objects
#' @noRd
conversion_report_combine <- function(gtfs_list) {
  reps <- lapply(gtfs_list, function(g) attr(g, "conversion_report"))
  has <- !vapply(reps, is.null, logical(1))
  if (!any(has)) return(NULL)
  part <- names(gtfs_list)
  if (is.null(part)) part <- as.character(seq_along(gtfs_list))
  part[!nzchar(part)] <- as.character(seq_along(gtfs_list))[!nzchar(part)]
  stack <- function(nm) {
    tabs <- lapply(which(has), function(i) {
      t <- reps[[i]][[nm]]
      if (is.null(t) || !is.data.frame(t) || nrow(t) == 0) return(NULL)
      cbind(part = part[i], t, stringsAsFactors = FALSE)
    })
    tabs <- tabs[!vapply(tabs, is.null, logical(1))]
    if (!length(tabs)) return(NULL)
    as.data.frame(data.table::rbindlist(tabs, fill = TRUE))
  }
  out <- list(settings = stack("settings"), files = stack("files"),
              failures = stack("failures"),
              stop_locations = stack("stop_locations"),
              steps = stack("steps"), copies = stack("copies"))
  if (is.null(out$steps)) {
    out$steps <- data.frame(part = character(0), step = character(0),
                            detail = character(0), n = numeric(0),
                            stringsAsFactors = FALSE)
  }
  out$steps <- rbind(out$steps, data.frame(
    part = "", step = "gtfs_merge",
    detail = paste0("merged ", length(gtfs_list), " feeds, ", sum(has),
                    " with reports"), n = length(gtfs_list),
    stringsAsFactors = FALSE))
  out
}


# ---- the Markdown summary --------------------------------------------------

#' @noRd
conversion_report_markdown <- function(r, max_rows = Inf) {
  md_table <- function(d, n = max_rows) {
    if (is.null(d) || nrow(d) == 0) return("(none)")
    more <- nrow(d) - n
    if (more > 0) d <- d[seq_len(n), , drop = FALSE]
    d[] <- lapply(d, function(v) {
      v <- as.character(v)
      v[is.na(v)] <- ""
      gsub("|", "/", v, fixed = TRUE)
    })
    c(paste0("| ", paste(names(d), collapse = " | "), " |"),
      paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
      apply(d, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |")),
      if (more > 0) paste0("\n... and ", more, " more rows") else NULL)
  }
  out <- c("# UK2GTFS conversion report", "", "## Settings", "",
           md_table(r$settings, Inf), "")

  f <- r$files
  out <- c(out, "## Input files", "")
  if (is.null(f) || nrow(f) == 0) {
    out <- c(out, "(no file decisions recorded)", "")
  } else {
    f$rule[is.na(f$rule)] <- ""
    tab <- stats::aggregate(list(files = f$file),
                            by = list(decision = f$decision, rule = f$rule,
                                      reason = sub(":.*$", "", f$reason)),
                            FUN = length)
    tab <- tab[order(tab$decision, tab$rule), ]
    out <- c(out, paste0(nrow(f), " files: ",
                         paste(names(table(f$decision)), table(f$decision),
                               sep = " ", collapse = ", "), "."), "",
             md_table(tab, Inf), "")
    gone <- f[f$decision != "kept", intersect(
      c("part", "file", "lines", "operator", "decision", "rule", "reason", "by",
        "new_start", "new_end"), names(f)), drop = FALSE]
    if (nrow(gone)) {
      out <- c(out, "### Files dropped or changed", "", md_table(gone), "")
    }
  }

  out <- c(out, "## Failures", "", md_table(r$failures), "")

  sl <- r$stop_locations
  out <- c(out, "## Stop locations", "")
  if (!is.null(sl) && nrow(sl) && "source" %in% names(sl)) {
    tab <- as.data.frame(table(source = sl$source), stringsAsFactors = FALSE)
    names(tab)[2] <- "stops"
    if ("calls" %in% names(sl)) {
      tab$calls <- as.numeric(tapply(sl$calls, sl$source, sum)[tab$source])
    }
    out <- c(out, md_table(tab, Inf), "",
             "Stops with source `none` have no position and are removed, with every call at them, by gtfs_clean().",
             "")
  } else {
    out <- c(out, "(none recorded)", "")
  }

  out <- c(out, "## Later steps", "", md_table(r$steps, Inf), "")
  cp <- r$copies
  if (!is.null(cp) && nrow(cp)) {
    cols <- intersect(c("part", "line", "kept_name", "kept_code", "dropped_name",
                        "dropped_code", "share_dropped", "decided_by",
                        "trips_removed", "trips_trimmed"), names(cp))
    out <- c(out, "## Copies of one service in two files", "",
             md_table(cp[, cols, drop = FALSE]), "")
  }
  out
}
