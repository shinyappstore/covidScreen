dist_table <- function(
  ...,
  .vac,
  .inf,
  .symp,
  .test,
  .detect,
  keep.rownames = FALSE,
  check.names = FALSE,
  key = NULL,
  stringsAsFactors = FALSE
) {

  dt <- data.table(...,
    keep.rownames = keep.rownames, check.names = check.names,
    key = key, stringsAsFactors = stringsAsFactors)

  as_dist_table(dt, keep.rownames = keep.rownames)
}

as_dist_table <- function(
  x,
  vac,
  inf,
  symp,
  test,
  detect,
  keep.rownames = FALSE,
  ...
) {
  dt <- as.data.table(x, keep.rownames = keep.rownames, ...)

  class(dt) <- c("dist_table", class(dt))

  attributes(dt) <- c(
    attributes(dt),
    vac = .vac, inf = .inf, symp = .symp, test = .test, detect = .detect)

  validate_dist_table(dt)
}

validate_dist_table <- function(x) {
  valid_dist_table <- checkmate::makeAssertCollection()

  checkmate::assert_data_table(x, add = valid_dist_table)
  checkmate::assert_class(x,
    classes = c("dist_table", "data.table", "data.frame"),
    ordered = TRUE, add = valid_dist_table)
  checkmate::assert_set_equal(names(attributes(x)),
    y = c("class", "row.names", "names", ".internal.selfref",
          "vac", "inf", "symp", "test", "detect"),
    add = valid_dist_table)

  checkmate::reportAssertions(valid_dist_table)

  x
}
