profile_dist <- function(dist_args, type) {
  reactive({
    # Convert reactive values to standard list
    arg_list <- purrr::map(reactiveValuesToList(dist_args), ~ .x())

    # Find profile arg; i subsets overall list, j subsets sub-list
    i_range <- purrr::map_int(arg_list, find_range)
    i <- which(i_range != 0L)
    j <- i_range[[i]]

    # Get profile arg and set to NULL for partial function
    profile_arg   <- arg_list[[i]]
    arg_list[[i]] <- NULL

    # Create partial function using arglist
    calc_dist_profile <- purrr::partial(calc_profile, !!!arg_list, type = type)

    result <- purrr::map_dbl(expand_list(profile_arg, j), calc_dist_profile)


  })
}

expand_list <- function(x, i, n = 110) {
  arg_range <- x[[i]]
  arg_vals  <- seq_profile(arg_range, n = n)

  purrr::map(arg_vals, ~ magrittr::inset2(x, i, .x))
}

find_range <- function(x) {
  i <- which(purrr::map_int(x, NROW) == 2)
  if (NROW(i) == 0L) 0L else i
}

calc_profile <- function(n, vac, inf, symp, test, detect, type) {
  d <- calc_dist(vac, inf, symp, test, detect)

  if (type == "risk") {
    # Undetected cases
    d[inf & !detect, sum(.SD$p)][[1]] * n
  } else {
    # % Reduction in undetected cases
    test$p_asymp_vac   <- 0
    test$p_asymp_unvac <- 0
    d0 <- calc_dist(vac, inf, symp, test0, detect)
    d[inf & !detect, sum(.SD$p)][[1]] / d0[inf & !detect, sum(.SD$p)][[1]]
  }
}

seq_profile <- function(x, n = 110, intervals = c(1, 2, 5, 10)) {
  # Ensure that x is length 2 numeric
  checkmate::assert_numeric(x, len = 2L, finite = TRUE, any.missing = FALSE)

  # Range
  r <- abs(diff(x))

  # Magnitude of intervals
  m <- floor(log10(r)) - 2

  # Scale candidate intervals
  intervals <- intervals * 10^m

  # Must yield `n` intervals or fewer; pick smallest from remaining
  by <- min(intervals[r / intervals <= n])

  # Create sequence; count down if x[[1]] is larger than x[[2]]
  s <- seq(x[[1]], x[[2]], by = if (x[[1]] > x[[2]]) -by else by)

  # Ensure endpoints are always included
  if (x[[2]] != s[[NROW(s)]]) c(s, x[[2]]) else s
}
