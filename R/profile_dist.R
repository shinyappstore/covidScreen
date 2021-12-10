profile_dist <- function(dist_args, arg) {
  reactive({
    list_args <- purrr::map(reactiveValuesToList(dist_args), ~ .x())
    profile_arg <- list_args[[arg]]
    list_args[[arg]] <- NULL

    calc_dist_profile <- purrr::partial(calc_undetected, !!!list_args)

    purrr::map_dbl(expand_list(profile_arg), calc_dist_profile)
  })
}

expand_list <- function(x, n = 101) {
  i <- find_range(x)
  arg_range <- x[[i]]
  arg_vals  <- seq(arg_range[[1]], arg_range[[2]], length.out = n)

  purrr::map(arg_vals, ~ magrittr::inset2(x, i, .x))
}

find_range <- function(x) {
  which(purrr::map_int(x, NROW) == 2)
}

calc_undetected <- function(n, vac, inf, symp, test, detect) {
  d <- calc_dist(vac, inf, symp, test, detect)

  d[inf & !detect, sum(.SD$p)][[1]] * n
}
