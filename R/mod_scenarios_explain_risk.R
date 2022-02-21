explain_risk <- function(data_risk, n) {

  grp_names <- c("cds", "cdt", "cu", "c")

  color <- paste0("color: ", c(
    "#90a4ae", # cds
    "#64b5f6", # cdt
    "#e57373", # cu
    "#29434e"  # c
  ))

  n_cases <- rev(data_risk[1:3, "n"][[1]])
  n_cases <- c(n_cases, sum(n_cases))

  lbl_p <- fifelse(n_cases == 1, "person", "people")
  lbl_p_tag <- purrr::map2(lbl_p, color, ~ tags$b(style = .y, .x))
  names(lbl_p_tag) <- grp_names

  lbl_c <- fifelse(n_cases == 1, "case", "cases")
  lbl_c_tag <- purrr::map2(lbl_c, color, ~ tags$b(style = .y, .x))
  names(lbl_c_tag) <- grp_names

  n_tag <- purrr::map2(format_number(n_cases), color, ~ tags$b(style = .y, .x))
  names(n_tag) <- grp_names

  pct_chr <- rev(data_risk[1:3, "pct"][[1]])
  pct_chr <- c(pct_chr, format_number(100 * n_cases["c"] / n))
  pct_tag <- purrr::map2(paste0(pct_chr, "%"), color, ~ tags$b(style = .y, .x))
  names(pct_tag) <- grp_names

  grp <- c("symptoms", "testing", "undetected", "cases")
  grp_tag <- purrr::map2(
    grp, color,
    ~ tags$b(style = paste0(.y, "; margin-right: -4px"), .x)
  )
  names(grp_tag) <- grp_names

  verb <- fifelse(n_cases == 1, "is", "are")
  names(verb) <- grp_names

  tagList(
    tags$p(
      "About", n_tag["c"], lbl_p_tag["c"], "in the organization ", verb["c"],
      " expected to bring COVID-19 with them each day.", n_tag["cu"],
      "of these cases ", verb["cu"], " expected to remain", grp_tag["cu"],
      "; that's roughly", pct_tag["cu"], "of the organization."
    ),
    tags$p(
      n_tag["cds"], lbl_c_tag["cds"], verb["cds"],
      "expected to be detected due to", grp_tag["cds"], "; another",
      n_tag["cdt"], lbl_c_tag["cdt"], verb["cdt"],
      "expected to be detected via regular", grp_tag["cdt"], "."
    )
  )
}

format_number <- function(x, digits = 0) {
  mag <- 10^digits
  fifelse(
    x == 0 | x >= 0.5 * mag,
    yes = as.character(round(x, digits)),
    no  = paste0("<", mag)
  )
}

group_cases <- function(inf, detect, symp) {
  fcase(inf & detect & symp, "Symptoms",
        inf & detect, "Testing",
        inf, "Undetected Case",
        default = "Not a Case")
}
