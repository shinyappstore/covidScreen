explain_benefit <- function(data_benefit) {

  grp_names <- c("u", "d")
  color <- paste0("color: ", c("#e57373", "#64b5f6"))
  margin <- "; margin-right: -4px"

  n <- round(data_benefit$n)
  n_tag <- purrr::map2(n, color, ~ tags$b(style = .y, .x))
  names(n_tag) <- grp_names

  pct <- round(data_benefit$pct)
  pct_tag <- purrr::map2(pct, color, ~ tags$b(style = .y, paste0(.x, "%")))
  names(pct_tag) <- grp_names
  pct_tag_m <- purrr::map2(pct, color, ~ tags$b(style = paste0(.y, margin), paste0(.x, "%")))
  names(pct_tag_m) <- grp_names

  lbl <- fifelse(n == 1, "case", "cases")
  lbl_tag <- purrr::map2(lbl, color, ~ tags$b(style = .y, .x))
  names(lbl_tag) <- grp_names
  lbl_tag_m <- purrr::map2(
    lbl,
    color,
    ~ tags$b(style = paste0(.y, margin), .x)
  )
  names(lbl_tag_m) <- grp_names

  grp_verb <- c("not detect", "detecting")
  grp_v_tag <- purrr::map2(grp_verb, color, ~ tags$b(style = .y, .x))
  names(grp_v_tag) <- grp_names

  grp <- c("undetected", "detected")
  grp_tag_m <- purrr::map2(
    grp,
    color,
    ~ tags$b(style = paste0(.y, margin), .x)
  )
  names(grp_tag_m) <- grp_names

  tagList(
    p(
      "Asymptomatic testing is expected to reduce undetected cases by",
      pct_tag_m["d"], ", ", grp_v_tag["d"], "an additional", n_tag["d"],
      lbl_tag_m["d"], "."
    ),
    p(
      "This leaves an expected", pct_tag["u"], "of asymptomatic cases",
      grp_tag_m["u"], ".",
      " Increase testing frequency to reduce this number further."
    )
  )
}
