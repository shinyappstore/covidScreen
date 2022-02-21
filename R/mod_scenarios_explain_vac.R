explain_vac <- function(data_vac) {

  grp_names <- c("v", "u")

  color <- paste0("color: ", c(
    "#64b5f6", # v
    "#78909c" # u
  ))

  ratio       <- data_vac$benefit[2] / data_vac$benefit[1]
  color_ratio <- if (!is.na(ratio) && ratio >= 1) color[2] else color[1]

  grp     <- tolower(data_vac$group)
  grp_tag <- purrr::map2(grp, color, ~ tags$b(style = .y, .x))
  names(grp_tag) <- grp_names
  if (!is.na(ratio) && ratio >= 1) grp_tag <- rev(grp_tag)

  pct <- data_vac$benefit
  pct_tag <- purrr::map2(
    pct, color,
    ~ tags$b(style = .y, paste0(round(.x, 1), "%"))
  )
  names(pct_tag) <- grp_names

  if (is.na(pct[1]) || is.infinite(pct[1])) {
    return(tags$p(
      "About", pct_tag["u"], "of tests in", grp_tag["u"], "people will detect",
      " a case. There are no", grp_tag["v"], "people in this scenario."
    ))
  } else if (is.na(pct[2]) || is.infinite(pct[2])) {
    return(tags$p(
      "About", pct_tag["v"], "of tests in", grp_tag["v"], "people will detect",
      " a case. There are no", grp_tag["u"], "people in this scenario."
    ))
  }

  if (!is.na(ratio) && ratio < 1) ratio <- 1 / ratio
  ratio_tag   <- tags$b(style = color_ratio, paste0(round(ratio, 1), "x"))

  tagList(
    tags$p(
      "About", pct_tag["u"], "of tests in", grp_tag["u"], "people will detect",
      " a case, compared to about", pct_tag["v"], "of tests in", grp_tag["v"],
      "people."
    ),
    tags$p(
      "This means that testing", grp_tag[1], "people is about",
      ratio_tag, "more cost-effective than testing", grp_tag[2], "people in",
      " this scenario."
    )
  )
}
