safe_round <- function(x, d = 4) {
  if (is.null(x) || is.na(x)) {
    return("NA")
  }
  return(round(as.numeric(x), d))
}

generateMarkdownReport <- function(models, ai_text) {
  relative_rows <- sapply(models, function(m) {
    paste0(
      "| ", m$name, " | ",
      m$fit$n_parm, " | ",
      safe_round(m$fit$deviance), " | ",
      safe_round(m$fit$aic), " | ",
      safe_round(m$fit$bic), " |"
    )
  })

  relative_table <- paste(
    "| Model | n_parm | Deviance | AIC | BIC |",
    "|-------|--------|----------|-----|-----|",
    paste(relative_rows, collapse = "\n"),
    sep = "\n"
  )

  absolute_rows <- sapply(models, function(m) {
    paste0(
      "| ", m$name, " | ",
      safe_round(m$fit$m2), " | ",
      m$fit$m2_df, " | ",
      safe_round(m$fit$m2_pv), " | ",
      safe_round(m$fit$rmsea2), " | ",
      safe_round(m$fit$srmsr), " |"
    )
  })

  absolute_table <- paste(
    "| Model | M2 | M2_df | M2_pv | RMSEA2 | SRMSR |",
    "|-------|----|-------|-------|--------|-------|",
    paste(absolute_rows, collapse = "\n"),
    sep = "\n"
  )

  reliability_rows <- sapply(models, function(m) {
    paste0(
      "| ", m$name, " | ",
      safe_round(m$reliability$ca_test), " |"
    )
  })

  reliability_table <- paste(
    "| Model | CA Test |",
    "|-------|---------|",
    paste(reliability_rows, collapse = "\n"),
    sep = "\n"
  )

  paste0(
    "## Perbandingan Model

### Indeks Kecocokan Relatif

", relative_table, "

## Interpretasi Kecocokan Absolut

### Indeks M2, RMSEA2, dan SRMSR

", absolute_table, "

## Interpretasi Reliabilitas

### Classification Accuracy

", reliability_table, "

## Analisis Akademik

", ai_text
  )
}
