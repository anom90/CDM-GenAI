library(openxlsx)
library(GDINA)

fit_cdm <- function(data, q_matrix, model, mono.constraint = FALSE) {
  if (!is.numeric(as.matrix(data))) {
    data <- data[, sapply(data, is.numeric), drop = FALSE]
  }
  
  # Untuk Q-matrix: pertahankan hanya kolom biner (0/1)
  # menyaring kolom nomor butir yang mungkin ikut terbaca
  is_binary_col <- function(x) is.numeric(x) && all(na.omit(x) %in% c(0, 1))
  q_matrix <- q_matrix[, sapply(as.data.frame(q_matrix), is_binary_col), drop = FALSE]

  # Preserve actual column names before matrix conversion
  data_col_names <- colnames(data)
  q_col_names    <- colnames(q_matrix)

  data      <- as.matrix(data)
  q_matrix  <- as.matrix(q_matrix)

  # Restore names (matrix conversion can drop dimnames in edge cases)
  colnames(data)     <- data_col_names
  colnames(q_matrix) <- q_col_names

  if (!is.numeric(data))     stop("Data harus numerik (0/1)")
  if (!is.numeric(q_matrix)) stop("Q-matrix harus numerik (0/1)")
  if (ncol(data) != nrow(q_matrix)) stop("Jumlah item dan baris Q-matrix tidak cocok")

  models <- GDINA::GDINA(dat = data, Q = q_matrix, model = model,
                         mono.constraint = mono.constraint)

  s  <- GDINA::modelfit(models)
  ca <- GDINA::CA(models)

  # ── Profil: mastery probability & latent class ────────────────────────────
  profil_data <- tryCatch({
    # Attribute prevalence: structural model parameter (Level1 = proportion masters)
    prev         <- GDINA::extract(models, what = "prevalence")
    mastery_prob <- prev$all[, 2]
    names(mastery_prob) <- q_col_names

    # Helper: strip multimodes column and coerce to plain numeric matrix
    strip_to_matrix <- function(raw) {
      m <- as.matrix(raw[, seq_len(length(q_col_names)), drop = FALSE])
      storage.mode(m) <- "integer"
      colnames(m) <- q_col_names
      m
    }

    # Per-person binary profiles (0/1)
    # MAP and MLE return data frames with a trailing 'multimodes' column
    # EAP returns a plain matrix with no extra column
    map_mat <- strip_to_matrix(GDINA::personparm(models, what = "MAP"))
    mle_mat <- strip_to_matrix(GDINA::personparm(models, what = "MLE"))
    eap_raw <- GDINA::personparm(models, what = "EAP")
    eap_mat <- {
      m <- as.matrix(eap_raw[, seq_len(length(q_col_names)), drop = FALSE])
      storage.mode(m) <- "integer"
      colnames(m) <- q_col_names
      m
    }

    # Proporsi penguasaan per atribut (colMeans of binary 0/1 matrix)
    mastery_prop_eap <- colMeans(eap_mat)
    mastery_prop_map <- colMeans(map_mat)
    mastery_prop_mle <- colMeans(mle_mat)

    # Latent class from MAP patterns
    patterns      <- apply(map_mat, 1, paste, collapse = "")
    pattern_table <- table(patterns)
    n_persons     <- nrow(map_mat)

    latent_class <- lapply(
      names(sort(pattern_table, decreasing = TRUE)),
      function(pat) {
        bits     <- as.integer(strsplit(pat, "")[[1]])
        attr_val <- setNames(as.list(bits), q_col_names)
        list(
          pattern    = pat,
          attributes = attr_val,
          proportion = round(as.numeric(pattern_table[pat]) / n_persons, 4),
          count      = as.integer(pattern_table[pat])
        )
      }
    )

    # Per-person lists for Profil Individu table (integer 0/1)
    to_person_list_int <- function(mat) {
      lapply(seq_len(nrow(mat)), function(i)
        setNames(as.list(as.integer(mat[i, ])), q_col_names))
    }

    mastery_eap_ind <- to_person_list_int(eap_mat)
    mastery_map_ind <- to_person_list_int(map_mat)
    mastery_mle_ind <- to_person_list_int(mle_mat)

    # mp for radar chart only (continuous marginal probability)
    mp_mat     <- GDINA::personparm(models, what = "mp")
    colnames(mp_mat) <- q_col_names
    mastery_mp <- lapply(seq_len(nrow(mp_mat)), function(i)
      setNames(as.list(round(as.numeric(mp_mat[i, ]), 4)), q_col_names))

    # Total score per respondent
    dat_mat      <- GDINA::extract(models, what = "dat")
    total_scores <- as.list(as.integer(rowSums(dat_mat, na.rm = TRUE)))

    round_named <- function(x) as.list(setNames(round(as.numeric(x), 4), q_col_names))

    list(
      mastery_prob        = round_named(mastery_prob),
      mastery_prop_eap    = round_named(mastery_prop_eap),
      mastery_prop_map    = round_named(mastery_prop_map),
      mastery_prop_mle    = round_named(mastery_prop_mle),
      mastery_eap_ind     = mastery_eap_ind,
      mastery_map_ind     = mastery_map_ind,
      mastery_mle_ind     = mastery_mle_ind,
      mastery_mp          = mastery_mp,
      total_scores        = total_scores,
      patterns_per_person = as.list(patterns),
      latent_class        = latent_class
    )
  }, error = function(e) {
    list(mastery_prob = list(),
         mastery_prop_eap = list(), mastery_prop_map = list(), mastery_prop_mle = list(),
         mastery_eap_ind = list(), mastery_map_ind = list(), mastery_mle_ind = list(),
         mastery_mp = list(), total_scores = list(), patterns_per_person = list(),
         latent_class = list())
  })

  item_fit_res <- tryCatch({
    itf <- GDINA::itemfit(models)
    mat <- itf$max.itemlevel.fit
    item_names <- rownames(mat)
    if (is.null(item_names)) {
      item_names <- colnames(GDINA::extract(models, "dat"))
    }
    
    lapply(seq_len(nrow(mat)), function(i) {
      list(
        item = item_names[i],
        z_prop = as.numeric(mat[i, "z.prop"]),
        pvalue_z_prop = as.numeric(mat[i, "pvalue[z.prop]"]),
        max_z_r = as.numeric(mat[i, "max[z.r]"]),
        pvalue_max_z_r = as.numeric(mat[i, "pvalue.max[z.r]"]),
        adj_pvalue_max_z_r = as.numeric(mat[i, "adj.pvalue.max[z.r]"]),
        max_z_logOR = as.numeric(mat[i, "max[z.logOR]"]),
        pvalue_max_z_logOR = as.numeric(mat[i, "pvalue.max[z.logOR]"]),
        adj_pvalue_max_z_logOR = as.numeric(mat[i, "adj.pvalue.max[z.logOR]"])
      )
    })
  }, error = function(e) {
    list()
  })

  list(
    model_object = models,
    model_fit = list(
      model    = model,
      n_parm   = s$npar,
      deviance = s$Deviance,
      aic      = s$AIC,
      bic      = s$BIC,
      m2       = s$M2,
      m2_df    = s$M2.df,
      m2_pv    = s$M2.pvalue,
      rmsea2   = s$RMSEA2,
      srmsr    = s$SRMSR
    ),
    reliability = list(
      ca_test      = ca$tau,
      ca_attribute = as.list(setNames(as.numeric(ca$tau_k), q_col_names))
    ),
    prevalensi  = s$`Attribute Prevalence`$all,
    profil      = profil_data,
    item_fit    = item_fit_res,
    empirical_stable = tryCatch({
      se_catprob <- GDINA::extract(models, what = "se.catprob.parm")
      if (!is.null(se_catprob)) {
        has_bad_se <- any(sapply(se_catprob, function(x) any(is.nan(x) | is.infinite(x) | is.na(x))))
        !has_bad_se
      } else {
        TRUE
      }
    }, error = function(e) TRUE)
  )
}

#' Check Q-matrix Identification (cdmTools)
#' @export
check_qmatrix_identification <- function(q_matrix, model_name) {
  cdm_model <- if (model_name == "DINA") "DINA"
               else if (model_name == "DINO") "DINO"
               else "others"

  # Cek load namespace secara aman untuk menangkap error dependensi (seperti CVXR)
  load_status <- tryCatch({
    loadNamespace("cdmTools")
    TRUE
  }, error = function(e) {
    conditionMessage(e)
  })

  if (!isTRUE(load_status)) {
    err_msg <- if (grepl("solve.*CVXR", load_status, ignore.case = TRUE)) {
      "Package 'cdmTools' gagal dimuat karena konflik versi 'CVXR' pada sistem Anda. Jalankan perintah ini di R untuk memperbaikinya: options(repos = c(CRAN = 'https://cloud.r-project.org')); remotes::install_version('CVXR', version = '1.0-15', upgrade = 'never')"
    } else {
      paste("Package 'cdmTools' gagal dimuat:", load_status)
    }
    return(list(
      strict     = FALSE,
      generic    = FALSE,
      conditions = list(),
      message    = err_msg,
      available  = FALSE
    ))
  }


  tryCatch({
    is_binary_col <- function(x) is.numeric(x) && all(na.omit(x) %in% c(0, 1))
    q_matrix <- q_matrix[, sapply(as.data.frame(q_matrix), is_binary_col), drop = FALSE]
    q_mat <- as.matrix(q_matrix)
    res <- cdmTools::is.Qid(Q = q_mat, model = cdm_model)
    list(
      strict     = isTRUE(as.logical(res$strict)),
      generic    = isTRUE(as.logical(res$generic)),
      conditions = if (!is.null(res$conditions)) as.list(res$conditions) else list(),
      message    = "Success",
      available  = TRUE
    )
  }, error = function(e) {
    list(
      strict     = FALSE,
      generic    = FALSE,
      conditions = list(),
      message    = paste("Error:", conditionMessage(e)),
      available  = FALSE
    )
  })
}
