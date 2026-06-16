try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)
library(plumber)
library(openxlsx)
library(GDINA)
library(httr)
library(jsonlite)

fit_cdm                <- cdm.gen.ai:::fit_cdm
check_qmatrix_identification <- cdm.gen.ai:::check_qmatrix_identification
buildModelReportPrompt <- cdm.gen.ai:::buildModelReportPrompt
buildItemReportPrompt  <- cdm.gen.ai:::buildItemReportPrompt
buildProfilPrompt      <- cdm.gen.ai:::buildProfilPrompt
buildProfilIndividuPrompt <- cdm.gen.ai:::buildProfilIndividuPrompt
buildChatSystemPrompt  <- cdm.gen.ai:::buildChatSystemPrompt
buildQvalReportPrompt  <- cdm.gen.ai:::buildQvalReportPrompt

# Load environment variables from .env if it exists
load_dot_env <- function() {
  paths_to_try <- c(".env", "../.env", "inst/.env", "../../.env")
  for (p in paths_to_try) {
    if (file.exists(p)) {
      lines <- readLines(p, warn = FALSE)
      lines <- lines[!grepl("^\\s*#", lines) & trimws(lines) != ""]
      for (line in lines) {
        eq_pos <- regexpr("=", line)
        if (eq_pos > 0) {
          key <- trimws(substr(line, 1, eq_pos - 1))
          val <- trimws(substr(line, eq_pos + 1, nchar(line)))
          val <- gsub("^['\"]|['\"]$", "", val)
          vars <- list(val)
          names(vars) <- key
          do.call(Sys.setenv, vars)
        }
      }
      break
    }
  }
}
load_dot_env()

# ── Simpan referensi PDF per sesi ─────────────────────────────────────────────
.reference_store <- new.env(parent = emptyenv())
.reference_store$documents <- list()
.reference_store$categories <- list()

get_all_references_text <- function() {
  docs <- .reference_store$documents
  categories <- .reference_store$categories
  if (is.null(docs) || length(docs) == 0) return(NULL)
  
  merged <- sapply(names(docs), function(name) {
    category <- if (!is.null(categories[[name]])) categories[[name]] else "Lainnya"
    paste0("--- DOKUMEN: ", name, " [Kategori: ", category, "] ---\n", docs[[name]])
  })
  paste(merged, collapse = "\n\n")
}

# Helper untuk mengambil data referensi dari Supabase
fetch_supabase_references <- function(category = NULL) {
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_anon <- Sys.getenv("SUPABASE_ANON_KEY")
  
  # Fallback ke database referensi akademis pusat CDM-GenAI pengembang jika salah satu kosong
  if (supabase_url == "" || supabase_anon == "") {
    supabase_url <- "https://bngjmlxsjwjgqvbjsfrx.supabase.co"
    supabase_anon <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJuZ2ptbHhzandqZ3F2YmpzZnJ4Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3ODE1NjAzMTgsImV4cCI6MjA5NzEzNjMxOH0.QLZqHWMyr5UWyzQGZ46wRshXXV-ixcbdASkRP14vadw"
  }
  
  if (supabase_url == "" || supabase_anon == "") {
    message("Supabase URL atau Anon Key tidak ditemukan di environment.")
    return(NULL)
  }
  
  supabase_url <- gsub("/+$", "", supabase_url)
  url <- paste0(supabase_url, "/rest/v1/cdm_references")
  
  query_params <- list()
  if (!is.null(category) && category != "" && category != "Lainnya" && category != "Umum") {
    query_params[["or"]] <- paste0("(category.eq.", category, ",category.eq.Umum)")
  }
  
  headers <- c(
    "apikey" = supabase_anon,
    "Authorization" = paste("Bearer", supabase_anon),
    "Accept" = "application/json"
  )
  
  res <- tryCatch({
    r <- httr::GET(url, httr::add_headers(.headers = headers), query = query_params)
    if (httr::status_code(r) == 200) {
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    } else {
      message("Gagal fetch Supabase (status code: ", httr::status_code(r), ")")
      NULL
    }
  }, error = function(e) {
    message("Gagal fetch Supabase: ", e$message)
    NULL
  })
  
  return(res)
}

# Helper untuk menggabungkan referensi pusat (Supabase) dan referensi lokal
get_combined_references_text <- function(category = NULL) {
  # 1. Fetch ALL Supabase references to provide comprehensive academic context
  supabase_docs <- fetch_supabase_references(NULL)
  
  # Sort supabase_docs to prioritize the requested category first, then "Umum", then the rest
  if (!is.null(supabase_docs) && length(supabase_docs) > 0 && !is.null(category) && category != "") {
    target_cat <- tolower(trimws(category))
    is_matching <- sapply(supabase_docs, function(doc) {
      !is.null(doc$category) && tolower(trimws(doc$category)) == target_cat
    })
    is_umum <- sapply(supabase_docs, function(doc) {
      !is.null(doc$category) && tolower(trimws(doc$category)) == "umum"
    })
    
    # Sort order: matching first, then "Umum", then the rest
    order_idx <- c(which(is_matching), which(is_umum & !is_matching), which(!is_matching & !is_umum))
    order_idx <- unique(order_idx)
    supabase_docs <- supabase_docs[order_idx]
  }
  
  supabase_text <- ""
  if (!is.null(supabase_docs) && length(supabase_docs) > 0) {
    formatted_supabase <- sapply(supabase_docs, function(doc) {
      author <- if (!is.null(doc$author)) doc$author else "Anonim"
      year   <- if (!is.null(doc$year)) doc$year else "n.d."
      title  <- if (!is.null(doc$title)) doc$title else "Tanpa Judul"
      cat_val<- if (!is.null(doc$category)) doc$category else "Umum"
      text   <- if (!is.null(doc$extracted_text)) doc$extracted_text else ""
      
      # Cap individual document text snippet to 12000 characters
      max_doc_chars <- 12000
      if (nchar(text) > max_doc_chars) {
        text <- paste0(substr(text, 1, max_doc_chars), "\n... [teks dipotong untuk efisiensi konteks] ...")
      }
      
      paste0("--- DOKUMEN PUSAT (Rujukan Utama): ", author, " (", year, "). ", title, ". [Kategori: ", cat_val, "] ---\n", text)
    })
    supabase_text <- paste(formatted_supabase, collapse = "\n\n")
  }
  
  # 2. Get local session references
  local_text <- get_all_references_text()
  
  # 3. Combine them
  combined_parts <- c()
  if (supabase_text != "") combined_parts <- c(combined_parts, supabase_text)
  if (!is.null(local_text) && local_text != "") combined_parts <- c(combined_parts, local_text)
  
  if (length(combined_parts) == 0) {
    # Fallback to default reference text
    default_ref_path <- system.file("reference/default_cdm_reference.txt", package = "cdm.gen.ai")
    if (default_ref_path == "" || !file.exists(default_ref_path)) {
      default_ref_path <- "inst/reference/default_cdm_reference.txt"
    }
    if (file.exists(default_ref_path)) {
      return(paste(readLines(default_ref_path, warn = FALSE), collapse = "\n"))
    }
    return("")
  }
  
  paste(combined_parts, collapse = "\n\n")
}

# ── Simpan file upload sementara antar request preview → analyze ──────────────
.upload_store <- new.env(parent = emptyenv())
.upload_store$temp_path <- NULL

# ── Helpers ───────────────────────────────────────────────────────────────────
success_response <- function(data, message = "Success") {
  list(status = jsonlite::unbox("success"), message = jsonlite::unbox(message), data = data)
}

find_col <- function(df, patterns) {
  cols <- colnames(df)
  for (p in patterns) {
    idx <- which(grepl(p, cols, ignore.case = TRUE))
    if (length(idx) > 0) return(cols[idx[1]])
  }
  return(NULL)
}

read_item_metadata <- function(temp_path, sheets, data_cols) {
  item_sheet_idx <- which(grepl("butir|item|metadata_butir", sheets, ignore.case = TRUE))
  if (length(item_sheet_idx) == 0) return(NULL)
  
  tryCatch({
    df <- openxlsx::read.xlsx(temp_path, sheet = item_sheet_idx[1])
    if (nrow(df) == 0) return(NULL)
    
    orig_col <- find_col(df, c("asli", "original", "kolom"))
    label_col <- find_col(df, c("label", "kostum", "kustom"))
    desc_col <- find_col(df, c("deskripsi", "kompetensi", "description", "competency", "context"))
    
    if (is.null(orig_col)) {
      orig_col <- colnames(df)[1]
    }
    
    metadata <- list()
    for (i in seq_len(nrow(df))) {
      orig_val <- as.character(df[i, orig_col])
      label_val <- if (!is.null(label_col)) as.character(df[i, label_col]) else NA
      desc_val <- if (!is.null(desc_col)) as.character(df[i, desc_col]) else ""
      
      if (is.na(orig_val) || orig_val == "NA") next
      if (is.na(label_val) || label_val == "NA" || label_val == "") label_val <- orig_val
      if (is.na(desc_val) || desc_val == "NA") desc_val <- ""
      
      if (orig_val %in% data_cols) {
        metadata[[orig_val]] <- list(
          label = jsonlite::unbox(label_val),
          description = jsonlite::unbox(desc_val)
        )
      }
    }
    return(metadata)
  }, error = function(e) {
    return(NULL)
  })
}

read_attr_metadata <- function(temp_path, sheets, q_cols) {
  attr_sheet_idx <- which(grepl("atribut|attribute|dimensi|metadata_atribut", sheets, ignore.case = TRUE))
  if (length(attr_sheet_idx) == 0) return(NULL)
  
  tryCatch({
    df <- openxlsx::read.xlsx(temp_path, sheet = attr_sheet_idx[1])
    if (nrow(df) == 0) return(NULL)
    
    orig_col <- find_col(df, c("asli", "original", "kolom"))
    label_col <- find_col(df, c("label", "kostum", "kustom"))
    desc_col <- find_col(df, c("deskripsi", "kompetensi", "description", "competency", "context"))
    
    if (is.null(orig_col)) {
      orig_col <- colnames(df)[1]
    }
    
    metadata <- list()
    for (i in seq_len(nrow(df))) {
      orig_val <- as.character(df[i, orig_col])
      label_val <- if (!is.null(label_col)) as.character(df[i, label_col]) else NA
      desc_val <- if (!is.null(desc_col)) as.character(df[i, desc_col]) else ""
      
      if (is.na(orig_val) || orig_val == "NA") next
      if (is.na(label_val) || label_val == "NA" || label_val == "") label_val <- orig_val
      if (is.na(desc_val) || desc_val == "NA") desc_val <- ""
      
      if (orig_val %in% q_cols) {
        metadata[[orig_val]] <- list(
          label = jsonlite::unbox(label_val),
          description = jsonlite::unbox(desc_val)
        )
      }
    }
    return(metadata)
  }, error = function(e) {
    return(NULL)
  })
}

error_response <- function(res, message, code = 400) {
  res$status <- code
  list(status = jsonlite::unbox("error"), message = jsonlite::unbox(message))
}

callGemini <- function(api_key, prompt, history = NULL) {
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1/models/gemini-2.5-flash:generateContent?key=",
    api_key
  )

  contents <- if (!is.null(history) && length(history) > 0) {
    if (is.data.frame(history)) {
      lapply(seq_len(nrow(history)), function(idx) {
        list(
          role  = ifelse(history$role[idx] == "user", "user", "model"),
          parts = list(list(text = history$content[idx]))
        )
      })
    } else {
      lapply(history, function(msg) {
        list(
          role  = ifelse(msg$role == "user", "user", "model"),
          parts = list(list(text = msg$content))
        )
      })
    }
  } else list()

  contents <- c(contents, list(list(
    role  = "user",
    parts = list(list(text = prompt))
  )))

  body <- list(contents = contents)

  res <- httr::POST(
    url,
    body    = body,
    encode  = "json",
    httr::timeout(120),
    httr::add_headers("Content-Type" = "application/json")
  )

  if (httr::status_code(res) != 200) {
    raw_err <- httr::content(res, as = "text", encoding = "UTF-8")
    err_msg <- raw_err
    
    try({
      parsed_err <- jsonlite::fromJSON(raw_err)
      if (!is.null(parsed_err$error)) {
        code <- parsed_err$error$code
        message <- parsed_err$error$message
        
        if (!is.null(code) && code == 503) {
          err_msg <- "Server AI Google saat ini sedang mengalami antrean/beban tinggi (High Demand). Spikes ini biasanya bersifat sementara, silakan coba lagi dalam beberapa saat."
        } else if (!is.null(code) && code == 429) {
          err_msg <- "Batas permintaan (kuota) API AI Google Anda telah tercapai (Too Many Requests). Silakan tunggu beberapa saat."
        } else if (!is.null(code) && code == 400 && grepl("API key not valid", message, ignore.case=TRUE)) {
          err_msg <- "API Key Gemini Anda tidak valid. Silakan periksa kembali di menu Setting AI."
        } else if (!is.null(message)) {
          err_msg <- paste0("API Error (", code, "): ", message)
        }
      }
    }, silent = TRUE)
    
    stop(err_msg)
  }

  parsed <- httr::content(res, as = "parsed", type = "application/json")

  if (!is.null(parsed$error)) stop(parsed$error$message)

  parts <- parsed$candidates[[1]]$content$parts
  text  <- paste(
    sapply(parts, function(p) if (!is.null(p$text)) p$text else ""),
    collapse = "\n\n"
  )

  if (nchar(trimws(text)) == 0) stop("Gemini mengembalikan respons kosong.")
  text
}

# ── POST /api/cdm/preview ──────────────────────────────────────────────────────
#* @serializer json
#* @post /api/cdm/preview
#* @param file:file
function(file, res) {
  if (is.null(file) || length(file) == 0) {
    return(error_response(res, "File tidak diterima", 400))
  }

  temp_path <- tempfile(fileext = ".xlsx")
  writeBin(file[[1]], temp_path)
  .upload_store$temp_path <- temp_path

  tryCatch({
    data     <- openxlsx::read.xlsx(temp_path, sheet = 1)
    q_matrix <- openxlsx::read.xlsx(temp_path, sheet = 2)

    data_cols <- colnames(data)[sapply(data, is.numeric)]
    q_cols    <- colnames(q_matrix)[sapply(q_matrix, is.numeric)]
    data_num  <- as.matrix(data[, data_cols, drop = FALSE])
    q_num     <- as.matrix(q_matrix[, q_cols, drop = FALSE])

    n_sample     <- min(5L, nrow(data_num))
    q_sample     <- min(nrow(q_num), 30L)
    sheets       <- openxlsx::getSheetNames(temp_path)
    item_metadata <- read_item_metadata(temp_path, sheets, data_cols)
    attribute_metadata <- read_attr_metadata(temp_path, sheets, q_cols)

    list(
      status = jsonlite::unbox("success"),
      meta = list(
        n_respondents = jsonlite::unbox(nrow(data_num)),
        n_items       = jsonlite::unbox(ncol(data_num)),
        n_attributes  = jsonlite::unbox(ncol(q_num))
      ),
      data_columns = as.list(data_cols),
      q_columns    = as.list(q_cols),
      data_sample  = lapply(seq_len(n_sample), function(i) as.list(as.numeric(data_num[i, ]))),
      q_sample     = lapply(seq_len(q_sample), function(i) as.list(as.numeric(q_num[i, ]))),
      item_metadata = item_metadata,
      attribute_metadata = attribute_metadata
    )
  }, error = function(e) {
    error_response(res, paste("Gagal membaca file:", e$message), 500)
  })
}

# ── POST /api/cdm ─────────────────────────────────────────────────────────────
#* @serializer json
#* @post /api/cdm
function(req, res) {
  if (is.null(.upload_store$temp_path) || !file.exists(.upload_store$temp_path)) {
    return(error_response(res, "Tidak ada file. Lakukan pratinjau file terlebih dahulu.", 400))
  }

  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
    error = function(e) list()
  )

  mono_constraint <- isTRUE(body$monoConstraint)
  model_list <- if (!is.null(body$models) && length(body$models) > 0)
    as.character(body$models)
  else
    c("GDINA", "DINA", "DINO", "LLM", "RRUM", "ACDM")

  temp_path <- .upload_store$temp_path

  tryCatch({
    data     <- openxlsx::read.xlsx(temp_path, sheet = 1)
    q_matrix <- openxlsx::read.xlsx(temp_path, sheet = 2)

    results <- lapply(model_list, function(m) {
      fit_cdm(data, q_matrix, model = m, mono.constraint = mono_constraint)
    })

    models <- lapply(results, function(r) {
      mod_obj <- r$model_object

      catprob <- GDINA::extract(mod_obj, what = "catprob.parm")
      discrim  <- GDINA::extract(mod_obj, what = "discrim")

      items <- lapply(seq_along(catprob), function(i) {
        probs <- catprob[[i]]
        list(
          item = names(catprob)[i],
          probabilities = lapply(seq_along(probs), function(j) {
            list(category = names(probs)[j], value = as.numeric(probs[j]))
          }),
          discrimination = list(delta_p = discrim[i, 1], gdi = discrim[i, 2])
        )
      })

      list(
        name             = r$model_fit$model,
        fit              = r$model_fit,
        reliability      = r$reliability,
        parameters       = items,
        profil           = r$profil,
        empirical_stable = r$empirical_stable,
        item_fit         = r$item_fit
      )
    })

    data_cols <- colnames(data)[sapply(data, is.numeric)]
    q_cols    <- colnames(q_matrix)[sapply(q_matrix, is.numeric)]
    data_num  <- as.matrix(data[, data_cols, drop = FALSE])
    q_num     <- as.matrix(q_matrix[, q_cols, drop = FALSE])

    sheets       <- openxlsx::getSheetNames(temp_path)
    item_metadata <- read_item_metadata(temp_path, sheets, data_cols)
    attribute_metadata <- read_attr_metadata(temp_path, sheets, q_cols)

    list(
      status = jsonlite::unbox("success"),
      data = list(
        models       = models,
        data         = lapply(1:nrow(data_num), function(i) as.list(as.numeric(data_num[i, ]))),
        data_columns = data_cols,
        q_matrix     = lapply(1:nrow(q_num), function(i) as.list(as.numeric(q_num[i, ]))),
        q_columns    = q_cols,
        item_metadata = item_metadata,
        attribute_metadata = attribute_metadata
      )
    )
  }, error = function(e) {
    error_response(res, paste("Gagal memproses file:", e$message), 500)
  })
}

# ── POST /api/ai/model-report ─────────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/model-report
function(req, res) {
  body             <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  api_key          <- body$apiKey
  models           <- body$models
  metadata         <- body$metadata
  research_context <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }

  ref_text <- get_combined_references_text("Model Fit")
  prompt   <- buildModelReportPrompt(models, metadata, reference_text = ref_text, research_context = research_context)

  tryCatch({
    ai_text <- callGemini(api_key, prompt)
    list(status = jsonlite::unbox("success"), report = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/ai/item-report ──────────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/item-report
function(req, res) {
  body             <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  api_key          <- body$apiKey
  model_name       <- body$modelName
  parameters       <- body$parameters
  metadata         <- body$metadata
  research_context <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }

  ref_text <- get_combined_references_text("Parameter Butir")
  prompt   <- buildItemReportPrompt(model_name, parameters, metadata, reference_text = ref_text, research_context = research_context)

  tryCatch({
    ai_text <- callGemini(api_key, prompt)
    list(status = jsonlite::unbox("success"), report = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/ai/profil-report ────────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/profil-report
function(req, res) {
  body              <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  api_key           <- body$apiKey
  model_name        <- body$modelName
  mastery_prob      <- body$masteryProb
  mastery_prop_eap  <- body$masteryPropEap
  mastery_prop_map  <- body$masteryPropMap
  mastery_prop_mle  <- body$masteryPropMle
  latent_class      <- body$latentClass
  metadata          <- body$metadata
  research_context  <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }

  ref_text <- get_combined_references_text("Reliabilitas")
  prompt   <- buildProfilPrompt(model_name, mastery_prob, mastery_prop_eap,
                                mastery_prop_map, mastery_prop_mle, latent_class, metadata, reference_text = ref_text, research_context = research_context)

  tryCatch({
    ai_text <- callGemini(api_key, prompt)
    list(status = jsonlite::unbox("success"), report = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/ai/profil-individu ──────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/profil-individu
function(req, res) {
  body             <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  api_key          <- body$apiKey
  model_name       <- body$modelName
  selected_persons <- body$selectedPersons
  metadata         <- body$metadata
  research_context <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }
  if (is.null(selected_persons) || length(selected_persons) == 0) {
    res$status <- 400
    return(list(status = "error", message = "Tidak ada responden yang dipilih"))
  }

  ref_text <- get_combined_references_text("Umum")
  prompt   <- buildProfilIndividuPrompt(model_name, selected_persons, metadata, reference_text = ref_text, research_context = research_context)

  tryCatch({
    ai_text <- callGemini(api_key, prompt)
    list(status = jsonlite::unbox("success"), report = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/ai/qval-report ──────────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/qval-report
function(req, res) {
  body             <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  api_key          <- body$apiKey
  suggestions      <- body$suggestions
  threshold        <- body$threshold
  metadata         <- body$metadata
  research_context <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }

  ref_text <- get_combined_references_text("Q-Matrix")
  prompt   <- buildQvalReportPrompt(suggestions, threshold, metadata, reference_text = ref_text, research_context = research_context)

  tryCatch({
    ai_text <- callGemini(api_key, prompt)
    list(status = jsonlite::unbox("success"), report = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/ai/chat ─────────────────────────────────────────────────────────
#* @serializer json
#* @post /api/ai/chat
function(req, res) {
  body             <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  api_key          <- body$apiKey
  message          <- body$message
  history          <- body$history
  cdm_context      <- body$context
  metadata         <- body$metadata
  research_context <- body$researchContext

  if (is.null(api_key) || api_key == "") {
    res$status <- 400
    return(list(status = "error", message = "API Key missing"))
  }

  if (is.null(message) || nchar(trimws(message)) == 0) {
    res$status <- 400
    return(list(status = "error", message = "Pesan tidak boleh kosong"))
  }

  ref_text      <- get_combined_references_text("Umum")
  system_prompt <- buildChatSystemPrompt(cdm_context, ref_text, metadata, research_context = research_context)
  full_message  <- paste0(system_prompt, "\n\nPertanyaan peneliti: ", message)

  tryCatch({
    ai_text <- callGemini(api_key, full_message, history = history)
    list(status = jsonlite::unbox("success"), reply = jsonlite::unbox(ai_text))
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}

# ── POST /api/reference/upload ────────────────────────────────────────────────
#* @serializer json
#* @post /api/reference/upload
#* @param file:file
#* @param filename:string
#* @param category:string
function(req, res) {
  # Debug logging
  message("--- Debug Upload ---")
  message("Class of req$body: ", class(req$body))
  message("Names of req$body: ", paste(names(req$body), collapse = ", "))
  if ("file" %in% names(req$body)) {
    message("Class of req$body$file: ", class(req$body$file))
    message("Names/attributes of req$body$file: ", paste(names(req$body$file), collapse = ", "))
  }
  if ("filename" %in% names(req$body)) {
    message("Class of req$body$filename: ", class(req$body$filename))
    message("Names/attributes of req$body$filename: ", paste(names(req$body$filename), collapse = ", "))
  }
  if ("category" %in% names(req$body)) {
    message("Class of req$body$category: ", class(req$body$category))
  }
  
  file <- req$body$file
  filename_raw <- req$body$filename
  category_raw <- req$body$category

  # Parse filename robustly
  filename <- NULL
  if (!is.null(filename_raw)) {
    if (is.list(filename_raw)) {
      if ("parsed" %in% names(filename_raw) && !is.null(filename_raw$parsed) && length(filename_raw$parsed) > 0 && filename_raw$parsed != "") {
        filename <- filename_raw$parsed
      } else if ("value" %in% names(filename_raw)) {
        val <- filename_raw$value
        if (is.raw(val) && length(val) > 0) {
          filename <- rawToChar(val)
        } else {
          filename <- val
        }
      }
    } else {
      filename <- filename_raw
    }
  }

  if (is.list(filename)) {
    filename <- unlist(filename)
  }
  
  if (is.null(filename) || length(filename) == 0 || is.na(filename[1]) || filename[1] == "") {
    filename <- NULL
  } else {
    filename <- as.character(filename[1])
  }

  # Parse category robustly
  category <- NULL
  if (!is.null(category_raw)) {
    if (is.list(category_raw)) {
      if ("parsed" %in% names(category_raw) && !is.null(category_raw$parsed) && length(category_raw$parsed) > 0 && category_raw$parsed != "") {
        category <- category_raw$parsed
      } else if ("value" %in% names(category_raw)) {
        val <- category_raw$value
        if (is.raw(val) && length(val) > 0) {
          category <- rawToChar(val)
        } else {
          category <- val
        }
      }
    } else {
      category <- category_raw
    }
  }

  if (is.list(category)) {
    category <- unlist(category)
  }
  
  if (is.null(category) || length(category) == 0 || is.na(category[1]) || category[1] == "") {
    category <- "Lainnya"
  } else {
    category <- as.character(category[1])
  }

  # Parse file data and extract default filename if needed
  file_data <- NULL
  if (!is.null(file)) {
    if (is.list(file) && "value" %in% names(file)) {
      file_data <- file$value
      if (is.null(filename) && "filename" %in% names(file)) {
        filename <- file$filename
      }
    } else if (is.list(file) && length(file) > 0 && is.raw(file[[1]])) {
      file_data <- file[[1]]
    } else if (is.raw(file)) {
      file_data <- file
    }
  }

  if (is.null(file_data) || length(file_data) == 0) {
    return(error_response(res, "File tidak diterima", 400))
  }

  if (is.null(filename) || filename == "") {
    filename <- "document.pdf"
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(error_response(res, "Package pdftools belum terinstall. Jalankan: install.packages('pdftools')", 500))
  }

  temp_path <- tempfile(fileext = ".pdf")
  writeBin(file_data, temp_path)

  tryCatch({
    pages     <- pdftools::pdf_text(temp_path)
    full_text <- paste(pages, collapse = "\n\n")
    full_text <- trimws(gsub("\\s+", " ", full_text))

    .reference_store$documents[[filename]]  <- full_text
    .reference_store$categories[[filename]] <- category

    list(
      status  = jsonlite::unbox("success"),
      message = jsonlite::unbox(paste0("Referensi '", filename, "' (", category, ") berhasil diunggah")),
      chars   = jsonlite::unbox(nchar(full_text)),
      preview = jsonlite::unbox(substr(full_text, 1, 300))
    )
  }, error = function(e) {
    error_response(res, paste("Gagal membaca PDF:", e$message), 500)
  }, finally = {
    if (file.exists(temp_path)) unlink(temp_path)
  })
}
# ── GET /api/admin/reference/status ───────────────────────────────────────────
#* @serializer json
#* @get /api/admin/reference/status
function(req, res) {
  dev_secret <- Sys.getenv("DEVELOPER_SECRET_KEY")
  dev_mode_available <- !is.null(dev_secret) && dev_secret != ""
  list(
    status = jsonlite::unbox("success"),
    devModeAvailable = jsonlite::unbox(dev_mode_available)
  )
}

# ── POST /api/admin/reference/upload ──────────────────────────────────────────
#* @serializer json
#* @post /api/admin/reference/upload
function(req, res) {
  # Verify Developer Secret Key
  auth_header <- req$HTTP_DEVELOPER_SECRET_KEY
  if (is.null(auth_header) || auth_header == "") {
    auth_header <- req$HTTP_AUTHORIZATION
    if (!is.null(auth_header) && grepl("^Bearer ", auth_header)) {
      auth_header <- gsub("^Bearer ", "", auth_header)
    }
  }
  dev_secret <- Sys.getenv("DEVELOPER_SECRET_KEY")
  if (dev_secret == "" || is.null(auth_header) || auth_header != dev_secret) {
    res$status <- 401
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("Unauthorized: Developer secret key mismatch atau tidak valid")))
  }

  file <- req$body$file
  
  # Parse form parameters using a robust internal helper
  parse_field <- function(field_raw, default_val = NULL) {
    if (is.null(field_raw)) return(default_val)
    val <- NULL
    if (is.list(field_raw)) {
      if ("parsed" %in% names(field_raw) && !is.null(field_raw$parsed) && length(field_raw$parsed) > 0 && field_raw$parsed != "") {
        val <- field_raw$parsed
      } else if ("value" %in% names(field_raw)) {
        v <- field_raw$value
        if (is.raw(v) && length(v) > 0) {
          val <- rawToChar(v)
        } else {
          val <- v
        }
      }
    } else {
      val <- field_raw
    }
    if (is.list(val)) val <- unlist(val)
    if (is.null(val) || length(val) == 0 || is.na(val[1]) || val[1] == "") return(default_val)
    return(as.character(val[1]))
  }

  author   <- parse_field(req$body$author, "Anonim")
  year_raw <- parse_field(req$body$year, "2000")
  year     <- as.integer(year_raw)
  if (is.na(year)) year <- 2000
  title    <- parse_field(req$body$title, "Tanpa Judul")
  journal  <- parse_field(req$body$journal, "")
  volume   <- parse_field(req$body$volume, "")
  issue    <- parse_field(req$body$issue, "")
  pages    <- parse_field(req$body$pages, "")
  doi      <- parse_field(req$body$doi, "")
  category <- parse_field(req$body$category, "Umum")
  filename <- parse_field(req$body$filename, "document.pdf")

  # Parse file data
  file_data <- NULL
  if (!is.null(file)) {
    if (is.list(file) && "value" %in% names(file)) {
      file_data <- file$value
      if (is.null(filename) || filename == "document.pdf") {
        if ("filename" %in% names(file)) filename <- file$filename
      }
    } else if (is.list(file) && length(file) > 0 && is.raw(file[[1]])) {
      file_data <- file[[1]]
    } else if (is.raw(file)) {
      file_data <- file
    }
  }

  if (is.null(file_data) || length(file_data) == 0) {
    return(error_response(res, "File PDF tidak diterima", 400))
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(error_response(res, "Package pdftools belum terinstall. Jalankan: install.packages('pdftools')", 500))
  }

  temp_path <- tempfile(fileext = ".pdf")
  writeBin(file_data, temp_path)

  tryCatch({
    pdf_pages <- pdftools::pdf_text(temp_path)
    full_text <- paste(pdf_pages, collapse = "\n\n")
    full_text <- trimws(gsub("\\s+", " ", full_text))

    # Send to Supabase using service_role key
    supabase_url <- Sys.getenv("SUPABASE_URL")
    supabase_service <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
    
    if (supabase_url == "" || supabase_service == "") {
      stop("SUPABASE_URL atau SUPABASE_SERVICE_ROLE_KEY tidak terkonfigurasi di environment backend.")
    }
    
    url <- paste0(gsub("/+$", "", supabase_url), "/rest/v1/cdm_references")
    
    headers <- c(
      "apikey" = supabase_service,
      "Authorization" = paste("Bearer", supabase_service),
      "Content-Type" = "application/json",
      "Prefer" = "return=representation"
    )
    
    payload <- list(
      author = jsonlite::unbox(author),
      year = jsonlite::unbox(year),
      title = jsonlite::unbox(title),
      journal = jsonlite::unbox(journal),
      volume = jsonlite::unbox(volume),
      issue = jsonlite::unbox(issue),
      pages = jsonlite::unbox(pages),
      doi = jsonlite::unbox(doi),
      category = jsonlite::unbox(category),
      extracted_text = jsonlite::unbox(full_text)
    )
    
    res_post <- httr::POST(url, httr::add_headers(.headers = headers), body = payload, encode = "json")
    
    if (httr::status_code(res_post) %in% c(200, 201)) {
      inserted_data <- jsonlite::fromJSON(httr::content(res_post, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      list(
        status  = jsonlite::unbox("success"),
        message = jsonlite::unbox(paste0("Referensi akademik pusat '", title, "' berhasil disimpan ke Supabase.")),
        data    = inserted_data[[1]]
      )
    } else {
      res$status <- httr::status_code(res_post)
      error_response(res, paste0("Gagal menulis ke Supabase: status ", httr::status_code(res_post), " - ", httr::content(res_post, "text", encoding = "UTF-8")), httr::status_code(res_post))
    }
  }, error = function(e) {
    error_response(res, paste("Gagal memproses dan menyimpan referensi:", e$message), 500)
  }, finally = {
    if (file.exists(temp_path)) unlink(temp_path)
  })
}

# ── POST /api/admin/reference/delete ──────────────────────────────────────────
#* @serializer json
#* @post /api/admin/reference/delete
function(req, res) {
  # Verify Developer Secret Key
  auth_header <- req$HTTP_DEVELOPER_SECRET_KEY
  if (is.null(auth_header) || auth_header == "") {
    auth_header <- req$HTTP_AUTHORIZATION
    if (!is.null(auth_header) && grepl("^Bearer ", auth_header)) {
      auth_header <- gsub("^Bearer ", "", auth_header)
    }
  }
  dev_secret <- Sys.getenv("DEVELOPER_SECRET_KEY")
  if (dev_secret == "" || is.null(auth_header) || auth_header != dev_secret) {
    res$status <- 401
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("Unauthorized: Developer secret key mismatch atau tidak valid")))
  }
  
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  id   <- body$id
  
  if (is.null(id) || id == "") {
    res$status <- 400
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("ID tidak boleh kosong")))
  }
  
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_service <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  
  url <- paste0(gsub("/+$", "", supabase_url), "/rest/v1/cdm_references?id=eq.", id)
  
  headers <- c(
    "apikey" = supabase_service,
    "Authorization" = paste("Bearer", supabase_service)
  )
  
  res_del <- tryCatch({
    r <- httr::DELETE(url, httr::add_headers(.headers = headers))
    if (httr::status_code(r) %in% c(200, 204)) {
      list(status = jsonlite::unbox("success"), message = jsonlite::unbox(paste0("Referensi dengan ID ", id, " berhasil dihapus")))
    } else {
      res$status <- httr::status_code(r)
      list(status = jsonlite::unbox("error"), message = jsonlite::unbox(paste0("Gagal menghapus dari Supabase: status ", httr::status_code(r))))
    }
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
  
  return(res_del)
}

# ── POST /api/admin/reference/update ──────────────────────────────────────────
#* @serializer json
#* @post /api/admin/reference/update
function(req, res) {
  # Verify Developer Secret Key
  auth_header <- req$HTTP_DEVELOPER_SECRET_KEY
  if (is.null(auth_header) || auth_header == "") {
    auth_header <- req$HTTP_AUTHORIZATION
    if (!is.null(auth_header) && grepl("^Bearer ", auth_header)) {
      auth_header <- gsub("^Bearer ", "", auth_header)
    }
  }
  dev_secret <- Sys.getenv("DEVELOPER_SECRET_KEY")
  if (dev_secret == "" || is.null(auth_header) || auth_header != dev_secret) {
    res$status <- 401
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("Unauthorized: Developer secret key mismatch atau tidak valid")))
  }
  
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  id   <- body$id
  
  if (is.null(id) || id == "") {
    res$status <- 400
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("ID tidak boleh kosong")))
  }
  
  author   <- if (!is.null(body$author)) as.character(body$author) else NULL
  year     <- if (!is.null(body$year)) as.integer(body$year) else NULL
  title    <- if (!is.null(body$title)) as.character(body$title) else NULL
  journal  <- if (!is.null(body$journal)) as.character(body$journal) else ""
  volume   <- if (!is.null(body$volume)) as.character(body$volume) else ""
  issue    <- if (!is.null(body$issue)) as.character(body$issue) else ""
  pages    <- if (!is.null(body$pages)) as.character(body$pages) else ""
  doi      <- if (!is.null(body$doi)) as.character(body$doi) else ""
  category <- if (!is.null(body$category)) as.character(body$category) else NULL
  
  if (is.null(author) || author == "" || is.null(title) || title == "" || is.null(category) || category == "") {
    res$status <- 400
    return(list(status = jsonlite::unbox("error"), message = jsonlite::unbox("Penulis, Judul, dan Kategori harus diisi")))
  }
  
  if (is.null(year) || is.na(year)) year <- 2000
  
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_service <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  
  url <- paste0(gsub("/+$", "", supabase_url), "/rest/v1/cdm_references?id=eq.", id)
  
  headers <- c(
    "apikey" = supabase_service,
    "Authorization" = paste("Bearer", supabase_service),
    "Content-Type" = "application/json"
  )
  
  payload <- list(
    author = jsonlite::unbox(author),
    year = jsonlite::unbox(year),
    title = jsonlite::unbox(title),
    journal = jsonlite::unbox(journal),
    volume = jsonlite::unbox(volume),
    issue = jsonlite::unbox(issue),
    pages = jsonlite::unbox(pages),
    doi = jsonlite::unbox(doi),
    category = jsonlite::unbox(category)
  )
  
  res_patch <- tryCatch({
    r <- httr::PATCH(url, httr::add_headers(.headers = headers), body = payload, encode = "json")
    if (httr::status_code(r) %in% c(200, 204)) {
      list(status = jsonlite::unbox("success"), message = jsonlite::unbox(paste0("Referensi dengan ID ", id, " berhasil diperbarui")))
    } else {
      res$status <- httr::status_code(r)
      list(status = jsonlite::unbox("error"), message = jsonlite::unbox(paste0("Gagal memperbarui di Supabase: status ", httr::status_code(r), " - ", httr::content(r, "text", encoding = "UTF-8"))))
    }
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
  
  return(res_patch)
}


# ── GET /api/reference/list ───────────────────────────────────────────────────
#* @serializer json
#* @get /api/reference/list
function(category = NULL, res) {
  docs <- fetch_supabase_references(category)
  if (is.null(docs)) {
    return(list())
  }
  cleaned_docs <- lapply(docs, function(doc) {
    list(
      id = jsonlite::unbox(doc$id),
      author = jsonlite::unbox(doc$author),
      year = jsonlite::unbox(doc$year),
      title = jsonlite::unbox(doc$title),
      journal = jsonlite::unbox(doc$journal),
      volume = jsonlite::unbox(doc$volume),
      issue = jsonlite::unbox(doc$issue),
      pages = jsonlite::unbox(doc$pages),
      doi = jsonlite::unbox(doc$doi),
      category = jsonlite::unbox(doc$category),
      created_at = jsonlite::unbox(doc$created_at)
    )
  })
  return(cleaned_docs)
}

# ── POST /api/reference/update_category ───────────────────────────────────────
#* @serializer json
#* @post /api/reference/update_category
function(req, res) {
  body     <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  filename <- body$filename
  category <- body$category
  
  if (is.null(filename) || filename == "") {
    return(error_response(res, "Filename tidak boleh kosong", 400))
  }
  if (is.null(category) || category == "") {
    category <- "Lainnya"
  }
  
  if (is.null(.reference_store$documents[[filename]])) {
    return(error_response(res, "Dokumen tidak ditemukan", 404))
  }
  
  .reference_store$categories[[filename]] <- category
  
  list(
    status  = jsonlite::unbox("success"),
    message = jsonlite::unbox(paste0("Kategori untuk '", filename, "' berhasil diupdate menjadi '", category, "'"))
  )
}

# ── GET /api/reference/clear ──────────────────────────────────────────────────
#* @serializer json
#* @get /api/reference/clear
function(filename = NULL, res) {
  if (is.null(filename) || filename == "") {
    .reference_store$documents <- list()
    .reference_store$categories <- list()
    list(status = jsonlite::unbox("success"), message = jsonlite::unbox("Semua referensi dihapus"))
  } else {
    .reference_store$documents[[filename]] <- NULL
    .reference_store$categories[[filename]] <- NULL
    list(status = jsonlite::unbox("success"), message = jsonlite::unbox(paste0("Referensi '", filename, "' dihapus")))
  }
}

# ── POST /api/cdm/qval ────────────────────────────────────────────────────────
#* @serializer json
#* @post /api/cdm/qval
function(req, res) {
  body     <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  data_raw <- body$data
  q_raw    <- body$qMatrix
  model    <- body$modelName
  eps      <- body$eps

  if (is.null(eps)) eps <- 0.95

  tryCatch({
    # Convert JSON list-of-lists to proper numeric matrices regardless of fromJSON output format
    to_num_matrix <- function(x) {
      if (is.matrix(x)) return(matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x)))
      rows <- lapply(x, function(r) as.numeric(unlist(r)))
      do.call(rbind, rows)
    }

    data_mat <- to_num_matrix(data_raw)
    q_mat    <- to_num_matrix(q_raw)

    # Get column names if passed, fallback to V1..Vn / A1..An
    data_cols <- if (!is.null(body$dataColumns)) unlist(body$dataColumns) else NULL
    q_cols    <- if (!is.null(body$qColumns)) unlist(body$qColumns) else NULL
    if (is.null(data_cols)) data_cols <- paste0("V", seq_len(ncol(data_mat)))
    if (is.null(q_cols)) q_cols <- paste0("A", seq_len(ncol(q_mat)))
    colnames(data_mat) <- data_cols
    colnames(q_mat)    <- q_cols

    if (ncol(data_mat) != nrow(q_mat)) {
      stop(sprintf(
        "Dimensi tidak cocok: data %d×%d (responden×butir), q_matrix %d×%d (butir×atribut).",
        nrow(data_mat), ncol(data_mat), nrow(q_mat), ncol(q_mat)
      ))
    }

    r       <- fit_cdm(data_mat, q_mat, model = model)
    mod_obj <- r$model_object

    qval  <- GDINA::Qval(mod_obj, method = "PVAF", eps = eps)
    sug_q <- as.matrix(qval$sug.Q)

    # Compute changed items by comparing sug.Q with original Q (no $index field in Qval)
    changed_items <- which(apply(sug_q != q_mat, 1, any))

    # Build per-item PVAF keyed by attribute position (single-attr Q-vector columns)
    # In GDINA encoding, column for attr k is 2^(k-1) (1-indexed)
    pvaf_mat <- as.matrix(qval$PVAF)
    K <- ncol(sug_q)
    single_attr_cols <- pmin(2L ^ (seq_len(K) - 1L), ncol(pvaf_mat))

    pvaf_list <- lapply(seq_len(nrow(pvaf_mat)), function(i) {
      as.list(as.numeric(pvaf_mat[i, single_attr_cols]))
    })

    # Generate base64 plots for changed items
    plots_list <- vector("list", nrow(q_mat))
    for (i in changed_items) {
      temp_png <- tempfile(fileext = ".png")
      # Adjust resolution for clear rendering
      png(temp_png, width = 750, height = 480, res = 130)
      tryCatch({
        plot(qval, item = i)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Gagal membuat plot:", conditionMessage(e)))
      })
      dev.off()
      plots_list[[i]] <- base64enc::base64encode(temp_png)
      unlink(temp_png)
    }

    list(
      status        = jsonlite::unbox("success"),
      sug_q         = lapply(seq_len(nrow(sug_q)), function(i) as.list(as.numeric(sug_q[i, ]))),
      changed_items = as.list(as.integer(changed_items)),
      pvaf          = pvaf_list,
      plots         = lapply(plots_list, function(p) if (is.null(p)) "" else jsonlite::unbox(p))
    )
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(conditionMessage(e)))
  })
}

#* @serializer json
#* @post /api/cdm/qval/plot
function(req, res) {
  body     <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  data_raw <- body$data
  q_raw    <- body$qMatrix
  model    <- body$modelName
  eps      <- body$eps
  item_idx <- as.integer(body$itemIndex)

  if (is.null(eps)) eps <- 0.95

  tryCatch({
    to_num_matrix <- function(x) {
      if (is.matrix(x)) return(matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x)))
      rows <- lapply(x, function(r) as.numeric(unlist(r)))
      do.call(rbind, rows)
    }

    data_mat <- to_num_matrix(data_raw)
    q_mat    <- to_num_matrix(q_raw)

    # Get column names if passed, fallback to V1..Vn / A1..An
    data_cols <- if (!is.null(body$dataColumns)) unlist(body$dataColumns) else NULL
    q_cols    <- if (!is.null(body$qColumns)) unlist(body$qColumns) else NULL
    if (is.null(data_cols)) data_cols <- paste0("V", seq_len(ncol(data_mat)))
    if (is.null(q_cols)) q_cols <- paste0("A", seq_len(ncol(q_mat)))
    colnames(data_mat) <- data_cols
    colnames(q_mat)    <- q_cols

    if (ncol(data_mat) != nrow(q_mat)) {
      stop(sprintf(
        "Dimensi tidak cocok: data %d×%d (responden×butir), q_matrix %d×%d (butir×atribut).",
        nrow(data_mat), ncol(data_mat), nrow(q_mat), ncol(q_mat)
      ))
    }

    r       <- fit_cdm(data_mat, q_mat, model = model)
    mod_obj <- r$model_object

    qval  <- GDINA::Qval(mod_obj, method = "PVAF", eps = eps)

    # Generate base64 plot for the specific item
    temp_png <- tempfile(fileext = ".png")
    png(temp_png, width = 750, height = 480, res = 130)
    tryCatch({
      plot(qval, item = item_idx)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Gagal membuat plot:", conditionMessage(e)))
    })
    dev.off()

    plot_b64 <- base64enc::base64encode(temp_png)
    unlink(temp_png)

    list(
      status = jsonlite::unbox("success"),
      plot   = jsonlite::unbox(plot_b64)
    )
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(conditionMessage(e)))
  })
}

# ── POST /api/cdm/check-id ────────────────────────────────────────────────────
#* @serializer json
#* @post /api/cdm/check-id
function(req, res) {
  body       <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  q_raw      <- body$qMatrix
  model_name <- body$modelName

  tryCatch({
    # Convert list-of-lists to proper numeric matrix
    rows     <- lapply(q_raw, function(r) as.numeric(unlist(r)))
    q_matrix <- do.call(rbind, rows)

    id_res <- check_qmatrix_identification(q_matrix, model_name)
    list(
      status     = jsonlite::unbox("success"),
      strict     = jsonlite::unbox(id_res$strict),
      generic    = jsonlite::unbox(id_res$generic),
      conditions = id_res$conditions,
      message    = jsonlite::unbox(id_res$message),
      available  = jsonlite::unbox(isTRUE(id_res$available))
    )
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(conditionMessage(e)))
  })
}

# ── POST /api/cdm/estimate ────────────────────────────────────────────────────
#* @serializer json
#* @post /api/cdm/estimate
function(req, res) {
  body     <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  data_raw <- body$data
  q_raw    <- body$qMatrix

  tryCatch({
    to_num_matrix <- function(x) {
      if (is.matrix(x)) return(matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x)))
      rows <- lapply(x, function(r) as.numeric(unlist(r)))
      do.call(rbind, rows)
    }

    data     <- to_num_matrix(data_raw)
    q_matrix <- to_num_matrix(q_raw)

    # Get column names, fallback if null
    data_cols <- if (!is.null(body$dataColumns)) unlist(body$dataColumns) else colnames(data)
    q_cols    <- if (!is.null(body$qColumns)) unlist(body$qColumns) else colnames(q_matrix)
    if (is.null(data_cols)) data_cols <- paste0("V", 1:ncol(data))
    if (is.null(q_cols)) q_cols <- paste0("A", 1:ncol(q_matrix))

    colnames(data)     <- data_cols
    colnames(q_matrix) <- q_cols

    model_list <- c("GDINA", "DINA", "DINO", "LLM", "RRUM", "ACDM")

    results <- lapply(model_list, function(m) {
      fit_cdm(data, q_matrix, model = m)
    })

    models <- lapply(results, function(r) {
      mod_obj <- r$model_object

      catprob <- GDINA::extract(mod_obj, what = "catprob.parm")
      discrim  <- GDINA::extract(mod_obj, what = "discrim")

      items <- lapply(seq_along(catprob), function(i) {
        probs <- catprob[[i]]
        list(
          item = names(catprob)[i],
          probabilities = lapply(seq_along(probs), function(j) {
            list(category = names(probs)[j], value = as.numeric(probs[j]))
          }),
          discrimination = list(delta_p = discrim[i, 1], gdi = discrim[i, 2])
        )
      })

      list(
        name             = r$model_fit$model,
        fit              = r$model_fit,
        reliability      = r$reliability,
        parameters       = items,
        profil           = r$profil,
        empirical_stable = r$empirical_stable,
        item_fit         = r$item_fit
      )
    })

    sheets <- if (!is.null(.upload_store$temp_path) && file.exists(.upload_store$temp_path)) {
      openxlsx::getSheetNames(.upload_store$temp_path)
    } else NULL

    item_metadata <- if (!is.null(sheets)) read_item_metadata(.upload_store$temp_path, sheets, data_cols) else NULL
    attribute_metadata <- if (!is.null(sheets)) read_attr_metadata(.upload_store$temp_path, sheets, q_cols) else NULL

    list(
      status = jsonlite::unbox("success"),
      data = list(
        models       = models,
        data         = lapply(1:nrow(data), function(i) as.list(as.numeric(data[i, ]))),
        data_columns = as.list(data_cols),
        q_matrix     = lapply(1:nrow(q_matrix), function(i) as.list(as.numeric(q_matrix[i, ]))),
        q_columns    = as.list(q_cols),
        item_metadata = item_metadata,
        attribute_metadata = attribute_metadata
      )
    )
  }, error = function(e) {
    res$status <- 500
    list(status = jsonlite::unbox("error"), message = jsonlite::unbox(e$message))
  })
}
