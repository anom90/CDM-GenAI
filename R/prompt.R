loadDefaultReference <- function() {
  default_ref_path <- system.file("reference/default_cdm_reference.txt", package = "cdm.gen.ai")
  if (default_ref_path != "" && file.exists(default_ref_path)) {
    con <- file(default_ref_path, encoding = "UTF-8")
    lines <- readLines(con, warn = FALSE)
    close(con)
    return(paste(lines, collapse = "\n"))
  }
  return("")
}

formatReferencePrompt <- function(reference_text) {
  default_ref <- loadDefaultReference()
  
  has_default <- (nchar(trimws(default_ref)) > 0)
  has_user    <- (!is.null(reference_text) && nchar(trimws(reference_text)) > 0)
  
  if (!has_default && !has_user) return("")
  
  ref_sections <- list()
  
  if (has_default) {
    ref_sections <- c(ref_sections, paste0(
      "1. ACUAN PARAMETER TEORITIS & AMBANG BATAS CDM (STANDAR APLIKASI):\n",
      default_ref
    ))
  }
  
  if (has_user) {
    max_chars <- 10000
    user_trimmed <- if (nchar(reference_text) > max_chars) {
      paste0(substr(reference_text, 1, max_chars), "\n... [dipotong karena terlalu panjang] ...")
    } else reference_text
    
    ref_sections <- c(ref_sections, paste0(
      "2. DOKUMEN RUJUKAN TAMBAHAN DARI PENELITI:\n",
      user_trimmed
    ))
  }
  
  paste0(
    "\n\n--- DOKUMEN REFERENSI AKADEMIK (WAJIB DIRUJUK) ---\n",
    "Anda WAJIB mematuhi ketentuan teoretis dan instruksi di bawah ini saat menyusun laporan interpretasi:\n",
    "- Untuk parameter statistik, model fit, reliabilitas, dan kualitas butir, wajib ikuti standar batas nilai (threshold) yang diuraikan pada acuan parameter CDM (1).\n",
    "- Jika peneliti menyediakan rujukan kustom (2) terkait kajian teori pembelajaran atau metode intervensi, integrasikan dan hubungkan hasil analisis CDM dengan pembahasan kurikulum/intervensi tersebut secara kontekstual.\n",
    "- PENTING: Jangan menyalin atau menuliskan label rujukan 'PANDUAN STANDAR AKADEMIK CDM', 'ACUAN PARAMETER TEORITIS & AMBANG BATAS CDM', atau 'BAWAAN APLIKASI' di dalam hasil laporan interpretasi. Rujuklah standar nilai batas tersebut secara ilmiah sebagai acuan standar teoretis CDM, atau jika ada dokumen rujukan spesifik di bawah ini, sitasi dokumen tersebut secara formal menggunakan format APA Style 7th Edition.\n\n",
    paste(ref_sections, collapse = "\n\n"), "\n",
    "----------------------------------------------------\n"
  )
}

formatMetadataPrompt <- function(metadata) {
  if (is.null(metadata) || length(metadata) == 0) return("")
  
  items_str <- ""
  if (!is.null(metadata$items) && (is.data.frame(metadata$items) || is.list(metadata$items)) && length(metadata$items) > 0) {
    items_df <- metadata$items
    if (is.data.frame(items_df) && nrow(items_df) > 0) {
      lines <- sapply(1:nrow(items_df), function(i) {
        code <- items_df$code[i]
        label <- items_df$label[i]
        desc <- if (!is.null(items_df$description[i])) items_df$description[i] else ""
        
        if ((!is.null(label) && label != "" && label != code) || (!is.null(desc) && desc != "")) {
          paste0("- Kode: ", code, 
                 ifelse(!is.null(label) && label != "" && label != code, paste0(" (Label Baru: ", label, ")"), ""),
                 ifelse(!is.null(desc) && desc != "", paste0(" -> Deskripsi/Materi Soal: ", desc), ""))
        } else {
          NULL
        }
      })
      lines <- unlist(lines)
      if (length(lines) > 0) {
        items_str <- paste0("Butir Soal (Items) dengan Konteks Kustom:\n", paste(lines, collapse = "\n"))
      }
    }
  }
  
  attrs_str <- ""
  if (!is.null(metadata$attributes) && (is.data.frame(metadata$attributes) || is.list(metadata$attributes)) && length(metadata$attributes) > 0) {
    attrs_df <- metadata$attributes
    if (is.data.frame(attrs_df) && nrow(attrs_df) > 0) {
      lines <- sapply(1:nrow(attrs_df), function(i) {
        code <- attrs_df$code[i]
        label <- attrs_df$label[i]
        desc <- if (!is.null(attrs_df$description[i])) attrs_df$description[i] else ""
        
        if ((!is.null(label) && label != "" && label != code) || (!is.null(desc) && desc != "")) {
          paste0("- Kode: ", code, 
                 ifelse(!is.null(label) && label != "" && label != code, paste0(" (Label Baru: ", label, ")"), ""),
                 ifelse(!is.null(desc) && desc != "", paste0(" -> Deskripsi/Kompetensi Atribut: ", desc), ""))
        } else {
          NULL
        }
      })
      lines <- unlist(lines)
      if (length(lines) > 0) {
        attrs_str <- paste0("Atribut Dimensi (Attributes) dengan Konteks Kustom:\n", paste(lines, collapse = "\n"))
      }
    }
  }
  
  if (items_str == "" && attrs_str == "") return("")
  
  paste0(
    "\n\n--- KONTEKS / DESKRIPSI VARIABEL ---\n",
    "Peneliti telah mendefinisikan label baru dan deskripsi materi/kompetensi untuk variabel berikut. ",
    "Gunakan informasi di bawah ini untuk menggantikan kode asli (seperti V1, V2, A1, A2) dengan label kustom ",
    "yang lebih kontekstual, nyata, dan bermakna dalam penjelasan Anda agar peneliti mendapatkan laporan yang intuitif:\n\n",
    ifelse(items_str != "", paste0(items_str, "\n\n"), ""),
    ifelse(attrs_str != "", paste0(attrs_str, "\n"), ""),
    "-------------------------------------\n"
  )
}

formatResearchContextPrompt <- function(research_context) {
  if (is.null(research_context) || nchar(trimws(research_context)) == 0) return("")
  
  paste0(
    "\n\n--- KONTEKS PENELITIAN & BIDANG KEILMUAN (RESEARCH CONTEXT) ---\n",
    "Berikut adalah latar belakang/konteks penelitian kustom yang disediakan oleh peneliti:\n",
    research_context, "\n",
    "Anda wajib mengintegrasikan konteks/bidang keilmuan di atas ke dalam analisis hasil CDM Anda agar saran pedagogis, interpretasi, dan pembahasan yang dihasilkan relevan dan kontekstual.\n",
    "--------------------------------------------------------------------\n"
  )
}

buildModelReportPrompt <- function(models, metadata = NULL, reference_text = NULL, research_context = NULL) {
  models_json <- jsonlite::toJSON(models, auto_unbox = TRUE, pretty = TRUE)
  meta_prompt <- formatMetadataPrompt(metadata)
  ref_prompt  <- formatReferencePrompt(reference_text)
  res_prompt  <- formatResearchContextPrompt(research_context)

  paste0(
    "Anda adalah asisten ahli psikometri sekaligus rekan diskusi tepercaya bagi peneliti dalam menganalisis Cognitive Diagnosis Model (CDM).

Berikut adalah hasil estimasi beberapa model CDM:

", models_json, "
", meta_prompt, "
", ref_prompt, "
", res_prompt, "

Tugas Anda adalah:

1. Bandingkan seluruh model menggunakan indeks kecocokan relatif (AIC, BIC, Deviance).
2. Interpretasikan indeks kecocokan absolut (M2, RMSEA2, SRMSR).
3. Interpretasikan indeks reliabilitas (Classification Accuracy/Test-level dan Attribute-level).
4. Tentukan model terbaik berdasarkan bukti statistik yang kuat.
5. Jelaskan alasan pemilihan model secara akademik dan metodologis.
6. Berikan kesimpulan akhir yang dapat digunakan dalam laporan penelitian atau publikasi jurnal.
7. Anda boleh menyertakan tabel Markdown sederhana jika sangat berguna mempermudah perbandingan model fit secara visual.
8. Selalu enter untuk pemisah section.
9. Jangan menulis teks sebelum heading pertama.

Gunakan bahasa Indonesia ilmiah yang komunikatif dan kolaboratif, layaknya seorang rekan sejawat (colleague) yang menjelaskan hasil analisis secara hangat namun tetap mempertahankan akurasi akademik yang tinggi. Hindari sapaan robotik atau frasa pembuka/penutup klise khas AI (seperti 'Tentu, ini analisis Anda...', 'Sebagai model AI...'). Rujuklah kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK (bila ada). Jangan pernah mengarang atau menambahkan referensi yang tidak terdapat di dokumen yang diberikan.

Susun laporan dalam format Markdown sebanyak 250 kata dengan struktur berikut:

## A. Interpretasi Kecocokan Relatif
## B. Interpretasi Kecocokan Absolut
## C. Interpretasi Reliabilitas
## D. Pemilihan Model Terbaik
## E. Kesimpulan Akhir

Tulisan harus objektif, berbasis teori psikometri, dan sesuai dengan praktik analisis CDM dalam literatur akademik."
  )
}

buildItemReportPrompt <- function(model_name, parameters, metadata = NULL, reference_text = NULL, research_context = NULL) {
  params_json <- jsonlite::toJSON(parameters, auto_unbox = TRUE, pretty = TRUE)
  meta_prompt <- formatMetadataPrompt(metadata)
  ref_prompt  <- formatReferencePrompt(reference_text)
  res_prompt  <- formatResearchContextPrompt(research_context)

  paste0(
    "Anda adalah asisten ahli psikometri sekaligus rekan diskusi tepercaya bagi peneliti dalam menganalisis Cognitive Diagnosis Model (CDM).

Berikut adalah parameter butir soal dari model ", model_name, ":

", params_json, "
", meta_prompt, "
", ref_prompt, "
", res_prompt, "

Tugas Anda adalah menginterpretasikan parameter butir soal tersebut secara akademik:

1. Interpretasikan probabilitas respons benar untuk setiap pola penguasaan atribut.
2. Interpretasikan indeks diskriminasi (delta_p dan GDI) untuk setiap butir.
3. Identifikasi butir dengan kualitas diskriminasi tinggi dan rendah.
4. Berikan rekomendasi perbaikan butir yang kurang baik.
5. Jangan menulis teks sebelum heading pertama.

Gunakan bahasa Indonesia ilmiah yang komunikatif dan kolaboratif, layaknya seorang rekan sejawat yang menjelaskan hasil analisis secara hangat dan bersahabat. Hindari kalimat pembuka/penutup klise khas AI. Tulis kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK (bila ada). Jangan menggunakan atau mengarang referensi lain di luar dokumen tersebut.

Ketika menginterpretasikan parameter butir (seperti tingkat kesukaran, Slip, dan Guessing), jangan hanya membaca angka statistika kering, tetapi berikan analisis diagnostik kognitif yang mendalam:
- Jika parameter Slip tinggi (> 0.20), analisis apakah terdapat ambiguitas pada kalimat soal atau faktor kecerobohan siswa yang telah menguasai konsep.
- Jika parameter Guessing tinggi (> 0.20), analisis apakah pilihan pengecoh (distractors) kurang berfungsi atau butir soal terlalu mudah ditebak secara logis tanpa memerlukan penguasaan atribut.

Format Markdown, sekitar 200 kata.

## A. Interpretasi Probabilitas Respons
## B. Interpretasi Indeks Diskriminasi
## C. Identifikasi Kualitas Butir
## D. Rekomendasi"
  )
}

buildProfilPrompt <- function(model_name, mastery_prob, mastery_prop_eap, mastery_prop_map,
                              mastery_prop_mle, latent_class, metadata = NULL, reference_text = NULL, research_context = NULL) {
  profil_json <- jsonlite::toJSON(
    list(
      attribute_prevalence   = mastery_prob,
      proportion_eap         = mastery_prop_eap,
      proportion_map         = mastery_prop_map,
      proportion_mle         = mastery_prop_mle,
      latent_class           = latent_class
    ),
    auto_unbox = TRUE, pretty = TRUE
  )
  meta_prompt <- formatMetadataPrompt(metadata)
  ref_prompt  <- formatReferencePrompt(reference_text)
  res_prompt  <- formatResearchContextPrompt(research_context)

  paste0(
    "Anda adalah asisten ahli psikometri sekaligus rekan diskusi tepercaya bagi peneliti dalam menganalisis Cognitive Diagnosis Model (CDM).

Berikut adalah profil penguasaan atribut peserta didik berdasarkan model ", model_name, ":

", profil_json, "
", meta_prompt, "
", ref_prompt, "
", res_prompt, "

Keterangan data:
- attribute_prevalence: proporsi penguasaan atribut dari parameter struktural model (prevalensi teoritis)
- proportion_eap: proporsi master per atribut berdasarkan EAP (Expected A Posteriori)
- proportion_map: proporsi master per atribut berdasarkan MAP (Maximum A Posteriori)
- proportion_mle: proporsi master per atribut berdasarkan MLE (Maximum Likelihood Estimation)
- latent_class: distribusi pola penguasaan (profil laten) dalam populasi

Tugas Anda:

1. Interpretasikan attribute_prevalence dan bandingkan dengan proportion_eap/MAP/MLE — apakah konsisten?
2. Deskripsikan profil laten dominan yang terbentuk beserta maknanya.
3. Jelaskan implikasi pedagogis dari pola penguasaan yang ditemukan.
4. Berikan rekomendasi pembelajaran berbasis profil atribut.
5. Jangan menulis teks sebelum heading pertama.

Gunakan bahasa Indonesia ilmiah yang komunikatif dan kolaboratif, layaknya seorang rekan sejawat yang menjelaskan profil laten siswa secara hangat dan bersahabat. Hindari kalimat pembuka/penutup klise khas AI. Tulis kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK (bila ada). Jangan mengarang referensi di luar dokumen tersebut.

Hubungkan profil latent dominan dengan deskripsi kompetensi/kurikulum yang didefinisikan dalam variabel kustom di metadata. Berikan rekomendasi pembelajaran remedial atau pengayaan secara konkret, sistematis, dan mudah diaplikasikan oleh guru di kelas.

Format Markdown, sekitar 250 kata.

## A. Profil Penguasaan Atribut
## B. Profil Laten Dominan
## C. Implikasi Pedagogis
## D. Rekomendasi Pembelajaran"
  )
}

buildProfilIndividuPrompt <- function(model_name, selected_persons, metadata = NULL, reference_text = NULL, research_context = NULL) {
  persons_json <- jsonlite::toJSON(selected_persons, auto_unbox = TRUE, pretty = TRUE)
  meta_prompt  <- formatMetadataPrompt(metadata)
  ref_prompt   <- formatReferencePrompt(reference_text)
  res_prompt   <- formatResearchContextPrompt(research_context)

  paste0(
    "Anda adalah asisten ahli psikometri sekaligus rekan diskusi tepercaya bagi peneliti dalam menganalisis Cognitive Diagnosis Model (CDM).

Berikut adalah profil individu peserta didik yang dipilih berdasarkan model ", model_name, ":

", persons_json, "
", meta_prompt, "
", ref_prompt, "
", res_prompt, "

Keterangan data:
- id: nomor urut responden
- totalScore: skor total butir soal
- pattern: pola penguasaan atribut (MAP, binary 0/1)
- mp: marginal probability penguasaan setiap atribut (nilai kontinu 0-1, digunakan pada radar chart)

Tugas Anda:

1. Deskripsikan profil kognitif masing-masing peserta didik secara individual berdasarkan mp and pattern.
2. Jika lebih dari satu peserta dipilih, bandingkan kesamaan dan perbedaan profil antar individu.
3. Identifikasi atribut yang sudah dikuasai (mp tinggi) dan yang masih perlu ditingkatkan (mp rendah).
4. Berikan rekomendasi intervensi pembelajaran yang spesifik dan personal untuk masing-masing peserta.
5. Jangan menulis teks sebelum heading pertama.

Gunakan bahasa Indonesia ilmiah yang komunikatif dan kolaboratif, layaknya seorang rekan sejawat yang berdiskusi secara hangat dan bersahabat. Hindari kalimat pembuka/penutup klise khas AI. Tulis kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK (bila ada). Jangan pernah mengarang atau menuliskan referensi lain.

Deskripsikan profil kognitif individu siswa dengan fokus pada kekuatan (kompetensi yang dikuasai) dan kelemahan (kompetensi yang belum dikuasai). Berikan rekomendasi intervensi personal yang konkret, taktis, dan spesifik untuk membantu siswa tersebut berkembang.

Format Markdown, sekitar 200 kata.

## A. Profil Kognitif Individu
## B. Perbandingan Antar Responden
## C. Atribut yang Perlu Ditingkatkan
## D. Rekomendasi Intervensi Personal"
  )
}

buildChatSystemPrompt <- function(cdm_context = NULL, reference_text = NULL, metadata = NULL, research_context = NULL) {
  base <- paste0(
    "Anda adalah asisten ahli analisis CDM (Cognitive Diagnosis Model) sekaligus rekan diskusi dan konsultan psikometri tepercaya bagi peneliti dalam menyusun naskah jurnal internasional bereputasi (seperti terindeks Scopus). ",
    "Gunakan bahasa Indonesia ilmiah yang sangat komunikatif, bersahabat, dan kolaboratif, layaknya teman sejawat yang ahli dalam analisis data. ",
    "Hindari kalimat pembuka/penutup klise khas AI. Gunakan sudut pandang orang pertama jamak (seperti 'kita', 'mari kita lihat') untuk menciptakan suasana kerja sama yang erat. ",
    "Berikan rujukan dan kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK (bila ada). Jangan pernah mengarang atau mereferensikan dokumen lain di luar dokumen tersebut."
  )

  guidelines <- paste0(
    "\n\nPedoman Penting untuk Menjawab:",
    "\n1. Jawablah pertanyaan peneliti secara LANGSUNG, spesifik, dan tepat sasaran sesuai konteks pertanyaan. JANGAN memulai dengan tinjauan umum kecocokan model (model fit) atau evaluasi model jika pertanyaan peneliti membahas tentang hal lain seperti profil mastery siswa atau parameter butir soal.",
    "\n2. Gunakan data konkret dari 'Konteks hasil analisis CDM' yang disediakan di bawah ini (misalnya tingkat penguasaan atribut/attribute mastery, proporsi kelas laten, atau indeks diskriminasi butir) untuk mendukung jawaban Anda secara kuantitatif.",
    "\n3. Rujuklah dokumen referensi akademik kustom yang disediakan (bila ada) untuk memperkuat argumen Anda dengan teori psikometri yang valid. Sebutkan secara eksplisit nama dokumen/artikel rujukan tersebut dan jelaskan hubungannya dengan temuan analisis Anda secara ilmiah menggunakan format APA Style Edisi Ke-7 dengan HANYA menggunakan dokumen yang benar-benar tercantum dalam DOKUMEN REFERENSI AKADEMIK.",
    "\n4. Sajikan jawaban secara ringkas, analitis, and profesional."
  )

  context_section <- if (!is.null(cdm_context) && length(cdm_context) > 0) {
    ctx_json <- jsonlite::toJSON(cdm_context, auto_unbox = TRUE, pretty = TRUE)
    paste0("\n\nKonteks hasil analisis CDM yang sedang dibahas:\n```json\n", ctx_json, "\n```")
  } else ""

  ref_prompt <- formatReferencePrompt(reference_text)
  meta_prompt <- formatMetadataPrompt(metadata)
  res_prompt  <- formatResearchContextPrompt(research_context)

  paste0(base, guidelines, context_section, ref_prompt, meta_prompt, res_prompt)
}

#' Build Q-Matrix Validation Report Prompt
#' @export
buildQvalReportPrompt <- function(suggestions, threshold, metadata = NULL, reference_text = NULL, research_context = NULL) {
  sug_json <- jsonlite::toJSON(suggestions, auto_unbox = TRUE, pretty = TRUE)
  meta_prompt <- formatMetadataPrompt(metadata)
  ref_prompt  <- formatReferencePrompt(reference_text)
  res_prompt  <- formatResearchContextPrompt(research_context)

  paste0(
    "Anda adalah asisten ahli psikometri sekaligus rekan diskusi tepercaya bagi peneliti dalam menganalisis Cognitive Diagnosis Model (CDM).

Berikut adalah hasil validasi empiris Q-Matrix menggunakan metode PVAF (Proportion of Variance Accounted For) dengan threshold ", threshold, ":

", sug_json, "
", meta_prompt, "
", ref_prompt, "
", res_prompt, "

Tugas Anda adalah:

1. Ringkas hasil validasi empiris Q-Matrix (berapa butir yang optimal, berapa butir yang disarankan untuk direvisi).
2. Analisis secara mendalam butir-butir yang disarankan berubah. Jelaskan mengapa penambahan atau penghapusan atribut direkomendasikan berdasarkan nilai PVAF-nya.
3. Berikan saran metodologis terkait bagaimana peneliti harus mendiskusikan temuan ini dengan ahli materi (content expert) sebelum menerapkan revisi.
4. Jelaskan implikasi psikometris jika perubahan ini diterapkan (misalnya pengaruhnya terhadap model fit dan klasifikasi peserta didik).
5. Jangan menulis teks sebelum heading pertama.

Gunakan bahasa Indonesia ilmiah yang komunikatif dan kolaboratif, layaknya seorang rekan sejawat yang berdiskusi secara hangat dan bersahabat. Hindari kalimat pembuka/penutup klise khas AI. Rujuk kutipan referensi ilmiah secara konsisten menggunakan format APA Style 7th Edition dengan HANYA merujuk pada dokumen yang benar-benar tercantum di bagian DOKUMEN REFERENSI AKADEMIK. Jangan mengarang referensi di luar dokumen yang disediakan.

Susun laporan dalam format Markdown sebanyak 250 kata dengan struktur berikut:

## A. Ringkasan Evaluasi Q-Matrix
## B. Analisis Butir yang Disarankan Revisi
## C. Rekomendasi Tindak Lanjut Akademik
## D. Implikasi Psikometris

Tulisan harus objektif, berbasis teori psikometri, dan sesuai dengan praktik analisis CDM dalam literatur akademik."
  )
}

