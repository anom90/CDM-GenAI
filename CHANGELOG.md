# Changelog

Seluruh perubahan penting pada **CDM-GenAI** didokumentasikan di berkas ini.
Format mengikuti [Keep a Changelog](https://keepachangelog.com/),
dan proyek ini menggunakan [Semantic Versioning](https://semver.org/lang/id/).

## [0.1.21] — 2026-06-19

### Ditambahkan
- Dokumentasi README profesional: panduan instalasi R & RStudio (Windows & macOS),
  cara mengaktifkan fitur AI (Google Gemini API Key), dan bagian sitasi.
- `CITATION.cff` + DOI Zenodo (10.5281/zenodo.20758637) → tombol
  "Cite this repository" di GitHub.
- Berkas `LICENSE`/`LICENSE.md` (MIT) dan `CHANGELOG.md`.

### Diperbaiki
- Dialog "Pratinjau & Konfigurasi" kini dibatasi tinggi & dapat di-scroll,
  sehingga tidak terpotong pada layar kecil atau display scaling Windows.
- Grid pemilihan model menjadi responsif (form unggah & validasi Q-Matrix).
- Menyenyapkan warning informatif `GDINA::CA()` ("multiple modes") di console R.

## [0.1.20] — 2026
- Injeksi pemetaan Q-Matrix ke dalam prompt AI; perbaikan deduplikasi model & UI.

## [0.1.17] — 2026
- Pengamanan estimasi GDINA dengan `tryCatch`/retry.

## [0.1.16] — 2026
- Pemulihan heatmap bivariat & penamaan butir yang aman terhadap array (kesesuaian butir).

[0.1.21]: https://github.com/anom90/CDM-GenAI/releases/tag/v0.1.21
