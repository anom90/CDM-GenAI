# cdm.gen.ai

**CDM-GenAI** — Aplikasi analisis *Cognitive Diagnosis Model* (CDM) berbasis R
dengan antarmuka web (Next.js) dan integrasi AI generatif (Gemini).

Aplikasi ini membantu Anda melakukan analisis CDM secara menyeluruh: mulai dari
unggah data & Q-Matrix, estimasi model (GDINA, DINA, DINO, RRUM, ACDM),
validasi Q-Matrix, parameter butir, profil peserta, hingga penyusunan draf laporan
dengan bantuan AI.

---

## Persyaratan

- **R** versi 4.1 atau lebih baru.
- Koneksi internet (untuk instalasi paket dan fitur AI).

## Instalasi

Aplikasi ini diinstal langsung dari GitHub menggunakan paket **`remotes`**.

### 1. Pasang paket `remotes` (sekali saja)

`remotes` adalah paket alat bantu yang dibutuhkan **untuk mengunduh dan memasang**
aplikasi dari GitHub. Paket ini ringan dan hanya dipakai saat proses instalasi
(bukan saat aplikasi berjalan).

```r
install.packages("remotes")
```

> Jika Anda menggunakan `devtools::install_github()`, paket `remotes` tetap
> dibutuhkan karena `devtools` memanggilnya di balik layar. Memakai `remotes`
> secara langsung lebih ringan dan cepat.

### 2. Pasang aplikasi cdm.gen.ai

```r
remotes::install_github("anom90/CDM-GenAI")
```

Proses ini akan ikut memasang seluruh paket dependensi yang dibutuhkan
(misalnya `GDINA`, `cdmTools`, `plumber`, dll.), sehingga pada instalasi pertama
mungkin memerlukan waktu beberapa menit.

> **Pemasangan ulang / pembaruan.** Jika versi di GitHub sudah berubah namun R
> melaporkan *"Skipping install ... SHA1 has not changed"*, paksa pemasangan ulang:
>
> ```r
> remotes::install_github("anom90/CDM-GenAI", force = TRUE)
> ```

## Menjalankan Aplikasi

```r
library(cdm.gen.ai)
run_app()
```

Aplikasi akan berjalan di `http://localhost:8000` dan otomatis terbuka di browser.

## Penggunaan Singkat

1. Buka menu **Data**, lalu unggah berkas Excel (`.xlsx`) berisi data respons
   dan Q-Matrix, atau muat berkas cadangan proyek (`.json`).
2. Verifikasi data pada dialog **Pratinjau & Konfigurasi**, atur model yang akan
   diestimasi, lalu jalankan analisis.
3. Telusuri hasil melalui menu **Fit Model**, **Validasi Q-Matrix**,
   **Parameter Butir**, dan **Profil Peserta**.
4. Gunakan **Asisten Analisis** dan **Draft Laporan** untuk interpretasi berbantuan AI.

## Catatan

- Jika `cdmTools` gagal dimuat karena konflik versi `CVXR`, jalankan:

  ```r
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  remotes::install_version("CVXR", version = "1.0-15", upgrade = "never")
  ```

## Lisensi

MIT © Kartianom
