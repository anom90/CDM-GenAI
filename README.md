<div align="center">

# 🧠 CDM-GenAI

### Analisis *Cognitive Diagnosis Model* berbasis R, dengan antarmuka web modern & berbantuan AI

[![Release](https://img.shields.io/github/v/release/anom90/CDM-GenAI?style=for-the-badge&color=00685f&label=release)](https://github.com/anom90/CDM-GenAI/releases)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-00685f?style=for-the-badge)](#-lisensi)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20758637-1682D4?style=for-the-badge)](https://doi.org/10.5281/zenodo.20758637)

[![Last commit](https://img.shields.io/github/last-commit/anom90/CDM-GenAI?style=flat-square&color=00685f)](https://github.com/anom90/CDM-GenAI/commits)
[![Stars](https://img.shields.io/github/stars/anom90/CDM-GenAI?style=flat-square&color=00685f)](https://github.com/anom90/CDM-GenAI/stargazers)
[![Made with R & Next.js](https://img.shields.io/badge/made%20with-R%20%26%20Next.js-00685f?style=flat-square)](#-teknologi)

<br/>

**Unggah data → Estimasi model → Validasi → Interpretasi AI → Laporan.**
Semua dalam satu aplikasi yang berjalan lokal di komputer Anda.

</div>

---

## 📑 Daftar Isi

- [Tentang](#-tentang)
- [Fitur Utama](#-fitur-utama)
- [Teknologi](#%EF%B8%8F-teknologi)
- [Tangkapan Layar](#-tangkapan-layar)
- [Persyaratan](#-persyaratan)
- [Langkah 1 — Pasang R & RStudio](#-langkah-1--pasang-r--rstudio)
- [Langkah 2 — Pasang Aplikasi](#-langkah-2--pasang-aplikasi)
- [Langkah 3 — Jalankan](#%EF%B8%8F-langkah-3--jalankan)
- [Alur Penggunaan Singkat](#-alur-penggunaan-singkat)
- [Mengaktifkan Fitur AI](#-mengaktifkan-fitur-ai-google-gemini)
- [Pemecahan Masalah](#-pemecahan-masalah)
- [Sitasi](#-sitasi)
- [Referensi & Penghargaan](#-referensi--penghargaan)
- [Lisensi](#-lisensi)

---

## ✨ Tentang

**CDM-GenAI** adalah aplikasi analisis *Cognitive Diagnosis Model* (CDM) yang
memadukan mesin statistik **R** dengan antarmuka web **Next.js** dan integrasi
**AI generatif (Gemini)**. Aplikasi dijalankan langsung dari R dan terbuka di
browser — tanpa server eksternal, data tetap berada di komputer Anda.

## 🚀 Fitur Utama

| | Fitur | Deskripsi |
|---|---|---|
| 📊 | **Manajemen Data & Q-Matrix** | Unggah data respons & Q-Matrix (`.xlsx`) atau muat cadangan proyek (`.json`). |
| 🧩 | **Identifikasi Q-Matrix** | Pemeriksaan identifiabilitas Q-Matrix (`cdmTools`). |
| ⚙️ | **Estimasi Model** | GDINA, DINA, DINO, RRUM, ACDM dengan opsi *monotonicity constraint*. |
| ✅ | **Validasi Q-Matrix** | Saran perbaikan & estimasi ulang Q-Matrix. |
| 📐 | **Parameter Butir** | Diskriminasi & probabilitas penguasaan tiap butir. |
| 👥 | **Profil Peserta** | Pola penguasaan atribut per individu (MAP/EAP/MLE). |
| 🤖 | **Asisten & Draf Laporan AI** | Interpretasi otomatis dan penyusunan laporan berbantuan AI. |

## 🛠️ Teknologi

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)
![Plumber](https://img.shields.io/badge/Plumber_API-15616D?style=flat-square)
![Next.js](https://img.shields.io/badge/Next.js-000000?style=flat-square&logo=nextdotjs&logoColor=white)
![Tailwind CSS](https://img.shields.io/badge/Tailwind_CSS-06B6D4?style=flat-square&logo=tailwindcss&logoColor=white)
![Gemini](https://img.shields.io/badge/Google_Gemini-8E75B2?style=flat-square&logo=googlegemini&logoColor=white)

---

## 🖼️ Tangkapan Layar

Antarmuka CDM-GenAI dirancang bersih, modern, dan responsif — dari manajemen data
& Q-Matrix, dialog pratinjau analisis, hingga visualisasi hasil estimasi model.

<!--
  GALERI SCREENSHOT — siap dipakai.
  Taruh 3 berkas gambar berikut di folder man/figures/, lalu HAPUS baris
  pembuka "<!--" dan penutup "-->" ini agar galeri tampil:
    - man/figures/screenshot-data.png      (Halaman Data & Q-Matrix)
    - man/figures/screenshot-preview.png   (Dialog Pratinjau & Konfigurasi)
    - man/figures/screenshot-hasil.png     (Hasil analisis / Fit Model)

<table>
  <tr>
    <td width="33%"><img src="man/figures/screenshot-data.png" alt="Halaman Data & Q-Matrix"/></td>
    <td width="33%"><img src="man/figures/screenshot-preview.png" alt="Dialog Pratinjau & Konfigurasi"/></td>
    <td width="33%"><img src="man/figures/screenshot-hasil.png" alt="Hasil analisis"/></td>
  </tr>
  <tr align="center">
    <td><sub>Data & Q-Matrix</sub></td>
    <td><sub>Pratinjau & Konfigurasi</sub></td>
    <td><sub>Hasil Analisis</sub></td>
  </tr>
</table>
-->

---

## 📋 Persyaratan

- **R** versi 4.1 atau lebih baru
- **RStudio** (disarankan, sebagai antarmuka untuk menjalankan perintah R)
- Koneksi internet (untuk instalasi paket & fitur AI)

## 💻 Langkah 1 — Pasang R & RStudio

> Lewati bagian ini jika R dan RStudio sudah terpasang. **Pasang R lebih dulu, baru RStudio.**

<details open>
<summary><b>Pasang R</b></summary>

<br/>

| Sistem Operasi | Tautan Unduhan | Catatan |
| -------------- | -------------- | ------- |
| 🪟 **Windows** | [cran.r-project.org/bin/windows/base](https://cran.r-project.org/bin/windows/base/) | Unduh installer `.exe`, jalankan, ikuti proses standar. |
| 🍎 **macOS** | [cran.r-project.org/bin/macosx](https://cran.r-project.org/bin/macosx/) | Pilih `.pkg` sesuai prosesor (Apple Silicon M1–M4 atau Intel). |

</details>

<details open>
<summary><b>Pasang RStudio Desktop</b></summary>

<br/>

Unduh **RStudio Desktop** (gratis) untuk Windows / macOS:

> 🔗 **[posit.co/downloads](https://posit.co/downloads)**

Setelah terpasang, buka **RStudio** dan jalankan seluruh perintah berikut melalui jendela **Console**.

</details>

## 📦 Langkah 2 — Pasang Aplikasi

**1. Pasang paket `remotes`** (sekali saja). Paket ini ringan dan hanya dipakai
saat proses instalasi dari GitHub — bukan saat aplikasi berjalan.

```r
install.packages("remotes")
```

**2. Pasang `cdm.gen.ai` dari GitHub:**

```r
remotes::install_github("anom90/CDM-GenAI")
```

> 💡 Instalasi pertama ikut memasang dependensi (`GDINA`, `cdmTools`, `plumber`, dll.),
> sehingga dapat memakan waktu beberapa menit.

<details>
<summary>🔄 <b>Memperbarui ke versi terbaru</b></summary>

<br/>

Jika R melaporkan *"Skipping install ... SHA1 has not changed"*, paksa pemasangan ulang:

```r
remotes::install_github("anom90/CDM-GenAI", force = TRUE)
```

</details>

## ▶️ Langkah 3 — Jalankan

```r
library(cdm.gen.ai)
run_app()
```

Aplikasi berjalan di **`http://localhost:8000`** dan otomatis terbuka di browser. 🎉

---

## 🧭 Alur Penggunaan Singkat

1. Buka menu **Data** → unggah `.xlsx` (data respons + Q-Matrix) atau muat proyek `.json`.
2. Verifikasi pada dialog **Pratinjau & Konfigurasi**, pilih model, lalu jalankan analisis.
3. Telusuri hasil di **Fit Model**, **Validasi Q-Matrix**, **Parameter Butir**, **Profil Peserta**.
4. Manfaatkan **Asisten Analisis** & **Draft Laporan** untuk interpretasi berbantuan AI.

> 💡 **Baru pertama kali / belum punya data?**
> - **Coba Dataset Contoh** — di halaman **Data**, klik **"Gunakan Dataset Contoh"**.
>   Aplikasi langsung memuat dataset bawaan sehingga Anda bisa menjelajahi seluruh
>   fitur tanpa perlu menyiapkan berkas sendiri.
> - **Unduh Template Excel** — pada tombol unggah, tersedia tautan
>   **"Unduh Template Excel (.xlsx)"**. Gunakan template ini sebagai acuan format
>   penulisan data respons & Q-Matrix sebelum mengunggah data Anda sendiri.

### 📑 Format Berkas Excel

Berkas `.xlsx` yang diunggah sebaiknya mengikuti struktur pada template:

- **Data respons** — baris = responden, kolom = butir soal (nilai biner `0`/`1`).
- **Q-Matrix** — baris = butir soal, kolom = atribut/kompetensi (nilai biner `0`/`1`).
- *(Opsional)* **Metadata** — label & deskripsi kustom untuk butir dan atribut,
  yang akan dimuat otomatis saat hasil ditampilkan.

## 🔑 Mengaktifkan Fitur AI (Google Gemini)

Fitur berbantuan AI — **Asisten Analisis**, **Draft Laporan**, dan interpretasi
otomatis — memerlukan **API Key Google Gemini**. Key bersifat pribadi, gratis
untuk penggunaan dasar, dan disimpan lokal di peramban Anda (dikirim hanya saat
memanggil layanan AI).

1. **Dapatkan API Key** di **Google AI Studio**:
   > 🔗 **[aistudio.google.com/app/apikey](https://aistudio.google.com/app/apikey)**

   Masuk dengan akun Google → **Create API key** → salin key yang dihasilkan.

2. **Masukkan ke aplikasi** — buka menu **Setting AI**, tempel API Key pada kolom
   yang tersedia, lalu **simpan**.

3. Fitur AI kini aktif. 🎉 Anda dapat menggunakan **Asisten Analisis** dan
   menghasilkan **Draft Laporan** otomatis.

> ⚠️ **Jaga kerahasiaan API Key Anda** — jangan dibagikan atau di-commit ke
> repositori publik. Model yang digunakan adalah `gemini-2.5-flash`.

## 🩹 Pemecahan Masalah

<details>
<summary><b><code>cdmTools</code> gagal dimuat karena konflik versi <code>CVXR</code></b></summary>

<br/>

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
remotes::install_version("CVXR", version = "1.0-15", upgrade = "never")
```

</details>

<details>
<summary><b>Fitur AI menampilkan error "API Key tidak valid" atau "kuota tercapai"</b></summary>

<br/>

- **API Key tidak valid** — pastikan key disalin lengkap dan benar di menu
  **Setting AI**. Buat key baru di
  [Google AI Studio](https://aistudio.google.com/app/apikey) bila perlu.
- **Kuota tercapai / High Demand** — batas permintaan gratis sedang penuh.
  Tunggu beberapa saat lalu coba lagi.

</details>

---

## 📚 Sitasi

Jika Anda menggunakan **CDM-GenAI** dalam riset atau publikasi, mohon sitasi karya ini.
Pada halaman repositori GitHub, tersedia tombol **"Cite this repository"** (di panel
kanan) yang menghasilkan sitasi otomatis dari berkas [`CITATION.cff`](CITATION.cff).

**Format APA:**

> Kartianom, Hadi, S., Retnawati, H., & Hidayati, K. (2026). *CDM-GenAI: Cognitive Diagnosis Model Analysis with Generative AI* (v0.1.21). Zenodo. https://doi.org/10.5281/zenodo.20758637

**BibTeX:**

```bibtex
@software{kartianom2026cdmgenai,
  author    = {Kartianom and Hadi, Samsul and Retnawati, Heri and Hidayati, Kana},
  title     = {{CDM-GenAI: Cognitive Diagnosis Model Analysis with Generative AI}},
  year      = {2026},
  version   = {0.1.21},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.20758637},
  url       = {https://doi.org/10.5281/zenodo.20758637},
  note      = {Produk disertasi doktoral, Program Studi Doktor PEP,
               Universitas Negeri Yogyakarta}
}
```

---

## 🙏 Referensi & Penghargaan

CDM-GenAI berdiri di atas paket-paket riset psikometri sumber terbuka. Mohon
sitasi juga paket inti yang relevan saat melaporkan hasil analisis Anda:

- **GDINA** — Ma, W., & de la Torre, J. (2020). GDINA: An R package for
  cognitive diagnosis modeling. *Journal of Statistical Software, 93*(14), 1–26.
  https://doi.org/10.18637/jss.v093.i14
- **cdmTools** — Nájera, P., Sorrel, M. A., & Abad, F. J. *cdmTools: Useful Tools
  for Cognitive Diagnosis Modeling* [Paket R]. CRAN.
  https://CRAN.R-project.org/package=cdmTools

> 💡 Untuk format sitasi paket yang paling mutakhir, jalankan di R:
> `citation("GDINA")` atau `citation("cdmTools")`.

Terima kasih pula kepada ekosistem yang menjadi fondasi aplikasi ini:
[plumber](https://www.rplumber.io/), [Next.js](https://nextjs.org/),
[Tailwind CSS](https://tailwindcss.com/), dan [Google Gemini](https://ai.google.dev/).

Riwayat perubahan lengkap tersedia di [CHANGELOG.md](CHANGELOG.md).

---

<div align="center">

## 📄 Lisensi

**MIT** © 2026 [Kartianom](mailto:kartianom@gmail.com), Samsul Hadi, Heri Retnawati & Kana Hidayati

<sub>Dibuat untuk analisis diagnostik kognitif yang lebih mudah dan cerdas.</sub>

</div>
