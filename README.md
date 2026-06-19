<div align="center">

# 🧠 CDM-GenAI

### Analisis *Cognitive Diagnosis Model* berbasis R, dengan antarmuka web modern & berbantuan AI

[![Version](https://img.shields.io/badge/version-0.1.21-00685f?style=for-the-badge)](https://github.com/anom90/CDM-GenAI)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-00685f?style=for-the-badge)](#-lisensi)

<br/>

**Unggah data → Estimasi model → Validasi → Interpretasi AI → Laporan.**
Semua dalam satu aplikasi yang berjalan lokal di komputer Anda.

</div>

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

## 🩹 Pemecahan Masalah

<details>
<summary><b><code>cdmTools</code> gagal dimuat karena konflik versi <code>CVXR</code></b></summary>

<br/>

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
remotes::install_version("CVXR", version = "1.0-15", upgrade = "never")
```

</details>

---

<div align="center">

## 📄 Lisensi

**MIT** © [Kartianom](mailto:kartianom@gmail.com)

<sub>Dibuat untuk analisis diagnostik kognitif yang lebih mudah dan cerdas.</sub>

</div>
