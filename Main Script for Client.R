setwd("C:/Users/Nela Aidatun/Downloads")

library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)

# =================== Parameter ===================
inflasi <- 0.0157  
bunga <- 0.0713
pensiun <- 55
proporsi <- 0.03
KenaikanGaji <- 0.06
v <- 1 / (1 + bunga)
TahunMasuk <- 2020
TahunValuasi<-2024
# =================== Baca Data ===================
data_peserta <- read_excel("data pensiun.xlsx")
tmi_iv <- read_excel("TM IV.xlsx")
lc_laki <- read_excel("TM LC.xlsx", sheet = "Laki-laki")
lc_perempuan <- read_excel("TM LC.xlsx", sheet = "Perempuan")

# --- Visualisasi 1: Grafik Garis tmi_iv ---
tmi_iv_long <- tmi_iv %>%
  pivot_longer(cols = starts_with("qx"), names_to = "Jenis_Kelamin", values_to = "qx")

plot1 <- ggplot(tmi_iv_long, aes(x = Usia, y = qx, color = Jenis_Kelamin)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(title = "Probabilitas Kematian (qx) menurut Usia - TMI IV",
       x = "Usia", y = "qx (log scale)",
       color = "Jenis Kelamin")

ggsave("plot_tmi_iv.png", plot = plot1, width = 8, height = 6, dpi = 300)

# --- Visualisasi 2: Line Plot lc_laki Tahun 2019 dan 2075 ---
lc_laki_subset <- lc_laki %>%
  select(Usia, `2019`, `2075`) %>%
  pivot_longer(cols = -Usia, names_to = "Tahun", values_to = "qx") %>%
  mutate(Tahun = as.factor(Tahun))

plot2 <- ggplot(lc_laki_subset, aes(x = Usia, y = qx, color = Tahun)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(title = "Probabilitas Kematian Laki-laki (Lee-Carter)\nTahun 2019 vs 2075",
       x = "Usia", y = "qx (log scale)", color = "Tahun")

ggsave("plot_line_laki.png", plot = plot2, width = 8, height = 6, dpi = 300)

# --- Visualisasi 3: Line Plot lc_perempuan Tahun 2019 dan 2075 ---
lc_perempuan_subset <- lc_perempuan %>%
  select(Usia, `2019`, `2075`) %>%
  pivot_longer(cols = -Usia, names_to = "Tahun", values_to = "qx") %>%
  mutate(Tahun = as.factor(Tahun))

plot3 <- ggplot(lc_perempuan_subset, aes(x = Usia, y = qx, color = Tahun)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(title = "Probabilitas Kematian Perempuan (Lee-Carter)\nTahun 2019 vs 2075",
       x = "Usia", y = "qx (log scale)", color = "Tahun")

ggsave("plot_line_perempuan.png", plot = plot3, width = 8, height = 6, dpi = 300)


# =================== Tahap 1 ===================
wb <- createWorkbook()
for (i in 1:nrow(data_peserta)) {
  
  peserta <- data_peserta[i, ]
  usia_sekarang <- peserta$UsiaSekarang
  gaji_awal <- peserta$Gaji
  masa_kerja <- peserta$MasaKerja
  id <- as.character(peserta$Nama)  
  
  Tahun_Menuju_Pensiun <- (pensiun)-1-usia_sekarang
  t <- 0:Tahun_Menuju_Pensiun
  usia_t <- usia_sekarang + t
  tahun_proyeksi <- TahunValuasi + t
  
  # Gaji Proyeksi (G_t)
  G_t <- gaji_awal * ((1 + KenaikanGaji) * (1 + inflasi)) ^ t
  
  # Benefit (B_x)
  B_x <- proporsi * (pensiun - usia_sekarang) * G_t
  B_x[1] <- 0
  
  hasil <- data.frame(
    t = t,
    x = usia_t,
    Tahun = tahun_proyeksi,
    Gaji_Proyeksi = round(G_t, 2),
    Benefit = round(B_x, 2)
  )
  
  # Tambahkan ke workbook sebagai sheet baru
  addWorksheet(wb, sheetName = paste0("Peserta_", id))
  writeData(wb, sheet = paste0("Peserta_", id), x = hasil)
}
# Simpan workbook hasil Tahap 1 ke file Excel
saveWorkbook(wb, "hasil_Tahap1_Gaji_Benefit.xlsx", overwrite = TRUE)

# ========== Visualisasi Gaji Proyeksi dan Benefit per Peserta ==========
dir.create("visualisasi_gaji_benefit", showWarnings = FALSE)

for (i in 1:nrow(data_peserta)) {
  peserta <- data_peserta[i, ]
  usia_sekarang <- peserta$UsiaSekarang
  gaji_awal <- peserta$Gaji
  id <- as.character(peserta$Nama)
  
  Tahun_Menuju_Pensiun <- (pensiun) - 1 - usia_sekarang
  t <- 0:Tahun_Menuju_Pensiun
  usia_t <- usia_sekarang + t
  tahun_proyeksi <- TahunValuasi + t
  G_t <- gaji_awal * ((1 + KenaikanGaji) * (1 + inflasi)) ^ t
  B_x <- proporsi * (pensiun - usia_sekarang) * G_t
  
  df_plot <- data.frame(Tahun = tahun_proyeksi, Gaji = G_t, Benefit = B_x) %>%
    pivot_longer(cols = c("Gaji", "Benefit"), names_to = "Tipe", values_to = "Nilai")
  
  p <- ggplot(df_plot, aes(x = Tahun, y = Nilai, color = Tipe)) +
    geom_line(size = 1.2) +
    labs(title = paste("Proyeksi Gaji & Benefit:", id),
         x = "Tahun", y = "Nilai (Rp)") +
    scale_color_manual(values = c("Gaji" = "steelblue", "Benefit" = "firebrick"))
  
  ggsave(filename = paste0("visualisasi_gaji_benefit/", id, "_gaji_benefit.png"),
         plot = p, width = 8, height = 5)
}

# =================== Tahap 2: Perhitungan Aktuaria ===================

hitung_lx_dx_ax <- function(qx_vector, usia_awal = 0, usia_akhir = 110, v) {
  usia_range <- usia_awal:usia_akhir
  qx <- qx_vector[1:length(usia_range)]
  lx <- numeric(length(usia_range))
  dx <- numeric(length(usia_range))
  Dx <- numeric(length(usia_range))
  Nx <- numeric(length(usia_range))
  ax <- numeric(length(usia_range))
  axn <- numeric(length(usia_range))
  
  lx[1] <- 100000
  for (i in 2:length(lx)) {
    lx[i] <- lx[i - 1] * (1 - qx[i - 1])
  }
  
  dx <- lx * qx
  Dx <- v^(usia_range) * lx
  Nx <- rev(cumsum(rev(Dx)))
  ax <- Nx / Dx
  
  n_axn <- function(Nx, usia_idx, n) {
    if ((usia_idx + n) <= length(Nx)) {
      return((Nx[usia_idx] - Nx[usia_idx + n]) / Dx[usia_idx])
    } else {
      return((Nx[usia_idx]) / Dx[usia_idx])
    }
  }
  
  for (i in 1:length(usia_range)) {
    axn[i] <- n_axn(Nx, i, 110 - usia_range[i])
  }
  
  return(data.frame(
    Usia = usia_range,
    qx = round(qx, 6),
    lx = round(lx),
    dx = round(dx),
    Dx = round(Dx, 2),
    Nx = round(Nx, 2),
    ax = round(ax, 4),
    axn = round(axn, 4)
  ))
}

# ========= Fungsi Ubah Format TM LC ke Long =========
reshape_tm_lc <- function(df, gender) {
  names(df)[1] <- "Usia"
  df <- df %>% mutate(Usia = as.numeric(Usia))
  
  df_long <- df %>%
    pivot_longer(-Usia, names_to = "Tahun", values_to = "qx") %>%
    mutate(
      Tahun = as.numeric(Tahun),
      qx = as.numeric(qx),
      Gender = gender
    ) %>%
    arrange(Tahun, Usia)
  
  return(df_long)
}

# ========== TMI IV ========== 
tmi_iv_p <- tmi_iv %>% select(Usia, qx = qx_P)
tmi_iv_l <- tmi_iv %>% select(Usia, qx = qx_L)

hasil_tmi_p <- hitung_lx_dx_ax(tmi_iv_p$qx, usia_awal = min(tmi_iv_p$Usia), usia_akhir = max(tmi_iv_p$Usia), v)
hasil_tmi_l <- hitung_lx_dx_ax(tmi_iv_l$qx, usia_awal = min(tmi_iv_l$Usia), usia_akhir = max(tmi_iv_l$Usia), v)

# Simpan TMI IV ke Excel
wb1 <- createWorkbook()
addWorksheet(wb1, "TMI_IV_P")
writeData(wb1, "TMI_IV_P", hasil_tmi_p)

addWorksheet(wb1, "TMI_IV_L")
writeData(wb1, "TMI_IV_L", hasil_tmi_l)

saveWorkbook(wb1, "hasil_TMI_IV.xlsx", overwrite = TRUE)

# ========== TM LC PEREMPUAN ========== 
lc_perempuan_long <- reshape_tm_lc(lc_perempuan, "P")
wb2 <- createWorkbook()

for (thn in 2020:2046) {
  data_tahun <- lc_perempuan_long %>%
    filter(Tahun == thn) %>%
    arrange(Usia)
  
  hasil_tahun <- hitung_lx_dx_ax(data_tahun$qx, min(data_tahun$Usia), max(data_tahun$Usia), v)
  
  sheet_name <- paste0("Tahun_", thn)
  addWorksheet(wb2, sheet_name)
  writeData(wb2, sheet_name, hasil_tahun)
}

saveWorkbook(wb2, "hasil_TMLC_P_Tahun2020-2046.xlsx", overwrite = TRUE)

# ========== TM LC LAKI-LAKI ========== 
lc_laki_long <- reshape_tm_lc(lc_laki, "L")
wb3 <- createWorkbook()

for (thn in 2020:2046) {
  data_tahun <- lc_laki_long %>%
    filter(Tahun == thn) %>%
    arrange(Usia)
  
  hasil_tahun <- hitung_lx_dx_ax(data_tahun$qx, min(data_tahun$Usia), max(data_tahun$Usia), v)
  
  sheet_name <- paste0("Tahun_", thn)
  addWorksheet(wb3, sheet_name)
  writeData(wb3, sheet_name, hasil_tahun)
}
# Ambil qx tahun valuasi (misal 2024) dari LC
qx_lc_l <- lc_laki_long %>% filter(Tahun == TahunValuasi) %>% arrange(Usia)
qx_lc_p <- lc_perempuan_long %>% filter(Tahun == TahunValuasi) %>% arrange(Usia)

hasil_lc_l <- hitung_lx_dx_ax(qx_lc_l$qx, usia_awal = min(qx_lc_l$Usia), usia_akhir = max(qx_lc_l$Usia), v)
hasil_lc_p <- hitung_lx_dx_ax(qx_lc_p$qx, usia_awal = min(qx_lc_p$Usia), usia_akhir = max(qx_lc_p$Usia), v)

saveWorkbook(wb3, "hasil_TMLC_L_Tahun2020-2046.xlsx", overwrite = TRUE)
get_D_and_ar <- function(hasil_lxDx, usia_r, usia_x) {
  Dr <- hasil_lxDx$Dx[which(hasil_lxDx$Usia == usia_r)]
  Nr <- hasil_lxDx$Nx[which(hasil_lxDx$Usia == usia_r)]
  ar <- Nr / Dr
  return(list(Dr = Dr, ar = ar))
}
# =================== Tahap 3: Perhitungan PVFB ===================
wb_pvfb_semua <- createWorkbook()

# Buat dua data.frame kosong untuk gabungan output
max_tahun <- max((pensiun ) - data_peserta$UsiaSekarang)
PVFB_TMI_IV <- data.frame(t = 0:max_tahun)
PVFB_TM_LC <- data.frame(t = 0:max_tahun)

for (i in 1:nrow(data_peserta)) {
  
  peserta <- data_peserta[i, ]
  id <- as.character(peserta$Nama)
  usia_sekarang <- peserta$UsiaSekarang
  gaji_awal <- peserta$Gaji
  gender <- peserta$JenisKelamin  # "L" atau "P"
  
  Tahun_Menuju_Pensiun <- (pensiun - 1) - usia_sekarang
  usia_pensiun <- pensiun
  
  B_r <- proporsi * (pensiun - usia_sekarang) * (gaji_awal * ((1 + KenaikanGaji) * (1 + inflasi)) ^ Tahun_Menuju_Pensiun)
  
  # ==================== Perhitungan TMI IV ====================
  hasil_tmi <- if (gender == "L") hasil_tmi_l else hasil_tmi_p
  tmi_vals <- get_D_and_ar(hasil_tmi, usia_r = usia_pensiun, usia_x = usia_sekarang)
  Dr <- tmi_vals$Dr
  ar <- tmi_vals$ar
  
  usia_rentang <- usia_sekarang:(usia_pensiun)
  Dx_vector <- hasil_tmi %>% filter(Usia %in% usia_rentang) %>% pull(Dx)
  PVFB_t_tmi <- B_r * ar * (Dr / Dx_vector)
  
  colname_tmi <- paste0("j=", i)
  PVFB_TMI_IV[1:length(PVFB_t_tmi), colname_tmi] <- round(PVFB_t_tmi, 2)
  
  # ==================== Perhitungan TM LC ====================
  hasil_lc <- if (gender == "L") hasil_lc_l else hasil_lc_p
  lc_vals <- get_D_and_ar(hasil_lc, usia_r = usia_pensiun, usia_x = usia_sekarang)
  Dr_lc <- lc_vals$Dr
  ar_lc <- lc_vals$ar
  
  Dx_vector_lc <- hasil_lc %>% filter(Usia %in% usia_rentang) %>% pull(Dx)
  PVFB_t_lc <- B_r * ar_lc * (Dr_lc / Dx_vector_lc)
  
  colname_lc <- paste0("j=", i)
  PVFB_TM_LC[1:length(PVFB_t_lc), colname_lc] <- round(PVFB_t_lc, 2)
}

# Tambahkan worksheet dan simpan
addWorksheet(wb_pvfb_semua, "PVFB_TMI_IV")
writeData(wb_pvfb_semua, "PVFB_TMI_IV", PVFB_TMI_IV)

addWorksheet(wb_pvfb_semua, "PVFB_TM_LC")
writeData(wb_pvfb_semua, "PVFB_TM_LC", PVFB_TM_LC)

saveWorkbook(wb_pvfb_semua, "hasil_PVFB_Tahunan_TMI_dan_TM_LC.xlsx", overwrite = TRUE)

# =================== Visualisasi PVFB Agregat ===================
dir.create("visualisasi_pvfb", showWarnings = FALSE)

PVFB_long <- PVFB_TMI_IV %>%
  pivot_longer(cols = -t, names_to = "Peserta", values_to = "PVFB_TMI_IV") %>%
  left_join(PVFB_TM_LC %>%
              pivot_longer(cols = -t, names_to = "Peserta", values_to = "PVFB_TM_LC"),
            by = c("t", "Peserta"))

# Loop untuk simpan grafik PVFB per peserta
peserta_list <- unique(PVFB_long$Peserta)

for (id in peserta_list) {
  df_plot <- PVFB_long %>% filter(Peserta == id) %>%
    pivot_longer(cols = c("PVFB_TMI_IV", "PVFB_TM_LC"), names_to = "Skenario", values_to = "PVFB")
  
  p <- ggplot(df_plot, aes(x = t, y = PVFB, color = Skenario)) +
    geom_line(size = 1.2) +
    labs(title = paste("Nilai PVFB per Tahun -", id),
         x = "Tahun ke-", y = "PVFB (Rp)") +
    scale_color_manual(values = c("PVFB_TMI_IV" = "darkgreen", "PVFB_TM_LC" = "purple"))
  
  ggsave(filename = paste0("visualisasi_pvfb/", id, "_pvfb.png"),
         plot = p, width = 8, height = 5)
}

# ========== Visualisasi Akumulasi Total PVFB Semua Peserta ==========
PVFB_total <- PVFB_long %>%
  group_by(t) %>%
  summarise(PVFB_TMI_IV = sum(PVFB_TMI_IV, na.rm = TRUE),
            PVFB_TM_LC = sum(PVFB_TM_LC, na.rm = TRUE)) %>%
  pivot_longer(cols = c("PVFB_TMI_IV", "PVFB_TM_LC"), names_to = "Skenario", values_to = "PVFB")

ggplot(PVFB_total, aes(x = t, y = PVFB, color = Skenario)) +
  geom_line(size = 1.4) +
  labs(title = "Total PVFB Semua Peserta per Tahun",
       x = "Tahun ke-", y = "Total PVFB (Rp)") +
  scale_color_manual(values = c("PVFB_TMI_IV" = "darkgreen", "PVFB_TM_LC" = "purple"))

ggsave("visualisasi_pvfb/Total_PVFB_Semua_Peserta.png", width = 8, height = 5)


# =================== Tahap 4: Hitung NC dan AL Metode EAN ===================
library(dplyr)
library(openxlsx)

# Fungsi hitung NC dan AL metode EAN, versi revisi
hitung_NC_AL_EAN <- function(PVFB_df, hasil_lxDx, usia_sekarang, usia_pensiun, peserta_id = NULL) {
  usia_r <- usia_pensiun - 1
  PVFB_df <- PVFB_df %>% filter(t + usia_sekarang <= usia_r)
  
  hasil_full <- hasil_lxDx
  Dr <- hasil_full$Dx[hasil_full$Usia == usia_r]
  Nr <- hasil_full$Nx[hasil_full$Usia == usia_r]
  ar <- hasil_full$axn[hasil_full$Usia == usia_r]
  Nx0 <- hasil_full$Nx[hasil_full$Usia == usia_sekarang]
  
  if (any(is.na(c(Dr, Nr, ar, Nx0)))) {
    warning(paste("NA pada input: peserta ID", peserta_id))
    return(list(NC = NA, AL = NA))
  }
  
  denominator <- ar * (Nx0 - Nr)
  if (denominator <= 0) {
    warning(paste("Denominator <= 0 untuk peserta ID", peserta_id))
    return(list(NC = NA, AL = NA))
  }
  
  PVFB_total <- sum(PVFB_df$PVFB, na.rm = TRUE)
  NC_tetap <- round((PVFB_total * Dr) / denominator, 2)
  NC <- rep(NC_tetap, nrow(PVFB_df))
  
  usia_t <- PVFB_df$t + usia_sekarang
  usia_t[usia_t > usia_r] <- usia_r
  
  Nx_t <- hasil_full$Nx[match(usia_t, hasil_full$Usia)]
  Dx_t <- hasil_full$Dx[match(usia_t, hasil_full$Usia)]
  
  AL <- rep(NA, length(PVFB_df$t))
  valid <- !is.na(Dx_t) & Dx_t != 0
  AL[valid] <- round(PVFB_df$PVFB[valid] - NC_tetap * (Nx_t[valid] - Nr) / Dx_t[valid], 2)
  AL[PVFB_df$t == 0] <- 0
  AL[is.na(AL)] <- NA
  
  # DEBUG untuk peserta J3 dan J6
  if (peserta_id %in% c(3, 6)) {
    cat("=== DEBUG: Peserta ID", peserta_id, "===\n")
    cat("PVFB_total =", PVFB_total, "\n")
    cat("NC =", NC_tetap, "\n")
    cat("Nx0 =", Nx0, ", Nr =", Nr, "\n")
    cat("Denominator =", denominator, "\n")
    cat("AL tahun 0 dan 1:\n")
    print(data.frame(t = PVFB_df$t[1:2], PVFB = PVFB_df$PVFB[1:2], AL = AL[1:2]))
  }
  
  return(list(
    NC = data.frame(t = PVFB_df$t, NC = NC),
    AL = data.frame(t = PVFB_df$t, AL = AL)
  ))
}


# Inisialisasi workbook dan list penampung hasil
wb_ean <- createWorkbook()
NC_TMI_IV_all <- list()
AL_TMI_IV_all <- list()
NC_TM_LC_all <- list()
AL_TM_LC_all <- list()

# Loop untuk setiap peserta
for (i in 1:nrow(data_peserta)) {
  usia <- data_peserta$UsiaSekarang[i]
  gender <- data_peserta$JenisKelamin[i]
  peserta_id <- data_peserta$Nama[i]  # Tambahkan ini
  usia_pensiun <- 55
  
  hasil_lxDx_tmi <- if (gender == "P") hasil_tmi_p else hasil_tmi_l
  hasil_lxDx_lc <- if (gender == "P") hasil_lc_p else hasil_lc_l
  
  colname <- paste0("j=", i)
  
  if (colname %in% colnames(PVFB_TMI_IV)) {
    PVFB_df_tmi <- data.frame(t = PVFB_TMI_IV$t, PVFB = PVFB_TMI_IV[[colname]])
    hasil_tmi <- hitung_NC_AL_EAN(PVFB_df_tmi, hasil_lxDx_tmi, usia, usia_pensiun, peserta_id)
    NC_TMI_IV_all[[i]] <- hasil_tmi$NC
    AL_TMI_IV_all[[i]] <- hasil_tmi$AL
  }
  
  if (colname %in% colnames(PVFB_TM_LC)) {
    PVFB_df_lc <- data.frame(t = PVFB_TM_LC$t, PVFB = PVFB_TM_LC[[colname]])
    hasil_lc <- hitung_NC_AL_EAN(
      PVFB_df = PVFB_df_lc,
      hasil_lxDx = hasil_lxDx_lc,
      usia_sekarang = usia,
      usia_pensiun = usia_pensiun,
      peserta_id = peserta_id
    )
    NC_TM_LC_all[[i]] <- hasil_lc$NC
    AL_TM_LC_all[[i]] <- hasil_lc$AL
  }
}


# Fungsi gabung data frame per peserta
gabung_df <- function(list_df, prefix) {
  hasil_df <- list_df[[1]]
  colnames(hasil_df)[colnames(hasil_df) != "t"] <- paste0(prefix, "_j1")
  
  for (i in 2:length(list_df)) {
    df_i <- list_df[[i]]
    colnames(df_i)[colnames(df_i) != "t"] <- paste0(prefix, "_j", i)
    hasil_df <- merge(hasil_df, df_i, by = "t", all = TRUE)
  }
  return(hasil_df)
}

# Gabungkan hasil dan simpan ke Excel
NC_TMI_IV_df <- gabung_df(NC_TMI_IV_all, "NC")
AL_TMI_IV_df <- gabung_df(AL_TMI_IV_all, "AL")
NC_TM_LC_df <- gabung_df(NC_TM_LC_all, "NC")
AL_TM_LC_df <- gabung_df(AL_TM_LC_all, "AL")

addWorksheet(wb_ean, "EAN_TMI_IV")
writeData(wb_ean, "EAN_TMI_IV", cbind(NC_TMI_IV_df, AL_TMI_IV_df[, -1]))

addWorksheet(wb_ean, "EAN_TM_LC")
writeData(wb_ean, "EAN_TM_LC", cbind(NC_TM_LC_df, AL_TM_LC_df[, -1]))

# Simpan workbook (ganti path sesuai kebutuhan)
saveWorkbook(wb_ean, "hasil_EAN_NC_AL_TMI_TM_LC.xlsx", overwrite = TRUE)

if (peserta_id %in% c(3, 6)) {
  print(paste("=== DEBUG: Peserta ID", peserta_id, "==="))
  print(paste("PVFB_total =", PVFB_total))
  print(paste("NC =", NC_tetap))
  print(paste("Nx0 =", Nx0, ", Nr =", Nr))
  print(paste("Denominator =", denominator))
  print("AL tahun 0 dan 1:")
  print(data.frame(t = PVFB_df$t[1:2], PVFB = PVFB_df$PVFB[1:2], AL = AL[1:2]))
}

# =================== Visualisasi Tahap 4: NC dan AL ===================
library(ggplot2)
library(tidyr)

# Gabungkan dan ubah ke long format untuk plotting
NC_TMI_IV_long <- pivot_longer(NC_TMI_IV_df, -t, names_to = "Peserta", values_to = "NC")
AL_TMI_IV_long <- pivot_longer(AL_TMI_IV_df, -t, names_to = "Peserta", values_to = "AL")

NC_TM_LC_long <- pivot_longer(NC_TM_LC_df, -t, names_to = "Peserta", values_to = "NC")
AL_TM_LC_long <- pivot_longer(AL_TM_LC_df, -t, names_to = "Peserta", values_to = "AL")

# Buat plot dan simpan (NC dan AL)
plot_NC_TMI <- ggplot(NC_TMI_IV_long, aes(x = t, y = NC, color = Peserta)) +
  geom_line() + labs(title = "Normal Cost - TMI IV", x = "Tahun", y = "NC")
ggsave("plot_NC_TMI_IV.png", plot_NC_TMI, width = 8, height = 5)

plot_AL_TMI <- ggplot(AL_TMI_IV_long, aes(x = t, y = AL, color = Peserta)) +
  geom_line() + labs(title = "Accrued Liability - TMI IV", x = "Tahun", y = "AL")
ggsave("plot_AL_TMI_IV.png", plot_AL_TMI, width = 8, height = 5)

plot_NC_LC <- ggplot(NC_TM_LC_long, aes(x = t, y = NC, color = Peserta)) +
  geom_line() + labs(title = "Normal Cost - TM LC", x = "Tahun", y = "NC")
ggsave("plot_NC_TM_LC.png", plot_NC_LC, width = 8, height = 5)

plot_AL_LC <- ggplot(AL_TM_LC_long, aes(x = t, y = AL, color = Peserta)) +
  geom_line() + labs(title = "Accrued Liability - TM LC", x = "Tahun", y = "AL")
ggsave("plot_AL_TM_LC.png", plot_AL_LC, width = 8, height = 5)

# =================== Tahap 5: Hitung ∆B_t dan PVFB̃_tj ===================

# Fungsi menghitung delta B dan PVFB tilde
hitung_deltaB_PVFB_tj <- function(data_peserta, usia_pensiun, inflasi, KenaikanGaji, proporsi, hasil_lxDx_list) {
  max_t <- max(usia_pensiun  - data_peserta$UsiaSekarang)
  n_peserta <- nrow(data_peserta)
  v <- 1 / (1 + bunga)
  
  # Inisialisasi matriks hasil
  deltaB_df <- data.frame(t = 0:max_t)
  PVFB_tilde_df <- data.frame(t = 0:max_t)
  
  for (j in 1:n_peserta) {
    usia_j <- data_peserta$UsiaSekarang[j]
    gaji_awal <- data_peserta$Gaji[j]
    gender <- data_peserta$JenisKelamin[j]
    hasil_lxDx <- hasil_lxDx_list[[j]]
    
    t_max <- usia_pensiun -1- usia_j
    
    delta_B <- numeric(max_t + 1)
    pvfb_tilde <- numeric(max_t + 1)
    
    for (t in 0:t_max) {
      usia_t <- usia_j + t
      G_t <- gaji_awal * ((1 + inflasi) * (1 + KenaikanGaji)) ^ (t)
      G_t1 <- gaji_awal * ((1 + inflasi) * (1 + KenaikanGaji)) ^ (t+1)
      
      B_t <- proporsi * (usia_pensiun - usia_j) * G_t
      B_t1 <- proporsi * (usia_pensiun - usia_j) * G_t1
      
      delta_B[t + 1] <- round(B_t1 - B_t, 2)
      
      Dr <- hasil_lxDx$Dx[which(hasil_lxDx$Usia == usia_pensiun - 1)]
      Dx_t1 <- hasil_lxDx$Dx[which(hasil_lxDx$Usia == usia_t + 1)]
      
      pvfb_tilde[t + 1] <- round(B_t * Dr / Dx_t1, 2)
    }
    
    deltaB_df[[paste0("j=", j)]] <- delta_B
    PVFB_tilde_df[[paste0("j=", j)]] <- pvfb_tilde
  }
  
  return(list(deltaB = deltaB_df, PVFB_tilde = PVFB_tilde_df))
}
# Buat list hasil_lxDx TMI IV dan TM LC untuk setiap peserta
hasil_lxDx_tmi_list <- lapply(1:nrow(data_peserta), function(i) {
  gender <- data_peserta$JenisKelamin[i]
  if (gender == "L") {
    hasil_tmi_l
  } else {
    hasil_tmi_p
  }
})

hasil_lxDx_lc_list <- lapply(1:nrow(data_peserta), function(i) {
  gender <- data_peserta$JenisKelamin[i]
  if (gender == "L") {
    hasil_lc_l
  } else {
    hasil_lc_p
  }
})

# Perhitungan untuk TMI IV
hasil_deltabt_tmi <- hitung_deltaB_PVFB_tj(
  data_peserta = data_peserta,
  usia_pensiun = 56,
  inflasi = inflasi,
  KenaikanGaji = KenaikanGaji,
  proporsi = proporsi,
  hasil_lxDx_list = hasil_lxDx_tmi_list
)

# Perhitungan untuk TM LC
hasil_deltabt_lc <- hitung_deltaB_PVFB_tj(
  data_peserta = data_peserta,
  usia_pensiun = 56,
  inflasi = inflasi,
  KenaikanGaji = KenaikanGaji,
  proporsi = proporsi,
  hasil_lxDx_list = hasil_lxDx_lc_list
)

# Simpan ke Excel
wb_delta <- createWorkbook()

# Sheet TMI IV
addWorksheet(wb_delta, "DeltaBt_TMI_IV")
writeData(wb_delta, "DeltaBt_TMI_IV", hasil_deltabt_tmi$deltaB)

addWorksheet(wb_delta, "PVFBtilde_TMI_IV")
writeData(wb_delta, "PVFBtilde_TMI_IV", hasil_deltabt_tmi$PVFB_tilde)

# Sheet TM LC
addWorksheet(wb_delta, "DeltaBt_TM_LC")
writeData(wb_delta, "DeltaBt_TM_LC", hasil_deltabt_lc$deltaB)

addWorksheet(wb_delta, "PVFBtilde_TM_LC")
writeData(wb_delta, "PVFBtilde_TM_LC", hasil_deltabt_lc$PVFB_tilde)

# Simpan workbook
saveWorkbook(wb_delta, "hasil_FIL_DeltaBt_PVFBtilde.xlsx", overwrite = TRUE)

# =================== LANJUTAN TAHAP 6: Perhitungan NC dan AL Metode FIL ===================

# Fungsi menghitung NC FIL tetap (di t=0) untuk setiap peserta
hitung_NC_FIL_Tetap <- function(data_peserta, PVFB_df, hasil_lxDx_list, usia_pensiun) {
  n_peserta <- nrow(data_peserta)
  NC_per_peserta <- numeric(n_peserta)
  
  for (j in 1:n_peserta) {
    usia_j <- data_peserta$UsiaSekarang[j]
    hasil_lxDx <- hasil_lxDx_list[[j]]
    
    idx_entry <- which(hasil_lxDx$Usia == usia_j)
    idx_pensiun <- which(hasil_lxDx$Usia == (usia_pensiun - 1))
    
    if (length(idx_entry) > 0 && length(idx_pensiun) > 0) {
      Ne <- hasil_lxDx$Nx[idx_entry]
      Nr <- hasil_lxDx$Nx[idx_pensiun]
      Dr <- hasil_lxDx$Dx[idx_pensiun]
      
      colname <- paste0("j=", j)
      Br_j <- PVFB_df[1, colname]  # Ambil PVFB t=0
      
      if (!is.na(Br_j) && (Ne - Nr) != 0) {
        NC_per_peserta[j] <- (Br_j * Dr) / (Ne - Nr)
      }
    }
  }
  
  return(NC_per_peserta)
}

# Fungsi hitung data NC per tahun dan AL FIL
hitung_FIL_dinamis <- function(data_peserta, PVFB_df, hasil_lxDx_list, NC_per_peserta, usia_pensiun) {
  max_t <- nrow(PVFB_df) - 1
  n_peserta <- nrow(data_peserta)
  
  hasil_FIL <- data.frame(
    t = 0:max_t,
    PVFB_Total = 0,
    PVFNC_Total = 0,
    AL_FIL_Total = 0,
    NC_FIL_Total = 0,
    Peserta_Aktif = 0
  )
  
  for (t in 0:max_t) {
    PVFB_t <- 0
    PVFNC_t <- 0
    NC_t <- 0
    aktif_t <- 0
    
    for (j in 1:n_peserta) {
      usia_j_t <- data_peserta$UsiaSekarang[j] + t
      if (usia_j_t < usia_pensiun) {
        aktif_t <- aktif_t + 1
        colname <- paste0("j=", j)
        
        if (colname %in% colnames(PVFB_df)) {
          pvfb_jt <- PVFB_df[t + 1, colname]
          if (!is.na(pvfb_jt)) {
            PVFB_t <- PVFB_t + pvfb_jt
          }
        }
        
        hasil_lxDx <- hasil_lxDx_list[[j]]
        idx_t <- which(hasil_lxDx$Usia == usia_j_t)
        idx_pensiun <- which(hasil_lxDx$Usia == (usia_pensiun - 1))
        
        if (length(idx_t) > 0 && length(idx_pensiun) > 0) {
          Nx_t <- hasil_lxDx$Nx[idx_t]
          Nr <- hasil_lxDx$Nx[idx_pensiun]
          Dx_t <- hasil_lxDx$Dx[idx_t]
          
          if (!is.na(Nx_t) && !is.na(Nr) && !is.na(Dx_t) && Dx_t != 0) {
            PVFNC_j <- NC_per_peserta[j] * (Nx_t - Nr) / Dx_t
            PVFNC_t <- PVFNC_t + PVFNC_j
            NC_t <- NC_t + NC_per_peserta[j]
          }
        }
      }
    }
    
    hasil_FIL$Peserta_Aktif[t + 1] <- aktif_t
    hasil_FIL$PVFB_Total[t + 1] <- round(PVFB_t, 0)
    hasil_FIL$PVFNC_Total[t + 1] <- round(PVFNC_t, 0)
    hasil_FIL$AL_FIL_Total[t + 1] <- round(PVFB_t - PVFNC_t, 0)
    hasil_FIL$NC_FIL_Total[t + 1] <- round(NC_t, 0)
  }
  
  return(hasil_FIL)
}
# Data input: hasil_lxDx_list_TMI dan hasil_lxDx_list_LC sudah disiapkan sesuai jenis kelamin
NC_per_peserta_TMI <- hitung_NC_FIL_Tetap(data_peserta, PVFB_TMI_IV, hasil_lxDx_tmi_list, pensiun)
hasil_FIL_TMI <- hitung_FIL_dinamis(data_peserta, PVFB_TMI_IV, hasil_lxDx_tmi_list, NC_per_peserta_TMI, pensiun)

NC_per_peserta_LC <- hitung_NC_FIL_Tetap(data_peserta, PVFB_TM_LC, hasil_lxDx_lc_list, pensiun)
hasil_FIL_LC <- hitung_FIL_dinamis(data_peserta, PVFB_TM_LC, hasil_lxDx_lc_list, NC_per_peserta_LC, pensiun)
wb <- createWorkbook()

addWorksheet(wb, "FIL_TMI_IV")
writeData(wb, "FIL_TMI_IV", hasil_FIL_TMI)

addWorksheet(wb, "FIL_TM_LC")
writeData(wb, "FIL_TM_LC", hasil_FIL_LC)

saveWorkbook(wb, "FIL_FIX_NC_Konstan.xlsx", overwrite = TRUE)

