# Pustaka yang digunakan
library(dplyr) # memanipulasi data
library(readxl) # I/0 dataset excel
library(ggplot2) # visualisasi data 
library(ggcorrplot) # visualisasi data untuk uji korelasi
library(gridExtra) # nested visulasisasi data
library(astsa) # lib autokorelasi dan autokorelasi parsial
theme_set(theme_bw())

# membaca dataset
enso <- read_excel("D:/Belajar Python/dataset/dataset tesis/dataset enso/dataset_enso.xlsx", sheet = "enso")

# mengkonversi tipe data menjadi time series dan numerik
enso <- data.frame(
  tanggal = as.Date(enso$tanggal,  
  sst = as.numeric(enso$sst),
  soi = as.numeric(enso$soi),
  oni = as.numeric(enso$oni)
)

# fungsi normalisasi data
normalize <- function(x) {
  if(is.numeric(x)) {
    x = (x - min(x)) / (max(x) - min(x))
  } else{
    print("Inputan data harus numerik")
  }
  return(x)
}

# konversi value menjadi bentuk normalisasi max-min (1-0)
enso2 <- data.frame(
  # cara praktis
  # lapply(enso[2:4], normalize) 
  tanggal = enso$tanggal,
  sst = round(normalize(enso$sst),4),
  soi = round(normalize(enso$soi),4),
  oni = round(normalize(enso$oni),4)
)

enso2
# menampilkan dataset
head(enso)
head(enso2)

# validasi hasil normalisasi (uji korelasi)
corr_enso <- round(cor(enso[2:4], method = "pearson"),4)
corr_enso_norm <- round(cor(enso[2:4], method = "pearson"),4)

# visualisasi uji pearson (heatmap)
plot_enso = ggcorrplot(corr_enso, lab = TRUE) + ggtitle(label = "Data Sebelum Normalisasi")
plot_enso_norm = ggcorrplot(corr_enso_norm, lab = TRUE) + ggtitle(label = "Data Setelah Normalisasi")
grid.arrange(plot_enso, plot_enso_norm, nrow = 1)

# visualisasi scatter plot time series belum memperhatikan pola musiman dan autokorelasi
# Data time series SST Nina 3.4
ggplot(data = enso2, aes(x = tanggal, y = sst)) + 
  geom_line(color = "#00AFBB", size = 1) +
  xlab('Tahun')+ ylab('Index SST Nina 3.4') +
  labs(title="Time Series Index SST Nina 3.4", subtitle="https://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices") +
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))

# Data time series SOI
ggplot(data = enso2, aes(x = tahun, y = soi)) + 
  geom_line(color = "#00AFBB", size = 1) +
  xlab('Tahun')+ ylab('Index SOI') +
  labs(title="Time Series Index SOI", subtitle="http://www.bom.gov.au/climate/current/soihtm1.shtml") +
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))

# Data time series ONI
ggplot(data = enso2, aes(x = tanggal, y = oni)) + 
  geom_line(color = "#00AFBB", size = 1) +
  xlab('Tahun')+ ylab('Index ONI') +
  labs(title="Time Series Index ONI", subtitle="https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php") +
  scale_x_date(labels = function(x) format(x, "%d-%b-%Y"))

# Visualisasi data dengan fungsi autokorelasi
acf(enso2$sst, main="Index SST Nina 3.4")
acf(enso2$soi, main="Index SOI")
acf(enso2$oni, main="Index ONI")

# Visualisasi data dengan fungsi autokorelasi parsial
pacf(enso2$sst, main="Index SST Nina 3.4")
pacf(enso2$soi, main="Index SOI")
pacf(enso2$oni, main="Index ONI")
