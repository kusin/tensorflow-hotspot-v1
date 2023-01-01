# PROGRAM
# Melakukan cek stasioneritas data menggunakan Uji-Augmented Dickey Fuller
library(readxl)
library(tseries)

# DEKLARASI fungsi-fungsi yang akan digunakan
normalize <- function(x) {
  if(is.numeric(x)) {
    x = (x - min(x)) / (max(x) - min(x))
  } else{
    print("Inputan data harus numerik")
  }
  return(x)
}

# ALGORITMA

# ALGORITMA -input dataset
df <- read_excel("D:/Belajar Python/dataset/dataset tesis/dataset.xlsx")
df <- data.frame(
  hotspot_riau = normalize(as.numeric(df$hotspot_riau)),
  hotspot_sumsel = normalize(as.numeric(df$hotspot_sumsel)),
  sst = normalize(as.numeric(df$sst)),
  soi = normalize(as.numeric(df$soi)),
  oni = normalize(as.numeric(df$oni))
)
df

# konversi data kedalam bentuk numeric -> time series
hotspot_riau <- as.ts(df$hotspot_riau)
str(hotspot_riau)

hotspot_sumsel <- as.ts(df$hotspot_sumsel)
str(hotspot_sumsel)

sst <- as.ts(df$sst)
str(sst)

soi <- as.ts(df$soi)
str(soi)

oni <- as.ts(df$oni)
str(oni)

# ALGORITMA - PROSES Uji-Augmented Dickey Fuller
adf.test(hotspot_riau)
adf.test(hotspot_sumsel)
adf.test(sst, alternative = 'stationary', k=7)
adf.test(soi)
adf.test(oni)


a <- data(ToothGrowth)
a
data(PlantGrowth)
