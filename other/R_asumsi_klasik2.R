# membaca dataset
enso <- read_excel("../dataset/dataset tesis/dataset.xlsx", sheet = "dataset")


# mengkonversi tipe data menjadi time series dan numerik
enso <- data.frame(
  tanggal = as.Date(enso$tanggal, format = "%Y/%m/%d"),
  sst = as.numeric(enso$sst),
  soi = as.numeric(enso$soi),
  oni = as.numeric(enso$oni),
  hotspot = as.numeric(enso$hotspot)
)
enso


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
  oni = round(normalize(enso$oni),4),
  hotspot = round(normalize(enso$hotspot),4)
)
enso2
