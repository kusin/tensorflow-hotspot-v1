#Autocorrelation untuk data penelitian (kuartal)

#panggil data
#panggil data
mydata1<-read.csv("E:/Thesis/Data Baru/Data Fix/Data Proses (Kuartal)-filled.csv")
mydata2<-subset(mydata1, select=-c(Kecamatan,Tahun, Kuartal,X10,X11))
X11<-as.numeric(mydata1$X11)
str(X11)

mydata<-cbind(mydata2,X11)
#cek struktur data
str(mydata)

#Visualisasi Data
library(dplyr)
library(ggplot2)
library(scales)

#normalisasi data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
nn_norm <- as.data.frame(lapply(mydata, normalize))

#Regression
produksi_ikan<-lm(Y ~ X1 + X2+X3+X4+X5+X6+X7+X8+X9+X11, data=nn_norm)
summary(produksi_ikan)

library(car)
vif(produksi_ikan)
library(lmtest)
dwtest(produksi_ikan)


#Breusch Godfrey Test for AC
bgtest(produksi_ikan,type="Chisq")

