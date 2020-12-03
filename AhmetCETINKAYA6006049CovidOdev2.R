#install.packages('hrbrthemes')
library(readr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

covidVeri <- read_csv("COVID19TurkiyeResmiVeri.csv" )
strptime(covidVeri$tarih, "%m/%d")
summary(covidVeri$tarih)
View(covidVeri)

# Toplam vaka sayısı
ggplot(data=covidVeri, aes(x=tarih, y=toplam_vaka, group=1)) +
  geom_line(color = "red")+
  geom_point(color = "red", size = 2)

# Günlük vaka sayısı
ggplot(data=covidVeri, aes(x=tarih, y=gunluk_vaka, group=1)) +
  geom_line(color = "red")+
  geom_point(color = "red", size = 2)

#vaka ve ölümler
df_gunluk_vaka <- data.frame(covidVeri$tarih, covidVeri$gunluk_vaka)
View(df_gunluk_vaka)
df_gunluk_olum <- data.frame(covidVeri$tarih, covidVeri$gunluk_olum)
View(df_gunluk_olum)
df_toplam_vaka <- data.frame(covidVeri$tarih, covidVeri$toplam_vaka)
View(df_toplam_vaka)

df_toplam_olum <- data.frame(covidVeri$tarih, covidVeri$toplam_olum)
View(df_toplam_olum)

df_tum_veriler <- data.frame(covidVeri$tarih, covidVeri$toplam_vaka, covidVeri$toplam_olum, covidVeri$gunluk_vaka, covidVeri$gunluk_olum)
View(df_tum_veriler)
melt_vakalari <- reshape2::melt(df_tum_veriler, id.var = "covidVeri.tarih")
View(melt_vakalari)

p <- ggplot(melt_vakalari, aes(x = covidVeri.tarih, y = value,group = variable, col = variable)) +
  geom_line() +
  scale_y_log10()

p


------------------------------------

df_merged_veriler <- merge(df_gunluk_vaka, df_toplam_vaka, df_gunluk_olum, df_toplam_olum, by = "covidVeri.tarih")
View(df_merged_veriler)


df_toplam_ve_gunluk <- merge(df_gunluk_vaka, df_toplam_vaka, by = "covidVeri.tarih")
View(df_toplam_ve_gunluk)

melt_vakaları <- reshape2::melt(df_toplam_ve_gunluk, id.var = "covidVeri.tarih")
View(melt_vakalari)

ggplot(melt_vakalari, aes(x = covidVeri.tarih, y = value, group = variable, col = variable)) +
  geom_line() +
  scale_y_log10(name = "Vaka Sayıları", labels = scales::comma)

ggplot(covidVeri, aes(covidVeri$tarih, covidVeri$gunluk_vaka)) + 
  geom_col(fill= "#FFCDD2") +
  labs(x = "Tarih", y = "Günlük Vaka", title = "Kasım 2020 COVID Günlük Vaka Sayısı")

