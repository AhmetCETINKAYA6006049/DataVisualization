install.packages('hrbrthemes')
install.packages("lubridate")
install.packages("digest")
install.packages("readxl")


library(readr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readxl)
library(lubridate)

#covid veri al
covidVeri <- read_excel("tarihselveri.xlsx")

#veriyi tarihe göre eskiden yeniye sırala
covidVeri$Tarih <- mdy(covidVeri$Tarih)
covidVeri <- covidVeri[sort(order(covidVeri$Tarih),decreasing = TRUE),]
covidVeri_tr <- filter(covidVeri, Ülke == "Türkiye")
covidVeriTurkiye <- data.frame(covidVeri_tr)

#Günlük vaka ve ölüm sayılarını yeni col olarak ekle
covidVeriTurkiye$yenitani <- 0
covidVeriTurkiye$yeniolum <- 0

for(i in 2 : nrow(covidVeriTurkiye)){
  covidVeriTurkiye$yenitani[i] <-  covidVeriTurkiye[i, "Tanı"] - covidVeriTurkiye[i-1, "Tanı"]
  covidVeriTurkiye$yeniolum[i] <-  covidVeriTurkiye[i, "Ölüm"] - covidVeriTurkiye[i-1, "Ölüm"]
}

covid_kasim <- filter(covidVeriTurkiye, Tarih >= "2020-11-15" & Tarih <= "2020-11-30")


# Toplam vaka sayısını topvaka png olarak kaydet
ggplot(covid_kasim, aes(Tarih, Tani)) + 
  geom_col(fill= "salmon") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Tarih", y = "Toplam Vaka", 
       title = "Kasım 2020 COVID Toplam Vaka Sayısı",
       subtitle = "15.11.2020-30.11.2020 tarihleri arası COVID-19 vaka sayısı",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History") + 
  theme_minimal()+
  theme(
    plot.title = element_text(color = "red", face = "bold"),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "green", face = "italic")
  )
ggsave("toplamvaka.png")

# Toplam Ölüm sayısı
ggplot(covid_kasim, aes(Tarih, Ölüm)) + 
  geom_col(fill= "darksalmon") +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Tarih", y = "Toplam Ölüm", 
       title = "Kasım 2020 COVID-19 Toplam Ölüm Sayısı",
       subtitle = "15.11.2020-30.11.2020 tarihleri arası COVID-19 ölüm sayısı",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History") + 
  theme_minimal()+
  theme(
    plot.title = element_text(color = "red", face = "bold"),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "green", face = "italic")
  )
ggsave("toplamolum.png")

# Günlük vaka sayısı
ggplot(covid_kasim, aes(Tarih, yenitani)) + 
  geom_col(fill= "darkred") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Tarih", y = "Günlük Vaka", 
       title = "Kasım 2020 COVID-19 Günlük Vaka Sayısı",
       subtitle = "15.11.2020-30.11.2020 tarihleri arası COVID-19 günlük vaka sayısı",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_minimal() +
  theme(
    plot.title = element_text(color = "red",face = "bold"),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "brown", face = "italic")
  )
ggsave("gunlukvaka.png")

# Günlük Ölüm sayısı
ggplot(covid_kasim, aes(Tarih, yeniolum)) + 
  geom_col(fill= "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Tarih", y = "Günlük Ölüm", 
       title = "Kasım 2020 COVID-19 Günlük Ölüm Sayısı",
       subtitle = "15.11.2020-30.11.2020 tarihleri arası COVID-19 ölüm sayısı",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_classic() +
  theme(
    plot.title = element_text(color = "red", face = "bold"),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "brown", face = "italic")
  )
ggsave("gunlukolum.png")

#Genel Durum Göster
df_toplam_vaka <- data.frame(covid_kasim$Tarih, covid_kasim$Tani)

df_toplam_olum <- data.frame(covid_kasim$Tarih, covid_kasim$Ölüm)

df_gunluk_vaka <- data.frame(covid_kasim$Tarih, covid_kasim$yenitani)

df_gunluk_olum <- data.frame(covid_kasim$Tarih, covid_kasim$yeniolum)

df_tum_veriler <- data.frame(covid_kasim$Tarih, covid_kasim$Tani, covid_kasim$Ölüm, covid_kasim$yenitani, covid_kasim$yeniolum)

colnames(df_tum_veriler)[] <- c('Tarih', 'Toplam vaka', 'Toplam ölüm', 'Günlük vaka', 'Günlük ölüm')

melt_vakalari <- reshape2::melt(df_tum_veriler, id.var = "Tarih")

ggplot(melt_vakalari, aes(x = Tarih, y = value, group = variable, col = variable)) +
  geom_line() +
  scale_y_log10(name = "Logaritmik Vaka Sayıları", labels = scales::comma) + 
  labs(x = "Tarih",  
       title = "Kasım 2020 COVID-19 Tablosu",
       subtitle = "15.11.2020-30.11.2020 tarihleri arası COVID-19 verisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_modern_rc()+
  theme(
    plot.title = element_text(color = "red", face = "bold"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "gray", face = "italic")
  )
ggsave("genelhat.png")