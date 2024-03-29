library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(cowplot)
library(ggpubr)
library(knitr)

#veriyi oku
covidVeri <- read_excel("tarihselveri.xlsx")

#veriyi tarihe g�re eskiden yeniye sirala
covidVeri$Tarih <- mdy(covidVeri$Tarih)
covidVeri <- covidVeri[sort(order(covidVeri$Tarih),decreasing = TRUE),]
covidVeri_tr <- filter(covidVeri, �lke == "T�rkiye")
covidVeriTurkiye <- data.frame(covidVeri_tr)

#G�nl�k tani ve �l�m sayilari i�in yeni col ekle
covidVeriTurkiye$yenitani <- 0
covidVeriTurkiye$yeniolum <- 0
for(i in 2 : nrow(covidVeriTurkiye)){
  covidVeriTurkiye$yenitani[i] <-  covidVeriTurkiye[i, "Tani"] - covidVeriTurkiye[i-1, "Tani"]
  covidVeriTurkiye$yeniolum[i] <-  covidVeriTurkiye[i, "�l�m"] - covidVeriTurkiye[i-1, "�l�m"]
}

#ilgili tarihleri filtere
covid_kasim <- filter(covidVeriTurkiye, Tarih >= "2020-11-15" & Tarih <= "2020-11-30")

# Toplam vaka sayisi
p_tv <- ggplot(covid_kasim, aes(Tarih, Tani)) + 
  geom_col(fill= "red") +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(350000, 550000)) +
  labs(x = "Tarih", y = "Toplam Vaka", 
       title = "Kasim 2020 COVID-19 Toplam Vaka Sayisi",
       subtitle = "15.11.2020-30.11.2020 tarihleri arasi COVID-19 toplam vaka sayisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History") + 
  theme_minimal_grid() +
  theme(
        plot.title = element_text(color = "red",  face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "brown", face = "italic")
  )
p_tv
# Toplam �l�m Sayisi
p_to <- ggplot(covid_kasim, aes(Tarih, �l�m)) + 
  geom_col(fill= "darkred") +
  scale_y_continuous(labels = scales::comma)+
  coord_cartesian(ylim = c(10000, 14500))+
  labs(x = "Tarih", y = "Toplam �l�m", 
       title = "Kasim 2020 COVID-19 Toplam Toplam �l�m Sayisi",
       subtitle = "15.11.2020-30.11.2020 tarihleri arasi COVID-19 toplam �l�m sayisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History") + 
  theme_minimal_grid() +
  theme(axis.text = element_text(size = 50),
        axis.title = element_text(size = 50),
        plot.title = element_text(color = "red", size = 50, face = "bold"),
        plot.subtitle = element_text(color = "blue", size = 40),
        plot.caption = element_text(color = "brown", size = 30, face = "italic")
  )

# G�nl�k Vaka Sayisi
p_gv <- ggplot(covid_kasim, aes(Tarih, yenitani)) + 
  geom_col(fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Tarih", y = "G�nl�k Vaka", 
       title = "Kasim 2020 COVID-19 G�nl�k Vaka Sayisi",
       subtitle = "15.11.2020-30.11.2020 tarihleri arasi COVID-19 g�nl�k vaka sayisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_minimal_grid()+
  theme(axis.text = element_text(size = 50),
        axis.title = element_text(size = 50),
        plot.title = element_text(color = "red", size = 50, face = "bold"),
        plot.subtitle = element_text(color = "blue", size = 40),
        plot.caption = element_text(color = "brown", size = 30, face = "italic")
  )

# G�nl�k �l�m Sayisi
p_go <- ggplot(covid_kasim, aes(Tarih, yeniolum)) + 
  geom_col(fill = "blue") +
  geom_text(mapping = aes(label = yeniolum), size = 15,color="red")+
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Tarih", y = "G�nl�k �l�m", 
       title = "Kasim 2020 COVID-19 G�nl�k �l�m Sayisi",
       subtitle = "15.11.2020-30.11.2020 tarihleri arasi COVID-19 g�nl�k �l�m sayisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_minimal_grid() +
  theme(axis.text = element_text(size = 50),
        axis.title = element_text(size = 50),
        plot.title = element_text(color = "red", size = 50, face = "bold"),
        plot.subtitle = element_text(color = "blue", size = 40),
        plot.caption = element_text(color = "brown", size = 30, face = "italic")
  )

#vaka ve �l�mler
df_toplam_vaka <- data.frame(covid_kasim$Tarih, covid_kasim$Tani)

df_toplam_olum <- data.frame(covid_kasim$Tarih, covid_kasim$�l�m)

df_gunluk_vaka <- data.frame(covid_kasim$Tarih, covid_kasim$yenitani)

df_gunluk_olum <- data.frame(covid_kasim$Tarih, covid_kasim$yeniolum)

df_tum_veriler <- data.frame(covid_kasim$Tarih, covid_kasim$Tani, covid_kasim$�l�m, covid_kasim$yenitani, covid_kasim$yeniolum)

#column adlarini degistir
colnames(df_tum_veriler)[] <- c('Tarih', 'Toplam vaka', 'Toplam �l�m', 'G�nl�k Vaka', 'G�nl�k �l�m')

#verilere g�re tek tablo yap
melt_vakalari <- reshape2::melt(df_tum_veriler, id.var = "Tarih")


#line olarak g�ster
p_gt <- ggplot(melt_vakalari, aes(x = Tarih, y = value, group = variable, col = variable)) +
  geom_line(size=3) +
  scale_y_log10(name = "Vaka Sayilari (Logaritmik)", labels = scales::comma) + 
  labs(x = "Tarih",  
       title = "Kasim 2020 COVID-19 Tablosu",
       subtitle = "15.11.2020-30.11.2020 tarihleri arasi COVID-19 verisi",
       caption = "Kaynak: https://corona.cbddo.gov.tr/Home/History")+
  theme_minimal_grid() +
  theme(axis.text = element_text(size = 50),
        axis.title = element_text(size = 50),
        plot.title = element_text(color = "red", size = 50, face = "bold"),
        plot.subtitle = element_text(color = "blue",size = 40),
        plot.caption = element_text(color = "gray", size =30, face = "italic"),
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 30)
  )


# t�m tablolari tek olarak hazirla   plot_grid(p_gt, p_tab,...)
plot_grid(p_tv, p_to, p_gv, p_go, p_gt, nrow = 3, ncol = 2, align = "hv", margin(20,20,20,20))