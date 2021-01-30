library(readr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readxl)
library(lubridate)
library(cowplot)
library(ggpubr)
library(ggmap)

#veriyi al
veriseti2 <- read.csv("sales-data-set.csv")
View(veriseti2)

# veriyi düzenle
colnames(veriseti2)[] <- c("Mağaza", "Bölüm", "Tarih", "HaftalıkSatış", "Tatilmi")
veriseti2$Mağaza <- as.character(veriseti2$Mağaza)
veriseti2$Bölüm <- as.character(veriseti2$Bölüm)
veriseti2$Tarih <- as.Date(veriseti2$Tarih, "%d/%m/%Y")
View(veriseti2)
str(veriseti2)

# ilgili yılı seç
yil2011_df <- filter(veriseti2, Tarih >="2011/01/01" & Tarih < "2012/01/01")

# Dükkanların yıllık paylarını hesapla
yillik_toplam_liste <- yil2011_df %>%
                    group_by(Mağaza) %>%
                    summarise(HaftalıkSatış = sum(HaftalıkSatış))

yillik_toplam <- data.frame(yillik_toplam_liste)

yillik_magaza_yuzde <- yillik_toplam %>% 
  arrange(desc(Mağaza)) %>%
  mutate(prop = HaftalıkSatış / sum(yillik_toplam$HaftalıkSatış) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

#donut graph ile çiz
ggplot(yillik_magaza_yuzde, aes(x = 2, y=prop, fill=Mağaza)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar(theta = "y", start=0) +
  geom_text(aes(y = ypos, label = round(prop,1)), color = "white", size=4) +
  scale_fill_brewer(palette="Set1")+
  xlim(0.5, 2.5) +
  labs(
       title = "2011 yılı Mağaza Satış Yüzdeleri",
       subtitle = "2011 yılında mağazaların toplam satış payları",
       caption = "Kaynak: https://www.kaggle.com/manjeetsingh/retaildataset") + 
  theme_void() +
  theme(
    plot.title = element_text(color = "Blue",  face = "bold"),
    plot.subtitle = element_text(color = "red"),
    plot.caption = element_text(color = "black", face = "italic")
  )

ggsave("2011 mağaza hasılat payları.png")

# Bölümlerin yıllık hasılatı
bolum2011 <- filter(veriseti2, Tarih >="2011/01/01" & Tarih < "2012/01/01")

#En yüksek satış yapan 10 bölümü seç
bolum2011_df <- bolum2011%>%
                  group_by(Bölüm) %>%
                  summarise(HaftalıkSatış = sum(HaftalıkSatış)) %>%
                  top_n(10, HaftalıkSatış) %>%
                  arrange(desc(HaftalıkSatış))

# göster
ggplot(bolum2011_df, aes(HaftalıkSatış, Bölüm, label = round(HaftalıkSatış/1000000, 1))) +
  geom_segment( aes(x=0, xend = HaftalıkSatış, y = reorder(Bölüm, HaftalıkSatış), yend=Bölüm), color = "red") +
  geom_point(color = "red", size =3 ) +
  geom_text(mapping = aes(label = round(HaftalıkSatış/1000000, 1)), position = position_dodge(0.9), hjust = -0.3, color="blue") +
  scale_y_discrete(labels = abbreviate) +
  scale_x_continuous(labels = scales::label_dollar(prefix="$", scale = 1e-6))+
  coord_cartesian(xlim = c(0, 30000000)) +
  labs( x = "Yıllık Satış Miktarı (Milyon USD)", y = "Bölüm Numarası",
    title = "2011 yılı En Çok Hasılat Yapan Bölümler ",
    subtitle = "2011 yılında en yüksek hasılatı olan bölüm numaraları",
    caption = "Kaynak: https://www.kaggle.com/manjeetsingh/retaildataset")+
  theme_light() +
  theme(
    plot.title = element_text(color = "Blue",  face = "bold"),
    plot.subtitle = element_text(color = "red"),
    plot.caption = element_text(color = "black", face = "italic")
  )

# kaydet
ggsave("2011enyüksekhasılatbölümler.png")

