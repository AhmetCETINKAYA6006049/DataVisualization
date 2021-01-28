library(readr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readxl)
library(lubridate)
library(cowplot)
library(ggpubr)
library(ggmap)

veriseti2 <- read.csv("sales-data-set.csv")
View(veriseti2)

colnames(veriseti2)[] <- c("Dükkan", "Bölüm", "Tarih", "HaftalıkSatış", "Tatilmi")
veriseti2$Dükkan <- as.character(veriseti2$Dükkan)
View(veriseti2)



yillik_toplam_liste <- veriseti2 %>%
                    group_by(Dükkan) %>%
                    summarise(HaftalıkSatış = sum(HaftalıkSatış))

yillik_toplam <- data.frame(yillik_toplam_liste)

yillik_dükkan_yuzde <- yillik_toplam %>% 
  arrange(desc(Dükkan)) %>%
  mutate(prop = HaftalıkSatış / sum(yillik_toplam$HaftalıkSatış) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(yillik_dükkan_yuzde, aes(x = 2, y=prop, fill=Dükkan)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar(theta = "y", start=0) +
  geom_text(aes(y = ypos, label = round(prop,1)), color = "white", size=4) +
  scale_fill_brewer(palette="Set1")+
  xlim(0.5, 2.5) +
  theme_void()



