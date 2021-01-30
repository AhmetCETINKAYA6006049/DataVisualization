
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
veriseti <- read.csv("TechStocks.csv")
View(veriseti)

stock_verileri <- data.frame(veriseti$Date, veriseti$AAPL, veriseti$GOOG,veriseti$MSFT)
stock_verileri$veriseti.Date <- mdy(veriseti$Date)
View(stock_verileri)
colnames(stock_verileri)[] <- c('Tarih', 'APPLE', 'GOOGLE', 'MICROSOFT')
#veriye göre tek tablo yap
melt_stocks <- reshape2::melt(stock_verileri, id.var = "Tarih")
colnames(melt_stocks)[] <- c('Tarih', 'Firma', 'Değer')
View(melt_stocks)
str(melt_stocks)

#line olarak göster
p_trend <- ggplot(melt_stocks, aes(x = Tarih, y = Değer, group = Firma, col = Firma)) +
  geom_line(size=1) +
  scale_x_date(limits = as.Date(c("2015-12-01","2017-12-01"))) +
  scale_y_log10(name = "Hisse Senedi Değerleri", labels = scales::comma) + 
  labs(x = "Tarih",  
       title = "Apple, Google ve Microsoft Borsa Değerleri",
       subtitle = "01.12.2015-01.12.2017 Tarihleri Arası Veriler",
       caption = "Kaynak:vincentarelbundock.github.io/Rdatasets/doc/Stat2Data/TechStocks.html") +
  theme_minimal_grid()  +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(color = "red", size = 15, face = "bold"),
        plot.subtitle = element_text(color = "blue",size = 10),
        plot.caption = element_text(color = "black", size =6, face = "italic"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6))+
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
p_trend
ggsave("APPLE_GOOGLE_MS.png")


#yüzde olarak artış
melt_stocks$yuzde <- 0
for(i in 2:504) {
  melt_stocks$yuzde[i] = ((melt_stocks$Değer[i]-melt_stocks$Değer[i-1])/melt_stocks$Değer[i-1])*100
}
for(i in 506:1008) {
  melt_stocks$yuzde[i] = ((melt_stocks$Değer[i]-melt_stocks$Değer[i-1])/melt_stocks$Değer[i-1])*100
}
for(i in 1010:1512) {
  melt_stocks$yuzde[i] = ((melt_stocks$Değer[i]-melt_stocks$Değer[i-1])/melt_stocks$Değer[i-1])*100
}
View(melt_stocks)

apple_yuzde <- ((melt_stocks$Değer[504]-melt_stocks$Değer[1])/melt_stocks$Değer[1])
google_yuzde <- ((melt_stocks$Değer[1008]-melt_stocks$Değer[505])/melt_stocks$Değer[505])
microsoft_yuzde <- ((melt_stocks$Değer[1512]-melt_stocks$Değer[1009])/melt_stocks$Değer[1009])

yuzde_degisim <- NULL
yuzde_degisim$firma <- c("APPLE", "GOOGLE", "MICROSOFT")
yuzde_degisim$oran <- c(apple_yuzde, google_yuzde, microsoft_yuzde)
yuzde.df <- data.frame(yuzde_degisim)

#bar olarak göster
p_yuzde <- ggplot(yuzde.df, aes(x = firma, y = oran)) +
  geom_bar(aes(reorder(firma,oran), oran),
           stat = "identity", 
           fill = "cornflowerblue") +
  geom_text(mapping = aes(label = round(oran*100)), position = position_dodge(1), vjust = -0.2, size = 5,color="red")+
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 0.60))+
  labs(x = "Firma",  y= "Yüzde Karı",
       title = "Apple, Google ve Microsoft Borsa Karı",
       subtitle = "01.12.2015-01.12.2017 Yüzde Kar",
       caption = "Kaynak:vincentarelbundock.github.io/Rdatasets/doc/Stat2Data/TechStocks.html") +
  theme_minimal_grid() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(color = "red", size = 15, face = "bold"),
        plot.subtitle = element_text(color = "blue",size = 10),
        plot.caption = element_text(color = "black", size =6, face = "italic"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6))
p_yuzde
ggsave("APPLE_GOOGLE_MS_yuzde.png")
