library(ggplot2)
library(readr)

#İlgili veri https://corona.cbddo.gov.tr/Home/History adresinden alınmıştır.
covid19veri <- read_csv("COVID19TurkiyeResmiVeri.csv")
View(covid19veri)

p <- ggplot(data = covid19veri,
            mapping = aes(x = tarih, y = gunluk_olum ))
p + geom_line()  +theme_classic()
