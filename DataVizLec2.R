install.packages('gapminder')
library(gapminder)
library(ggplot2)

summary(gapminder)

p <- ggplot(data = gapminder,#select data
            mapping = aes(x = gdpPercap, y = lifeExp)) #select axis variable

p + geom_point() + geom_smooth() + theme_classic() #select points and theme

#show logaritmic more meaningful
p + geom_point() + geom_smooth() + scale_x_log10() + theme_classic()


p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) 
p + geom_point() + geom_smooth(method = "gam") + 
  scale_x_log10(labels = scales::dollar) + theme_grey() #show x axis in USD


#show in linear model
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_log10(labels = scales::dollar) + 
  theme_grey()


#show in loess model, points in purple and logaritmic x axis
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point(color = "purple") + 
  geom_smooth(method = "loess") + 
  scale_x_log10() + 
  theme_bw()

#add opacity alpha to the points, scale x axis in USD, set custom labels
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point(alpha = 0.3) + 
  geom_smooth(method = "gam") + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "Kişi Bası GSYİH", y = "Yaşam Beklentisi (yıl)", title = "Ekonomik Büyüme ve Yaşam Beklentisi", subtitle = "Noktalar ülke-yıl verisidir", caption = "Kaynak: Gapminder.")+
  theme_bw()

#show different curve for each continent
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent))

p + geom_point() + 
  geom_smooth(method = "loess") + 
  scale_x_log10() + 
  theme_classic()

#show different colours for curves of each continent
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))

p + geom_point() + 
  geom_smooth(method = "loess") + 
  scale_x_log10(labels = scales::dollar) + 
  theme_classic()

#show continent points in different color and only the main curve

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point(mapping = aes(color = continent)) + 
  geom_smooth(method = "loess") + 
  scale_x_log10(labels = scales::dollar) + 
  theme_classic()

#show each point in different color for log(pop) , no curve

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point(mapping = aes(color = log(pop))) + 
  scale_x_log10(labels = scales::dollar) + 
  theme_classic()

ggsave(filename = "my_figure.pdf")


#use line for gdp change for years
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))

p + geom_line() +
  theme_classic()

#group by country
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))

p + geom_line(aes(group = country)) +
  theme_classic()

#facet_wrap by continent ( group by country and show different chart for each continent)
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))

p + geom_line(aes(group = country)) +
  facet_wrap(~continent) +
  theme_classic()

#use colors, scale y axis logaritmic, divide facet by 5, label axes
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))

p + geom_line(color = "gray70", aes(group = country)) +
  geom_smooth(size = 1.1, method = "loess", se = FALSE) +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~continent, ncol = 5) +
  labs(x = "Yıl",
       y = "kişi başı GSYİH",
       title = "Kıtalara ilişkin kişibaşı GSYİH") 



#https://visualizingsociety.com/show/08-show/
#duke university content öğretiliyor







