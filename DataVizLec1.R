pressure

#line plot
plot(pressure$temperature, pressure$pressure, type = "l")

library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = "line" )

plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

qplot(pressure$temperature, pressure$pressure, geom = "line" )
points(pressure$temperature, pressure$pressure)


#bar plot 
BOD
barplot(BOD$demand, names.arg = BOD$Time)


qplot(BOD$Time, BOD$demand, geom = "col")

#histogram plot
mtcars
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)

qplot(mtcars$mpg) #bindwidth default 30
qplot(mpg, data = mtcars, binwidth = 4)

#box plot
ToothGrowth

boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)

qplot(supp , len , data=ToothGrowth , geom="boxplot")
qplot(interaction(ToothGrowth$supp , ToothGrowth$dose),
      ToothGrowth$len ,
      geom="boxplot")


