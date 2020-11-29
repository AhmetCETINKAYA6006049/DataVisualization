pressure

plot(pressure$temperature, pressure$pressure, type = "l")

library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = "line" )

plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

qplot(pressure$temperature, pressure$pressure, geom = "line" )
points(pressure$temperature, pressure$pressure)

