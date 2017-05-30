library(ggplot2)
library(ggfortify)

temperatures = read.csv("data/NY temps.csv", header=TRUE)
temperatures = temperatures[2]
colnames(temperatures) = "Temperature"
temperatures = temperatures[-169]
temperatures.ts = ts(temperatures, start=1946, end=1959, freq=12)
autoplot(temperatures.ts, main="New York monthly average temperature")

# Wolf sunspot numbers 1700 - 1988
wolf = read.csv("data/wolfsunspot.csv", header=TRUE)
wolf.ts = ts(wolf, start=1700, end=1988)
autoplot(wolf.ts, main="Sunspot activity")
