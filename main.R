library(ggplot2)
library(shiny)

dane <- read.csv(file.choose())

df <- dane[c('price', 'area')]

df <- df[complete.cases(df$price), ]

df$price <- round(df$price)
df$area <- round(df$area)

wspolczynniki_regresji <- function(df) {
  n <- nrow(df)
  sum_x <- sum(df$area)
  sum_y <- sum(df$price)
  sum_xy <- sum(df$area * df$price)
  sum_x_squared <- sum(df$area^2)
  
  a <- (n * sum_xy - sum_x * sum_y) / (n * sum_x_squared - sum_x^2)
  b <- (sum_y - a * sum_x) / n
  
  return(c(a, b))
}

predict_price <- function(x, a, b) {
  return(a * x + b)
}

coefficients <- wspolczynniki_regresji(df)
a <- coefficients[1]
b <- coefficients[2]

wartosc <- as.numeric(readline(prompt = "Podaj metraz mieszkania: "))

predicted_price <- predict_price(wartosc, a, b)

cat("Współczynniki regresji: a =", a, ", b =", b, "\n")
cat("Przewidywana cena dla powierzchni", wartosc, ":", predicted_price, "\n")

x_range <- seq(min(df$area), max(df$area), by = 1)
predicted_prices <- sapply(x_range, function(x) predict_price(x, a, b))

plot_data <- data.frame(area = x_range, price = predicted_prices)

ggplot(df, aes(x = area, y = price)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = area, y = price), color = 'red') +
  labs(x = 'Powierzchnia', y = 'Cena', title = 'Regresja liniowa dla cen mieszkań') +
  theme_minimal()
