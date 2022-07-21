# Import libraries
library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data
# Import functions
source("functions/log_growth.R")
source("functions/lotka_volterra.R")

# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
# t <- 1:20 #seq(1,200,by = 0.5) #seq(1,200, length.out = 400)
t <- seq(1, 20, length.out = 400)

# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

head(out_log)

df_log <- as.data.frame(out_log) #Siempre pasar a dataframe o sino no funciona el plot
ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()

# Lotka-Volterra parameters ----
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- seq(1, 100, length.out = 400)

out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)

df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3) #Une las columnas pero agrega una más con el numero al que representa, en este caso es especie 1 y 2

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()
