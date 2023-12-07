#Install packages if necessary
#install.packages("ggplot2")
#install.packages("gridExtra")

#Library the required packages
library(ggplot2)
library(gridExtra)

#Define the random walk function. The function "set.seed(42)" on line 14 will ensure the randomness is reproducible
random_walk  <- function (n_steps) {

  set.seed(42)
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  df[1,] <- c(0,0,1)
  
  for (i in 2:n_steps) {
    
    h <- 0.25
    
    angle <- runif(1, min = 0, max = 2*pi)
    
    df[i,1] <- df[i-1,1] + cos(angle)*h
    
    df[i,2] <- df[i-1,2] + sin(angle)*h
    
    df[i,3] <- i
    
  }
  
  return(df)
  
}

#Generate a random walk
data1 <- random_walk(500)

#Produce a plot of this random walk
plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

#Generate another random walk
data2 <- random_walk(500)

#Plot this second random walk
plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

#Visualise the two random walks together
grid.arrange(plot1, plot2, ncol=2)
