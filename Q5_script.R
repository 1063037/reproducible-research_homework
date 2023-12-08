#Import the data used in the paper
virus_data <- read.csv("question-5-data/Cui_etal2014.csv")
#View the data to see how many rows/columns there are
View(virus_data)

#Visualise data
#install.packages("ggplot2")
library(ggplot2)
ggplot(virus_data, aes(x = Genome.length..kb., y = Virion.volume..nm.nm.nm.))+
  geom_point(alpha = 0.5)

#Visualise log-transforming the data
ggplot(virus_data, aes(x = log(Genome.length..kb.), y = log(Virion.volume..nm.nm.nm.)))+
  geom_point(alpha = 0.5)

#Log transform the data so the relationship between L and log(V) is linear
#install.packages("dplyr")
library(dplyr)
log_transform <- virus_data %>%
  mutate(L_log = log(Genome.length..kb.)) %>%
  mutate(V_log = log(Virion.volume..nm.nm.nm.))

#create linear model with the log'ed data
virus_model <- lm(V_log ~ L_log, log_transform)
#view model results
summary(virus_model)

exp(7.0748)

#Recreate figure
ggplot(data = log_transform, aes(x = L_log, y = V_log))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", colour = "#3366ff", linewidth = 0.7, se = TRUE)+
  theme_bw()+
  xlab("log [Genome length (kb)]")+
  ylab("log [Virion volume (nm3)]")

#Info on package versions
sink(file = "package-versions.txt")
sessionInfo()
sink()
