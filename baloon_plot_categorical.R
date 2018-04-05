
install.packages("ggpubr")
install.packages("tidyverse")

library(ggplot2)
library(ggpubr)
library(tidyverse)

theme_set(theme_pubr())



housetasks <- read.delim(
  system.file("demo-data/housetasks.txt", package = "ggpubr"),
  row.names = 1
)
head(housetasks, 4)

ggballoonplot(housetasks, fill = "value") 
