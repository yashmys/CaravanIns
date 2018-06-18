library(tidyverse)
library(reshape2)
library(scales)



dat <- read.table(text = "    ONE TWO THREE
                  1   23  234 324
                  2   23  234 324
                 3    123  234 324
                  4   23  234 324
                  5   23  234 324",sep = "",header = TRUE)

datm <- melt(cbind(dat, ind = rownames(dat)), id.vars = c('ind'))

datm



ggplot(datm,aes(x = variable, y = value,fill = ind)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())