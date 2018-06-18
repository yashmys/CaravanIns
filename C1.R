#install.packages("tidyverse")
#install.packages("sqldf")

library(tidyverse)
library(sqldf)

#load the caravan insurance data 

Cins <-  read.csv.sql("C1.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")


#head(caravan_ins_data)
dim(Cins)
names(Cins)

#str(caravan_ins_data)

#check for  blank values in any on the columns

Cins[Cins == ""] <-  NA
Cins <-  na.omit(Cins)
dim(Cins)

# see graph of the % of people having policies and not having policies 
#caravan_ins_data$Purchased

len1 = sum( Cins$CARAVAN == 1 )
len2 = sum( Cins$CARAVAN == 0 )
len1
len2

#
# caravan_ins_data$Sump <- factor(caravan_ins_data$Purchased,c(0,1),labels = c(len2,len1))
# caravan_ins_data$Sump <-  as.integer(caravan_ins_data$Sump)


# ggplot(data =  caravan_ins_data, aes( y = caravan_ins_data$Sump ,  x = caravan_ins_data$Purchased,fill=factor(caravan_ins_data$Purchased) ) )+ 
#   geom_bar(stat = "identity" )
# From the graph we can see that only a small % of people actually buy insurance 

bar_label <- factor(Cins$CARAVAN, labels=c("NotPurchased","Purchased"))
#bar_label
ggplot(data =  Cins, aes(y = "", x = Cins$CARAVAN, fill = bar_label ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Purchased / Not Purchased")

# Graph Customer Main Type / no of people bought the insurance

# group_by(grp_data$`Cins$MOSHOOFD`,grp_data$`Cins$CARAVAN'') %>%
# summarise(counts = n()) 
# grp_data 

bar_label <- ""
#levels(bar_label)
lbl_str <- c("Successful hedonists   ",
                       "Driven Growers         ",
                       "Average Family         ",
                       "Career Loners          ",
                       "Living well            ",
                       "Cruising Seniors       ",
                       "Retired and Religeous  ",
                       "Family with grown ups  ",
                       "Conservative families  ",
                       "Farmers                ")
bar_label <- factor(Cins$MOSHOOFD,labels = lbl_str)

ggplot(data =  Cins, aes(x = Cins$MOSHOOFD , y = ""  , fill = bar_label ) )+ 
  geom_bar(stat = "identity", position = "dodge" ) + 
  facet_grid(Cins$CARAVAN ~.)
  ggtitle("Customer Main Type")

  ggplot(data =  Cins, aes(y = "", x = Cins$CARAVAN, fill = bar_label ) )+ 
    geom_bar(stat = "identity" ) + 

  ggtitle("Customer Main Type")
  


# By Customer Sub type 



