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


ggplot(data =  Cins, aes(y = "", x = Cins$CARAVAN ,fill=factor(Cins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Purchased / Not Purchased")
# Graph Customer Main Type / no of people bought the insurance

# 
# ggplot(subset(caravan_ins_data,caravan_ins_data$Purchased == 1 ), aes( y = "" ,  x = caravan_ins_data$Purchased,fill=factor(caravan_ins_data$Purchased) ) )+ 
#   geom_bar(stat = "identity" )

# plot to see the distribution of customer main types 
# 
# 
# ggplot(data =  caravan_ins_data, aes(y = "", x = caravan_ins_data$`Customer main type`,fill=factor(caravan_ins_data$Purchased) ) )+ 
#   geom_bar(stat = "identity" )

# Distribution of the  Customer Main Type 
# 

ggplot(data = Cins, aes(y = "", x = reorder(Cins$MOSHOOFD,Cins$MOSHOOFD,function(x) {length(x)}),fill=factor(Cins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Distribution by Customer Main type")

# Bins <-  Cins[ which (Cins$CARAVAN ==  '1' )]

Bins <- subset(Cins,CARAVAN == '1' )
dim(Bins)

#Distribution of the customer main type among the people who bought the insurance
#From the graph we can see that Customer type -4 (Career loners) have not bought at all 
 
ggplot(data = Bins, aes(y = "", x = reorder(Bins$MOSHOOFD,Bins$MOSHOOFD,function(x) {length(x)}),fill=factor(Bins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Distribution by Customer Main type  (Purchased only)")

#Lets check based on the age group
# We can infer that people who bought insurance are from the age group 4 ,2 , 3 and 5  

ggplot(data = Bins, aes(y = "", x = reorder(Bins$MGEMLEEF,Bins$MGEMLEEF,function(x) {length(x)}),fill=factor(Bins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Distribution by Age ( Purchased only )")


ggplot(data = Cins, aes(y = "", x = reorder(Cins$MGEMLEEF,Cins$MGEMLEEF,function(x) {length(x)}),fill=factor(Cins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Distribution by Age ")

#Based on religion 
# From the graph we can infer that religion doesnt factor much whether insurance will bought or not 

temp <-   sqldf(" select MGODRK ,  MGODPR ,  MGODOV ,  MGODGE ,  CARAVAN  from Cins  ")
a <- gather(temp, Religion , Value, 1:4  )
head(a)
ggplot(data = a , aes (y = Value, x = a$Religion , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Religion ")

temp <-   sqldf(" select MGODRK ,  MGODPR ,  MGODOV ,  MGODGE ,  CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, Religion , Value, 1:4  )
head(a)
ggplot(data = a , aes (y = Value, x = a$Religion , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Religion (Purchased only) ")
  
  
# Distribution by Customer Sub Type who  wants insurance
# we can see the customer sub type 8 ( Lower large families) and 33 (Middle class families) are most likey to buy 
ggplot(data = Bins, aes(y = "", x = reorder(Bins$MOSTYPE,Bins$MOSTYPE,function(x) {- length(x)}),fill=factor(Bins$CARAVAN) ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Distribution by Customer Sub Type ( Purchased only )")


# Distribution based on  marital status 
# We can see that married people are more likely to buy insurance 
temp <-  ""
a <- ""
temp <-  sqldf(" select MRELGE ,  MRELSA,  MRELOV,  MFALLEEN , MFGEKIND , MFWEKIND ,  CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, MaritalStatus , Value, 1:6  )
ggplot(data = a , aes (y = Value, x = a$MaritalStatus , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Marital Status (Purchased only) ")


# Distribution based on education 
# we  can infer that education is not a important criteria 
temp <-  ""
a <- ""
temp <-  sqldf(" select MOPLHOOG ,MOPLMIDD ,  MOPLLAAG ,   CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, Education , Value, 1:3  )
ggplot(data = a , aes (y = Value, x = a$Education , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Education (Purchased only) ")


#  Based on social status 

temp <-  ""
a <- ""
temp <-  sqldf(" select MBERHOOG , MBERZELF , MBERBOER , MBERMIDD , MBERARBG , MBERARBO , MSKA  , 
           MSKB1, MSKB2 , MSKC , MSKD ,     CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, SocialStatus , Value, 1:11  )
ggplot(data = a , aes (y = Value, x = a$SocialStatus  , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Social Status (Purchased only) ")



# based on CAR ownerer ship
# people with 1 car are more likely to buy the car 

temp <-  ""
a <- ""
temp <-  sqldf(" select MAUT1, MAUT2 , MAUT0 ,     CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, Car , Value, 1:3  )
ggplot(data = a , aes (y = Value, x = a$Car  , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Car (Purchased only) ")


# Health Insurance distribution

temp <-  ""
a <- ""
temp <-  sqldf(" select MZFONDS , MZPART ,      CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, tempvar , Value, 1:2  )
ggplot(data = a , aes (y = Value, x = a$tempvar  , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Insurance (Purchased only) ")


# Distribution by income 
# We can see that most people who buy insurance have income between 30-45 and 45-75

temp <-  ""
a <- ""
temp <-  sqldf(" select MINKM30 , MINK3045 , MINK4575 , MINK7512 , MINK123M  ,       CARAVAN  from Cins where  CARAVAN = '1' ")
a <- gather(temp, tempvar , Value, 1:5  )
ggplot(data = a , aes (y = Value, x = a$tempvar  , fill = factor(a$CARAVAN))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Distribution by Income (Purchased only) ")


#





