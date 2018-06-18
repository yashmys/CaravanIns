#install.packages("tidyverse")
#install.packages("sqldf")

library(tidyverse)
library(sqldf)

#load the caravan insurance data 

caravan_ins_data <-  read.csv.sql("C1.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")

#head(caravan_ins_data)
dim(caravan_ins_data)



#set the name of the columns 

colnames(caravan_ins_data) <-  c (" Origin"                                        ,
                                  "Customer Subtype"                                      ,
                                  "Number of houses"                                      ,
                                  "Avg size household "                                   ,
                                  "Avg age"                                               ,
                                  "Customer main type"                                    ,
                                  "Roman catholic"                                        ,
                                  "Protestant"                                            ,
                                  "Other religion"                                        ,
                                  "No religion"                                           ,
                                  "Married"                                               ,
                                  "Living together"                                       ,
                                  "Other relation"                                        ,
                                  "Singles"                                               ,
                                  "Household without children"                            ,
                                  "Household with children"                               ,
                                  "High level education "                                 ,
                                  "Medium level education "                               ,
                                  "Lower level education  "                               ,
                                  "High status "                                          ,
                                  "Entrepreneur "                                         ,
                                  "Farmer "                                               ,
                                  "Middle management"                                     ,
                                  "Skilled labourers"                                     ,
                                  "Unskilled labourers"                                   ,
                                  "Social class A"                                        ,
                                  "Social class B1"                                       ,
                                  "Social class B2"                                       ,
                                  "Social class C"                                        ,
                                  "Social class D "                                       ,
                                  "Rented house"                                          ,
                                  "Home owners "                                          ,
                                  "1 car"                                                 ,
                                  "2 cars"                                                ,
                                  "No car"                                                ,
                                  "National Health Service"                               ,
                                  "Private health insurance"                              ,
                                  "Income < 30.000"                                       ,
                                  "Income 30-45.000"                                      ,
                                  "Income 45-75.000"                                      ,
                                  "Income 75-122.000"                                     ,
                                  "Income >123.000 "                                      ,
                                  "Average income"                                        ,
                                  "Purchasing power class"                                ,
                                  "Contribution private third party insurance"            ,
                                  "Contribution third party insurance (firms)"            ,
                                  "Contribution third party insurane (agriculture)"       ,
                                  "Contribution car policies"                             ,
                                  "Contribution delivery van policies"                    ,
                                  "Contribution motorcycle/scooter policies"              ,
                                  "Contribution lorry policies"                           ,
                                  "Contribution trailer policies"                         ,
                                  "Contribution tractor policies"                         ,
                                  "Contribution agricultural machines policies"           ,
                                  "Contribution moped policies"                           ,
                                  "Contribution life insurances"                          ,
                                  "Contribution private accident insurance policies"      ,
                                  "Contribution family accidents insurance policies"      ,
                                  "Contribution disability insurance policies"            ,
                                  "Contribution fire policies"                            ,
                                  "Contribution surfboard policies"                       ,
                                  "Contribution boat policies"                            ,
                                  "Contribution bicycle policies"                         ,
                                  "Contribution property insurance policies"              ,
                                  "Contribution social security insurance policies"       ,
                                  "Number of private third party insurance "              ,
                                  "Number of third party insurance (firms)"               ,
                                  "Number of third party insurance (agriculture)"         ,
                                  "Number of car policies"                                ,
                                  "Number of delivery van policies"                       ,
                                  "Number of motorcycle/scooter policies"                 ,
                                  "Number of lorry policies"                              ,
                                  "Number of trailer policies"                            ,
                                  "Number of tractor policies"                            ,
                                  "Number of agricultural machines policies"              ,
                                  "Number of moped policies"                              ,
                                  "Number of life insurances"                             ,
                                  "Number of private accident insurance policies"         ,
                                  "Number of family accidents insurance policies"         ,
                                  "Number of disability insurance policies"               ,
                                  "Number of fire policies"                               ,
                                  "Number of surfboard policies"                          ,
                                  "Number of boat policies"                               ,
                                  "Number of bicycle policies"                            ,
                                  "Number of property insurance policies"                 ,
                                  "Number of social security insurance policies"          ,
                                  "Purchased" )                                        


#str(caravan_ins_data)

#check for  blank values in any on the columns

caravan_ins_data[caravan_ins_data == ""] <-  NA
caravan_ins_data <-  na.omit(caravan_ins_data)
dim(caravan_ins_data)



#

class(caravan_ins_data)

c1 <-  data.frame(caravan_ins_data)
class(c1)

c2 <- data.frame(c1$Purchased)
class(c2)

ggplot(data =c2 , aes(y = "" , x = c2 , fill = c2)) + 
  geom_bar(stat = "identity")

c3 <- c1
c3$c1.p <-  as.factor(c1$Purchased)


class(c3)

ggplot(data =c3 , aes(y = "" , x = c3$c1.p  )) + geom_bar(stat = "identity") + labs(x="CARAVAN")



