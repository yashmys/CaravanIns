
library(tidyverse)
library(sqldf)
library(reshape2)
library(scales)

#load the caravan insurance data 

Cins <-  read.csv.sql("C1.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")

# Exclude the  first column 
Caravan_train <- Cins[-1]


#head(caravan_ins_data)
dim(Caravan_train )
names(Caravan_train )


# Inspect the data 
str(Caravan_train )
head(Caravan_train )


## check for missing  values 

Missing_Count <-  sum(is.na.data.frame(Caravan_train))
print(paste ("Missing count -> " , Missing_Count))

      
## lets see  number of insurance purchased / not purchased

attach(Caravan_train)
bar_label <- factor( Number_of_mobile_home_policies  , labels=c("NotPurchased","Purchased"))
#bar_label
ggplot(data =  Caravan_train, aes(y = "", x =Number_of_mobile_home_policies , fill = bar_label ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Not Purchased /  Purchased")

# from the graph - its evident that very few people have bought insurance and its highly imbalanced dataset

print(paste("Insurance count : 0 -> not having insurance , 1 - Having Insurance"))
table(Caravan_train$Number_of_mobile_home_policies)


##

#########################################################################################################################################
##########               BY NUMBER OF HOUSES
######  PEOPLE WHO HAVE BOUGHT INSURANCE OWN 1 OR 2 HOUSE  
###################################################################################################################################

grp_data <- ""
grp_data <- sqldf(" select  Number_of_houses  ,    Number_of_mobile_home_policies  
  from Caravan_train    ")
class(grp_data)
dim(grp_data)
#grp_data
#head(grp_data)

temp1  <- group_by(grp_data,Number_of_houses,Number_of_mobile_home_policies  ) 
templ1 <- filter(temp1,Number_of_mobile_home_policies   == 1 )
temp2 <- summarise(temp1,counts= n())
names(temp1)
names(grp_data)
names(temp2)
temp2

ggplot(data = temp2,aes(x = Number_of_houses ,y=counts,fill = factor(Number_of_houses ) )) + 
  geom_bar(stat = "identity") + 
  ggtitle ("NO OF HOUSES OWNED/PURCHASED")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


tempa3 <-  summarise(temp1,counts= n())
ggplot(data = tempa3,aes(x = Number_of_houses ,y=counts,fill = factor(Number_of_mobile_home_policies)  )) + 
  geom_bar(stat = "identity" , position = "fill") +
  ggtitle("HOUSES OWNED  BY %") + 
  scale_y_continuous(labels = percent_format())



## ##  inspect distribution of marital status /  insurance purchase 
##   most people who buy insurance are married 

temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select Married , Living_together    , Other_relation from Caravan_train  where Number_of_mobile_home_policies = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c(" Married","LiveIn","OtherRelation")


temp2 <- gather(grp_data,MaritalStatus,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ MaritalStatus ) + 
  ggtitle("Marital Status distribution")

#-------------------------------

##########################################################################################################################
###############################################################################################################
### DISTRIBUTION BY NUMBER OF CARS OWNED 
#  PEOPLE WITH 1 CAR ARE MORE LIKELY TO BUY INSURANCE 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select  X1_car                                       
,  X2_cars                                      
 ,  No_car   from Caravan_train where Number_of_mobile_home_policies = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("1 CAR","2 CAR","0 CAR")


temp2 <- gather(grp_data,CarOwnership,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ CarOwnership  ) + 
  ggtitle("CarOwnership")

###############
######################################################################################################################
#### Contribution towards  insurance policies 
####################

temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select 
                  private_third_party_insurance                
                  , third_party_insurance_firms                  
                  , third_party_insurane_agriculture             
                  , car_policies                                 
                  , delivery_van_policies                        
                  , Contribution_motorcycle_scooter              
                  , lorry_policies                               
                  , trailer_policies                             
                  , tractor_policies                             
                  , agricultural_machines_policies               
                  , moped_policies                               
                  , life_insurances                              
                  , private_accident_insurance_policies          
                  , family_accidents_insurance_policies          
                  , disability_insurance_policies                
                  , fire_policies                                
                  , surfboard_policies                           
                  , boat_policies                                
                  , bicycle_policies                             
                  , property_insurance_policies                  
                  , social_security_insurance_policies           
                  
                  from Caravan_train where Number_of_mobile_home_policies = 1  ")
class(grp_data)
dim(grp_data)

temp2 <- gather(grp_data,PolCount,Value,1:21)

ggplot(data= temp2, aes (x = PolCount,y = Value,fill = Value)) + 
  geom_bar(stat = 'identity' )  +
  ggtitle("Contribution Towards Policy") + 
  coord_flip()

################


################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select 
                  
                    Number_of_private_third_party_insurance      
                  , Number_of_third_party_insurance_firms        
                  , Number_of_third_party_insurane_agriculture   
                  , Number_of_car_policies                       
                  , Number_of_delivery_van_policies              
                  , Number_of_motorcycle_scooter_policies        
                  , Number_of_lorry_policies                     
                  , Number_of_trailer_policies                   
                  , Number_of_tractor_policies                   
                  , Number_of_agricultural_machines_policies     
                  , Number_of_moped_policies                     
                  , Number_of_life_insurances                    
                  , Number_of_private_accident_insurance_policies
                  , Number_of_family_accidents_insurance_policies
                  , Number_of_disability_insurance_policies      
                  , Number_of_fire_policies                      
                  , Number_of_surfboard_policies                 
                  , Number_of_boat_policies                      
                  , Number_of_bicycle_policies                   
                  , Number_of_property_insurance_policies        
                  , Number_of_social_security_insurance_policies
                  from Caravan_train where Number_of_mobile_home_policies = 1  ")
class(grp_data)
dim(grp_data)

temp2 <- gather(grp_data,PolCount,Value,1:21)

ggplot(data= temp2, aes (x = PolCount,y = Value,fill = Value)) + 
  geom_bar(stat = 'identity' )  +
  ggtitle("Insurance Policy Count") + 
  coord_flip()

###################





