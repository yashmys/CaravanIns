#install.packages("tidyverse")
#install.packages("sqldf")

library(tidyverse)
library(sqldf)
library(reshape2)
library(scales)

#load the caravan insurance data 

Cins <-  read.csv.sql("C3.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")


#head(caravan_ins_data)
dim(Cins)
names(Cins)



# ggplot(data =  caravan_ins_data, aes( y = caravan_ins_data$Sump ,  x = caravan_ins_data$Purchased,fill=factor(caravan_ins_data$Purchased) ) )+ 
#   geom_bar(stat = "identity" )
# From the graph we can see that only a small % of people actually buy insurance 

bar_label <- factor(Cins$CARAVAN, labels=c("NotPurchased","Purchased"))
#bar_label
ggplot(data =  Cins, aes(y = "", x = Cins$CARAVAN, fill = bar_label ) )+ 
  geom_bar(stat = "identity" ) + 
  ggtitle("Purchased / Not Purchased")

#################################################################################################################################
# Graph Customer Main Type / no of people bought the insurance
###############################################################################################################################
bar_label <- ""
#levels(bar_label)
lbl_str <- c("Successful hedonists   ",
             "Driven Growers         ",
             "Average Family         ",
             "Living well            ",
             "Cruising Seniors       ",
             "Retired and Religeous  ",
             "Family with grown ups  ",
             "Conservative families  ",
             "Farmers                ")
            


grp_data <- sqldf(" select MOSHOOFD ,   CARAVAN  from Cins  ")

class(grp_data)
dim(grp_data)
#grp_data
#head(grp_data)

temp1  <- group_by(grp_data,MOSHOOFD,CARAVAN) 
templ1 <- filter(temp1,CARAVAN == 1 )
temp2 <- summarise(templ1,counts= n())
names(temp1)
names(grp_data)
names(temp2)

bar_label <- ""
rr <- factor(temp2$MOSHOOFD)
rr
bar_label <- factor(temp2$MOSHOOFD,labels = lbl_str )
bar_label


ggplot(data = temp2,aes(x = MOSHOOFD,y=counts,fill = bar_label )) + 
  geom_bar(stat = "identity") +
  ggtitle("OCCUPATION / PURCHASED")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

tempa3 <-  summarise(temp1,counts= n())
ggplot(data = tempa3,aes(x = MOSHOOFD,y=counts,fill = CARAVAN )) + 
       geom_bar(stat = "identity" , position = "fill") +
       ggtitle("OCCUPATION / PURCHASED BY %") + 
  scale_y_continuous(labels = percent_format())
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>






#########################################################################################################################################
##########               BY NUMBER OF HOUSES
###### THE HOUSE HOLD/PURCHASED IS JUST 1 AND 2 
###################################################################################################################################

grp_data <- ""
grp_data <- sqldf(" select MAANTHUI ,   CARAVAN  from Cins  ")
class(grp_data)
dim(grp_data)
#grp_data
#head(grp_data)

temp1  <- group_by(grp_data,MAANTHUI,CARAVAN) 
templ1 <- filter(temp1,CARAVAN == 1 )
temp2 <- summarise(templ1,counts= n())
names(temp1)
names(grp_data)
names(temp2)
temp2

ggplot(data = temp2,aes(x = MAANTHUI,y=counts,fill = factor(MAANTHUI) )) + 
  geom_bar(stat = "identity") + 
  ggtitle ("NO OF HOUSEHOLD/PURCHASED")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


tempa3 <-  summarise(temp1,counts= n())
ggplot(data = tempa3,aes(x = MAANTHUI,y=counts,fill = CARAVAN )) + 
  geom_bar(stat = "identity" , position = "fill") +
  ggtitle("HOUSEHOLD  BY %") + 
  scale_y_continuous(labels = percent_format())


####################################################################################################################################
####################################################################################################################################
#########################################################################################################################################
##########              AVG SIZE OF HOUSEHOLDS
######                SIZE OF THE HOUSEHOLD WHO HAVE BOUGHT INSURANCE IS  2 , 3 AND 4 
#####             BY THE % GRAPH WE CAN MAKE OUT THAT HOUSEHOLD SIZE DOESNT MATTER
###################################################################################################################################

grp_data <- ""
grp_data <- sqldf(" select MGEMOMV ,   CARAVAN  from Cins  ")
class(grp_data)
dim(grp_data)
#grp_data
#head(grp_data)
temp1 <-  ""
templ1 <-  ""
temp2 <- ""


temp1  <- group_by(grp_data,MGEMOMV,CARAVAN) 
templ1 <- filter(temp1,CARAVAN == 1 )
temp2 <- summarise(templ1,counts= n())
names(temp1)
names(grp_data)
names(temp2)
temp2

ggplot(data = temp2,aes(x = MGEMOMV,y=counts,fill = factor(MGEMOMV) )) + 
  geom_bar(stat = "identity") + 
  ggtitle ("AVG SIZE OF HOUSEHOLDS")






#--------------------------------------------------

tempa3 <-  summarise(temp1,counts= n())
ggplot(data = tempa3,aes(x = MGEMOMV,y=counts,fill = CARAVAN )) + 
  geom_bar(stat = "identity" , position = "fill") +
  ggtitle("HOUSEHOLD SIZE BY %") + 
  scale_y_continuous(labels = percent_format())

#---------------------------------------------------------------------------



#################################################################################################################################
#########################################################################################################################################
##########              AVG AGE - PEOPLE WHO BOUGHT INSURANCE ARE IN THE AGE GROUP OF 20-50 YEARS 
###################################################################################################################################

grp_data <- ""
grp_data <- sqldf(" select MGEMLEEF ,   CARAVAN  from Cins  ")
class(grp_data)
dim(grp_data)
#grp_data
#head(grp_data)
temp1 <-  ""
templ1 <-  ""
temp2 <- ""


temp1  <- group_by(grp_data,MGEMLEEF,CARAVAN) 
templ1 <- filter(temp1,CARAVAN == 1 )
temp2 <- summarise(templ1,counts= n())
names(temp1)
names(grp_data)
names(temp2)
temp2

ggplot(data = temp2,aes(x = MGEMLEEF,y=counts,fill = factor(MGEMLEEF) )) + 
  geom_bar(stat = "identity") + 
  ggtitle ("AVG AGE")

#------------------------------------------------

tempa3 <-  summarise(temp1,counts= n())
ggplot(data = tempa3,aes(x = MGEMLEEF,y=counts,fill = CARAVAN )) + 
  geom_bar(stat = "identity" , position = "fill") +
  ggtitle("AVG AGE BY %") + 
  scale_y_continuous(labels = percent_format())

# --------------------------------------------------------------------------------------



###########################################################################################################################################
##########   DISTRIBUTION BY RELIGION 
########################################################################################################################################

temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <- ""
grp_data <- sqldf(" select MGODRK , MGODPR ,MGODOV ,MGODGE ,   CARAVAN  from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("RomanCatholic","Protestant","OtherReligion","NoReligion")
temp2 <- gather(grp_data,Religion,Value,1:4)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
   geom_bar(stat = 'identity') +
   facet_grid(. ~ Religion ) + 
  ggtitle("Distribution by Religion")  + 
  geom_text(aes(label = Value))

#--------------------------

# -----------------------------------------------

#############################################################################################################
######  DISTRIBUTION BY MARITAL STATUS 
###  We can see that most of the people who have bought insurance are married people
###############################################################################################################


temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MRELGE	 ,MRELSA  ,MRELOV	,  CARAVAN  from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c(" Married","LiveIn","OtherRelation")


temp2 <- gather(grp_data,MaritalStatus,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ MaritalStatus ) + 
  ggtitle("Marital Status distribution")

#-------------------------------


#


###############################################################################################################
### DISTRIBUTION BY HOUSEHOLD SIZE 
###############################################################################################################


temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MFALLEEN	 , MFGEKIND  , MFWEKIND   from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("Singles","HouseHold with Children","Household without children")


temp2 <- gather(grp_data,HouseholdSize,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ HouseholdSize ) + 
  ggtitle("HouseholdSize distribution")


##################################################################################################################
#  PEOPLE WITH MEDIUM/LOW LEVEL EDUCATION ARE MORE LIKELY TO BUY INSURANCE
#
###############################################################################################################
### DISTRIBUTION BY EDUCATION 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MOPLHOOG	 , MOPLMIDD  , MOPLLAAG   from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("High","Medium","Low")


temp2 <- gather(grp_data,Education,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ Education ) + 
  ggtitle("Education distribution")


##################################################################################################################
###############################################################################################################
### DISTRIBUTION BY  SOCIAL STATUS 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MBERHOOG ,MBERZELF ,MBERBOER, MBERMIDD , MBERARBG , MBERARBO    from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("HighStatus","Entrepreneur","Farmer","MiddleManagement","SkilledLabour","UnskilledLabour")


temp2 <- gather(grp_data,SocialStatus,Value,1:6)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ SocialStatus ) + 
  ggtitle("Social Status")

##################################################################################################################################
###############################################################################################################
### DISTRIBUTION BY  SOCIAL STATUS 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MSKA , MSKB1 ,MSKB2 , MSKC , MSKD   from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("CLASS-A","CLASS-B1","CLASS-B2","CLASS-C","CLASS-D")


temp2 <- gather(grp_data,ClassSoc,Value,1:5)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ ClassSoc ) + 
  ggtitle("Class")



###############################################################################################################################
###############################################################################################################
### DISTRIBUTION OF HouseOwnership
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MHHUUR , MHKOOP   from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("Rented","Own House")


temp2 <- gather(grp_data,HouseOwnership,Value,1:2)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~HouseOwnership  ) + 
  ggtitle("HouseOwnership")


##########################################################################################################################
###############################################################################################################
### DISTRIBUTION BY NUMBER OF CARS OWNED 
#  PEOPLE WITH 1 CAR ARE MORE LIKELY TO BUY INSURANCE 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MAUT1 , MAUT2 , MAUT0 from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("1 CAR","2 CAR","0 CAR")


temp2 <- gather(grp_data,CarOwnership,Value,1:3)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ CarOwnership  ) + 
  ggtitle("CarOwnership")


###########################################################################################
###############################################################################################################
### DISTRIBUTION BY  HEALTH SERVICE 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MZFONDS , MZPART from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("NationalHealthService","PrivateHealthInsurance")


temp2 <- gather(grp_data,HealthService,Value,1:2)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ HealthService  ) + 
  ggtitle("HealthService")
######################################################################################################################

###############################################################################################################
### DISTRIBUTION BY  INCOME 
### PEOPLE WHO BUY INS ARE HAVING INCOME BETWEEN 30 - 75 
###############################################################################################################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select MINKM30 , MINK3045 , MINK4575 , MINK7512 , MINK123M , MINKGEM from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)
names(grp_data) <- c("INC <30","INC 30-45","INC 45 75","INC 75 122","INC >123","AVGINCOME")


temp2 <- gather(grp_data,Income,Value,1:6)

ggplot(data= temp2, aes (x = Value,y = "",fill = Value)) + 
  geom_bar(stat = 'identity') +
  facet_grid(. ~ Income  ) + 
  ggtitle("Income")


#################################################################################
####  contribution towards insurance policies 
#   Contribution private third party insurance
#  Contribution car policies
#  Contribution fire policies
# we can see a corelation between people who bought  caravan insurance and people  who bought these above insurance 
###############################################################################################################################



temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select
                  PWAPART   ,PWABEDR   ,PWALAND   ,PPERSAUT  ,PBESAUT   ,PMOTSCO   ,PVRAAUT   ,PAANHANG  ,PTRACTOR  ,PWERKT    ,PBROM     ,PLEVEN    ,PPERSONG  ,PGEZONG   ,PWAOREG   ,PBRAND    ,PZEILPL   ,PPLEZIER  ,PFIETS    ,PINBOED   
                  from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)

temp2 <- gather(grp_data,ConPol,Value,1:20)

ggplot(data= temp2, aes (x = ConPol,y = Value,fill = Value)) + 
  geom_bar(stat = 'identity' )  +
  ggtitle("Contribution Towards Policy") + 
  coord_flip()

######################################################################################################################
#### Number of insurance policies 
####################

temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select 
AWAPART ,AWABEDR,AWALAND,APERSAUT,ABESAUT,AMOTSCO,AVRAAUT,AAANHANG,ATRACTOR,AWERKT,ABROM,ALEVEN,APERSONG,AGEZONG,AWAOREG,ABRAND,AZEILPL,APLEZIER,AFIETS,AINBOED,ABYSTAND
from Cins where CARAVAN = 1  ")
class(grp_data)
dim(grp_data)

temp2 <- gather(grp_data,PolCount,Value,1:21)

ggplot(data= temp2, aes (x = PolCount,y = Value,fill = Value)) + 
  geom_bar(stat = 'identity' )  +
  ggtitle("Insurance Policy Count") + 
  coord_flip()

################
temp1 <-  ""
templ1 <-  ""
temp2 <- ""
grp_data <-  ""
grp_data <- sqldf(" select 
                  
                  ,   Number_of_private_third_party_insurance      
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


