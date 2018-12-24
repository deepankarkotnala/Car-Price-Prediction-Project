################################################################################################
##                                                                                            ##
##   Project     : Geely Auto - Car Price Model                                               ##
##                                                                                            ##
##   Description : To model the price of cars with the available independent variables.       ##                                               
##                 We need to exaplin how exactly the prices vary with the independent        ##
##                 variables so the management can accordingly manipulate the design of       ##
##                 the cars, the business strategy etc. to meet certain price levels.         ##
##                 The model will be good to understand the pricing dynamics of a new market. ## 
##                                                                                            ##
##   Date        : 28-Oct-2018                                                                ##
##                                                                                            ##
##   Author      : Deepankar Kotnala                                                          ##
##                                                                                            ##
################################################################################################


# Clearing the previously loaded objects. 
remove(list = ls())
# Suppressing warnings
options(warn = -1)

# Install the required packages and load libraries:

# Uncomment these three lines if the packages are not installed.

# install.packages('MASS') #for StepAIC
# install.packages('car') #for VIF
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("reshape2")

library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tidyverse)

##**************************************************************************************##
##                                                                                      ##
##          I M P O R T I N G   A N D   E X P L O R I N G   T H E   D A T A             ##
##                                                                                      ##
##**************************************************************************************##

carsdata <- carsdata_orig <- read.csv("carprice_Assignment.csv")

# Having a look at the data

dim(carsdata)
str(carsdata)
summary(carsdata)
# View(carsdata)

# Checking for NULL values
sum(is.na(carsdata))  # No NULL values present in the dataset

# Checking for duplicate records
nrow(carsdata) != length(unique(carsdata$car_ID))
# There are unique car_IDs present in the dataset. 
# That means details of 205 unique car models is available to us.


##**************************************************************************************##
##               Understanding the data from the provided data dictionary               ##
##**************************************************************************************##

##**************************************************************************************##
##                                 Dependent Variable                                   ##
##**************************************************************************************##

#price => Price of car (Numeric)

##**************************************************************************************##
##                                Independent Variables                                 ##
##**************************************************************************************##

#Car_ID			      => Unique id of each observation (Interger)		
#Symboling 		    => Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, 
#                    -3 that it is probably pretty safe.(Categorical) 		
#carCompany		    => Name of car company (Categorical)		
#fueltype		      => Car fuel type i.e gas or diesel (Categorical)		
#aspiration		    => Aspiration used in a car (Categorical)		
#doornumber		    => Number of doors in a car (Categorical)		
#carbody		      => body of car (Categorical)		
#drivewheel		    => type of drive wheel (Categorical)		
#enginelocation	  => Location of car engine (Categorical)		
#wheelbase		    => Weelbase of car (Numeric)		
#carlength		    => Length of car (Numeric)		
#carwidth		      => Width of car (Numeric)		
#carheight		    => height of car (Numeric)		
#curbweight		    => The weight of a car without occupants or baggage. (Numeric)		
#enginetype		    => Type of engine. (Categorical)		
#cylindernumber	  => cylinder placed in the car (Categorical)		
#enginesize		    => Size of car (Numeric)		
#fuelsystem		    => Fuel system of car (Categorical)		
#boreratio		    => Boreratio of car (Numeric)		
#stroke			      => Stroke or volume inside the engine (Numeric)		
#compressionratio => compression ratio of car (Numeric)		
#horsepower		    => Horsepower (Numeric)		
#peakrpm		      => car peak rpm (Numeric)		
#citympg		      => Mileage in city (Numeric)		
#highwaympg		    => Mileage on highway (Numeric)


##**************************************************************************************##
##                                                                                      ##
##                             D A T A   C L E A N I N G                                ##
##                                                                                      ##
##**************************************************************************************##


#car_ID would not be of any use for our analysis. So removing car_ID from our dataset.
carsdata <- carsdata[,-1]

#Extracting the Brand name of the carmaker from CarName column. 
#Also, the car model would not be of much significance in our prediction model.
#There will be 205 different car models, and hence 205 different categories. This can make our task of creating the model a lot more tedious.
#Since it's irrelevant to keep the car model, we will only be keeping the car brand name and will remove the car model number as of now.
#So, removing the model of each car, and saving only the brand name.
carsdata <- separate(carsdata,CarName,into="CarBrand",sep=" ",remove=T)


# Looking at the unique car models available.
levels(as.factor(carsdata$CarBrand))
#  [1] "alfa-romero" "audi"        "bmw"         "buick"       "chevrolet"   "dodge"       "honda"       "isuzu"       "jaguar"      "maxda"      
# [11] "mazda"       "mercury"     "mitsubishi"  "nissan"      "Nissan"      "peugeot"     "plymouth"    "porcshce"    "porsche"     "renault"    
# [21] "saab"        "subaru"      "toyota"      "toyouta"     "vokswagen"   "volkswagen"  "volvo"       "vw"  

# We can see that there are some spelling mistakes in the brand names of the cars.
# Also, there are companies whose names are written in different cases.
# Let's correct these spelling mistakes and improper cases.

# Converting all CarBrand names to lowercase for consistency.
carsdata$CarBrand <- tolower(carsdata$CarBrand)


# Correcting the spelling mistakes (typos or inconsistencies)

carsdata$CarBrand <- gsub(pattern="maxda",x=carsdata$CarBrand,replacement="mazda")
carsdata$CarBrand <- gsub(pattern="toyouta",x=carsdata$CarBrand,replacement="toyota")
carsdata$CarBrand <- gsub(pattern="porcshce",x=carsdata$CarBrand,replacement="porsche")
carsdata$CarBrand <- gsub(pattern="vokswagen",x=carsdata$CarBrand,replacement="volkswagen")
carsdata$CarBrand <- gsub(pattern="vw",x=carsdata$CarBrand,replacement="volkswagen")


##**************************************************************************************##
##                                                                                      ##
##                        C H E C K I N G    F O R   O U T L I E R S                    ##
##                                                                                      ##
##**************************************************************************************##

# Having outliers can change in slope of the best fit line
# So, it's better to remove these outliers/ extreme values which can reduce the capability of our model.
ggplot(carsdata, aes(x = "", y = carsdata$wheelbase)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Wheel Base", title = "Wheel Base Plot") + 
  theme_light()
# Values above 114 are outliers

ggplot(carsdata, aes(x = "", y = carsdata$carlength)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Car Length", title = "Car Length Plot") + 
  theme_light()
# Values less than 145

ggplot(carsdata, aes(x = "", y = carsdata$carheight)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Car height", title = "Car height Plot") + 
  theme_light()
# No outliers

ggplot(carsdata, aes(x = "", y = carsdata$curbweight)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Curb Weight", title = "Curb Weight Plot") + 
  theme_light()
# No outliers

ggplot(carsdata, aes(x = "", y = carsdata$enginesize)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Engine Size", title = "Engine Size Plot") + 
  theme_light()
# Values above 205 are outliers

ggplot(carsdata, aes(x = "", y = carsdata$boreratio)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Bore Ratio", title = "Bore Ratio Plot") + 
  theme_light() 
# No Outliers

ggplot(carsdata, aes(x = "", y = carsdata$stroke)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Stroke", title = "Stroke Plot") + 
  theme_light()
# Values greater than 3.8 or the values less than 2.7

ggplot(carsdata, aes(x = "", y = carsdata$compressionratio)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Compression Ratio", title = "Compression Ratio") + 
  theme_light()
# Values greater than 10 and values lesser than 7.5

ggplot(carsdata, aes(x = "", y = carsdata$horsepower)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Horse Power", title = "Horse Power Plot") + 
  theme_light()
# Values greater than 180

ggplot(carsdata, aes(x = "", y = carsdata$peakrpm)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Peak RPM", title = "Peak RPM Plot") + 
  theme_light()
# Values greater than 6000

ggplot(carsdata, aes(x = "", y = carsdata$citympg)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "City mpg", title = "City mpg Plot") + 
  theme_light()
# Values greater than 45

ggplot(carsdata, aes(x = "", y = carsdata$highwaympg)) +
  geom_boxplot(fill = "khaki1", outlier.colour = "red", 
               outlier.shape = 1, outlier.size = 3) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  labs( y = "Highway mpg", title = "Highway mpg Plot") + 
  theme_light()
# Values greater than 47

##**************************************************************************************##
##                                                                                      ##
##                          H A N D L I N G   T H E  O U T L I E R S                    ##
##                                                                                      ##
##**************************************************************************************##

# Having outliers can change in slope of the best fit line
# Suppressing the outliers so that they don't interfere with our analysis.

carsdata[which(carsdata$wheelbase > 114),]$wheelbase <-  114
carsdata[which(carsdata$carlength < 145),]$carlength <-  145
carsdata[which(carsdata$enginesize > 205),]$enginesize <-  205
carsdata[which(carsdata$stroke < 2.7),]$stroke <-  2.7
carsdata[which(carsdata$stroke > 3.8),]$stroke <-  3.8
carsdata[which(carsdata$compressionratio < 7.5),]$compressionratio <-  7.5
carsdata[which(carsdata$compressionratio > 10),]$compressionratio <-  10
carsdata[which(carsdata$horsepower > 180),]$horsepower <-  180
carsdata[which(carsdata$peakrpm > 6000),]$peakrpm <-  6000
carsdata[which(carsdata$citympg > 45),]$citympg <-  45
carsdata[which(carsdata$highwaympg > 47),]$highwaympg <-  47


##**************************************************************************************##
##                                                                                      ##
##                            L I N E A R   R E G R E S S I O N                         ##
##                                                                                      ##
##**************************************************************************************##

# Changing the CarBrand column to factor
carsdata$CarBrand <- as.factor(carsdata$CarBrand)

# Changing symboling column to factor
carsdata$symboling <- as.factor(carsdata$symboling)

#Counting the different kind of symbolings used
levels(carsdata$symboling)
# Six levels : "-2" "-1" "0"  "1"  "2"  "3" 

# Counting the number of car makers (CarBrands)
length(levels(as.factor(carsdata$CarBrand)))
# 22 different Car Brands are present in the US market

# Going ahead to the next column i.e, fueltype
levels(carsdata$fueltype)
# Two levels:  "diesel" "gas" 

levels(carsdata$aspiration)
# Two levels:  "std"   "turbo"

levels(carsdata$doornumber)
# Two levels: "four" "two" 

levels(carsdata$carbody)
# Five levels: "convertible" "hardtop"     "hatchback"   "sedan"       "wagon"

levels(carsdata$drivewheel)
# Three levels: "4wd" "fwd" "rwd"

levels(carsdata$enginelocation)
# Two levels: "front" "rear"  

levels(carsdata$enginetype)
# "dohc"  "dohcv" "l"     "ohc"   "ohcf"  "ohcv"  "rotor"

levels(carsdata$cylindernumber)
# Seven levels: "eight"  "five"   "four"   "six"    "three"  "twelve" "two" 

levels(carsdata$fuelsystem)
# Eight levels: "1bbl" "2bbl" "4bbl" "idi"  "mfi"  "mpfi" "spdi" "spfi"


##**************************************************************************************##
##                                                                                      ##
##                  Convert factors with 2 levels to numerical variables                ##
##                                                                                      ##
##**************************************************************************************##

# These are the categorical variables present in the dataset as per the data dictionary provided.
# Symboling, carName, fueltype, aspiration, doornumber, carbody, drivewheel, enginelocation, 
# enginetype, fuelsystem, cylindernumber, doornumber

levels(carsdata$fueltype) <- c(1,0)
# diesel: 1, gas: 0
carsdata$fueltype <- as.numeric(levels(carsdata$fueltype))[carsdata$fueltype]

levels(carsdata$aspiration) <- c(1,0)
# std: 1, turbo: 0
carsdata$aspiration <- as.numeric(levels(carsdata$aspiration))[carsdata$aspiration]

levels(carsdata$doornumber) <- c(1,0)
# four: 1, two: 0
carsdata$doornumber <- as.numeric(levels(carsdata$doornumber))[carsdata$doornumber]

levels(carsdata$enginelocation) <- c(1,0)
# front: 1, rear: 0
carsdata$enginelocation <- as.numeric(levels(carsdata$enginelocation))[carsdata$enginelocation]


##**************************************************************************************##
##         C R E A T I O N   O F   D U M M Y   V A R I A B L E S   F O R  T H E         ##
##             V A R I A B L E S  W I T H    M U L T I P L E   L E V E L S              ##
##                                                                                      ##
##**************************************************************************************##

# Create the dummy variable for symboling variable
dummy_symboling <- data.frame(model.matrix( ~symboling, data = carsdata))
dummy_symboling <- dummy_symboling[,-1]
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata[,-1], dummy_symboling)


# Create the dummy variable for carbody variable
dummy_carbody <- data.frame(model.matrix( ~ carbody, data = carsdata))
dummy_carbody <- cbind(dummy_carbody[,-1])
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-5], dummy_carbody)


# Create the dummy variable for drivewheel variable
dummy_drivewheel <- data.frame(model.matrix( ~ drivewheel, data = carsdata))
dummy_drivewheel <- cbind(dummy_drivewheel[,-1])
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-5], dummy_drivewheel)


# Create the dummy variable for enginetype variable
dummy_enginetype <- data.frame(model.matrix( ~ enginetype, data = carsdata))
dummy_enginetype <- cbind(dummy_enginetype[,-1])
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-11], dummy_enginetype)


# Create the dummy variable for enginetype variable
dummy_cylindernumber <- data.frame(model.matrix( ~ cylindernumber, data = carsdata))
dummy_cylindernumber <- cbind(dummy_cylindernumber[,-1])
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-11], dummy_cylindernumber)


# Create the dummy variable for symboling variable
dummy_carBrand <- data.frame(model.matrix( ~CarBrand, data = carsdata))
dummy_carBrand <- dummy_carBrand[,-1]
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-1], dummy_carBrand)


# Create the dummy variable for symboling variable
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carsdata))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
# Combine the dummy variables and the numeric columns of carsdata dataset, in a new dataset called carsdata_1
carsdata_1 <- cbind(carsdata_1[,-11], dummy_fuelsystem)


# Now that we have converted all the categorical variables into numeric values, 
# let us proceed further to separating the data into Training and Test data.

##**************************************************************************************##
##                                                                                      ##
##                            S E P A R A T I N G  D A T A                              ##
##                   I N T O   T R A I N   A N D   T E S T   D A T A                    ##
##                                                                                      ##
##**************************************************************************************##

set.seed(1000)
trainindices= sample(1:nrow(carsdata_1), 0.7*nrow(carsdata_1))
train = carsdata_1[trainindices,]
test  = carsdata_1[-trainindices,]


##**************************************************************************************##
##                                                                                      ##
##                             M O D E L   B U I L D I N G                              ##
##                                                                                      ##
##**************************************************************************************##

# Build the 1st model containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

# Now let's make use of stepAIC
# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

step <- stepAIC(model_1, direction="both")

# Multiple iterations have been done here through stepAIC.
# Now let's check the model equation by using the "step" command.
# stepAIC makes multiple calls while checking which variables to keep


model_2 <- lm(formula = price ~ doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + highwaympg + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo + fuelsystemmpfi + 
                fuelsystemspdi, data=train)

summary(model_2)
#Multiple R-squared:  0.9626,	Adjusted R-squared:  0.9494  

# Check for multicollinearity
vif(model_2)

# We have to remove the variables having:
# high VIF (>2 generally), 
# high p-value (p> 0.05), i.e, variables which are insignificant
# We will be doing this process one by one for all variables.

# Variables with high p-value and High VIF should be removed.
# Removing the highwaympg variable from the model.

model_3 <- lm(formula = price ~ doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo + fuelsystemmpfi + 
                fuelsystemspdi, data=train)

summary(model_3)
# Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9481
vif(model_3)

# Removing cylindernumberthree and re-running the model.

model_4 <- lm(formula = price ~ doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo + fuelsystemmpfi + 
                fuelsystemspdi, data=train)

summary(model_4)
# Multiple R-squared:   0.96,	Adjusted R-squared:  0.9469 

vif(model_4)
# Removing fuelsystemspdi and re-running the model.
model_5 <- lm(formula = price ~ doornumber + enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo + fuelsystemmpfi, data=train)

summary(model_5)
# Multiple R-squared:  0.9592,	Adjusted R-squared:  0.9464  

vif(model_5)
# Removing doornumber and re-running the model.

model_6 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo + fuelsystemmpfi, data=train)

summary(model_6)
# Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9458 

vif(model_6)
# Removing fuelsystemmpfi and re-running the model

model_7 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_7)
# Multiple R-squared:  0.9569,	Adjusted R-squared:  0.9444 

vif(model_7)
# Removing CarBrandjaguar and re-running the model

model_8 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + carbodyhatchback + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_8)
# Multiple R-squared:  0.9567,	Adjusted R-squared:  0.9446 

vif(model_8)
# Removing carbodyhatchback and re-running the model.

model_9 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + CarBrandsaab + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_9)
# Multiple R-squared:  0.9553,	Adjusted R-squared:  0.9433

vif(model_9)
# Removing CarBrandsaab and re-running the model.

model_10 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                carwidth + carheight + curbweight + stroke + compressionratio + 
                peakrpm + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarBrandaudi + 
                CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_10)
# Multiple R-squared:  0.9539,	Adjusted R-squared:  0.9421

vif(model_10)
# Removing compressionratio and re-running the model.

model_11 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                 carwidth + carheight + curbweight + stroke +  
                 peakrpm + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CarBrandaudi + 
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_11)
# Multiple R-squared:  0.9526,	Adjusted R-squared:  0.9409

vif(model_11)
# Removing stroke and re-running the model.

model_12 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                 carwidth + carheight + curbweight + peakrpm + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandaudi + 
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_12)
# Multiple R-squared:  0.9509,	Adjusted R-squared:  0.9394

vif(model_12)
# Removing peakrpm and re-running the model.

model_13 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                 carwidth + carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandaudi + 
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_13)
# Multiple R-squared:  0.9496,	Adjusted R-squared:  0.9383 

vif(model_13)
# Removing CarBrandaudi and re-running the model.

model_14 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                 carwidth + carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix +  
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_14)
# Multiple R-squared:  0.9484,	Adjusted R-squared:  0.9373

vif(model_14)
# Removing carwidth and re-running the model.

model_15 <- lm(formula = price ~ enginelocation + wheelbase + carlength + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix +  
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_15)
# Multiple R-squared:  0.9458,	Adjusted R-squared:  0.9348

vif(model_15)
# Removing carlength and re-running the model.

model_16 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix +  
                 CarBrandbmw + CarBrandchevrolet + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_16)
# Multiple R-squared:  0.943,	Adjusted R-squared:  0.932

vif(model_16)
# Removing CarBrandchevrolet and re-running the model

model_17 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix +  
                 CarBrandbmw + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmazda + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_17)
# Multiple R-squared:  0.9402,	Adjusted R-squared:  0.9293

vif(model_17)
# Removing CarBrandmazda and re-running the model

model_18 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix +  
                 CarBrandbmw + CarBranddodge + CarBrandhonda + 
                 CarBrandisuzu + CarBrandmitsubishi + 
                 CarBrandnissan + CarBrandplymouth + CarBrandrenault + 
                 CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_18)
# Multiple R-squared:  0.9374,	Adjusted R-squared:  0.9265

vif(model_18)
# Removing CarBrandhonda and re-running the model

model_19 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + CarBrandplymouth + 
                 CarBrandrenault +CarBrandtoyota + CarBrandvolkswagen + CarBrandvolvo, data=train)

summary(model_19)
# Multiple R-squared:  0.9355,	Adjusted R-squared:  0.9249 

vif(model_19)
#Removing CarBrandvolvo and re-running the model

model_20 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandisuzu + CarBrandmitsubishi + CarBrandnissan + CarBrandplymouth + 
                 CarBrandrenault +CarBrandtoyota + CarBrandvolkswagen , data=train)

summary(model_20)
# Multiple R-squared:  0.934,	Adjusted R-squared:  0.9238

vif(model_20)
# Removing CarBrandnissan and re-running the model

model_21 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandisuzu + CarBrandmitsubishi + CarBrandplymouth + 
                 CarBrandrenault +CarBrandtoyota + CarBrandvolkswagen , data=train)

summary(model_21)
# Multiple R-squared:  0.9332,	Adjusted R-squared:  0.9236

vif(model_21)
# Removing CarBrandvolkswagen and re-running the model.

model_22 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandisuzu + CarBrandmitsubishi + CarBrandplymouth + 
                 CarBrandrenault +CarBrandtoyota , data=train)

summary(model_22)
# Multiple R-squared:  0.9332,	Adjusted R-squared:  0.9241 

vif(model_22)
# Removing CarBrandisuzu and re-running the model

model_23 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandmitsubishi + CarBrandplymouth + CarBrandrenault +CarBrandtoyota , data=train)

summary(model_23)
# Multiple R-squared:  0.9327,	Adjusted R-squared:  0.9242  

vif(model_23)
#Removing CarBrandplymouth and re-running the model

model_24 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandmitsubishi + CarBrandrenault +CarBrandtoyota , data=train)

summary(model_24)
# Multiple R-squared:  0.9307,	Adjusted R-squared:  0.9225  

vif(model_24)
# Removing CarBrandrenault and re-running the model

model_25 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw + CarBranddodge +  
                 CarBrandmitsubishi + CarBrandtoyota , data=train)

summary(model_25)
# Multiple R-squared:  0.9287,	Adjusted R-squared:  0.9209  

vif(model_25)
# Removing CarBranddodge and re-running the model

model_26 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw +  
                 CarBrandmitsubishi + CarBrandtoyota , data=train)

summary(model_26)
# Multiple R-squared:  0.9266,	Adjusted R-squared:  0.9192 

vif(model_26)
# Removing enginetypeohcf and re-running the model

model_27 <- lm(formula = price ~ enginelocation + wheelbase + 
                 carheight + curbweight + enginetypel + 
                 enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw +  
                 CarBrandmitsubishi + CarBrandtoyota , data=train)

summary(model_27)
# Multiple R-squared:  0.9241,	Adjusted R-squared:  0.9171 

vif(model_27)
# Removing CarBrandtoyota and re-running the model

model_28 <- lm(formula = price ~ enginelocation + wheelbase + carheight + curbweight + 
                 enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CarBrandbmw + CarBrandmitsubishi  , data=train)

summary(model_28)
# Multiple R-squared:  0.9203,	Adjusted R-squared:  0.9136  

vif(model_28)
# Removing CarBrandmitsubishi and re-running the model

model_29 <- lm(formula = price ~ enginelocation + wheelbase + carheight + curbweight + 
                 enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CarBrandbmw  , data=train)

summary(model_29)
# Multiple R-squared:  0.917,	Adjusted R-squared:  0.9107  

vif(model_29)
# All the variables are having *** , i.e, all of them are significant.

# Now we have these variables left with 3 stars *** (i.e, these are the most significant variables)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         1.796e+04  5.825e+03   3.084 0.002486 ** 
# enginelocation     -2.197e+04  1.789e+03 -12.281  < 2e-16 ***
# wheelbase           3.139e+02  8.036e+01   3.906 0.000149 ***
# carheight          -4.536e+02  1.184e+02  -3.831 0.000196 ***
# curbweight          9.249e+00  8.307e-01  11.133  < 2e-16 ***
# enginetypel        -5.498e+03  9.772e+02  -5.626 1.06e-07 ***
# enginetyperotor    -1.329e+04  2.078e+03  -6.396 2.54e-09 ***
# cylindernumberfive -1.115e+04  1.715e+03  -6.501 1.50e-09 ***
# cylindernumberfour -1.389e+04  1.518e+03  -9.149 9.23e-16 ***
# cylindernumbersix  -1.375e+04  1.679e+03  -8.191 1.96e-13 ***
# CarBrandbmw         1.048e+04  1.199e+03   8.741 9.20e-15 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2461 on 132 degrees of freedom
# Multiple R-squared:  0.917,	Adjusted R-squared:  0.9107 
# F-statistic: 145.9 on 10 and 132 DF,  p-value: < 2.2e-16


# Plotting a correlation matrix and removing the ones with high correlation
cormat <- carsdata_1[, c(4,5,8,9,30,34:38,43)]
str(cormat)
cormat <- round(cor(cormat),2)

# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)

# Plot the correlation matrix
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "blue", mid = "grey", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+ 
  coord_fixed() + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)

# From the above correlation matrix, we can Clearly see that there is a high correlation between:
# curbweight and wheelbase            : 0.78
# carheight and wheelbase             : 0.59

# Again having a look at the model_29 summary and vif:

summary(model_29)
vif(model_29)

# wheelbase is having a higher vif value of 5.705404 as compared to the vif of curbweight = 4.579051
# both wheelbase and curbweight are highly correlated, and we can remove one of them having higher vif.
# So removing wheelbase and re-running the model

model_30 <- lm(formula = price ~ enginelocation +  carheight + curbweight + 
                 enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CarBrandbmw  , data=train)

summary(model_30)
# Multiple R-squared:  0.9074,	Adjusted R-squared:  0.9012   
# The Adjusted R squared value has not fallen down much. That means we are fine here by removing this variable.
vif(model_30)
# p-value of carheight is high ( * one star).
# So, removing variable carheight and re-running the model

model_31 <- lm(formula = price ~ enginelocation + curbweight + enginetypel + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + CarBrandbmw  , data=train)

summary(model_31)
# Multiple R-squared:  0.9044,	Adjusted R-squared:  0.8986   
# The Adjusted R squared value has not fallen down much. That means we are fine here by removing this variable.
vif(model_31)
# cylindernumberfive and curbweight are correlated.
# looking at the vif values, cylindernumberfive has a vif = 4.221780
# Removing cylindernumberfive and re-running the model

model_32 <- lm(formula = price ~ enginelocation + curbweight + enginetypel + enginetyperotor + 
                 cylindernumberfour + cylindernumbersix + CarBrandbmw  , data=train)

summary(model_32)
# Multiple R-squared:  0.8798,	Adjusted R-squared:  0.8736   
# The Adjusted R squared value has not fallen down much. That means we are fine here by removing this variable.
vif(model_32)
# enginetyperotor is now insignificant. pvalue = 0.054919
# So removing enginetyperotor and re-running the model

model_33 <- lm(formula = price ~ enginelocation + curbweight + enginetypel +  
                 cylindernumberfour + cylindernumbersix + CarBrandbmw  , data=train)

summary(model_33)
# Multiple R-squared:  0.8765,	Adjusted R-squared:  0.8711    
# The Adjusted R squared value has not fallen down much. That means we are fine here by removing this variable.
vif(model_33)

# So now we have the most significant variables left with us.
# There is no multicollinearity shown among these.

#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         7.867e+03  2.538e+03   3.100 0.002353 ** 
#enginelocation     -1.923e+04  1.957e+03  -9.824  < 2e-16 ***
#curbweight          1.108e+01  6.114e-01  18.119  < 2e-16 ***
#enginetypel        -3.762e+03  1.053e+03  -3.573 0.000489 ***
#cylindernumberfour -4.730e+03  8.676e+02  -5.452 2.27e-07 ***
#cylindernumbersix  -4.030e+03  1.187e+03  -3.396 0.000896 ***
#CarBrandbmw         1.026e+04  1.424e+03   7.205 3.61e-11 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


##**************************************************************************************##
##                                                                                      ##
##                       P R E D I C T I N G   T H E   P R I C E                        ##
##                                                                                      ##
##**************************************************************************************##

Predict_1 <- predict(model_33,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# 0.8834295

# All the variables selected for the final model are significant 
# i.e their p-value < 0.05 
# Multiple R-squared for train data set => 87.65% 
# Adjusted R-squared for train data set => 87.11% 
# R-squared for test data set           => 88.34% 
# The model explains 88.34% of variation of price in the test dataset.

##**************************************************************************************##
##                                   E N D  O F  F I L E                                ##
##**************************************************************************************##