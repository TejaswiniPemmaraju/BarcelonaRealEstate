library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(randomForest)

# data_price <- read_excel("PriceSubmission.xlsx", sheet = 1)
# data_all <- read_excel("BarcelonaRE_Data.xlsx", sheet = "413 properties for analysis")
BarcelonaRE_Data <- read_excel(file.choose(), 
                               sheet = "413 properties for analysis")
PriceSubmission <- read_excel(file.choose())

df <- BarcelonaRE_Data

# replacing column names
colnames(df)[which(names(df) == "Price             [euros]")] <- "Price"
colnames(df)[which(names(df) == "m^2")] <- "Area"
colnames(df)[which(names(df) == "\"Atico\"")] <- "Atico"
colnames(df)[which(names(df) == "City Zone")] <- "CityZone"
colnames(df_p)[which(names(df_p) == "...1")] <- "Sno"

# Creating a version
df1= df

#Create dummy variables for city zones(baseline = Ciuta Vella)
df1$Eixample <- ifelse(df1$CityZone == 'Eixample', 1, 0)
df1$Gracia <- ifelse(df1$CityZone == 'Gràcia',1, 0)
df1$Horta <- ifelse(df1$CityZone == 'Horta - Guinardó',1,0)
df1$Les_corts <- ifelse(df1$CityZone == 'Les Corts',1,0)
df1$nou_barris <- ifelse(df1$CityZone == 'Nou Barris',1,0)
df1$sant_andreu <- ifelse(df1$CityZone == 'Sant Andreu',1,0)
df1$sant_marti <- ifelse(df1$CityZone == 'Sant Marti', 1, 0)
df1$sants_montjuic <- ifelse(df1$CityZone == 'Sants - Montjuïc',1,0)
df1$sarria <- ifelse(df1$CityZone == 'Sarria - Sant Gervasi',1 , 0)

# Price per square meter
# df1$priceperm2 <- df1$Price/df1$Area
# df1$priceperroom <- df1$Price/(df1$Rooms+(0.5*df1$Bathrooms))
df1$areaperroom <- df1$Area/(df1$Rooms+(0.5*df1$Bathrooms))

# Variable Interations
df1$premium <- df1$Elevator+df1$Atico+df1$Terrasse+df1$Parking+df1$Yard

# Room & Bathroom Combinations
df1$r3b1 = ifelse(df1$Rooms == 3 & df1$Bathrooms==1, 1, 0)
df1$r4b2 = ifelse(df1$Rooms == 4 & df1$Bathrooms==2, 1, 0)
df1$r2b1 = ifelse(df1$Rooms == 2 & df1$Bathrooms==1, 1, 0)
df1$r3b2 = ifelse(df1$Rooms == 3 & df1$Bathrooms==2, 1, 0)
df1$r4b1 = ifelse(df1$Rooms == 4 & df1$Bathrooms==1, 1, 0)
df1$r1b1 = ifelse(df1$Rooms == 1 & df1$Bathrooms==1, 1, 0)

# Area Bands
df1$a0_50 = ifelse(df1$Area<=50, df1$Area, 0)
df1$a50_100 = ifelse(df1$Area >50 & df1$Area<=100, df1$Area, 0)
df1$a100_150 = ifelse(df1$Area >100 & df1$Area<=150, df1$Area, 0)
# df1$a150_250 = ifelse(df1$Area >150, df1$Area, 0)
df1$a150_200 = ifelse(df1$Area >150 & df1$Area<=200, df1$Area, 0)
df1$a200_250 = ifelse(df1$Area >200, df1$Area, 0)

# Remove CitYZone
df2 = subset(df1, select = -c(CityZone,Sno) )

corMatrix = data.frame(cor(df2))
# write.csv(corMatrix, "corMatrix.csv")

# Spliting Data
# Building Model
# Testing Model For Errors

#make this example reproducible
set.seed(1)
#use 80% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df2), replace=TRUE, prob=c(0.8,0.2))
train  <- df2[sample, ]
test   <- df2[!sample, ]

# Regression model with Period as independent variable 
model1 <- lm(Price ~ Area+Rooms+Bathrooms+Elevator+ Atico+Terrasse+Parking+Kitchen+Type+Yard           
             +Eixample       +Gracia         +Horta          +Les_corts     
             +nou_barris     +sant_andreu    +sant_marti     +sants_montjuic+sarria        
              +areaperroom    +premium        +r3b1          
             +r4b2           +r2b1           +r3b2           +r4b1           +r1b1          
             +a0_50          +a50_100        +a100_150       +a150_200       +a200_250, data = train)
summary(model1)

# Check the assumptions (including the auto-correlation of residuals)
hist(residuals(model1), breaks = 10)
plot(fitted.values(model1), residuals(model1))

# Regression model with Period as independent variable 
model2 <- lm(Price ~ 
               Area
             # +Rooms
             +Elevator+Terrasse
             +Bathrooms
             + Atico+Parking+Kitchen+Yard           
             +Eixample       +Gracia         
             +Horta          +Les_corts  +sants_montjuic
             +nou_barris     +sant_andreu    +sant_marti     +sarria 
             # +areaperroom
             # +r3b1          +r4b2           +r2b1           +r3b2           +r4b1           +r1b1
             +a0_50          +a50_100        +a100_150       +a150_200       
             +a200_250
             , data = df2)
summary(model2)

# Check the assumptions (including the auto-correlation of residuals)
hist(residuals(model2),breaks = 100)
plot(fitted.values(model2), residuals(model2))

# Creating Variables for PriceSubmission

df_p <- PriceSubmission

# replacing column names
colnames(df_p)[which(names(df_p) == "Your Price Prediction")] <- "Price"
colnames(df_p)[which(names(df_p) == "m^2")] <- "Area"
colnames(df_p)[which(names(df_p) == "\"Atico\"")] <- "Atico"
colnames(df_p)[which(names(df_p) == "City Zone")] <- "CityZone"
colnames(df_p)[which(names(df_p) == "...1")] <- "Sno"

# Creating a version
df_p1= df_p

#Create dummy variables for city zones(baseline = Ciuta Vella)
df_p1$Eixample <- ifelse(df_p1$CityZone == 'Eixample', 1, 0)
df_p1$Gracia <- ifelse(df_p1$CityZone == 'Gràcia',1, 0)
df_p1$Horta <- ifelse(df_p1$CityZone == 'Horta - Guinardó',1,0)
df_p1$Les_corts <- ifelse(df_p1$CityZone == 'Les Corts',1,0)
df_p1$nou_barris <- ifelse(df_p1$CityZone == 'Nou Barris',1,0)
df_p1$sant_andreu <- ifelse(df_p1$CityZone == 'Sant Andreu',1,0)
df_p1$sant_marti <- ifelse(df_p1$CityZone == 'Sant Marti', 1, 0)
df_p1$sants_montjuic <- ifelse(df_p1$CityZone == 'Sants - Montjuïc',1,0)
df_p1$sarria <- ifelse(df_p1$CityZone == 'Sarria - Sant Gervasi',1 , 0)

# Price per square meter
# df_p1$priceperm2 <- df_p1$Price/df_p1$Area
# df_p1$priceperroom <- df_p1$Price/(df_p1$Rooms+(0.5*df_p1$Bathrooms))
df_p1$areaperroom <- df_p1$Area/(df_p1$Rooms+(0.5*df_p1$Bathrooms))

# Variable Interations
df_p1$premium <- df_p1$Elevator+df_p1$Atico+df_p1$Terrasse+df_p1$Parking+df_p1$Yard

# Room & Bathroom Combinations
df_p1$r3b1 = ifelse(df_p1$Rooms == 3 & df_p1$Bathrooms==1, 1, 0)
df_p1$r4b2 = ifelse(df_p1$Rooms == 4 & df_p1$Bathrooms==2, 1, 0)
df_p1$r2b1 = ifelse(df_p1$Rooms == 2 & df_p1$Bathrooms==1, 1, 0)
df_p1$r3b2 = ifelse(df_p1$Rooms == 3 & df_p1$Bathrooms==2, 1, 0)
df_p1$r4b1 = ifelse(df_p1$Rooms == 4 & df_p1$Bathrooms==1, 1, 0)
df_p1$r1b1 = ifelse(df_p1$Rooms == 1 & df_p1$Bathrooms==1, 1, 0)

# Area Bands
df_p1$a0_50 = ifelse(df_p1$Area<=50, df_p1$Area, 0)
df_p1$a50_100 = ifelse(df_p1$Area >50 & df_p1$Area<=100, df_p1$Area, 0)
df_p1$a100_150 = ifelse(df_p1$Area >100 & df_p1$Area<=150, df_p1$Area, 0)
# df_p1$a150_250 = ifelse(df_p1$Area >150, df_p1$Area, 0)
df_p1$a150_200 = ifelse(df_p1$Area >150 & df_p1$Area<=200, df_p1$Area, 0)
df_p1$a200_250 = ifelse(df_p1$Area >200, df_p1$Area, 0)

# Remove CitYZone
df_p2 = subset(df_p1, select = -c(CityZone,Sno) )

# Make a forecast for the last quarter of 2020
price_prediction <- predict(model2, newdata = df_p2, interval = "prediction")
df_price_prediction <- data.frame(df_p2, price_prediction)

write.csv(df_price_prediction, "df_price_prediction.csv")

# Trying Random Forest Model for Prediction
model_rf <- randomForest(
  Price ~ 
    
    Area
  # +Rooms
  +Elevator+Terrasse
  +Bathrooms
  + Atico+Parking+Kitchen+Yard           
  +Eixample       +Gracia         
  +Horta          +Les_corts  +sants_montjuic
  +nou_barris     +sant_andreu    +sant_marti     +sarria 
  # +areaperroom
  # +r3b1          +r4b2           +r2b1           +r3b2           +r4b1           +r1b1
  +a0_50          +a50_100        +a100_150       +a150_200       
  +a200_250
    ,data=df2
)
summary(model_rf)
print(model_rf)
