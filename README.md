# BarcelonaRealEstate
Predicting real estate prices for a data set based in Barcelona. This is a team project done as a part of Duke Coursework for the course Applied Probability and Statistics. 

The data set has the following variables with Price being the dependent variable and the rest of the columns like m^2, rooms, bathrooms, and city zone being the independent variables.
A City zone is just a list of names of cities and does not hold much value. We converted that to categorical variables with 9 dummy variables (for each city) wherein the value is 1 if the city zone matches else it’s zero. For the 10th city, all the dummy variables should have a value of 0.
On running the base model, we found that area and rooms are highly correlated. Instead of using these variables, we introduced another variable called area per room. It was calculated as follows:
df1$areaperroom <- df1$Area/(df1$Rooms+(0.5*df1$Bathrooms))
We have also introduced another variable with respect to the area bands. The values of area column were quite dispersed, so we introduced 4 categorical variables to classify the prices to the area band. We classified them as follows:
# Area Bands
1. df1$a0_50 = ifelse(df1$Area<=50, df1$Area, 0)
2. df1$a50_100 = ifelse(df1$Area >50 & df1$Area<=100, df1$Area, 0)
3. df1$a100_150 = ifelse(df1$Area >100 & df1$Area<=150, df1$Area, 0)
4. df1$a150_250 = ifelse(df1$Area >150, df1$Area, 0)
5. df1$a150_250 = ifelse(df1$Area >150 & df1$Area<=200, df1$Area, 0)
6. df1$a200_250 = ifelse(df1$Area >200, df1$Area, 0)

In conclusion, we did the following changes to our model:
1.	City zone as categorical variables
2.	Calculated area per room
3.	Converted area as a categorical variable by using area bands

Histogram plots
  
![image](https://github.com/TejaswiniPemmaraju/BarcelonaRealEstate/assets/129342521/631a29e6-5277-4c70-ab5e-8b8552e0eff5)

![image](https://github.com/TejaswiniPemmaraju/BarcelonaRealEstate/assets/129342521/7b2e1732-b5d9-4aa0-ba42-88300630d703)

RMSE calculated = 54193

Other attributes taken into consideration:
1.	As the variables area, room and bathroom were correlated we tried to convert all 3 into a single variable by using a new variable area/(rooms+bathroom)
2.	Bathroom can also be counted as a separate room, we tried adding rooms and bathrooms into a single variable
3.	We tried dividing the columns to 2 distinct groups (2 coulmns) wherein one would denote area, room and bathroom combination and the other group would taking summation of the additional attributes (Elevator+Atico+Terrasse+Parking+Kitchen+Type+Yard)
4.	Another variable that we have considered is creating the rooms and bathrooms combination variable by putting them into categorical variables and adding the total number of rooms to see the correlation. We determined 1 or 0 based on the number of rooms and bathrooms numbers and categorized them into 1 or 0 and added up all the combinations together. However, after checking on the summary for the model, we decided that we could not add this variable because the p-value is quite large, and the standard residual error also increases for our model.




