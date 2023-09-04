---
  title: "Best Model Selection"
author: "Tobi Joshua"
date: "`r Sys.Date()`"
---
  
library(knitr)
library(shiny)

Economic and unemployment data were recorded.
      
There are 16 rows of data.  The data include:
  
  I,  the index;
A1, the percentage price deflation;
A2, the GNP in millions of dollars;
A3, the number of unemployed;
A4, the number of people employed by the military;
A5, the number of people over 14;
A6, the year
Y,  the number of people employed.

A3-A5 are in units of thousands
We seek a model of the form:
  ```

$Y = X_0 + A_1X_1 + A_2X_2 + A_3X_3 + A_4X_4 + A_5X_5 + A_6X_6$
  
  # Load Data
  
# Load data
data <- data.frame(A1=c(83.0, 88.5, 88.2, 89.5, 96.2, 98.1, 99.0, 100.0, 101.2, 104.6, 108.4, 110.8, 112.6, 114.2, 115.7, 116.9),
                   A2=c(234289, 259426, 258054, 284599, 328975, 346999, 365385, 363112, 397469, 419180, 442769, 444546, 482704, 502601, 518173, 554894),
                   A3=c(2356, 2326, 3682, 3351, 2099, 1932, 1870, 3578, 2904, 2822, 2936, 4681, 3813, 3931, 4806, 4007),
                   A4=c(1590, 1456, 1616, 1650, 3099, 3594, 3547, 3350, 3048, 2857, 2798, 2637, 2552, 2514, 2572, 2827),
                   A5=c(107608, 108632, 109773, 110929, 112075, 113270, 115094, 116219, 117388, 118734, 120445, 121950, 123366, 125368, 127852, 130081),
                   A6=c(1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962),
                   Y=c(60323, 61122, 60171, 61187, 63221, 63639, 64989, 63761, 66019, 67857, 68169, 66513, 68655, 69564, 69331, 70561))
data <- data.frame(data)
data


# Check the features that contributed to the the number of people employed postively and negatively


cor(x=data[,c(1:6)],y=data[,c(7)])


It is clear that the GNP in millions of dollars(A2) has the highest contribution to the number of people employed  follow by the year(A6) and the percentage price deflation(A1).



# Fit Different Models by changing A1 ,A2 AND A6

# Fit a multiple linear regression model  A1
model1 <- lm(Y ~ A1 + A3 + A4 + A5, data = data)
summary(model1)


# Fit a multiple linear regression model with A2
model2 <- lm(Y ~ A2+ A3 + A4 + A5 , data = data)
summary(model2)

# Fit a multiple linear regression model with A6
model3 <- lm(Y ~A3+ A4 + A5 + A6, data = data)
summary(model3)


# Fit a multiple linear regression model with A1 and A6
model4 <- lm(Y ~ A1 + A3 + A4 + A6, data = data)
summary(model4)


# Fit a multiple linear regression model with A1 and A2
model5 <- lm(Y ~ A1 + A2 + A4 + A5, data = data)
summary(model5)


# Fit a multiple linear regression model with A1 and A2
model6 <- lm(Y ~  A2 + A4 + A5 + A6, data = data)
summary(model6)


# Fit a multiple linear regression model
model7 <- lm(Y ~ A1 + A2 + A3 + A4 + A5 + A6, data = data)
summary(model7)


# Pick the best model using the AIC

# Compare models based on AIC
model_compare <- data.frame(Model = c("Model 1", "Model 2", "Model 3","Model 4","Model 5","Model 6","Model 7"),
                            AIC = c(AIC(model1), AIC(model2), AIC(model3),AIC(model4),AIC(model5),AIC(model6),AIC(model7)))

model_compare

The  First  best  model  is  Model 3  with  an  AIC  of  233.7882 

The  Second  best  model  is  Model 7  with  an  AIC  of  235.1489. But  Using  the  Adjusted  R-squared  values ,

Model3  has  Adjusted  R-squared  value  of  0.9928

Model7  has  Adjusted  R-squared  value  of  0.9925

The  overall  best  model  is  the  model3  with  the  linear  regression  equation:
  
$Y=-2.440e^{6} - 1.501A_3 - 0.9340A_4 - 0.2262A_5 + 1299 A_6$
  