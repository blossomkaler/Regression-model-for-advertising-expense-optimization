library(readxl)
library(dplyr)                #%>%
library(tidyr)
library(lubridate)
library(Metrics)

data <- read_excel("D:/MTECH/SEM 3/predictive analytics/ca2/dataCA2.xlsx")
df<-data
View(df)
summary(df)
str(df)
colnames(df)
#````````````````FEATURE  ENGINEERING``````````````````````
#NA's
sum(is.na(df))


#Convert DATE to Date format
df$DATE <- as.Date(df$DATE, format = "%Y-%m-%d")


# Normalization
norm<-function(x){(x-min(x))/(max(x)-min(x))}
df <- df %>%mutate(across(where(is.numeric), norm))


# Interaction terms between GRPs and media spends
data <- data %>%
  mutate(interaction_SMS_GRP = `Advertising Expenses (SMS)` * `GRP(SMS)`,
         interaction_Newspaper_GRP = `Advertising Expenses(Newspaper ads)` * `GRP (NewPaper ads)`,
         interaction_Radio_GRP = `Advertising Expenses(Radio)` * `GRP(Radio`,
         interaction_TV_GRP = `Advertising Expenses(TV)` * `GRP(TV)`,
         interaction_Internet_GRP = `Advertising Expenses(Internet)` * `GRP(Internet)`)

#NOTE:-- I did'nt used these interaction variables for final model training 
#        because it was increasing the error in the model (increase was very little although)



#````````````````MODEL TRAINING AND TESTING ``````````````````````
# Train-Test Split
set.seed(123)
trainIndex <- sample(1:nrow(df),0.8*nrow(df))
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Linear Regression model
linear_model <- lm(`SALES ($)` ~ `Advertising Expenses (SMS)` + `Advertising Expenses(Newspaper ads)` + 
                     `Advertising Expenses(Radio)` + `Advertising Expenses(TV)` + 
                     `Advertising Expenses(Internet)` + `Consumer Price Index (CPI)` + 
                     `Consumer Confidence Index(CCI)` + `Producer Price Index (PPI)`+`GRP (NewPaper ads)`+
                     `GRP(SMS)`+`GRP(Radio`+`GRP(Internet)`+`GRP(TV)`, data = train_data)

(model_summary <- summary(linear_model))

# Model Prediction
predictions <- predict(linear_model, newdata = test_data)


#```````````````````PERFORMANCE EVALUATION```````````````````
rmse_value <- rmse(test_data$`SALES ($)`, predictions)
mae_value <- mae(test_data$`SALES ($)`, predictions)

cat("RMSE: ", rmse_value, "\nMAE: ", mae_value)


#```````````````Effectiveness of Media Channels on sales`````````````
#Extracting Coefficients
(coefficients <- model_summary$coefficients)

effectiveness <- coefficients[grep("GRP", rownames(coefficients)),1]
#effectiveness <- coefficients[grep("Advertising Expenses", rownames(coefficients)),1]

effectiveness <- as.data.frame(effectiveness)
colnames(effectiveness) <- "Estimate"

#handling negative Estimate Values
effectiveness$Adjusted_Estimate <- ifelse(effectiveness$Estimate < 0, 0, effectiveness$Estimate)

#percentage contribution of each media channel to sales
effectiveness$Percentage_Contribution <- effectiveness$Adjusted_Estimate / sum(effectiveness$Adjusted_Estimate) * 100

#Effectiveness of each media channel
View(effectiveness)



#`````````````````````MODEL OPTIMIZATION```````````````
#if we want to allocate budget on the basis of historical Advertising Expenses
#optimizationCoeff<-as.data.frame(coefficients[grep("Advertising Expenses", rownames(coefficients)),1])

#if we want to allocate budget on the basis of historical GRP
optimizationCoeff<-as.data.frame(coefficients[grep("GRP", rownames(coefficients)),1])
colnames(optimizationCoeff) <- "Estimate"
View(optimizationCoeff)

# Total budget constraint
total_budget <- 100000  

# Objective function (Maximize sales)
objective <- optimizationCoeff

# Constraints: 
# At least 10% of total budget for each channel
constraints <- matrix(c(1, 0, 0, 0, 0,
                        0, 1, 0, 0, 0,
                        0, 0, 1, 0, 0,
                        0, 0, 0, 1, 0,
                        0, 0, 0, 0, 1,
                        1, 1, 1, 1, 1), nrow = 6, byrow = TRUE)

# Budget limits (10% minimum constraint for each channel)
rhs <- c(0.1 * total_budget, 0.1 * total_budget, 0.1 * total_budget, 
         0.1 * total_budget, 0.1 * total_budget, total_budget)

# Direction of inequalities
directions <- c(">=", ">=", ">=", ">=", ">=", "=")

# Solve the linear programming problem
opt_solution <- lp("max", objective, constraints, directions, rhs)

# Optimal media spend for each channel
optimizationCoeff$budget_allocated<- opt_solution$solution

View(optimizationCoeff)




