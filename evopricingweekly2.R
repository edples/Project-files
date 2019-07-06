setwd("C:/test_input")

airportmaster <- read.csv("airport_master.csv")
head(airportmaster)

storemaster <- read.csv("store_master.csv")
head(storemaster)

weatherdata <- read.csv("weather_data.csv")
head(weatherdata)

stores_data <- read.csv("store_data.csv")
head(stores_data)
str(stores_data)

# the other .csv's has no importance to the analysis, their influence is 
#reflected in the "traffic" store_data's variable.
#(the airport neighborhood mean increasing the store traffic; the weather, when
#seasonal shoping influences the traffic and then the seasonal items sales), 
#so I will keep only the"store_data" for analysis.

# the "seasonality" is a factor, with huge number of levels, convert to numeric for
#the analysis purpose
stores_data$seasonality <- as.numeric(levels(stores_data$seasonality))[stores_data$seasonality]
colSums(is.na(stores_data))

#as the data has 18 Na's, which is  aproximately the 1% of the 1038 observations, 
#decide to dropped them
stores_data<- na.omit(stores_data)
colSums(is.na(stores_data))

#The first column has a strange name
#decide to change it to "store"
names(stores_data)[1] <- "store"
head(stores_data)
stores_data$store <- as.factor(stores_data$store)
str(stores_data)

# I observed that the "txn_week" column has a maximum value 53, 
#which, in absence of any data sourcer info, I will
#consider this an error, a year having no more than 52 weeks. 
#Decide to drop the repective records
library(dplyr)
stores_data <- filter(stores_data, txn_week <= 52)
summary(stores_data)
str(stores_data)
head(stores_data, n=10)

#an intuition tells me that the "qty" variable has a correlation
#with the "traffic" variable.
#more clients in store = greater probability of increased sales.
#plot the relation of them
library(ggplot2)
ggplot(stores_data, aes(x= traffic, y= qty)) + geom_point()
#there is good positive linear correlation between them, 
#which makes a good fit for a linear model.

head(stores_data)
#the "units_per_ticket" variable has no relevance to our question,
#I will keep the rest of the variables.
stores_data <- stores_data[,-9]
head(stores_data)

# aggregating the data for the linear model
data_for_model <- stores_data %>%
          group_by(store,txn_year) %>%
          mutate(count= (txn_week = n())) %>% 
                    filter(count == 52)
        
summary(data_for_model)
data_for_model
 
# Split the data into 2 sets: "test"(80% of the"data_for_model" data)
#and "train"(20% of the "data_for_model"data) sets, to fit model to.
set.seed(123)
row_number <- sample(1:nrow(data_for_model),0.8*nrow(data_for_model))
train <- data_for_model[row_number,]
test <- data_for_model[-row_number,]
dim(train)
dim(test)

#I choose the below formula since it has
#the best R-squared value(72.61% of the variablity of the "qty" is explained by the predictors)
# and all the explanatory variables have p < 0.05, meaning that
# they are statistic significant. The same for  the p value of the model.
# I split the dataset into portions, to evaluate the model on a unseen dataset.
linearmodel_train<- lm(qty ~  traffic * avg_price * avg_markdown + 
                         avg_list_price * seasonality, data=train)
summary(linearmodel_train)


predict_test<- predict(linearmodel_train, test)
predict_test

#make a data frame to evaluate the accuracy of the model
accuracy_frame <- data.frame(cbind(test$qty, predict_test))
dim(accuracy_frame)
min_max_accuracy <- mean(apply(accuracy_frame, 1, min)/ apply(accuracy_frame, 1, max))
min_max_accuracy
#the model on the "test"set  has a good Min Max accuracy value:78.66%



predicted <- predict(linearmodel_train, data_for_model)
accuracy_model <- data.frame(predicted, data_for_model$qty)
model_accuracy <-  mean(apply(accuracy_model, 1, min)/ apply(accuracy_model, 1, max))
model_accuracy
#the accuracy model on the whole dataset has slightly increased to 80.23%


#create a data frame for plotting the results.
comparation_table <- data.frame(actual_values = data_for_model$qty, 
                                predicted_values = predicted, 
                                store = data_for_model$store, year=data_for_model$txn_year, 
                                week = data_for_model$txn_week)
head(comparation_table)
comparation_table

# plot the predicted values of"qty" vs "week" values.
ggplot(comparation_table, aes(x=week, y=predicted_values, color=store)) +
  geom_point()+
  geom_line() +
  facet_grid(year ~ store)

# the predicted values of "qty" of the "30793" store and "34130" store, for "year" 2015 are missing because in the 
#2015 they have less than 52 weekly sales records.