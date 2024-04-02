library(data.table)

Mydata <- read.csv("/Users/nikroshitha/Downloads/Fantasy_Premium_League.csv")
Mydata
head(Mydata)# Gives you the first six rows
tail(Mydata)# Gives you the last six rows
colnames(Mydata)
#dim() function in R Language is used to get or set the dimension of the specified matrix, array or data frame
dim(Mydata)
describe(Mydata, exclude.missing=TRUE,digits=4)
# get means for variables in data frame mydata
# excluding missing values
sapply(Mydata, mean, na.rm=TRUE)
# mean,median,25th and 75th quartiles,min,max
summary(Mydata)
install.packages(psych)
library(Hmisc)
describe(Mydata)

# Fit a simple linear regression model
my_model <- lm(goals_scored ~ expected_goals, data = Mydata)

# Print the model summary
summary(my_model)
anova(my_model)

set.seed(123)
install.packages("caret")
install.packages("plyr")

library(caret)
train_idx <- createDataPartition(Mydata$goals_scored, p=0.7,list=FALSE)
train_data <- Mydata[train_idx, ]
test_data <- Mydata[-train_idx, ]
my_model <- lm(goals_scored ~ expected_goals, data = Mydata)

predictions <- predict(my_model, newdata=test_data)
accuracy <- 1 - mean((test_data$goals_scored- predictions)^2)/mean((test_data$goals_scored - mean(test_data$goals_scored))^2)



ggplot(Mydata, aes(x = goals_scored , y = expected_goals)) +
  geom_point() +
  geom_smooth(method = lm, color = "OrangeRed ")


confint(my_model)
summary(my_model)$coefficient


