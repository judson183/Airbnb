#importing the necessary packages
library(tidyr)
library(readxl)
library(lmtest)
library(sandwich)
library(stringi)
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)


#dataframe
airbnb_df <- read.csv("listings.csv")
airbnb_df<- data.frame(airbnb_df)


#creating a subset
airbnb <- subset(airbnb_df, select = c(review_scores_value,host_is_superhost,beds,bedrooms,host_response_rate,host_response_time,number_of_reviews,reviews_per_month,review_scores_rating))


#data cleaning
#Checking for null values and reoming them. Also checking the type. Creation of dummy variables


#host is superhost and ratings
#t==1 
#f==0
#dummy variable
typeof(airbnb$host_is_superhost)
airbnb$host_is_superhost <- ifelse(airbnb$host_is_superhost == "t",1,0)


head(airbnb$host_is_superhost)



#number of reviews
typeof(airbnb$number_of_reviews)
head(airbnb$number_of_reviews)
sum(is.na(airbnb$number_of_reviews))



#review score rating
typeof(airbnb$review_scores_rating)
sum(is.na(airbnb$review_scores_rating))
head(airbnb$review_scores_rating)
airbnb$review_scores_rating <- as.numeric(airbnb$review_scores_rating)

#drop na
airbnb <- drop_na(airbnb)


#review_scores_value
typeof(airbnb$review_scores_value)
sum(is.na(airbnb$review_scores_value))
head(airbnb$review_scores_value)
airbnb$review_scores_value <- as.numeric(airbnb$review_scores_value)



#reviews per month
head(airbnb$reviews_per_month)
airbnb$reviews_per_month <- as.numeric(airbnb$reviews_per_month)
sum(is.na(airbnb$reviews_per_month))

#beds
typeof(airbnb$beds)
head(airbnb$beds)
sum(is.na(airbnb$beds))

#bedrooms
typeof(airbnb$bedrooms)
head(airbnb$bedrooms)
sum(is.na(airbnb$bedrooms))


#host response rate
typeof(airbnb$host_response_rate)

#host_response_rate
airbnb$host_response_rate = substr(airbnb$host_response_rate, 1, nchar(airbnb$host_response_rate)-1)
airbnb$host_response_rate <- gsub('/', '',airbnb$host_response_rate)

airbnb$host_response_rate <- as.numeric(airbnb$host_response_rate)

head(airbnb$host_response_rate)
sum(is.na(airbnb$host_response_rate))


#host response time
typeof(airbnb$host_response_time)
head(airbnb$host_response_time)
unique(airbnb$host_response_time)


airbnb$rep_within_an_hr <- ifelse(airbnb$host_response_time == 1, 1, 0)
airbnb$rep_within_few_hours <- ifelse(airbnb$host_response_time == 2, 1, 0)
airbnb$rep_within_a_day <- ifelse(airbnb$host_response_time == 3, 1, 0)
airbnb$rep_few_days <- ifelse(airbnb$host_response_time == 4, 1, 0)

airbnb$host_response_time <- as.numeric(airbnb$host_response_time)
sum(is.na(airbnb$host_response_time))

sum(is.na(airbnb))


#drop na
airbnb <- drop_na(airbnb)


#assumptions for logistic regression
#1) to check if the the outcome is binary and the data are independent
View(airbnb)
#outcome is binary and the data are independent

#2) To check that the independent variables do not strongly correlate with each other
airbnb <- na.omit(airbnb)
correlation_airbnb<- cor(airbnb)

corrplot(correlation_airbnb)
cor(airbnb)

#as a rule of thumb, anything above 0.7 should be removed. I didn't find any. NO multi collinearity

#3)To check that continuous scale variables have linearity against the log odds of dependent variable


```

```{r}

# we need to do a logistic regression model
#training data
#make this example reproducible
set.seed(1)

#create ID variable
airbnb$id <- 1:nrow(airbnb)

#Use 60% of dataset as training set and remaining 40% as testing set 
train_data <- airbnb %>% dplyr::sample_frac(0.6)
test_data  <- dplyr::anti_join(airbnb, train_data, by = 'id')
```


```{r}
logistic model
formula = airbnb$host_is_superhost~airbnb$review_scores_value
model_logistic_1<-glm(formula,data=airbnb,family=binomial(link = "logit"))


coeftest(model_logistic_1, vcov. = vcovHC, type = "HC1")

#plotting first model
ggplot(airbnb, aes(x=review_scores_value, y=host_is_superhost)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial)) + annotate("text", x=1.5, y=0.97, label= "Super Host")+annotate("text", x=2, y=0.10, label= "Not a super host")


#creating models


#model 2
model_logistic_2 <- glm(formula = host_is_superhost~review_scores_value+host_response_rate,data=airbnb,family=binomial)
coeftest(model_logistic_2, vcov. = vcovHC, type = "HC1")


#model 3

model_logistic_3 <- glm(formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews,data=airbnb,family=binomial)
coeftest(model_logistic_3, vcov. = vcovHC, type = "HC1")


#model 4
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds
model_logistic_4 <- glm(formula,data=airbnb,family=binomial)
coeftest(model_logistic_4, vcov. = vcovHC, type = "HC1")

#model 5
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds+review_scores_rating
model_logistic_5 <- glm(formula,data=airbnb,family=binomial)
coeftest(model_logistic_5, vcov. = vcovHC, type = "HC1")


#model 6
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds+review_scores_rating+bedrooms
model_logistic_6 <- glm(formula,data=airbnb,family=binomial)
coeftest(model_logistic_6, vcov. = vcovHC, type = "HC1")


#model 7
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds+review_scores_rating+bedrooms+reviews_per_month
model_logistic_7 <- glm(formula,data=airbnb,family=binomial)
coeftest(model_logistic_7, vcov. = vcovHC, type = "HC1")


#model 8
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds+review_scores_rating+bedrooms+reviews_per_month+host_response_time
model_logistic_8 <- glm(formula,data=airbnb,family=binomial)
coeftest(model_logistic_8, vcov. = vcovHC, type = "HC1")



#training and testing the data
formula = host_is_superhost~review_scores_value+host_response_rate+number_of_reviews+beds+review_scores_rating+bedrooms+reviews_per_month+host_response_time
model_logistic_1_train<-glm(formula,data=train_data,family=binomial)
predited_output_test <- predict(model_logistic_8,data=test_data,type = 'response')

logit <- log(predited_output_test/(1-predited_output_test))


#3)To check that continuous scale variables have linearity against the log odds of dependent variable


ggplot(airbnb,aes(logit,review_scores_value))+
  geom_point(size = 0.5,alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

#reviews per month
ggplot(airbnb,aes(logit,reviews_per_month))+
  geom_point(size = 0.5,alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

#number of reviews
ggplot(airbnb,aes(logit,number_of_reviews))+
  geom_point(size = 0.5,alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()


#outliers
plot(model_logistic_8,which =4,id,n=6)


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(model_logistic_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_2, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_3, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_4, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_5, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_6, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_7, type = "HC1"))),
               sqrt(diag(vcovHC(model_logistic_8, type = "HC1"))))


#used stargazer to tget the output
models <- list(model_logistic_1,model_logistic_2,model_logistic_3,model_logistic_4,model_logistic_5,model_logistic_6,model_logistic_7,model_logistic_8)

#stragazer output
stargazer(models,type = "text",se =rob_se,odd.ratio = odd_ratio,title = "Logistic Regression Models of factors impacting if a person is a superhost",
          out="Logistic_regression_output.htm")



#interpretation of the coefficients

odd_ratio <-exp(coef(model_logistic_8))

# Measures of Fit
###################
# compute the null model
logit_null <- glm(formula = host_is_superhost~1, 
                  family = binomial(link = "logit"), 
                  data = airbnb)

# compute the pseudo-R2 using 'logLik'
1 - logLik(model_logistic_8)[1]/logLik(logit_null)[1]


#output

#the main variables that impacts a person being a superhost
#A 1 unit increase in review scores increases the odds of being a superhost  by a factor of 1.22 , holding the other predictor variables constant at certain value.
#A 1 unit increase in host response data increases the odds of being a superhost  by a factor of 1.08, holding the other predictor variables constant at certain value.
#A 1 unit increase in review score rating increases the odds of being a superhost  by a factor of 3.5, holding the other predictor variables constant at certain value.
