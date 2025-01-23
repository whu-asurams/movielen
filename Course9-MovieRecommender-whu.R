
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)




rm(dl, ratings, movies, test_index, temp, movielens, removed)


head(edx, n=10)

###################################
# Section 2. Analysis
###################################


# Our recommending system is based on the assumption that different users may have different tastes.
# For instance, young male adults are more likely to watch action movies, while middle aged and old
# generations are more likely to watch movies from their generations.
#
# The original model proposed in the course lecture.
# $Y_{u,i} = mu + b_i + b_u +\epsilon_{u,i}$
#     mu            -- the average rating of all movies by all users
#     b_i           -- the average rating of the movie i by all users
#     b_u           -- the average rating of all movies by user u
#     epsilon_{u,i} -- the random difference by user u and movie i.

# Our new model
# Classification of users
#     age: earliest rate_year
#     status:  majority of rate_month
#         students: summer or december
#         household wife: not summer, not december, majority in weekday, morning, afternoon
#         workers: majority in weekends
#              male: genres in action/thriller/wars/sci fic
#               female: others
#     (month, weekday, hours)
#     
# $Y_{u,i} = b_s + b_sa + \epsilon_{u,i}$
#     b_s         -- the average rating of all movies of rated by users of status s
#     b_sa     -- the average rating of all movies of rated by users of status s and age appropriate a
#     

# Step 1. Add new columes to the dataset edx and name it as edx1. Those columns 
# include
# movie_year:  the year the movie is released
# movie_generation: the generation of the movie
# rate_time: the time the movie is rated by an user
# rate_weekday: the week day the movie is rated, such as Sunday
# rate_hour: the hour when the movie is rated, such as 11am in the morning
# rate_year: the yeat when the movie is rated, such as 1996.
# rate_montg: the month when the movie is rated, such as 8 as august.


library(lubridate)

edx1<- edx%>%
  mutate(movie_year= as.numeric(str_extract(str_extract(title, "\\(\\d+\\)$"), "\\d+")),
         rate_time = as_datetime(timestamp, origin="1970-01-01", tz="UTC"),
         rate_weekday = wday(rate_time,  week_start=1),
         rate_hour = hour(rate_time),
         rate_year = year(rate_time),
         rate_month = month(rate_time))

head(edx1, n=10)



# Step 2: Add age estimate

edx1<- edx1 %>%
    group_by(userId) %>%
   # filter(row_number()%in% 1:10) %>%
    mutate(age=min(rate_year))


# Step 3: The month, weekday and hour when a user watched all movies are calculated using the following code.

edx1<- edx1 %>%
  mutate(age=min(rate_year)) %>%
  group_by(userId, rate_month) %>%
  mutate(count_month=n()) %>%
  ungroup() %>%
  group_by(userId, rate_weekday) %>%
  mutate(count_weekday = n()) %>%
  ungroup() %>%
  group_by(userId, rate_hour) %>%
  mutate(count_hour=n()) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(max_month= rate_month[which.max(count_month)],
         max_weekday = rate_weekday[which.max(count_weekday)],
         max_hour = rate_hour[which.max(count_hour)]) %>%
  ungroup()#



edx1<- edx1 %>%
  mutate(status=case_when(
    max_month == 6                          ~ 1, # "Student"
    max_weekday %in% c("Sat", "Sun")        ~ 2, # "Worker",
    max_hour %in% c(9,10,11,12,13,14,15,16) ~ 3, #"House Wife",
    TRUE                                    ~ 4 #"General"
  )) %>%
  #select(userId, rate_time, count_weekday, max_weekday, count_hour, max_hour, status)
  group_by(status) %>%
  mutate(b_s = mean(rating))




# Step 4: Other columns

current_year<- year(Sys.Date())
current_year
edx1<- edx1 %>%
  mutate(age_r = case_when(
    movie_year <= current_year - 10 ~ 1, #"Recent",
    abs(movie_year - age)<= 10      ~ 2, #"Good Old",
    movie_year <= (age - 20)        ~ 3, #"Old",
    TRUE                            ~ 4, #"Really Old"
  ))%>%
  group_by(status, age) %>%
  mutate(b_sa = mean(rating - b_s))%>%
  ungroup()
  



# Step 5:  Calculating RMSE


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Create epsilon_{u,i}

n<- nrow(edx1)
set.seed(1)
epsilon<-  rnorm(n, 0, 1/5)
qplot(epsilon, bins = 30, color = I("black"))



# Model 1: hat(Y)_{u,i} = b_s + epsilon_{u,i}
model1_rmse <- RMSE(edx1$b_s + epsilon, edx1$rating)
model1_rmse

model1_rmse <- RMSE(edx1$b_s, edx1$rating)
model1_rmse


# Model 2: hat(Y)_{u,i} = b_s + b_sa + epsilon_{u,i}
model2_rmse <- RMSE(edx1$b_s+edx1$b_sa+epsilon, edx1$rating)
model2_rmse

model2_rmse_e <- RMSE(edx1$b_s+edx1$b_sa, edx1$rating)
model2_rmse_e




#########################################
# Using KNN and Random Forest Algorithms
#########################################

x<- edx1 %>%
  select(max_month, max_weekday, max_hour,movie_year, age, status, age_r)
#  accuracy, k=11, 0.3102587, kappa = 0.1178905, run time = 30 minutes

x<- edx1 %>%
   select(max_month, max_weekday, max_hour,movie_year, age)
#  accuray k=11, 0.3099627, kappa = 0.1173742, run time = 20 minutes

y<-factor(edx1$rating)
x1<- x[1:202000,]
y1<- y[1:202000]

set.seed(1)



train_index <- createDataPartition(y = edx1$rating, times = 1, p = 0.3, list = FALSE)
edx2 <- edx1[train_index,]

x2<- edx2 %>% select(max_month, max_weekday, max_hour,movie_year, age, b_s, b_sa)
y2<- factor(edx2$rating)
#run time: 7 hours
#k  Accuracy   Kappa    
#3  0.3026957  0.1156499
#5  0.3053175  0.1170436
#7  0.3076552  0.1180165
#9  0.3089619  0.1177750
#11  0.3099627  0.1173742

set.seed(1, sample.kind = "Rounding")
train_index <- createDataPartition(y = edx1$rating, times = 1, p = 0.1, list = FALSE)
edx2b <- edx1[train_index,]

x2<- edx2b %>% select(max_month, max_weekday, max_hour,movie_year, age, b_s, b_sa)
y2<- factor(edx2b$rating)

x2a<- edx2b %>% 
  select(max_month, max_weekday, max_hour,movie_year, age)
  # Error as NA's when training using rf
  # run time: 2 hours
x2a<- edx2b %>%
  select(max_month, max_weekday, max_hour,movie_year, age, b_s, b_sa)
  # Error as NA's when training using knn
  # run time: 2 hours
  # Error as NA's when training using rf
  # run time: 12 hours

y2a<- factor(edx2b$rating)



x2b<- edx2b %>% select(movie_year, age, b_s, b_sa)
y2b<- factor(edx2b$rating)
  #Error: Something is wrong; all the Accuracy metric values are missing:
  #Accuracy       Kappa    
  #Min.   : NA   Min.   : NA  
  #1st Qu.: NA   1st Qu.: NA  
  #Median : NA   Median : NA  
  #Mean   :NaN   Mean   :NaN  
  #3rd Qu.: NA   3rd Qu.: NA  
  #Max.   : NA   Max.   : NA  
  #NA's   :5     NA's   :5 

x2c<- edx2b %>% select(movie_year, age, b_s, b_sa, status, age_r)
y2c<- factor(edx2b$rating)
  # Error as above with too many NA's.


x2c<- edx2b %>% select(movie_year, age, status, age_r)
  # Error as above with too many NA's.

####################
# Training using KNN
####################
control <- trainControl(method = "boot", number = 5, p = .3)

set.seed(1, sample.kind = "Rounding")
train_knn <- train(x2, y2,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7,9,11)),
                   trControl = control)
train_knn


#####################
#Training using RF
#####################
library(randomForest)
control <- trainControl(method="cv", number = 5, p=0.3)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <-  train(x2, y2,
                   method = "rf",
                   nTree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
train_rf



########################################
# Apply the model on final_holdout_test
########################################

### Step 1: Convert timestamp and add rate year, month, weekday information.


library(lubridate)

edx1<- final_holdout_test%>%
  mutate(movie_year= as.numeric(str_extract(str_extract(title, "\\(\\d+\\)$"), "\\d+")),
         rate_time = as_datetime(timestamp, origin="1970-01-01", tz="UTC"),
         rate_weekday = wday(rate_time,  week_start=1),
         rate_hour = hour(rate_time),
         rate_year = year(rate_time),
         rate_month = month(rate_time))

head(edx1, n=10)




### Step 2: Estimate the age of a user based on the earlist rating by that user. 

edx1<- edx1 %>%
  group_by(userId) %>%
  # filter(row_number()%in% 1:10) %>%
  mutate(age=min(rate_year))



### Step 3: The month, weekday and hour when a user matched all movies are calculated using the following code.

edx1<- edx1 %>%
  mutate(age=min(rate_year)) %>%
  group_by(userId, rate_month) %>%
  mutate(count_month=n()) %>%
  ungroup() %>%
  group_by(userId, rate_weekday) %>%
  mutate(count_weekday = n()) %>%
  ungroup() %>%
  group_by(userId, rate_hour) %>%
  mutate(count_hour=n()) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(max_month= rate_month[which.max(count_month)],
         max_weekday = rate_weekday[which.max(count_weekday)],
         max_hour = rate_hour[which.max(count_hour)]) %>%
  ungroup()#


edx1<- edx1 %>%
  mutate(status=case_when(
    max_month == 6                          ~ 1, # "Student"
    max_weekday %in% c("Sat", "Sun")        ~ 2, # "Worker",
    max_hour %in% c(9,10,11,12,13,14,15,16) ~ 3, #"House Wife",
    TRUE                                    ~ 4 #"General"
  )) %>%
  #select(userId, rate_time, count_weekday, max_weekday, count_hour, max_hour, status)
  group_by(status) %>%
  mutate(b_s = mean(rating))



### Step 4: Other columns 

current_year<- year(Sys.Date())
current_year
edx1<- edx1 %>%
  mutate(age_r = case_when(
    movie_year <= current_year - 10 ~ 1, #"Recent",
    abs(movie_year - age)<= 10      ~ 2, #"Good Old",
    movie_year <= (age - 20)        ~ 3, #"Old",
    TRUE                            ~ 4, #"Really Old"
  ))%>%
  group_by(status, age) %>%
  mutate(b_sa = mean(rating - b_s))%>%
  ungroup()


### Step 5: Calculate the RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

n<- nrow(edx1)
set.seed(1)
epsilon<-  rnorm(n, 0, 1/5)
qplot(epsilon, bins = 30, color = I("black"))


model1_rmse_e <- RMSE(edx1$b_s + epsilon, edx1$rating)
model1_rmse_e

model1_rmse <- RMSE(edx1$b_s, edx1$rating)
model1_rmse


model2_rmse_e <- RMSE(edx1$b_s+edx1$b_sa+epsilon, edx1$rating)
model2_rmse_e

model2_rmse <- RMSE(edx1$b_s+edx1$b_sa, edx1$rating)
model2_rmse



## Subsection 4.2. Using the KNN algorithm on the test dataset.


x2<- edx1 %>% select(max_month, max_weekday, max_hour,movie_year, age, b_s, b_sa)
y2<- factor(edx1$rating)


# The following block takes about 30 hours to complete running

control <- trainControl(method = "boot", number = 5, p = .3)
set.seed(1, sample.kind = "Rounding")
train_knn <- train(x2, y2,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7,9,11)),
                   trControl = control)
train_knn


# The following takes about 4 hours to complete running

knn_preds <- predict(train_knn, x2)

# > RMSE(knn_preds_number, y2)
# [1] NA
# Warning message:
#  In Ops.factor(true_ratings, predicted_ratings) :
#  ‘-’ not meaningful for factors

class(y2)
class(knn_preds)

knn_preds_number<- as.numeric(as.character(knn_preds))
y2_number <- as.numeric(as.character(y2))

RMSE(knn_preds_number, y2_number)

