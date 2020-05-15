library(tidyverse)
library(lubridate)
library(caret)
library(ggplot2)
library(gridExtra)
library(purrr)

source("loadLocalData.R")
source("loadLocalSampleData.R")

################################################################################
# Auxiliary functions
################################################################################
#' Returns data frame with format of a given data frame
#' 
#' @param df The data frame
#' @param firstvalues=5 Number of first values displayed
#' @param sep=" " Separator for first values displayed
#' @return Data frame with column names, their types and the first values
df_format <- function(df, nfirstvalues = 5, sep = " ") {
    n = ncol(df)
    res <- data.frame(
        colname = colnames(df),
        type = sapply(1:n, function(i) {class(df[, i])}),
        firstvalues = 
            sapply(1:n, function(i){
            paste0(head(df[, i], n = nfirstvalues), collapse = sep)    
            })
        )
    return(res)
    }

#' Calculates RMSE of ratings
#' 
#' @param pred_ratings Predicted ratings
#' @paran true_ratings Actual ratings
#' @return Root mean-squared error (RMSE) of predicted ratings
RMSE_rating <- function(pred_ratings, true_ratings) {
    ifelse(length(pred_ratings) > 0,
           sqrt(mean((pred_ratings - true_ratings)^2)),
           NA)
    }


################################################################################
# 3.2.2  Basic Data Structure
################################################################################
# data format, with first 3 values only
edx_format <- df_format(edx, nfirstvalues = 3)
edx_format %>% knitr::kable()

# check if data are tidy
edx %>% summarise_all(~sum(is.na(.)))
validation %>% summarise_all(~sum(is.na(.)))

################################################################################
# 3.3 Data Cleaning
################################################################################

################################################################################
# 3.3.1 Conversion of timestamp to date
################################################################################
edx <- edx %>%
    mutate(ratingdate = as_datetime(timestamp)) %>%
    select(-timestamp)

validation <- validation %>%
    mutate(ratingdate = as_datetime(timestamp)) %>%
    select(-timestamp)

################################################################################
# 3.3.2 Separation of different genres
################################################################################
# TBD - use separate to split out the values of different genres
# first check how many separators there are at most

# this does NOT yet work, try with wrapper around do or sth
edx_1000 %>%
    mutate(nseps = length(gregexpr("\\|", genres)[[1]])) %>% head()

# SAME, something isn't right
edx_1000 %>%
    mutate(nseps = nrow(str_locate_all(genres, "\\|")[[1]])) %>% head()

# for single strings it DOES work
length(gregexpr("\\|", "1|222|2")[[1]])
length(gregexpr("\\|", "1|222|2|123|q2|asdfjn")[[1]])

nrow(str_locate_all("1|222|2|123|q2|asdfjn", "\\|")[[1]])

################################################################################
# 3.4 Data Exploration and Visualisation
################################################################################

################################################################################
# 3.4.1 General Data Distribution Properties
################################################################################

# basic facts
edx %>%
    summarise(n_movies = n_distinct(movieId),
              n_users = n_distinct(userId))
# total # combinations
10677 * 69878
# 746,087,406

# sparsity - code into appendix
n_movies_sub <- length(unique(edx_1000$movieId))
n_users_sub <- length(unique(edx_1000$userId))
edx_1000 %>%
    mutate(rating = 1) %>%
    select(movieId, userId, rating) %>%
    spread(movieId, rating) %>% 
    column_to_rownames(var = "userId") %>% 
    as.matrix() %>% t() %>%
    image(x = 1:n_movies_sub, y = 1:n_users_sub, z = ., 
          xlab = "Movies", ylab = "Users",
          col = grey.colors(n = 1, start = 0, end = 1))

# ratings by movie - code into appendix
edx %>%
    group_by(movieId) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = n)) +
    scale_x_log10() +
    geom_histogram(col = "black") +
    labs(title = "Rating counts by movie", 
         x = "Number of Ratings (log scale)")

# ratings by user - code into appendix
edx %>%
    group_by(userId) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = n)) +
    scale_x_log10() +
    geom_histogram(bins = 40, col = "black") +
    labs(title = "Rating counts by user", 
         x = "Number of Ratings (log scale)")

# distribution of ratings overall
# DO NOT RUN - this does work but is SLOW
edx %>%
    ggplot(aes(x = rating)) +
    geom_histogram(binwidth = 0.5, col = "black") +
    labs(title = "Distribution of ratings - total",
         x = "Rating") 

# quicker version
edx %>%
    count(rating) %>%
    ggplot(aes(x = factor(rating), y = n)) +
    geom_bar(stat = "identity", width = 1, col = "black")+
    labs(title = "Distribution of ratings - total",
         x = "Rating") 

################################################################################
# 3.4.2 Movie effect
################################################################################

# distribution of ratings by movie
movie_avgs <- edx %>%
    group_by(movieId) %>%
    summarise(movie_avg = mean(rating),
              movie_sd = ifelse(n()>1,sd(rating),0),
              n_ratings_bymovie = n()) %>%
    arrange(movie_avg) %>%
    mutate(row = row_number(movie_avg))

# plot average of ratings and their
# standard deviation, horizontally aligned
p1 <- movie_avgs %>%
    ggplot(aes(x = row, y = movie_avg)) +
    geom_point() +
    labs(x = "Movie (sorted by average rating)", 
         y = "Average Rating", title = "Ratings by movie")
p2 <- movie_avgs %>%
    ggplot(aes(x = row, y = movie_sd)) +
    geom_point() +
    labs(x = "Movie (sorted by average rating)",
         y = "Rating Standard Deviation")
grid.arrange(p1, p2, nrow = 2)

################################################################################
# 3.4.3 User effect
################################################################################

# distribution of ratings by user
user_avgs <- edx %>%
    group_by(userId) %>%
    summarise(user_avg = mean(rating),
              user_sd = ifelse(n()>1,sd(rating),0),
              n_ratings_byuser = n()) %>%
    arrange(user_avg) %>%
    mutate(row = row_number(user_avg))

# plot average of ratings and their
# standard deviation, horizontally aligned
p1 <- user_avgs %>%
    ggplot(aes(x = row, y = user_avg)) +
    geom_point() +
    labs(x = "User (sorted by average rating)", 
         y = "Average Rating", title = "Ratings by user")
p2 <- user_avgs %>%
    ggplot(aes(x = row, y = user_sd)) +
    geom_point() +
    labs(x = "User (sorted by average rating)",
         y = "Rating Standard Deviation")
grid.arrange(p1, p2, nrow = 2)

# discard temporary objects
rm(p1, p2)

################################################################################
# 3.4.4 Time of Rating
################################################################################


################################################################################
# 3.4.5 Genre Effect
################################################################################


################################################################################
# 3.4.7 Data in "validation" set that are not in "edx" set
################################################################################

validation %>%
    anti_join(edx, by = "movieId") %>%
    group_by(movieId, title) %>%
    summarise(n = n())

validation %>%
    anti_join(edx, by = "userId") %>%
    group_by(userId) %>%
    summarise(n = n())

validation %>%
    anti_join(edx, by = "genres") %>%
    group_by(genres) %>%
    summarise(n = n())

################################################################################
# 3.5 Modelling approach
################################################################################

# Generate list of training and test sets from "edx" for k-fold cross-validation
# Use function createFolds with k = 25 
# For a test run k = 10 was used
set.seed(342, sample.kind = "Rounding")
index_list <- createFolds(edx$rating, k = 25)

################################################################################
# 3.5.1 Constant Value
################################################################################

# predict this value for each rating, irrespective of other features
mu <- mean(edx$rating)

# wrap into function to predict average rating for each movie/user combination
predict_const <- function(newdata) {
    mu <- mean(edx$rating)
    return(rep(mu, nrow(newdata)))
    }

################################################################################
# 3.5.2 Movie Effect Only
################################################################################

# The movie effects $b_i$ are calculated similar to Section 3.4.2
# These are now used for prediction of the rating with the model
# $\hat{y}_{u,i} = \mu + b_i$
predict_movieb <- function(newdata) {
    # calculate mean $\mu$ of overall `edx` training set
    mu <- mean(edx$rating)

    # estimate $b_i = y_{u,i} - \mu$ for each movie $i$.
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = mean(rating - mu))

    # look up $b_i$ for each movie to predict $\hat{y}_{u,i} = \mu + b_i$
    pred_ratings <- newdata %>%
        left_join(b_i_tbl, by = "movieId") %>%
        mutate(y_hat = mu + b_i) %>% .$y_hat
    return(pred_ratings)
    }


################################################################################
# 3.5.3 User Effect Only
################################################################################

# The user effects $b_u$ are calculated similar to Section 3.4.3
# These are now used for prediction of the rating with the model
# $\hat{y}_{u,i} = \mu + b_u$
predict_userb <- function(newdata) {
    # calculate mean $\mu$ of overall `edx` training set
    mu <- mean(edx$rating)
    
    # estimate $b_u = y_{u,i} - \mu$ for each movie $i$.
    b_u_tbl <- edx %>%
        group_by(userId) %>%
        summarise(b_u = mean(rating - mu))
    
    # look up $b_u$ for each movie to predict $\hat{y}_{u,i} = \mu + b_u$
    pred_ratings <- newdata %>%
        left_join(b_u_tbl, by = "userId") %>%
        mutate(y_hat = mu + b_u) %>% .$y_hat
    return(pred_ratings)
    }


################################################################################
# 3.5.5 Combined Movie and User Effects
################################################################################


################################################################################
# 3.5.6 Regularised Movie and User Effects
################################################################################

#' Calculate RMSE using k-fold cross validation
#' for regularised movie-user effect with regularisation parameter lambda
#' 
#' @param data Complete training data, to be split into (sub-)training
#'        and validation data.
#' @param ind_list List of $k$ indices of *validation* set.
#' @param lambda Regularisation parameter $\lambda$. 
#' @return Vector of RMSEs of length $k$
RMSE_movieuser_kfold <- function(data, ind_list, lambda) {
    n <- length(ind_list)
    # iterate over indices
    rmse_v <- sapply(1:n, function (listIdx) {
        # split data int training and validation set
        train_set <- data[-ind_list[[listIdx]], ]
        validation_set <- data[ind_list[[listIdx]], ]
        # use training set for regularised movie + user effects
        mu <- mean(train_set$rating)
        b_i <- train_set %>%
            group_by(movieId) %>%
            summarise(b_i = sum(rating - mu)/(n()+lambda))
        b_u <- train_set %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
        # modify test set so all movies and users from training
        # set are contained in it
        validation_set <- validation_set %>%
            semi_join(train_set, by = "movieId") %>%
            semi_join(train_set, by = "userId")
        # if nothing left, can already return NA
        if(length(validation_set) == 0) {
            return(NA)
        } else {
            # otherwise estimate ratings as \mu + b_i + b_u
            ratings_hat <- 
                validation_set %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                mutate(pred = mu + b_i + b_u) %>%
                .$pred
            #  and finally, calculate RMSE
            return(RMSE(ratings_hat, validation_set$rating))
            }
        })
    return(rmse_v)
    }

# apply this for a list of lambdas
lambdas <- seq(0, 10, 0.25)
RMSE_data <- map_df(lambdas, function(lambda) {
    # calculate vector of RMSEs
    rmse_vec <- RMSE_movieuser_kfold(edx, index_list, lambda)
    # strip out the NA values
    rmse_vec <- na.omit(rmse_vec)
    # calculate mean and standard deviation of RMSEs
    list(RMSE_avg = mean(rmse_vec),
         RMSE_sd = sd(rmse_vec))
    })

# add lambda as first columns
RMSE_data <- cbind(data.frame(lambda = lambdas), 
                   RMSE_data)

# look up lambda for which the RMSE is minimal
lambda_opt <- lambdas[which.min(RMSE_data$RMSE_avg)]
RMSE_lambda_opt <- RMSE_data$RMSE_avg[which.min(RMSE_data$RMSE_avg)]

# join RMSE data for diferent k - for k = 10 calculated by
# setting k = 10 at the start of Section 3.5 , recomputing "index_list"
# and running the code from RMSE_data
RMSE_data_k10 <- cbind(data.frame(k = rep(10, nrow(RMSE_data_k10))),
                       RMSE_data_k10)
RMSE_data_k25 <- cbind(data.frame(k = rep(25, nrow(RMSE_data_k10))),
                       RMSE_data)
RMSE_data_joint <-rbind(RMSE_data_k10, RMSE_data_k25)
RMSE_data_joint <- RMSE_data_joint %>% 
    mutate(k = factor(k, levels = c(10, 25)))

# plot 
RMSE_data_joint %>%
    ggplot(aes(x = lambda, y = RMSE_avg,
               ymin = RMSE_avg - RMSE_sd,
               ymax = RMSE_avg + RMSE_sd,
               col = k)) +
    geom_point() +
    geom_errorbar() +
    geom_vline(xintercept = lambda_opt, linetype = "dashed") +
    labs(y = "RMSE (with bootstrapped error)", 
         title = "Tuning of lambda for regularisation")

# save for plotting in document
save(RMSE_data_joint, file = "RMSE_data_regularisation.Rdata")

# prediction function for regularised movie/user effect
predict_regmovieuser <- function(newdata, lambda) {
    # overall mean on training data
    mu <- mean(edx$rating)
    # tabulate movie effects "b_i" with regularisation
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = sum(rating - mu)/(n()+lambda))
    # tabulate user effects "b_u" with regularisation
    b_u_tbl <- edx %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
    # calculate rating prediction by looking up "b_i" and "b_u"
    # from the tables just created and compute on the NEW data
    # \mu + b_i + b_u
    pred_ratings <- 
        newdata %>% 
        left_join(b_i_tbl, by = "movieId") %>%
        left_join(b_u_tbl, by = "userId") %>%
        mutate(pred = mu + b_i + b_u) %>%
        .$pred
    return(pred_ratings)
    }

# save down for k = 10
# for k = 10 had optimal lambda = 5, same for k = 25

################################################################################
# 3.5.5 Additional Time Effect
################################################################################

# need to strip out (regularised) movie and user effect

################################################################################
# 3.5.6 Additional Genre Effect
################################################################################


################################################################################
# 4 Results
################################################################################

################################################################################
# 4.1 Constant Value
################################################################################

# predict ratings and calculate RMSE
ratings_hat_const <- predict_const(validation)
RMSE_rating(ratings_hat_const, validation$rating)
# create table with first row of results


################################################################################
# 4.2 Movie Effect Only
################################################################################

# predict ratings and calculate RMSE
ratings_hat_movieb <- predict_movieb(validation)
rmse_movieb <- RMSE_rating(ratings_hat_movieb, validation$rating)
# append to results


################################################################################
# 4.3 User Effect Only
################################################################################

# predict ratings and calculate RMSE
ratings_hat_userb <- predict_userb(validation)
rmse_userb <- RMSE_rating(ratings_hat_userb, validation$rating)
# append to results

################################################################################
# 4.4 Regularised Movie and User Effect
################################################################################

# predict ratings and calculate RMSE
ratings_hat_regmovieuser <- predict_regmovieuser(validation, lambda_opt)
rmse_regmovieuser <- RMSE_rating(ratings_hat_regmovieuser, validation$rating)
# append to results

################################################################################
# 4.5 Regularised Movie and User Effect, Additional Time Effect
################################################################################

################################################################################
# 4.6 Regularised Movie and User Effect, Additional Time and Genre Effect
################################################################################


################################################################################
################################################################################
# Saving progress
################################################################################
################################################################################
save.image(file = "movielens_20200515.Rdata")
