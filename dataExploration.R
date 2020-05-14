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
# 3.4.1 Movie effect
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
# 3.4.2 User effect
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

# get rid of temporary objects
rm(p1, p2)

# generate list of training and validation sets
# use function createResample
set.seed(342, sample.kind = "Rounding")
index_list <- createFolds(edx$rating, k = 10)

# run cross-validation of regularisation with indices
RMSE_movieuser_kfold <- function(data, ind_list, lambda) {
    n <- length(ind_list)
    # iterate over indices
    rmse_v <- sapply(1:n, function (listIdx) {
        # split data int training and validation set
        train_set <- data[-ind_list[[listIdx]], ]
        test_set <- data[ind_list[[listIdx]], ]
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
        test_set <- test_set %>%
            semi_join(train_set, by = "movieId") %>%
            semi_join(train_set, by = "userId")
        # if nothing left, can already return NA
        if(length(test_set) == 0) {
            return(NA)
        } else {
            # otherwise estimate ratings
            ratings_hat <- 
                test_set %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                mutate(pred = mu + b_i + b_u) %>%
                .$pred
            return(RMSE(ratings_hat, test_set$rating))
            }
        })
    return(rmse_v)
    }

# apply this for a list of lambdas
lambdas <- seq(0, 10, 0.25)
RMSE_data <- map_df(lambdas, function(lambda) {
    paste0("Calculate for lambda = ",lambda)
    # calculate vector of RMSEs
    rmse_vec <- RMSE_movieuser_kfold(edx, index_list, lambda)
    # strip out the NA values
    rmse_vec <- na.omit(rmse_vec)
    # calculate mean and standard deviation of RMSE
    list(RMSE_avg = mean(rmse_vec),
         RMSE_sd = sd(rmse_vec))
    })

# add lambda as first columns
RMSE_data <- cbind(data.frame(lambda = lambdas), 
                   RMSE_data)

# look up lambda for which the RMSE is minimal
lambda_opt <- lambdas[which.min(RMSE_data$RMSE_avg)]
RMSE_lambda_opt <- RMSE_data$RMSE_avg[which.min(RMSE_data$RMSE_avg)]

# plot to illustrate min RMSE
RMSE_data %>%
    ggplot(aes(x = lambda, y = RMSE_avg,
               ymin = RMSE_avg - RMSE_sd,
               ymax = RMSE_avg + RMSE_sd)) +
    geom_point() +
    geom_errorbar() +
    geom_vline(xintercept = lambda_opt, linetype = "dashed") +
    geom_hline(yintercept = RMSE_lambda_opt, linetype = "dashed") +
    labs(y = "RMSE (with error)", title = "Tuning of lambda")

save.image(file = "movielens_20200514.Rdata")
