library(tidyverse)
library(lubridate)
library(caret)
library(ggplot2)
library(gridExtra)
library(purrr)
library(gam)

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

# same for edx_1000 and validation_1000 sample sets

edx_1000 <- edx_1000 %>%
    mutate(ratingdate = as_datetime(timestamp)) %>%
    select(-timestamp)

validation_1000 <- validation_1000 %>%
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

# sparsity
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
    geom_histogram(bins = 30, col = "black") +
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
# edx %>%
#     ggplot(aes(x = rating)) +
#     geom_histogram(binwidth = 0.5, col = "black") +
#     labs(title = "Distribution of ratings - total",
#          x = "Rating") 

# quicker version with grouping by tidyverse (not by ggplot)
edx %>%
    count(rating) %>%
    ggplot(aes(x = factor(rating), y = n)) +
    geom_bar(stat = "identity", width = 1, col = "black")+
    labs(title = "Distribution of ratings - total",
         x = "Rating", y = "Count") 

################################################################################
# 3.4.2 Movie effect
################################################################################

# distribution of ratings by movie - in appendix
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

# distribution of ratings by user - in appendix
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

# tabulate rating by time
edx_1000 %>%
    mutate(ratingdate_wk = round_date(ratingdate, unit = "week")) %>%
    group_by(ratingdate_wk) %>%
    summarise(rating_wk = mean(rating)) %>%
    ggplot(aes(x = ratingdate_wk, y = rating_wk)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    labs(title = "Rating by date",
         x = "Rating date (granularity of weeks)",
         y = "Rating")
    
# standardise the rating date as weeks since first date
min_date = min(edx$ratingdate)

# train_set <- edx_1000
lambda <- lambda_opt
# train model to time effect, AFTER stripping out 
# regularised movie and user effect
mu <- mean(edx$rating)
# Movie effect
b_i_tbl <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+lambda_mur))
# User effect
b_u_tbl <- edx %>% 
    left_join(b_i_tbl, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+lambda_mur))
# Residual time effect
d_ui_tbl <- edx %>%
    mutate(ratingdate_wk = round_date(ratingdate, unit = "week")) %>%
    mutate(rating_wk = (ratingdate_wk - min_date)/7) %>%
    left_join(b_i_tbl, by = "movieId") %>%
    left_join(b_u_tbl, by = "userId") %>%
    group_by(rating_wk) %>%
    summarise(d_ui = mean(rating - mu - b_i - b_u))

# plot the overall time effect once movie and user effect are accounted for
d_ui_tbl %>%
    ggplot(aes(x = as.numeric(rating_wk), y = d_ui)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess") +
    labs(title = "Residual time effect of ratings",
         x = paste0("Weeks since ",format(min_date, "%d %b %Y")),
         y = "Residual rating effect (stars)")


################################################################################
# 3.4.5 Genre Effect
################################################################################

# Determine distribution of number of ratings
edx %>%
    group_by(genres) %>%
    summarise(rating_avg = mean(rating),
              nratings = n()) %>%
    arrange(nratings) %>%
    ggplot(aes(x = nratings)) +
    scale_x_log10() +
    geom_histogram(bins = 30, col = "black")
# -> so there are sufficiently many genres with >=1000 data points

# plot for all genres with at least 1000 ratings their average
# and standard deviation
p1 <- edx %>%
    select(genres, rating) %>%
    mutate(genres = reorder(genres, rating, mean)) %>%
    group_by(genres) %>%
    summarise(rating_avg = mean(rating),
              rating_sd = sd(rating),
              nratings = n()) %>%
    filter(nratings >= 1000) %>%
    ggplot(aes(x = genres, y = rating_avg)) +
    scale_y_log10() +
    theme(axis.text.x = element_text("")) +
    geom_point() +
    labs(title = "Ratings by genres (with at least 1000 ratings)",
         x = "Genre (sorted by average rating)",
         y = "Average Rating")

p2 <- edx %>%
    select(genres, rating) %>%
    mutate(genres = reorder(genres, rating, mean)) %>%
    group_by(genres) %>%
    summarise(rating_avg = mean(rating),
              rating_sd = sd(rating),
              nratings = n()) %>%
    filter(nratings >= 1000) %>%
    ggplot(aes(x = genres, y = rating_sd)) +
    scale_y_log10() +
    theme(axis.text.x = element_text("")) +
    geom_point() +
    labs(title = "",
         x = "Genre (sorted by average rating)",
         y = "Rating Standard Deviation")

grid.arrange(p1, p2, nrow = 2)

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

# visualisation of training data (edx) and test data validation, with 
# training data further split into training sets and test sets under a
# k-fold cross-validation with k = 25
n_data <- nrow(edx)
validation_split <- data.frame(
    k = seq(1, 25, 1),
    C = seq(0, 24, 1) / 25 * n_data,
    B = rep(1/25 * n_data, time = 25))
validation_split <- validation_split %>%
    mutate(A = n_data - B - C)
validation_split <- validation_split %>%
    gather(set, value, -k) 

validation_split %>%
    ggplot(aes(x = k, y = value, fill = set)) +
    scale_fill_manual(values = c("#ACE5EE", "#FAF0BE", "#ACE5EE")) +
    theme(plot.margin = margin(0,0,0,0, "cm")) +
    geom_bar(stat = "identity", position = "stack", show.legend = FALSE) +
    labs(title = "Illustration of k-fold cross validation (k = 25)",
         y = "Values")
# discard data after plot
rm(validation_split)
    
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

    # estimate $b_i = avg(y_{u,i} - \mu)$ for each movie $i$.
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
# $\hat{y}_{u,i} = \mu + \hat b_u$
predict_userb <- function(newdata) {
    # calculate mean $\mu$ of overall `edx` training set
    mu <- mean(edx$rating)
    
    # estimate $\hat b_u = avg(y_{u,i} - \mu)$ for each movie $i$.
    b_u_tbl <- edx %>%
        group_by(userId) %>%
        summarise(b_u = mean(rating - mu))
    
    # look up $b_u$ for each movie to predict $\hat{y}_{u,i} = \mu + \hat b_u$
    pred_ratings <- newdata %>%
        left_join(b_u_tbl, by = "userId") %>%
        mutate(y_hat = mu + b_u) %>% .$y_hat
    return(pred_ratings)
    }


################################################################################
# 3.5.5 Combined Movie and User Effects
################################################################################

# successively calculte movie and user effec for all data in `edx`, then
# predict for desired `newdata`
predict_movieuserb <- function(newdata) {
    # calculate mean $\mu$ of overall `edx` training set
    mu <- mean(edx$rating)
    
    # estimate $\hat b_i = avg(y_{u,i} - \mu)$ for each movie $i$.
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = mean(rating - mu))
    
    # estimate $\hat b_u = avg(y_{u,i} - \mu - \hat b_i)$
    b_u_tbl <- edx %>% 
        left_join(b_i_tbl, by="movieId") %>%
        group_by(userId) %>%
        summarise(b_u = mean(rating - b_i - mu))

    # apply to new data to calculate 
    # $\hat \y_{u,i} = \hat\mu + \hat b_i + \hat b_u$
    pred_ratings <- newdata %>%
        left_join(b_i_tbl, by = "movieId") %>%
        left_join(b_u_tbl, by = "userId") %>%
        mutate(y_hat = mu + b_i + b_u) %>% .$y_hat
    
    return(pred_ratings)
    }

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
        b_i_tbl <- train_set %>%
            group_by(movieId) %>%
            summarise(b_i = sum(rating - mu)/(n()+lambda))
        b_u_tbl <- train_set %>% 
            left_join(b_i_tbl, by="movieId") %>%
            group_by(userId) %>%
            summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))
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
                left_join(b_i_tbl, by = "movieId") %>%
                left_join(b_u_tbl, by = "userId") %>%
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
RMSE_data_mur <- map_df(lambdas, function(lambda) {
    # calculate vector of RMSEs
    rmse_vec <- RMSE_movieuser_kfold(edx, index_list, lambda)
    # strip out the NA values
    rmse_vec <- na.omit(rmse_vec)
    # calculate mean and standard deviation of RMSEs
    list(RMSE_avg = mean(rmse_vec),
         RMSE_sd = sd(rmse_vec))
    })

# add lambda as first columns
RMSE_data_mur <- cbind(data.frame(lambda = lambdas), 
                       RMSE_data_mur)

# look up lambda for which the RMSE is minimal
lambda_mur_opt <- lambdas[which.min(RMSE_data_mur$RMSE_avg)]
RMSE_lambda_opt <- RMSE_data_mur$RMSE_avg[which.min(RMSE_data_mur$RMSE_avg)]

# plot 
RMSE_data_mur %>%
    ggplot(aes(x = lambda, y = RMSE_avg)) +
    geom_point() +
    geom_vline(xintercept = lambda_mur_opt, linetype = "dashed") +
    labs(y = "RMSE (with bootstrapped error)", 
         title = "Tuning of movie/user lambda for regularisation")

# save for plotting in report document
save(RMSE_data_mur, lambda_mur_opt, file = "RMSE_data_mur.Rdata")

#' Prediction function for regularised movie/user effect
#' 
#' @param newdata Data for which to predict the ratings.
#' @param lambda Regularisation parameter $\lambda$.
#' @return Predicted ratings
predict_regmovieuser <- function(newdata, lambda) {
    # overall mean on training data
    mu <- mean(edx$rating)
    # tabulate movie effects "b_i" with regularisation
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = sum(rating - mu)/(n()+lambda))
    # tabulate user effects "b_u" with regularisation
    b_u_tbl <- edx %>% 
        left_join(b_i_tbl, by="movieId") %>%
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


################################################################################
# 3.5.5 Additional Time Effect
################################################################################

# List of tunable parameters in loess function "gamLoess"
modelLookup("gamLoess")
# -> span and degree

# run with built-in cross-validation
train_loess <- train(d_ui ~ rating_wk, data = d_ui_tbl, 
                     method = "gamLoess",
                     tuneGrid = data.frame(degree = 1,
                                           span = seq(0.10, 0.60, 0.05)))
# save model for report
save(train_loess, file = "train_loess.Rdata")

# plot and look up best span parameter
ggplot(train_loess, highlight = TRUE)
span_opt <- train_loess$bestTune$span

# plot fit of best model to time effect
d_ui_tbl %>%
    mutate(d_ui_hat = predict(train_loess, newdata = .)) %>%
    ggplot(aes(x = as.numeric(rating_wk))) +
    geom_point(aes(y = d_ui), alpha = 0.5) +
    geom_line(aes(y = d_ui_hat), col = "blue", size = 2) +
    ylim(c(-0.1, 0.1)) +
    labs(title = "Additional time effect fitted with LOESS",
         x = "Weeks since 1 Sep 1995",
         y = "Rating Effect")

# fit model for prediction of RESIDUAL time effect
fit_loess <- train_loess$finalModel

# prediction function for regularised movie and user effect plus
# additional time effect

#' Prediction function for regularised movie/user and time effect
#' 
#' @param newdata Data for which to predict the ratings.
#' @param lambda Regularisation parameter $\lambda$ for movie and user effects.
#' @param d_ui_loess LOESS model object to predict residual time effect
#'        based on week in which rating was awared.
#' @return Predicted ratings
predict_rmu_t <- function(newdata, lambda, d_ui_loess) {
    # overall mean on training data
    mu <- mean(edx$rating)
    # tabulate movie effects "b_i" with regularisation and given lambda
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = sum(rating - mu)/(n()+lambda))
    # tabulate user effects "b_u" with regularisation and given lambda
    b_u_tbl <- edx %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarise(b_u = sum(rating - b_i - mu)/(n()+lambda))

    # calculate rating prediction by looking up "b_i" and "b_u",
    # and taking the model $f(d_ui)$ provided by d_ui_loess
    # from the tables just created and compute on the NEW data
    # $\mu + b_i + b_u + f(d_ui)$
    pred_ratings <- 
        newdata %>% 
        left_join(b_i_tbl, by = "movieId") %>%
        left_join(b_u_tbl, by = "userId") %>%
        mutate(ratingdate_wk = round_date(ratingdate, unit = "week")) %>%
        mutate(rating_wk = (ratingdate_wk - min_date)/7) %>%
        mutate(d_ui = predict(d_ui_loess, newdata = .)) %>%
        mutate(pred = mu + b_i + b_u + d_ui) %>%
        .$pred
    return(pred_ratings)
    }


################################################################################
# 3.5.6 Additional Genre Effect
################################################################################

#' Calculate RMSE using k-fold cross validation
#' for regularised genre effect with regularisation parameter lambda
#' based on already regularised movie-user effect
#' 
#' @param data Complete training data, to be split into (sub-)training
#'        and validation data.
#' @param ind_list List of $k$ indices of *validation* set.
#' @param lambda_mur Regularisation parameter $\lamdba$ for the regularised
#'        movie and user effects.
#' @param lambda_g Regularisation parameter $\lambda$ for the genre effect.
#' @return Vector of RMSEs of length $k$
RMSE_rmu_g_kfold <- function(data, ind_list, lambda_mur, lambda_g) {
    k <- length(ind_list)
    # iterate over indices
    rmse_v <- sapply(1:k, function (listIdx) {
        # split data int training and validation set
        train_set <- data[-ind_list[[listIdx]], ]
        validation_set <- data[ind_list[[listIdx]], ]
        # use training set for regularised movie + user effects
        mu <- mean(train_set$rating)
        b_i_tbl <- train_set %>%
            group_by(movieId) %>%
            summarise(b_i = sum(rating - mu)/(n()+lambda_mur))
        b_u_tbl <- train_set %>% 
            left_join(b_i_tbl, by="movieId") %>%
            group_by(userId) %>%
            summarise(b_u = sum(rating - b_i - mu)/(n()+lambda_mur))
        # also add on genre effect
        g_ik_tbl <- train_set %>%
            left_join(b_i_tbl, by = "movieId") %>%
            left_join(b_u_tbl, by = "userId") %>%
            group_by(genres) %>%
            summarise(g_ik = mean(rating - mu - b_i - b_u)/(n()+lambda_g))

        # modify test set so all moviess, users and genres from training
        # set are contained in it
        validation_set <- validation_set %>%
            semi_join(train_set, by = "movieId") %>%
            semi_join(train_set, by = "userId") %>%
            semi_join(train_set, by = "genres")
        
        # if nothing left, can already return NA
        if(length(validation_set) == 0) {
            return(NA)
        } else {
            # otherwise estimate ratings as \mu + b_i + b_u + \sum_k g_ik
            ratings_hat <- 
                validation_set %>% 
                left_join(b_i_tbl, by = "movieId") %>%
                left_join(b_u_tbl, by = "userId") %>%
                left_join(g_ik_tbl, by = "genres") %>%
                mutate(pred = mu + b_i + b_u + g_ik) %>%
                .$pred
            #  and finally, calculate RMSE
            return(RMSE(ratings_hat, validation_set$rating))
            }
        })
    return(rmse_v)
    }

# apply this for a list of lambdas
lambdas <- seq(0, 10, 0.25)
RMSE_data_g <- map_df(lambdas, function(lambda) {
    # calculate vector of RMSEs
    rmse_vec <- RMSE_rmu_g_kfold(edx, index_list, lambda_mur_opt, 
                                 lambda)
    # strip out the NA values
    rmse_vec <- na.omit(rmse_vec)
    # calculate mean and standard deviation of RMSEs
    list(RMSE_avg = mean(rmse_vec),
         RMSE_sd = sd(rmse_vec))
    })

# add lambda as first columns
RMSE_data_g <- cbind(data.frame(lambda = lambdas), 
                     RMSE_data_g)

# plot lambdas
RMSE_data_g %>%
    ggplot(aes(x = lambda, y = RMSE_avg)) +
    geom_point() +
    labs(y = "RMSE (with bootstrapped error)", 
         title = "Tuning of genre lambda for regularisation")

# and select optimal lambda
lambda_g_opt <- RMSE_data_g$lambda[which.min(RMSE_data_g$RMSE_avg)]
# is zero, so there is NO regularisation!

#' Prediction function for regularised movie/user and genre effect
#' 
#' @param newdata Data for which to predict the ratings.
#' @param lambda_mur Regularisation parameter $\lambda$ for movie and user 
#'        effects.
#' @param lamdba_g Regularisation parameter $\lambda$ for genre effect.
#' @return Predicted ratings
predict_rmu_g <- function(newdata, lamdba_mur, lambda_g) {
    # use total training set to first calculate overall average rating $\mu$
    mu <- mean(edx$rating)
    
    # Then construct table of regularised movie effects
    b_i_tbl <- edx %>%
        group_by(movieId) %>%
        summarise(b_i = sum(rating - mu)/(n()+lambda_mur))
    
    # Then tabulate the regularised user effects
    b_u_tbl <- edx %>% 
        left_join(b_i_tbl, by="movieId") %>%
        group_by(userId) %>%
        summarise(b_u = sum(rating - b_i - mu)/(n()+lambda_mur))

    # Last, tabulate the regularised genre effect
    g_ik_tbl <- edx %>%
        left_join(b_i_tbl, by = "movieId") %>%
        left_join(b_u_tbl, by = "userId") %>%
        group_by(genres) %>%
        summarise(g_ik = mean(rating - mu - b_i - b_u)/(n()+lambda_g))
    
    # With that, look up all three effect (movie, user, genre) to compute 
    # $\hat{y}_{u,i} = \mu + b_i + b_u + #\sum_{k=1}^k \delta_{i,k}\beta_k$
    ratings_hat <- 
        newdata %>% 
        left_join(b_i_tbl, by = "movieId") %>%
        left_join(b_u_tbl, by = "userId") %>%
        left_join(g_ik_tbl, by = "genres") %>%
        mutate(pred = mu + b_i + b_u + g_ik) %>%
        .$pred
    
    return(ratings_hat)
    }


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
# 4.4 Combined Movie and User Effects
################################################################################

# predict ratings and calculate RMSE
ratings_hat_movieuserb <- predict_movieuserb(validation)
rmse_movieuserb <- RMSE_rating(ratings_hat_movieuserb, validation$rating)
# append to results

################################################################################
# 4.5 Regularised Movie and User Effect
################################################################################

# predict ratings and calculate RMSE
ratings_hat_regmovieuser <- predict_regmovieuser(validation, lambda_mur_opt)
rmse_regmovieuser <- RMSE_rating(ratings_hat_regmovieuser, validation$rating)
# append to results

################################################################################
# 4.6 Regularised Movie and User Effect, Additional Time Effect
################################################################################

# predict ratings and calculate RMSE
ratings_hat_rmu_t <- predict_rmu_t(validation, lambda_mur_opt, fit_loess)
rmse_rmu_t <- RMSE_rating(ratings_hat_rmu_t, validation$rating)
# append to results

################################################################################
# 4.7 Regularised Movie and User Effect, Additional Genre Effect
################################################################################

# predict ratings and calculate RMSE
ratings_hat_rmu_g <- predict_rmu_g(validation, lambda_mur_opt, lambda_g_opt)
RMSE_rating(ratings_hat_rmu_g, validation$rating)
# append to results

################################################################################
################################################################################
# Saving progress
################################################################################
################################################################################
# save.image(file = "movielens_20200515.Rdata")
# save.image(file = "movielens_20200517.Rdata")
save.image(file = "movielens_20200518.Rdata")

# load on demand
# load(file = "movielens_20200515.Rdata")
load(file = "movielens_20200517.Rdata")
