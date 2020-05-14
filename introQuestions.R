library(tidyverse)
library(caret)
library(data.table)

source("loadLocalData.R")

# Prep questions

# Q1
# #rows and #columns in edx dataset
nrow(edx)
# 9000055

ncol(edx)
# 6

# Q2
# # zeroes and threes in rating column in edx set
sum(edx$rating == 0)
# 0

sum(edx$rating == 3)
# 2121240

# Q3 and Q4 number of different movies and users in edx set
edx %>% 
    summarise(n_users = n_distinct(userId),
              n_movies = n_distinct(movieId))

# movies: 10677
# users: 69878

# Q5

# test by extracting first 1000 rows only
edx_sub <- edx %>% slice(1:1000000)

edx_sub_genres <- edx_sub %>%
    group_by(genres) %>% .$genres

# Now, test on subset, for head numbers should be 
# 2, 2, 2, 1
# "head()" commented out for full test run
edx_sub %>%
    mutate(isDrama = str_detect(genres, "Drama"),
           isComedy = str_detect(genres, "Comedy"),
           isThriller = str_detect(genres, "Thriller"),
           isRomance = str_detect(genres, "Romance")) %>% # head() %>%
    summarise(nDrama = sum(isDrama), 
              nComedy = sum(isComedy),
              nThriller = sum(isThriller),
              nRomance = sum(isRomance))

# run on full data set
edx %>%
    mutate(isDrama = str_detect(genres, "Drama"),
           isComedy = str_detect(genres, "Comedy"),
           isThriller = str_detect(genres, "Thriller"),
           isRomance = str_detect(genres, "Romance")) %>%
    summarise(nDrama = sum(isDrama), 
              nComedy = sum(isComedy),
              nThriller = sum(isThriller),
              nRomance = sum(isRomance))
# Results
# nDrama nComedy nThriller nRomance
# 1 3910127 3540930   2325899  1712100
# clean up temp data
rm(edx_sub, edx_sub_genres)

# Q6 movie, distinct by #ratings
edx %>% group_by(movieId, title) %>%
    summarise(n_ratings = n()) %>%
    arrange(desc(n_ratings))
# -> Pulp Fiction

# Q7 - most given ratings,
# so group by ratings
edx %>% group_by(rating) %>%
    summarise(freq_rating = n()) %>%
    arrange(desc(freq_rating))
# so 4, 3, 5, 3.5, 2

# Q8 half star ratings LESS common than full star (integer) ratings?
edx %>% group_by(rating) %>%
    summarise(freq_rating = n()) %>%
    mutate(rating_round = ifelse(rating == round(rating, 0),
                                 "full star", "half star")) %>%
    group_by(rating_round) %>%
    summarise(n = sum(freq_rating))
# So it's TRUE