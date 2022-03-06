rm(list=ls(all=TRUE))

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
								 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
																					 title = as.character(title),
																					 genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

movielens %>% summarise(unique(userId)) %>% nrow()	## Reports total users in dataset
movielens %>% summarise(unique(movieId)) %>% nrow()	## Reports total films in dataset
movielens %>% nrow()																## Reports total reviews in dataset

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
	semi_join(edx, by = "movieId") %>%
	semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##-------------------------------------------------
## START OF DATA CLEANING SECTION
## Data preparation (prior to splitting into training and test sets)
## Find distinct genres

## edx$genres %>% table()

genre_split <- str_split(edx$genres, "\\|", simplify = TRUE)

gather(cbind(unique(genre_split[,1]), 
						 unique(genre_split[,2]),
						 unique(genre_split[,3]),
						 unique(genre_split[,4]),
						 unique(genre_split[,5]),
						 unique(genre_split[,6]),
						 unique(genre_split[,7]),
						 unique(genre_split[,8])), genre, value)


unique(genre_split[,1]) ## List of genres listed first

## Check that this represents all genres
unique(genre_split[,2]) %in% unique(genre_split[,1])
unique(genre_split[,2])[8] ## ""

unique(genre_split[,3]) %in% unique(genre_split[,1])
unique(genre_split[,3])[1] ## ""

unique(genre_split[,4]) %in% unique(genre_split[,1])
unique(genre_split[,4])[1] ## ""

unique(genre_split[,5]) %in% unique(genre_split[,1])
unique(genre_split[,5])[1] ## ""

unique(genre_split[,6]) %in% unique(genre_split[,1])
unique(genre_split[,6])[1] ## ""

unique(genre_split[,7]) %in% unique(genre_split[,1])
unique(genre_split[,7])[1] ## ""

unique(genre_split[,8]) %in% unique(genre_split[,1])
unique(genre_split[,8])[1] ## ""

## There are no "secondary genres" - those that appear only as a 2nd+ genre
## "" appears in all the later options to fill the table (as there are missing values) -> can be ignored

genres_list <- unique(genre_split[,1])

rm(genre_split) ## Remove this as massive 

grepl("Sci", genres_list, fixed = TRUE)  ## only one instance of Sci (so can simplify Sci-Fi search to Sci)
grepl("Film", genres_list, fixed = TRUE) ## only one instance of Film (so can simplify Film_noir search to Film)

## Add columns for genres
edx_g <- edx %>% mutate(Comedy = grepl("Comedy", genres, fixed = TRUE),
												Action = grepl("Action", genres, fixed = TRUE),
												Children = grepl("Children", genres, fixed = TRUE),
												Adventure = grepl("Adventure", genres, fixed = TRUE),
												Animation = grepl("Animation", genres, fixed = TRUE),
												Drama = grepl("Drama", genres, fixed = TRUE),
												Crime = grepl("Crime", genres, fixed = TRUE),
												SciFi = grepl("Sci", genres, fixed = TRUE),
												Horror = grepl("Horror", genres, fixed = TRUE),
												Thriller = grepl("Thriller", genres, fixed = TRUE),
												FilmNoir = grepl("Film", genres, fixed = TRUE),
												Mystery = grepl("Mystery", genres, fixed = TRUE),
												Western = grepl("Western", genres, fixed = TRUE),
												Documentary = grepl("Documentary", genres, fixed = TRUE),
												Romance = grepl("Romance", genres, fixed = TRUE),
												Fantasy = grepl("Fantasy", genres, fixed = TRUE),
												Musical = grepl("Musical", genres, fixed = TRUE),
												War = grepl("War", genres, fixed = TRUE),
												IMAX = grepl("IMAX", genres, fixed = TRUE),
												None = grepl("None", genres, fixed = TRUE))

## Also wish to split out the date from the title:
## Useful to have the following left and right functions

right = function (string, char) {
	substr(string,nchar(string)-(char-1),nchar(string))
}

left = function (string,char) {
	substr(string,1,char)
}

## Adding new column with year (as numeric)
edx_gy <- edx_g %>% mutate(year = as.numeric(left(right(title, 5), 4))) 

## Removing the year from the title
edx_gy <- edx_gy %>% mutate(title = left(title, str_length(title) - 7))

## Adding datetime

edx_gy <- edx_gy %>% mutate(dt = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))

## Split the edx set into train and test sets (50% - may change if too slow):

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index_edx <- createDataPartition(y = edx_gy$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx_gy[-test_index_edx,]
edx_te <- edx_gy[test_index_edx,]

## Remove the items that aren't in both sets

edx_test <- edx_te  %>% 
	semi_join(edx_train, by = "movieId") %>%
	semi_join(edx_train, by = "userId")

removed <- anti_join(edx_te, edx_test)
edx_train <- rbind(edx_train, removed)

##====================================================================================================
## Data visualisation:

## Movie averages - is there variation?
avg_mov_hist <- edx_train %>% group_by(movieId) %>% ## Histogram of movie averages
	summarise(mr = mean(rating)) %>%
	ggplot(aes(mr)) +
	geom_histogram(bins = 200) +
	xlim(0, 5) +
	xlab("Average movie rating") +
	ylab("Count of movies") +
	theme_bw()

edx_train %>% group_by(movieId) %>% .$rating %>% mean() ## average rating
edx_train %>% group_by(movieId) %>% .$rating %>% sd()   ## standard deviation

## User averages - is there variation?
avg_user_hist <- edx_train %>% group_by(userId) %>% ## Histogram of movie averages
	summarise(mu = mean(rating)) %>%
	ggplot(aes(mu)) +
	geom_histogram(bins = 200) +
	xlab("Average user rating") +
	ylab("Count of users") +
	theme_bw()

## Example variation in user averages for a single film - Toy Story
avg_user_TS_hist <- edx_train %>% filter(movieId == 1) %>% group_by(userId) %>% ## Histogram of movie averages
	summarise(mu = mean(rating)) %>%
	ggplot(aes(mu)) +
	geom_histogram(bins = 200) +
	xlim(0, 5.1) +
	xlab("User ratings for Toy Story") +
	ylab("Count of users") +
	theme_bw()

##-----------------------------------------------------------------------------------------------------------
## Explore how ratings vary within users - Genres
## Firstly explore which genres represent a large number of films
## There are likely to be better improvements in accuracy if exploring popular genres (i.e. those with many films)
## rather than those that have few 

edx_train %>% group_by(Action) %>% summarise(mean(rating), n())
edx_train %>% group_by(Drama) %>% summarise(mean(rating), n())  
edx_train %>% group_by(Children) %>% summarise(mean(rating), n())
edx_train %>% group_by(IMAX) %>% summarise(mean(rating), n())
edx_train %>% group_by(Adventure) %>% summarise(mean(rating), n())
edx_train %>% group_by(Crime) %>% summarise(mean(rating), n())
edx_train %>% group_by(Horror) %>% summarise(mean(rating), n())
edx_train %>% group_by(SciFi) %>% summarise(mean(rating), n())

## Action, Drama, Adventure, Crime, Sci-Fi have the best split - explore these further
## We want there to be a difference within users on the scores they give

## Let's look at the average scores for each genre in turn for TRUE and FALSE
## We can report the standard deviation of the difference between TRUE and FALSE averages

##----------------------------------------------------------------------------------------------------------

## Action:
## The mean of the absolute differences
act_m <- edx_train %>% group_by(userId, Action) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Action, values_from = mrating, names_prefix = "Act_") %>% 
	mutate(abs_diff = abs(Act_TRUE - Act_FALSE)) %>% ungroup(userId) %>% summarise(a_m = mean(abs_diff, na.rm = TRUE))

## Histogram of differences
act_hist <- edx_train %>% group_by(userId, Action) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Action, values_from = mrating, names_prefix = "Act_") %>% 
	mutate(diff = Act_TRUE - Act_FALSE) %>% ggplot(aes(diff)) + geom_histogram(binwidth = 0.02)

## Standard deviation of differences
act_sd <- edx_train %>% group_by(userId, Action) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Action, values_from = mrating, names_prefix = "Act_") %>% 
	mutate(diff = Act_TRUE - Act_FALSE) %>% ungroup(userId) %>% summarise(sd = sd(diff, na.rm = TRUE))

act <- cbind(act_m, act_sd)

##----------------------------------------------------------------------------------------------------------

## Drama:
## The mean of the absolute differences
dra_m <- edx_train %>% group_by(userId, Drama) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Drama, values_from = mrating, names_prefix = "Dra_") %>% 
	mutate(abs_diff = abs(Dra_TRUE - Dra_FALSE)) %>% ungroup(userId) %>% summarise(a_m = mean(abs_diff, na.rm = TRUE))

## Histogram of differences
dra_hist <- edx_train %>% group_by(userId, Drama) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Drama, values_from = mrating, names_prefix = "Dra_") %>% 
	mutate(diff = Dra_TRUE - Dra_FALSE) %>% ggplot(aes(diff)) + geom_histogram(binwidth = 0.02)

## Standard deviation of differences
dra_sd <- edx_train %>% group_by(userId, Drama) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Drama, values_from = mrating, names_prefix = "Dra_") %>% 
	mutate(diff = Dra_TRUE - Dra_FALSE) %>% ungroup(userId) %>% summarise(sd = sd(diff, na.rm = TRUE))

dra <- cbind(dra_m, dra_sd)

##----------------------------------------------------------------------------------------------------------
## Adventure:
## The mean of the absolute differences
adv_m <- edx_train %>% group_by(userId, Adventure) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Adventure, values_from = mrating, names_prefix = "Adv_") %>% 
	mutate(abs_diff = abs(Adv_TRUE - Adv_FALSE)) %>% ungroup(userId) %>% summarise(a_m = mean(abs_diff, na.rm = TRUE))

## Histogram of differences
adv_hist <- edx_train %>% group_by(userId, Adventure) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Adventure, values_from = mrating, names_prefix = "Adv_") %>% 
	mutate(diff = Adv_TRUE - Adv_FALSE) %>% ggplot(aes(diff)) + geom_histogram(binwidth = 0.02)

## Standard deviation of differences
adv_sd <- edx_train %>% group_by(userId, Adventure) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Adventure, values_from = mrating, names_prefix = "Adv_") %>% 
	mutate(diff = Adv_TRUE - Adv_FALSE) %>% ungroup(userId) %>% summarise(sd = sd(diff, na.rm = TRUE))

adv <- cbind(adv_m, adv_sd)

##----------------------------------------------------------------------------------------------------------
## Crime:
## The mean of the absolute differences
cri_m <- edx_train %>% group_by(userId, Crime) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Crime, values_from = mrating, names_prefix = "Cri_") %>% 
	mutate(abs_diff = abs(Cri_TRUE - Cri_FALSE)) %>% ungroup(userId) %>% summarise(a_m = mean(abs_diff, na.rm = TRUE))

## Histogram of differences
cri_hist <- edx_train %>% group_by(userId, Crime) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Crime, values_from = mrating, names_prefix = "Cri_") %>% 
	mutate(diff = Cri_TRUE - Cri_FALSE) %>% ggplot(aes(diff)) + geom_histogram(binwidth = 0.02)

## Standard deviation of differences
cri_sd <- edx_train %>% group_by(userId, Crime) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = Crime, values_from = mrating, names_prefix = "Cri_") %>% 
	mutate(diff = Cri_TRUE - Cri_FALSE) %>% ungroup(userId) %>% summarise(sd = sd(diff, na.rm = TRUE))

cri <- cbind(cri_m, cri_sd)

##----------------------------------------------------------------------------------------------------------
## Sci-fi:
## The mean of the absolute differences
sci_m <- edx_train %>% group_by(userId, SciFi) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = SciFi, values_from = mrating, names_prefix = "Sci_") %>% 
	mutate(abs_diff = abs(Sci_TRUE - Sci_FALSE)) %>% ungroup(userId) %>% summarise(a_m = mean(abs_diff, na.rm = TRUE))

## Histogram of differences
sci_hist <- edx_train %>% group_by(userId, SciFi) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = SciFi, values_from = mrating, names_prefix = "Sci_") %>% 
	mutate(diff = Sci_TRUE - Sci_FALSE) %>% ggplot(aes(diff)) + geom_histogram(binwidth = 0.02)

## Standard deviation of differences
sci_sd <- edx_train %>% group_by(userId, SciFi) %>% 
	summarise(mrating = mean(rating)) %>% 
	pivot_wider(names_from = SciFi, values_from = mrating, names_prefix = "Sci_") %>% 
	mutate(diff = Sci_TRUE - Sci_FALSE) %>% ungroup(userId) %>% summarise(sd = sd(diff, na.rm = TRUE))

sci <- cbind(sci_m, sci_sd)

##----------------------------------------------------------------------------------------------------------
## Compare across the different genres:
genres <- c("Action", "Drama", "Adventure", "Crime", "Sci-Fi")
genre_m_sd <- rbind(act, dra, adv, cri, sci) %>% mutate(Genre = genres)

genre_m_sd %>% arrange(desc(a_m)) ## sci > cri > act > dra > adv
genre_m_sd %>% arrange(desc(sd)) ## sci > cri > act > dra > adv

## Both have the same order

## However, Sci-Fi and Crime movies are more rare (add in the proportion of FALSE)

act_n <- edx_train %>% group_by(Action) %>% summarise(n = n())
dra_n <- edx_train %>% group_by(Drama) %>% summarise(n = n())  
adv_n <- edx_train %>% group_by(Adventure) %>% summarise(n = n())
cri_n <- edx_train %>% group_by(Crime) %>% summarise(n = n())
sci_n <- edx_train %>% group_by(SciFi) %>% summarise(n = n())

genre_m_sd_display <- genre_m_sd[, c(3,1,2)] %>% 
	cbind(rbind(act_n[2, 2]/(act_n[1, 2] + act_n[2, 2]), 
							dra_n[2, 2]/(dra_n[1, 2] + dra_n[2, 2]),
							adv_n[2, 2]/(adv_n[1, 2] + adv_n[2, 2]),
							cri_n[2, 2]/(cri_n[1, 2] + cri_n[2, 2]),
							sci_n[2, 2]/(sci_n[1, 2] + sci_n[2, 2]))) %>% 
	rename(Average_abs_diff = a_m,
				 SD_of_diff = sd, 
				 Prop_TRUE = n) %>% mutate(Weight = Average_abs_diff*SD_of_diff*Prop_TRUE)

##====================================================================================================
## Start point (algorithm development)

##-----------------------------------------------------------------------------------------------------------
## Remove the following variables to free up memory
rm(edx_g)
rm(edx_gy)
rm(edx_te)

##-----------------------------------------------------------------------------------------------------------
## (1) Predict based on ratings average
mu_hat <- mean(edx_train$rating)  ## average rating across the training set = 3.512456
naive_rmse <- RMSE(edx_test$rating, mu_hat) ## 1.060054

algorithms1 <- data.frame(cbind("Rating average", naive_rmse)) %>% rename(Algorithm = V1, RMSE = naive_rmse)

##-----------------------------------------------------------------------------------------------------------
## (2) Movie effects
## want the an estimate of what would be found with lm - i.e. conditional average based on film
movie_avgs <- edx_train %>%  group_by(movieId) %>% summarise(b_i = mean(rating - mu_hat))

predicted_ratings_m <- mu_hat + edx_test %>% left_join(movie_avgs, by = 'movieId') %>% ## pulling the averages taken from the train set
	.$b_i

mov_rmse <- RMSE(predicted_ratings_m, edx_test$rating) ## 0.9429615 (improvement on (1))

algorithms2 <- rbind(algorithms1, data.frame(cbind("Movie effects", mov_rmse)) %>%
											rename(Algorithm = V1, RMSE = mov_rmse))

##-----------------------------------------------------------------------------------------------------------
## (3) Predict using a movie effect and a user effect

user_avgs <- edx_train %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarise(b_u = mean(rating - mu_hat - b_i))

predicted_ratings_u <- edx_test %>% left_join(movie_avgs, by='movieId') %>% 
	left_join(user_avgs, by='userId') %>% 
	mutate(pred = mu_hat + b_i + b_u) %>% 
	pull(pred)

mov_user_rmse <- RMSE(predicted_ratings_u, edx_test$rating) ## 0.8646843 (improvement on (2))

algorithms3 <- rbind(algorithms2, data.frame(cbind("Movie and user effects", mov_user_rmse)) %>%
											rename(Algorithm = V1, RMSE = mov_user_rmse))

##-----------------------------------------------------------------------------------------------------------
## Is regularisation required?
## Do the best and worst movies align to expectations, and if not is this because low n makes the estimates noisy?

## Create list of movie titles to join to movie_avgs so we can see interpret the movie names
movie_titles <- edx %>% 
	select(movieId, title) %>%
	distinct()

## Which movies have the highest movie average adjustment (i.e. are deemed the best)?
movie_avgs %>% left_join(movie_titles, by="movieId") %>% ## These are not well known movies!
	arrange(desc(b_i)) %>% 
	slice(1:10)  %>% 
	pull(title)

edx_train %>% count(movieId) %>%                         ## How many ratings do the best movies have?
	left_join(movie_avgs, by="movieId") %>%                ## Very few! (many have only 1)
	left_join(movie_titles, by="movieId") %>%
	arrange(desc(b_i)) %>% 
	slice(1:10) %>% 
	pull(n)

best_movies <- cbind(Best_movies = movie_avgs %>% left_join(movie_titles, by="movieId") %>%
										 	arrange(desc(b_i)) %>% 
										 	slice(1:10)  %>% 
										 	pull(title), 
										 Number_of_reviews = edx_train %>% count(movieId) %>%                         
										 	left_join(movie_avgs, by="movieId") %>%                
										 	left_join(movie_titles, by="movieId") %>%
										 	arrange(desc(b_i)) %>% 
										 	slice(1:10) %>% 
										 	pull(n))

## Which movies have the lowest movie average adjustment (i.e. are deemed the worst)?
movie_avgs %>% left_join(movie_titles, by="movieId") %>% ## These are not well known movies!
	arrange(b_i) %>% 
	slice(1:10)  %>% 
	pull(title)

edx_train %>% count(movieId) %>% 
	left_join(movie_avgs, by="movieId") %>%                ## How many ratings do the worst movies have?
	left_join(movie_titles, by="movieId") %>%              ## Very few! However "SuperBabies: Baby Geniuses 2 (2004)" may be genuinely bad
	arrange(b_i) %>% 
	slice(1:10) %>% 
	pull(n)

worst_movies <- cbind(Worst_movies = movie_avgs %>% left_join(movie_titles, by="movieId") %>%
												arrange(b_i) %>% 
												slice(1:10)  %>% 
												pull(title), 
											Number_of_reviews = edx_train %>% count(movieId) %>%                         
												left_join(movie_avgs, by="movieId") %>%                
												left_join(movie_titles, by="movieId") %>%
												arrange(b_i) %>% 
												slice(1:10) %>% 
												pull(n))

## These are noisy estimates since n is so low, we are likely to improve our estimates by regularising

## Now try regularisation

lambda <- 3 ## Using an example lambda (regularisation factor)

mu <- mean(edx_train$rating)
movie_reg_avgs <- edx_train %>% 
	group_by(movieId) %>% 
	summarise(b_i = sum(rating - mu)/(n()+ lambda), n_i = n()) ## regularised estimates

best_movies_reg <- movie_reg_avgs %>% left_join(movie_titles, by="movieId") %>% arrange(desc(b_i)) ## Regularised BEST movies look much better
worst_movies_reg <- movie_reg_avgs %>% left_join(movie_titles, by="movieId") %>% arrange(b_i) ## Regularised WORST movies look much better

## These responses are much more reasonable

##-----------------------------------------------------------------------------------------------------------
## (4) Regularise movie effects (user effects excluded)
## Does this improve our list of best and worst movies?

## We picked lambda at random here, but we can use the sapply function to check a variety of lambdas

lambdas <- seq(0, 10, 0.25)

mov_sum <- edx_train %>% 						## The sum of all the differences between ratings and the average movie rating and n   
	group_by(movieId) %>%             ## Doing this here means it doesn't have to be done within the sapply function 
	summarize(s = sum(rating - mu), n_i = n())

rmses_mr <- sapply(lambdas, function(l){
	predicted_ratings_mr <- edx_test %>% 
		left_join(mov_sum, by='movieId') %>% 
		mutate(b_i = s/(n_i+l)) %>%	                      ## Computes the regularisation (i.e. degrades the b_i based on the number of ratings)
		mutate(pred = mu_hat + b_i) %>%                   ## Computes a new set of preds for each lambda
		pull(pred)
	return(RMSE(predicted_ratings_mr, edx_test$rating))  
})

qplot(lambdas, rmses_mr) ## plots the lambdas against the rmses 
mov_reg_l <-lambdas[which.min(rmses_mr)] ## Minimised at lambda = 1.5
mov_reg_rmse <- rmses_mr[which.min(rmses_mr)] ## Movie only regularisations = 0.942937 (vs. 0.9429615 without regularisation) (NOT big difference...)
## Those with v low N (e.g. 1) from train set are likely lost in the NA removal procedure

algorithms4 <- rbind(algorithms3, data.frame(cbind("Regularised movie effects", mov_reg_rmse)) %>%
											rename(Algorithm = V1, RMSE =  mov_reg_rmse))

##-----------------------------------------------------------------------------------------------------------
## (5) Regularise movie effects and the user effects at the same time 
## The same lambda is used for user and movie effects - this is to minimise the impact on memory 

lambdas <- seq(4.75, 5.25, 0.25)

rmses_mur <- sapply(lambdas, function(l){
	
	mu <- mean(edx_train$rating)
	
	b_i <- edx_train %>% 
		group_by(movieId) %>%
		summarize(b_i = sum(rating - mu)/(n()+l))
	
	b_u <- edx_train %>% 
		left_join(b_i, by="movieId") %>%
		group_by(userId) %>%
		summarize(b_u = sum(rating - b_i - mu)/(n()+l))
	
	predicted_ratings_mur <- 
		edx_test %>% 
		left_join(b_i, by = "movieId") %>%
		left_join(b_u, by = "userId") %>%
		mutate(pred = mu + b_i + b_u) %>%
		pull(pred)
	
	return(RMSE(predicted_ratings_mur, edx_test$rating))
})

qplot(lambdas, rmses_mur)  
mov_user_reg_l <- lambdas[which.min(rmses_mur)] ## lambda optimised at 5
mov_user_reg_rmse <- rmses_mur[which.min(rmses_mur)] ## 0.8641362 

algorithms5 <- rbind(algorithms4, data.frame(cbind("Regularised movie and user effects", mov_user_reg_rmse)) %>%
											rename(Algorithm = V1, RMSE =  mov_user_reg_rmse))

##----------------------------------------------------------------------------------------------------------
## (6) Use genre-specific user effects
## The rationale being that by identifying polarising genres, we can make better predictions in the test set

## First we need to do some work to prepare:

users_t <- edx_train %>% distinct(userId) %>% mutate(genre = TRUE)
users_f <- edx_train %>% distinct(userId) %>% mutate(genre = FALSE)

empty <- rbind(users_t, users_f) %>% arrange(userId)
## empty is a data frame with two entries for each user, genre is TRUE and genre is FALSE
## We will put the genre averages in here

edx_test$id <- 1:nrow(edx_test) ## I am going to use the merge function and that changes the order, so I need to create an id

##----------------------------------------------------------------------------------------------------------
##(6A) Movie and action-specific user effects

lambdas <- seq(2, 6, 1)

rmses <- sapply(lambdas, function(l){
	
	mu <- mean(edx_train$rating)
	
	movie_avgs_reg_g <- edx_train %>%
		group_by(movieId) %>%
		summarise(b_i = sum(rating - mu)/(n()+ l))
	
	Act <- edx_train %>% 
		left_join(movie_avgs_reg_g, by = 'movieId') %>%
		group_by(userId, Action) %>%
		summarise(r_a = sum(rating - mu - b_i)/(n()+ l))%>%
		rename(genre = Action)
	
	ac <- empty %>% left_join(Act) %>% rename(Action = genre)
	
	act_df <- merge(edx_test, ac, by=c("userId","Action")) %>%
		left_join(movie_avgs, by='movieId') %>% arrange(id)
	
	act_df[is.na(act_df)] <- 0
	
	predicted_ratings_act <- act_df %>% 
		mutate(pred = mu + b_i + r_a) %>% pull(pred)
	
	return(RMSE(predicted_ratings_act, edx_test$rating))
})


qplot(lambdas, rmses)  
mov_user_act_r_l <- lambdas[which.min(rmses)] ## lambda optimised at 4
mov_user_act_r_rmse <- rmses[which.min(rmses)] ## 0.8599548

rbind(cbind("Movie + action-dependent user effects", rmses[which.min(rmses)]))

algorithms6A <- rbind(algorithms5, data.frame(cbind("Regularised movie + action-dependent user effects", mov_user_act_r_rmse)) %>%
										 	rename(Algorithm = V1, RMSE =  mov_user_act_r_rmse))

##----------------------------------------------------------------------------------------------------------
##(6A) Movie and drama-specific user effects

lambdas <- seq(2, 6, 1)

rmses <- sapply(lambdas, function(l){
	
	mu <- mean(edx_train$rating)
	
	movie_avgs_reg_g <- edx_train %>%
		group_by(movieId) %>%
		summarise(b_i = sum(rating - mu)/(n()+ l))
	
	Dra <- edx_train %>% 
		left_join(movie_avgs_reg_g, by = 'movieId') %>%
		group_by(userId, Drama) %>%
		summarise(r_d = sum(rating - mu - b_i)/(n()+ l))%>%
		rename(genre = Drama)
	
	dr <- empty %>% left_join(Dra) %>% rename(Drama = genre)
	
	dra_df <- merge(edx_test, dr, by=c("userId","Drama")) %>%
		left_join(movie_avgs, by='movieId') %>% arrange(id)
	
	dra_df[is.na(dra_df)] <- 0
	
	predicted_ratings_dra <- dra_df %>% 
		mutate(pred = mu + b_i + r_d) %>% pull(pred)
	
	return(RMSE(predicted_ratings_dra, edx_test$rating))
})


qplot(lambdas, rmses)  
mov_user_dra_r_l <- lambdas[which.min(rmses)] ## lambda optimised at 5
mov_user_dra_r_rmse <- rmses[which.min(rmses)] ## 0.8609532

rbind(cbind("Movie + drama-dependent user effects", rmses[which.min(rmses)]))

algorithms6B <- rbind(algorithms6A, data.frame(cbind("Regularised movie + drama-dependent user effects", mov_user_dra_r_rmse)) %>%
										 	rename(Algorithm = V1, RMSE =  mov_user_dra_r_rmse))

##----------------------------------------------------------------------------------------------------------
## The accuracy using the action effects are slightly higher than for the drama effects.

##=======================================================================================
## (7) - Regularised movie + both action- and drama-dependent user effects

lambdas <- seq(240, 270, 10)

lambda_action <- 4

rmses <- sapply(lambdas, function(l){
	
	mu <- mean(edx_train$rating)
	
	movie_avgs_reg_g <- edx_train %>%
		group_by(movieId) %>%
		summarise(b_i = sum(rating - mu)/(n()+ lambda_action))
	
	Act <- edx_train %>% 
		left_join(movie_avgs_reg_g, by = 'movieId') %>%
		group_by(userId, Action) %>%
		summarise(r_a = sum(rating - mu - b_i)/(n()+ lambda_action))%>%
		rename(genre = Action)
	
	Dra <- edx_train %>%
		left_join(movie_avgs, by='movieId') %>% 
		left_join(Act, 'userId') %>% 
		group_by(userId, Drama) %>%
		summarise(r_d = sum(rating - mu - b_i - r_a)/(n() + l)) %>% rename(genre = Drama)
	
	ac <- empty %>% left_join(Act) %>% rename(Action = genre)
	dr <- empty %>% left_join(Dra) %>% rename(Drama = genre)
	
	act_dra_df <- merge(edx_test, ac, by=c("userId","Action")) %>% 
		merge(dr, by=c("userId","Drama")) %>%
		left_join(movie_avgs, by='movieId') %>% arrange(id)
	
	act_dra_df[is.na(act_dra_df)] <- 0
	
	predicted_ratings_act_dra <- act_dra_df %>% 
		mutate(pred = mu + b_i + r_a + r_d) %>% pull(pred)
	
	return(RMSE(predicted_ratings_act_dra, edx_test$rating))
})

qplot(lambdas, rmses)  
mov_user_act_dra_r_l <- lambdas[which.min(rmses)] ## lambda optimised at 250
mov_user_act_dra_r_rmse <- rmses[which.min(rmses)] ## 0.8579755

algorithms7 <- rbind(algorithms6B, data.frame(cbind("Regularised movie + both action- and drama-dependent user effects", mov_user_act_dra_r_rmse)) %>%
												rename(Algorithm = V1, RMSE = mov_user_act_dra_r_rmse))

##=======================================================================================

## PREPARE THE DATA:

##---------------------------------------------------------------------------------------
## DEVELOPMENT SET - EDX
## Add columns for genres
development <- edx %>% mutate(Comedy = grepl("Comedy", genres, fixed = TRUE),
												Action = grepl("Action", genres, fixed = TRUE),
												Children = grepl("Children", genres, fixed = TRUE),
												Adventure = grepl("Adventure", genres, fixed = TRUE),
												Animation = grepl("Animation", genres, fixed = TRUE),
												Drama = grepl("Drama", genres, fixed = TRUE),
												Crime = grepl("Crime", genres, fixed = TRUE),
												SciFi = grepl("Sci", genres, fixed = TRUE),
												Horror = grepl("Horror", genres, fixed = TRUE),
												Thriller = grepl("Thriller", genres, fixed = TRUE),
												FilmNoir = grepl("Film", genres, fixed = TRUE),
												Mystery = grepl("Mystery", genres, fixed = TRUE),
												Western = grepl("Western", genres, fixed = TRUE),
												Documentary = grepl("Documentary", genres, fixed = TRUE),
												Romance = grepl("Romance", genres, fixed = TRUE),
												Fantasy = grepl("Fantasy", genres, fixed = TRUE),
												Musical = grepl("Musical", genres, fixed = TRUE),
												War = grepl("War", genres, fixed = TRUE),
												IMAX = grepl("IMAX", genres, fixed = TRUE),
												None = grepl("None", genres, fixed = TRUE))

## Also wish to split out the date from the title:

## Adding new column with year (as numeric)
development <- development %>% mutate(year = as.numeric(left(right(title, 5), 4))) 

## Removing the year from the title
development <- development %>% mutate(title = left(title, str_length(title) - 7))

## Adding datetime
development <- development %>% mutate(dt = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))

## DEVELOPMENT DATASET PREPARED

##---------------------------------------------------------------------------------------
## VALIDATION SET
## Add columns for genres
validation <- validation %>% mutate(Comedy = grepl("Comedy", genres, fixed = TRUE),
															Action = grepl("Action", genres, fixed = TRUE),
															Children = grepl("Children", genres, fixed = TRUE),
															Adventure = grepl("Adventure", genres, fixed = TRUE),
															Animation = grepl("Animation", genres, fixed = TRUE),
															Drama = grepl("Drama", genres, fixed = TRUE),
															Crime = grepl("Crime", genres, fixed = TRUE),
															SciFi = grepl("Sci", genres, fixed = TRUE),
															Horror = grepl("Horror", genres, fixed = TRUE),
															Thriller = grepl("Thriller", genres, fixed = TRUE),
															FilmNoir = grepl("Film", genres, fixed = TRUE),
															Mystery = grepl("Mystery", genres, fixed = TRUE),
															Western = grepl("Western", genres, fixed = TRUE),
															Documentary = grepl("Documentary", genres, fixed = TRUE),
															Romance = grepl("Romance", genres, fixed = TRUE),
															Fantasy = grepl("Fantasy", genres, fixed = TRUE),
															Musical = grepl("Musical", genres, fixed = TRUE),
															War = grepl("War", genres, fixed = TRUE),
															IMAX = grepl("IMAX", genres, fixed = TRUE),
															None = grepl("None", genres, fixed = TRUE))

## Also wish to split out the date from the title:

## Adding new column with year (as numeric)
validation <- validation %>% mutate(year = as.numeric(left(right(title, 5), 4))) 

## Removing the year from the title
validation <- validation %>% mutate(title = left(title, str_length(title) - 7))

## Adding datetime
validation <- validation %>% mutate(dt = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))

## VALIDATION DATASET PREPARED
##---------------------------------------------------------------------------------------
## PREP FOR EVALUATION

memory.limit(150000)

users_t <- development %>% distinct(userId) %>% mutate(genre = TRUE)
users_f <- development %>% distinct(userId) %>% mutate(genre = FALSE)

empty <- rbind(users_t, users_f) %>% arrange(userId)

## empty is a data frame with two entries for each user, genre is TRUE and genre is FALSE
## We will put the genre averages in here

validation$id <- 1:nrow(validation) ## I am going to use the merge function and that changes the order, so I need to create an id

##---------------------------------------------------------------------------------------
## FINAL ALGORITHM - Regularised movie and both action- and drama-specific user effects
lambda_action <- 4
lambda_drama <- 250

	mu <- mean(development$rating)
	
	movie_avgs_reg_g <- development %>%
		group_by(movieId) %>%
		summarise(b_i = sum(rating - mu)/(n()+ lambda_action))
	
	Act <- development %>% 
		left_join(movie_avgs_reg_g, by = 'movieId') %>%
		group_by(userId, Action) %>%
		summarise(r_a = sum(rating - mu - b_i)/(n()+ lambda_action))%>%
		rename(genre = Action)
	
	Dra <- development %>%
		left_join(movie_avgs, by='movieId') %>% 
		left_join(Act, 'userId') %>% 
		group_by(userId, Drama) %>%
		summarise(r_d = sum(rating - mu - b_i - r_a)/(n() + lambda_drama)) %>% rename(genre = Drama)
	
	ac <- empty %>% left_join(Act) %>% rename(Action = genre)
	dr <- empty %>% left_join(Dra) %>% rename(Drama = genre)
	
	act_dra_df <- merge(validation, ac, by=c("userId","Action")) %>% 
		merge(dr, by=c("userId","Drama")) %>%
		left_join(movie_avgs, by='movieId') %>% arrange(id)
	
	act_dra_df[is.na(act_dra_df)] <- 0
	
	predicted_ratings_final <- act_dra_df %>% 
		mutate(pred = mu + b_i + r_a + r_d) %>% pull(pred)
	
final_rmse <- RMSE(predicted_ratings_final, validation$rating) ## 0.858301

final_algorithm <- data.frame(cbind("Regularised movie + both action- and drama-dependent user effects", final_rmse)) %>%
										 	rename(Final_algorithm = V1, RMSE = final_rmse)

