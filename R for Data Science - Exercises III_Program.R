#Hierarchy (in order to ease searching via Strg + f):

# Part I - xxx
# Chapter 1 - Introduction

# 1.1.3 -------------------------------------------------------------------



# Part III - Program
# Chapter 19 - Functions


# 19.2.1 ------------------------------------------------------------------


  #1
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(rnorm(10))
    #TRUE is not necessarily an argument to the function because it just specifies the behavior of the called 'range' function
    #Users of rescale01 needn't control the behavior differently than the default.
    #theoretically, you can rewrite the function so that it gives control over the handling of NA's and Infinite Values.
      #This isn't sensible but still, the execution is shown below.
rescale01_alt <- function(x, na = TRUE, fin = TRUE) {
  rng <- range(x, na.rm = na, finite = fin)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01_alt(c(rnorm(10), NA), na = FALSE)
    #If na.rm is specified as FALSE, the code still works due to the finite argument being stronger
    #However, setting the finite argument to FALSE additionally results in all values being returned as NA
rescale01_alt(c(rnorm(10), NA), na = FALSE, fin = FALSE)

  #2
x <- c(rnorm(10), NA, Inf, -Inf)
rescale01_var <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  case_when(
    x ==  Inf ~ 1,
    x == -Inf ~ 0,
    is.na(x)  ~ NA_real_,
    TRUE      ~ (x - rng[1]) / (rng[2] - rng[1])
  )
}
rescale01(x)
rescale01_var(x)
  #3
x <- c(rnorm(10), NA)
    #a)
perc_na <- function(x) {
  all.na <- is.na(x)
  mean(all.na)
}
      #This function returns the amount of NA's in a vector
perc_na(x)
    #b)
portion_of_sum <- function(x) {
  sum.vec <- sum(x, na.rm = TRUE)
  x / sum.vec
}
portion_of_sum(1:5)
      #This function normalises a vector so that it sums to 1
    #c
rsd <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
rsd(1:5)
      #This function returns the relative standard deviation

  #4
library(tidyverse)
    #the mpg dataset (and mostly the hwy & cty variables) from the tidyverse is used
hwy <- mpg$hwy; cty <- mpg$cty

se <- function(x) {
  c <- var(x)
  l <- length(x)
  sqrt( c/l )
}
se(hwy); se(cty)

variance <- function(x) {
  n <- length(x)
  m <- mean(x)
  (1 / (n - 1))    *    sum((x - m)^2)
}
variance(hwy); var(hwy); variance(1:10)

x <- cty
skew <- function(x) {
  n <- length(x)
  m <- mean(x)
  sde <- sd(x)
  numerator <- sum((x - m)^3)
  denominator <- (n-2) * sde^3
  numerator / denominator
}
skew(cty); skew(hwy); skew(c(1,2,5,100))

  #5
    #Question is a little ambiguous.
      #a) Either the function shall return the total number of missing values
      #b) Or the function shall return the position within each vector
    #Both interpretations will be implemented. The following vectors are used for demonstration.
x <- c(NA, 1:10, NA, NA)
y <- c(0, NA, 1:10, NA)
      #a)
both_na_a <- function(x,y) {
  sum(is.na(x) + is.na(y))  
}
both_na_a(x,y)
      #b)
both_na_b <- function(x,y) {
  na.x <- which(is.na(x))
  na.y <- which(is.na(y))
  list(na.x, na.y)  
}
both_na_b(x,y)

  #6
is_directory <- function(x) file.info(x)$isdir
  #the function gives an info whether or not a particular path is a directory
  #Using this function has 2 benefits over just using file.info("path")$isdir
    #a) the name of the function is clearer which makes the code more readable
    #b) it just returns the information we're interested in
is_directory("C:/Users/psymo/OneDrive/Studium/Statistik/Daten/College.csv")

is_readable <- function(x) file.access(x, 4) == 0
  #the function gives an info whether or not a particular path is readable
  #Using this function has 2 benefits over using file.access(x, 4) == 0
    #a) It's more readable
    #b) It's reasonably faster to write the command line
is_readable("C:/Users/psymo/OneDrive/Studium/Statistik/Daten/College.csv")

  #7
    #TBD



# 19.3.1 ------------------------------------------------------------------

  #1
has_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
    #f1 checks each element of string on having a specfified prefix
    #a better name than f1 would be 'has_prefix'
has_prefix(c("fin_table1", "fin_table2", "metadata"), "fin_")

truncate_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
    #f2 truncates the last element of a vector.
    #a better name than f2 would be 'truncate_last'
truncate_last(1:8); truncate_last(c("here", "I", "am", "Rock", "you", "like", "a", "hurricane"))

count_and_rep <- function(x, y) {
  rep(y, length.out = length(x))
}
    #f3 counts the number of elements of x and returns y that many times
    #a better name than f3 would be 'count_and_rep' (?)
count_and_rep(1:10, "PARTY!")

  #2
    #TBD

  #3
?rnorm
?MASS::mvrnorm
  #rnorm draws random values from a normal distribution
  #mvrnorm draws values from a multivariate normal distribution
  #They could be made more consistent by:
    #having the same argument names (n, mean/mu, sd/Sigma)
    #having the same default argument values(n vs. n = 1 | mean = 0 vs. mu | sd = 0 vs. Sigma)

  #4
    #norm_r and norm_d have the same prefix. By typing 'norm' and then hitting tab, all functions are immediately available
    #rnorm, and dnorm are a little more intuitive names because 'random normal distribution' rolls better off the tongue than 'normal distribution random'


# 19.4.4 ------------------------------------------------------------------

  #1
?`if`
?ifelse
    #if tests a single condition
    #ifelse tests a condition for each element within a vector
x <- -1:4
if(x < 0) {NA} else {sqrt(x)}
ifelse(x < 1, NA, sqrt(x))
sqrt(ifelse(x<1, NA, x))

  #2
greeting <- function() {
  library(lubridate)
  x <- hour(now())
  case_when(x>=18 ~ "Good Evening",
            x>=12 ~ "Good Afternoon",
            x>=0 ~ "Good Morning"
  )
}
greeting()

  #3
fizzbuzz <- function(x) {
  case_when((x %% 5 == 0 & x %% 3 == 0) ~ "fizzbuzz",
            x %% 5 == 0  ~ "buzz",
            x %% 3 == 0  ~ "fizz",
            TRUE         ~ as.character(x))  
}
fizzbuzz(1:50)

  #4
?cut
temp <- seq(-5, 35, by = 5); temp
cut(temp,
    breaks = c(-Inf,    0,     10,     20,     30,  Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"))
      #in order to mimic the behavior of the if-statement with '<' instead of '<=', the right argument can be switched
cut(temp,
    breaks = c(-Inf,    0,     10,     20,     30,  Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"),
    right = FALSE)
    #There are 2 advantages of cut over 'if'
      #a) It's easier to read
      #b) it can process single values as well as vectors containing more than one element

  #5
?switch
    #TBD

  #6
    #TBD


# 19.5.5 ------------------------------------------------------------------

  #1
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters, collapse = "-")
  #it gives an error because collapse is already specified in the function

  #2
library(stringr)
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width / str_length(pad)), "\n", sep = "")
}
rule("Important output")
rule("Title", pad = "-+")
rule("Important", pad = "!.+-=")

  #3
?mean
    #it excludes some extreme values for the calculation. This might be helpful when working with strong outliers or infinite values
mean(c(-Inf, rnorm(20), Inf))   ;  mean(c(-Inf, rnorm(20), Inf), trim = 0.1)

  #4
?cor
    #"Pearson", "Kendall" & "Spearman" reflect different ways of computing the covariance/correlation coefficient
    #If not specified manually, Pearson is chosen as default.
    #The other two are merely written down to show their availability


# Chapter 20 - Vectors

# 20.3.5 ------------------------------------------------------------------

  #1
?is.finite
is.finite(c(0, Inf, -Inf, NA, NaN))
    #is.finite checks just for numbers, i.e. Inf, -Inf, NA & NaN don't are not finite
!is.infinite(c(0, Inf, -Inf, NA, NaN))
    #!is.infinite checks just for infinite values, i.e. NA, NaN & any real number is not infinite

  #2
dplyr::near
    #dplyr::near tests whether the absolute difference between two values is smaller than some threshold
    #The threshold can be specified manually. By default it's eight 0's.

  #3
    #Theory would suggest that both doubles integers and doubles can take an infinite amount of values.
    #However, there probably will be some computational limits both left and right to the decimal point.

  #4
do <- c(1, 1.3, 1.5, 1.7, 2.5)
?as.integer; as.integer(do)  #converts real numbers truncating everything behind the decimal point
as.integer(round(do)) #using appropriate rounding
ceiling(do)
floor(do)

  #5
str_count(words) %>% str()   #str_count returns an integer vector
                                  #There are others, e.g. str_length & str_locate
str_detect(words, "th") %>% str()  #str_detect returns a logical vector


# 20.4.6 ------------------------------------------------------------------

  #1
x <- c(1:96, NA, Inf, -Inf, NaN)
    #mean(is.na(x)) gives the proportion of NAs (& NaNs) in a vector
mean(is.na(x))
    #sum(!is.finite(x)) returns the total amount of NAs, NaNs, Infs & -Infs
sum(!is.finite(x))

  #2
?is.vector
    #is vector returns FALSE for augmented vectors (like tibbles or data frames)
is.vector(1); is.vector("hi")
is.vector(tibble(1)); is.vector(as.Date("2019-01-01"))
?is.atomic
  #is atomic doesn't agree with the prevailing definition because it returns TRUE for NULL
is.atomic(NULL)

  #3
?setNames
?purrr::set_names
    #set_names returns an error when the vector to name has a different length as the vector of names
setNames(1:3, c("a", "b"))
purrr::set_names(1:3, c("a", "b"))
    #and a lot of other stuff (check https://jrnold.github.io/r4ds-exercise-solutions/vectors.html)

  #4
test <- c(10:1)
    #a)
last <- function(x) { x[length(x)] }
last(test)
    #b)
even_positions <- function(x) {x[seq(2, length(x), by = 2)]}
even_positions(test)
    #c)
drop_last <- function(x) { x[-length(x)]}
drop_last(test)
    #d)
even_values <- function(x) { x[x %% 2 == 0] }
even_values(test)

  #5
test[which(test > 0)]
    #this code returns all elements with a value greater than 0
test[test <= 0]
    #this code returns all elements with a value smaller or equal to 0

  #6
test2 <- c("a" = 1, "b" = 2)
test2[3]
    #subsetting with a higher number than the length of the vector returns NA
test["c"]
    #subsetting with a non-existant name also returns NA



# 20.5.4 ------------------------------------------------------------------

  #1
    #see paper or solution: https://jrnold.github.io/r4ds-exercise-solutions/vectors.html
  #2
diamonds[1]; diamonds["cut"]; diamonds[-3]
    #subsetting with [] also returns a list
diamonds[[1]]; diamonds[["cut"]]
    #subsetting with [[]] removes the tibble property and returns a single vector
diamonds$cut
    #subsetting with does the same as [[]]
      #the key difference between a list and a tibble is that the columns of a tibble must have the same length



# 20.7.4 ------------------------------------------------------------------

  #1
library(lubridate)
hour1 <- hms::hms(3600); hour1
  #it returns 1 hour in the corresponding format
typeof(hour1)
  #it's built on the primitive vector 'double'
attributes(hour1)
  #there is a unit attribute (seconds) which interprets 3600 as 3600 seconds = 60 minutes = 1 hour
  #the classes hms (format) and difftime are associated with this augmented vector

  #2
tibble(c(1:10), c(11:30))
    #There is an error message. Exceptions are single values
tibble(c(1:10), "single value")

  #3
tibble(list(1))


# Chapter 21 - Iteration
library(tidyverse)
  #1
    #a)
str(mtcars)   #numeric columns exclusively

res_a <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  res_a[i] <- mean(mtcars[,i])
  names(mtcars[i])
  
}
(res_a <- set_names(res_a, names(mtcars)))

    #b)
library(nycflights13)
res_b <- vector("character", ncol(flights))
for (i in seq_along(flights)) {
  res_b[i] <- typeof(flights[[i]])
}
(res_b <- set_names(res_b, names(flights)))

    #c)
res_c <- vector("integer", ncol(iris))
for (i in seq_along(iris)) {
  res_c[i] <- length(unique(iris[,i]))
}
(res_c <- set_names(res_c, names(iris)))

    #d)
means <- c(-10, 0, 10, 100)
draws <- 10
res_d <- matrix(nrow = draws, ncol = length(means))
for (i in seq_along(means)) {
  res_d[,i] <- rnorm(draws, means[i])
}
res_d

  #2
    #a)
library(stringr)
out <- ""
for (x in letters) {
  out
  <- stringr::str_c(out, x)
}
out
    #use str_c with the collapse argument
str_c(letters, collapse = "")

    #b)
x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd
    #use sd or manual vector computation instead
sd(x)
sqrt(sum((x - mean(x))^2) * (1/(length(x) - 1)))

    #c)
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out
    #use cumsum instead
cumsum(x)

  #3
    #TBD

  #4
x <- rnorm(1e+5)

    #concatenating the output vector 
output <- vector("integer", 0)
library(lubridate)
start_c <- now();   for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}; end_c <- now()
(diff_c <- end_c - start_c)
length(output); head(output)

    #overwriting elements of a fully prepared output vector
output2 <- vector("integer", length(x))
start_v <- now();   for (i in seq_along(x)) {
  output2[i] <- lengths(x[[i]])
}; end_v <- now()
(diff_v <- end_v - start_v)
length(output2); head(output2)

    #difference between both approaches
diff_c - diff_v



# 21.3.5 ------------------------------------------------------------------

  #1
library(tidyverse); library(stringr)
  #Identify all files in a certain directory
files <- dir("C:/Users/psymo/OneDrive/Studium/Statistik/Daten/", pattern = "^table", full.names = TRUE)
 
 #Read all identified files
res <- vector("list", length(files))  #1. prepare output
for (i in seq_along(res)) {           #2. define sequence
  res[[i]] <- read_csv(files[[i]])    #3. execute body: read files
}


all_names <- vector("list", length(res))    #1. prepare output: all available names
for(i in seq_along(res)) {                  #2.define sequence
                                            #3. execute body: rename 'Samplesize' and extract column names
#rename the column 'Samplesize' so that it matches across dataframes
  names(res[[i]]) <- str_replace(names(res[[i]]), "\\[.*\\]", "")
#extract all the column names and atatch them to the list
  all_names[[i]] <- str_c(names(res[[i]]))
}
  #get unique colun names across all df's
unique_names <- unique(unlist(all_names))

for(i in seq_along(res)) {
    #check what column names are not available in the specific df
  missing_columns <- setdiff(unique_names, names(res[[i]]))
    #add missing columns to specific df and fill them with NA
  res[[i]][,missing_columns] <- NA_character_
}
    #union all single tables into a single tibble
data <- Reduce(dplyr::union_all, res)
data

  #2
demo1 <- list(c(3,4), c(4,5))
demo2 <- list(a = c(3,4), c(4,5))
demo3 <- list(a = c(3,4), c(8,9))

for (nm in names(demo1)) {
  print(demo1[[nm]])
}
  #if the object has no names, nothing will happen
for (nm in names(demo2)) {
  print(demo2[[nm]])
}
  #if not every object has names, the loop functions properly for named elements and returns NULL for non-names ones
for (nm in names(demo3)) {
  print(demo3[[nm]])
}
  #if some elements have the same name, the loop functions properly for the first and treats the second as not named

  #3
head(iris)
x <- iris[2]
df <- iris

show_mean <- function(df) {
  max_str_length <- max(str_length(names(df)))
  for (i in seq_along(df)) {
    x <- df[i]
    var_name <- names(x)
    
    if (is.numeric(x[,1])) {
      whitespace <- str_pad(": ", width = (max_str_length - str_length(var_name) + 2), side = "right")
      str_mean <- as.character(round(mean(x[,1]), 2))
      cat(str_c(var_name, whitespace, str_mean, sep = ""), "\n")
    }
  }
}
show_mean(iris)

  #4
trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
mtcars



# 21.4.1 ------------------------------------------------------------------

  #1
?apply
  #for the 2d case (dataframes/tibbles), apply generalises apllying functions to each row or column
demo_matrix <- matrix(c(rnorm(10, -10), rnorm(10), rnorm(10,10)), nrow = 10)

    #a)calculating the mean over rows works by..
      #i) either using a for loop
res <- vector("double", nrow(demo_matrix))
for (i  in seq_along(res)) {
  res[i] <- mean(demo_matrix[i,])
}
res
      #ii) or the apply function with MARGIN = 1
apply(demo_matrix, MARGIN = 1, FUN = mean)

    #b) calculating the mean over columns works by
      #i) either using a for loop
res <- vector("double", ncol(demo_matrix))
for (i in seq_along(res)) {
  res[i] <- mean(demo_matrix[,i])
}
res
      #ii) or the apply function with MARGIN = 2
apply(demo_matrix, MARGIN = 2, FUN = mean)

  #2
demo_tibble <- tibble(a = rnorm(10,-10), b = rnorm(10), c = rnorm(10,10))

col_summary <- function(df, fun) {
  numerics <- unlist(lapply(df, is.numeric))
  df <- df[numerics]
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  names(out) <- names(df)
  out
}

col_summary(demo_tibble, fun = mean)
col_summary(iris, fun = median)
col_summary(mpg, fun = max)




# 21.5.3 ------------------------------------------------------------------

  #1
    #a)
map_dbl(mtcars, mean)
    #b)
library(nycflights13)
map_chr(flights, typeof)
    #c)
iris %>%
  map(unique) %>%
  map_int(length)
    #d)
c(-10, 0, 10, 100) %>%
  map(~rnorm(10, .)) 

  #2
demo_factor <- tibble(f = factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef")),
       b = 1:3,
       c = str_c(3:1, " check"))

map_lgl(demo_factor, is.factor)
map_lgl(iris, is.factor)

  #3
    #applying map to vectors that aren't lists will apply the specified function to every element within the vector
map(1:5, runif)
    #here runif takes every element of 1:5 and uses is at it's first argument.
    #An alternative approach could be
map_dbl(1:5, ~runif(1, min = ., max = (.+1)))

  #4
map(-2:2, rnorm, n = 5)
    #This map randomly draws five times from the normal distribution for each mapping
    #Every element of the vector -2:2 is passed to the mean-argument (n is already specified: 5)
map_dbl(-2:2, rnorm, n = 5)
    #This map_dbl tries the same as the above map. But it can't return hierarchical elements and thus,
      #throws an error.

  #5
mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

mtcars %>% 
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))



x <- list(1, "a", 3)




mu <- c(5, 10, -3)
sigma <- c(1,2,3)

seq_along(mu) %>%
  map(~rnorm(10, mean = mu[[.]] , sd = sigma[[.]] )) %>%
  str()

map2(mu, sigma, rnorm, n = 10) %>% str()

n <- c(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()



dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)



# 21.9.3 ------------------------------------------------------------------

  #1
x <- list(1:5, letters, list(10))   #to test the functions behavior compared to purrr's 'every'

every2 <- function(x, FUN) {
  res <- vector("logical", length(x))
  for (i in seq_along(x)) {
    res[[i]] <- FUN(x[[i]])
  }
  res %>%
    mean() %>%
    floor() %>%
    as.logical()
}

every2(x, is.vector); every(x, is.vector)
every2(x, is.list); every(x, is.list)
?every
    #every takes more alternatives at the second argument (e.g. a logical vector) and passes further arguments to the function

  #2

col_summary <- function(df, fun) {
  df <- keep(df, map_lgl(df, is.numeric))   #this line is added to throw out all non numeric vectors
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(iris, mean)



  #3
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)
