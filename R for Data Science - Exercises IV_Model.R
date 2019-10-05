#Hierarchy (in order to ease searching via Strg + f):

# Part I - xxx
# Chapter 1 - Introduction

# 1.1.3 -------------------------------------------------------------------



# Part IV - Model
# Chapter 23 - Model basics
library(tidyverse); library(modelr); options(na.action = na.warn)


# 23.2.1 ------------------------------------------------------------------

  #1 + 2
n <- 6

dgp <- function(i) {
tibble(x = rep(1:10, each = 3),
       y = x * 1.5 + 6 + rt(length(x), df = 2),
       run = i
    )
}
res_raw <- map_df(1:n, dgp)

res <- res_raw %>%
  split(res_raw$run)

lad <- function(a, data) {
  pred <- a[1] + data$x * a[2]
  diff <- data$y - pred
  mean(abs(diff))
}

lad_res <- res %>%
  map_df(.f = ~optim(par = c(0,0), fn = lad, data = .)$par) %>%
  t() %>%
  as.tibble() %>%
  rename(Intercept = V1, Slope = V2) %>%
  mutate(run = 1:n,
         model = "LAD")

ols_res<- res %>%
  map_df(.f = ~lm(y ~ x, data = .)$coef) %>%
  t() %>%
  as.tibble() %>%
  rename(Intercept = V1, Slope = V2) %>%
  mutate(run = 1:n,
         model = "OLS")

union_all(lad_res, ols_res) %>%
  ggplot() +
  geom_point(data = res_raw,aes(x, y)) +
  geom_abline(aes(intercept = Intercept, slope = Slope, col = model)) +
  facet_wrap(~ run)


  #3
model1 <- function(a, data) {
  a[1] + data$x * a[2] +a[3]
}
optim(0:2, sim1)
    #There are 2 problems:
      #1. optim only wants single values (or one value for every data point).
        #-in order to account for that, we should rewrite the model
model2 <- function(a, data) {
  pred <- a[1] + data$x * a[2] + a[3]
  sqrt(mean((data - pred)^2))
}
model2(0:2, sim1)
optim(1:3, model2, data = sim1)$par
optim(c(0,0,0), model2, data = sim1)$par
    #The model function now returns a single value and can be optimised
      #2. However, the model is still problematic because a[1] and a[3] represent the same thing: the intercept
      #Thus any combination of a[1] and a[3] that (roughly) equals the intercept is valid.
      #As can be seen above, given sifferent starting points, the parameter results differ
      #To combat this, I would exclude a[3] from the formula
model3 <- function(a, data) {
  pred <- a[1] + data$x * a[2]
  sqrt(mean((data - pred)^2))
}
optim(1:2, model3, data = sim1)$par
optim(c(0,0), model3, data = sim1)$par1
      #Now the result is the same for different starting parameters



# 23.3.3 ------------------------------------------------------------------

  #1
sim1.loess <- loess(y ~ x, data = sim1)


sim1.enh <- sim1 %>%
  add_predictions(sim1.loess) %>%
  add_residuals(sim1.loess)

ggplot(data = sim1.enh, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), col = "red", size = 1)

ggplot(sim1.enh, aes(x, y)) +
  geom_point() +
  geom_smooth(se = F, col = "red", size = 1)

    #geom_smooth does the same as geom_line with loess()-generated predictions.
    #By default, it also includes a confidence interval

ggplot(sim1.enh, aes(resid)) + geom_freqpoly(binwidth = 0.5)
ggplot(sim1.enh, aes(x = x, y = resid)) + geom_ref_line(h = 0, col = "red") + geom_point() 

    #Overall, loess and lm are very similar. That indicates that the data is in fact rather linear.

  #2
?add_predictions
    #spread_residuals & gather_residuals return predictions for more than one model
      #spread_residuals adds one column for each model
      #gather_residuals adds a column to indicate which model is used and concatenates the models row-wise
sim1.lm <- lm(y ~ x, data = sim1)

spread_predictions(sim1, sim1.lm, sim1.loess)
gather_predictions(sim1, sim1.lm, sim1.loess)
      #This is useful to visualise the differences between models
gather_predictions(sim1, sim1.lm, sim1.loess) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = pred, col = model), size = 1) +
  geom_point(aes(y = y))
      #The same works for residuals with spread_residuals & gather_residuals
?add_residuals

  #3
?geom_ref_line
    #geom_ref_line comes from the modelr package. It adds a reference line 
gather_residuals(sim1, sim1.lm, sim1.loess) %>%
  ggplot(aes(x = x, y = resid)) +
  geom_ref_line(h = 0, col = "blue") +
  geom_point() +
  facet_wrap(~model)

  #4
gather_residuals(sim1, sim1.lm, sim1.loess) %>%
  ggplot(aes(x  = abs(resid), color = model)) +
  geom_freqpoly(binwidth = 0.5) + theme_bw()
    #looking at absolute residuals gives a better feeling of the extremeness of the residuals
gather_residuals(sim1, sim1.lm, sim1.loess) %>%
  ggplot(aes(x  = resid, color = model)) +
  geom_freqpoly(binwidth = 0.5) + theme_bw()
    #looking at normal residuals shows better in what direction predictions are wrong



# 23.4.5 ------------------------------------------------------------------

  #1
sim2.lm <- lm(y ~ x, data = sim2); coef(sim2.lm)
sim2.no_intercept <- lm(y ~ 0 + x, data = sim2); coef(sim2.no_intercept)
    #On first glance, the coefficients seem to differ between models.
    #However, they are in fact the same.
      #The intercept in the original model captures the baseline case a.
        #In the second model, the intercept is replaced by a variable indicating affiliation to group a.
        #They are necessarily equal
      #As for b, c & d, the have fully valid coefficients in the second model.
        #In the first model, you need to add the baseline (intercept) in order to get the best guess for a particular group
near(coef(sim2.lm)[2:4] + coef(sim2.lm) [1], coef(sim2.no_intercept)[2:4])

model_matrix(sim2, y ~ x)
model_matrix(sim2, y ~ 0 +x)
    #naturally, the second model doesn't have an intercept anymore
    #It is replaced by a variable indicating affiliation to group a (which before would have just been the baseline)

pred <- sim2 %>%
  data_grid(x) %>%
  gather_predictions(sim2.lm, sim2.no_intercept)
ggplot() +
  geom_point(data = pred, aes(x = x, y = pred, col = model), size = 5) +
  geom_point(data = sim2, aes(x = x, y = y)) +
  facet_wrap( ~ model)
    #the predictions are the same among models.


  #2
    #a)  sim3
model_matrix(sim3, y ~ x1 + x2)
      #the additive model includes 5 columns: 1 for the intercept, 1 for x1 and 3 for the factor x2
model_matrix(sim3, y ~ x1 * x2)
      #the multiplicative model includes 8 columns. All of the 5 above and 3 additional ones that capture any interaction between x1 and x2
        #Using x1 * x2 is useful because it's a reliable way to involve all interactions easily
    #b) sim4
model_matrix(sim4, y ~ x1 + x2)
      #the additive model includes 3 columns: 1 for the intercept and 1 for x1 and x2 respectively
model_matrix(sim4, y ~ x1 * x2)
      #The multiplicative model adds the interaction between x1 and x2 resulting in 4 total columns.

  #3
model_matrix_sim3a <- function(data) {
  data %>%
    mutate(Intercept = 1,
           b = as.numeric(x2 == "b"),
           c = as.numeric(x2 == "c"),
           d = as.numeric(x2 == "d")) %>%
    select(Intercept, x1, b, c, d)
}
model_matrix_sim3a(sim3)
model_matrix(sim3, y ~ x1 + x2)

model_matrix_sim3b <- function(data) {
  data %>%
    mutate(Intercept = 1,
           b = as.numeric(x2 == "b"),
           c = as.numeric(x2 == "c"),
           d = as.numeric(x2 == "d"),
           x1b = x1 * b,
           x1c = x1 * c,
           x1d = x1 * d) %>%
    select(Intercept, x1, b, c, d, x1b, x1c, x1d)
}
model_matrix_sim3b(sim3)
model_matrix(sim3, y ~ x1 * x2)

  #4
sim4.lin <- lm(y ~ x1 + x2, data = sim4)
sim4.sq <- lm(y ~ x1 * x2, data = sim4)

sim4 %>%
  gather_predictions(sim4.lin, sim4.sq) %>%
  ggplot(aes(x = x1, y = x2, fill = pred)) +
  geom_tile() +
  facet_wrap(~ model)
    #this doesn't really show a difference

sim4 %>%
  gather_residuals(sim4.lin, sim4.sq) %>%
  ggplot(aes(x = abs(resid), col = model)) +
  geom_freqpoly(binwidth = 0.2)
    #same...


# Chapter 24 - Model building
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)
library(hexbin)


# 24.2.3 ------------------------------------------------------------------

  #1
diamonds2 <- diamonds %>%
  filter(carat < 2.5) %>%
  mutate(lprice = log2(price),
         lcarat = log2(carat))

ggplot(diamonds2, aes(x = lcarat, y = lprice)) + geom_hex(bins = 50)
    #The stripes represent common combinations of carat and price. Since they are denoted as logs
    #they should be re-calculated, e.g. stripe at lcarat = 0, and lprice between 10 & 13
c(2^0 , 2^10, 2^13)
      #A frequent combination is a 1 carater that costs between 1000 and 8200


  #2
    #It says that the relationship between the two variables is not necessarily linear.
    #It can be confirmed by looking at the predictions and the scattered data
reg_diamonds_complex <- lm(lprice ~ lcarat, diamonds2)

diamonds2 %>%
  add_predictions(reg_diamonds) %>%
  mutate(pred = 2 ^ pred) %>%
  ggplot(aes(x = carat)) +
  geom_hex(aes(y = price), bins = 50) +
  geom_line(aes(y = pred), color = "red", size = 1.5)
    #Apparently diamond prices stronger than linear when carat increases.

diamonds_adjusted <- diamonds2 %>%
  add_residuals(reg_diamonds) %>%
  mutate(abs_resid = abs(resid),
         outlier = percent_rank(abs_resid),
         pos_resid = (resid > 0 ))

diamonds_filtered <- diamonds_adjusted %>%
  filter(outlier > 0.95)
ggplot(diamonds_filtered, aes(x = color, y = price)) + geom_boxplot() +
  facet_wrap( ~ pos_resid)
      #As regards color, diamonds that are pricier than the predicted value only have medium to high quality colors
ggplot(diamonds_filtered, aes(x = cut, y = price)) + geom_boxplot() +
  facet_wrap( ~ pos_resid)
ggplot(diamonds_filtered, aes(x = clarity, y = price)) + geom_boxplot() +
  facet_wrap( ~ pos_resid)
      #The trend with respect to clarity is even more extreme


reg_diamonds_complex <- lm(lprice ~ lcarat + color + cut + clarity, diamonds2)

diamonds2 %>%
  add_residuals(reg_diamonds_complex) %>%
  mutate(abs_resid = abs(resid),
         outlier = percent_rank(abs_resid),
         pos_resid = (resid > 0 )) %>%
  filter(outlier > 0.95) %>%
ggplot(aes(x = clarity, y = price)) + geom_boxplot() +
  facet_wrap( ~ pos_resid)
    #Even after accounting for color, cut & clarity in an additive way, there are still some patterns
    #This could be combated by including interaction terms

  #4
diamonds2 %>%
  add_residuals(reg_diamonds_complex) %>%
  ggplot(aes(lcarat, resid)) + geom_hex(bins = 50)

  #the model does a decent job predicting prices. However, the image suggests that there a a few
  #instances where the price is overpredicted or underpredicted quite strongly
price_corridor <- log2(c(0.9, 1.1)) / log2(2)
  #if a price corridor of 10% would be acceptable, that would be reflected by the above residuals
  #residuals outside this range would be 'inacceptable'

diamonds2 %>%
  add_residuals(reg_diamonds_complex) %>%
  mutate(acceptable = between(resid, price_corridor[1], price_corridor[2])) %>%
  group_by(acceptable) %>%
  summarise(n = n())
    #more than 50% of diamonds are in the defined price corridor

  #what is the range 95% of the diamonds fall into
bounds <- diamonds2 %>%
  add_residuals(reg_diamonds_complex) %>%
  mutate(perc = percent_rank(resid)) %>%
  filter(perc > 0.025, perc < 0.975) %>%
  summarise(max = max(resid),
            min = min(resid))

2 ^ bounds
    #95% of diamonds fall into a ~25-30% price corridor


# 24.3.5 ------------------------------------------------------------------
library(MASS); library(tidyverse); library(splines); library(nycflights13); library(lubridate); library(modelr)
  #0 - Preparation
    #Prepare dataframe    
daily <- flights %>%
  transmute(date = make_date(year, month, day)) %>%
  count(date) %>%
  mutate(wday = wday(date, label = T),
         term = cut(date,
                    breaks = as.Date(c("2013-01-01", "2013-02-15", "2013-06-05", "2013-08-25", "2014-01-01")),
                    labels = c("winter", "spring", "summer", "fall")))
    #Execute all models
daily.mod <- lm(n ~ wday, data = daily)
daily.mod2 <- lm(n ~ wday * term, data = daily)
daily.rlm <- MASS::rlm(n ~ wday * term, data = daily)
daily.splines <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

  #1
daily %>%
  spread_residuals(daily.mod2, daily.rlm, daily.splines) %>%
  filter( (abs(daily.mod2) + abs(daily.rlm) + abs(daily.splines))/3 > 100)
    #in fact, given the 3 more enhanced models, Jan 20 isn't that surprising.
    #Still, May 26 & Sep 1 remain heavy outliers
      #Googling the days reveals one commonality: The days are all Sundays and are followed by a holiday
        #Jan 20 was a Sunday and was followed by a holiday: Jan 21 - Marthin Luther King Jr. Day
        #May 26 was a Sunday and was followed by a holiday: May 27 - Memorial Day
        #Sep 01 was a Sunday and was followed by a holiday: Sep 02 - Day of Work

    #To account for this in future years, one could filter out Sundays that are followed
    #by holidays

  #2
daily %>%
  add_residuals(daily.mod) %>%
  top_n(3, resid)
    #These are the weekends after Thanksgiving & Christmas

  #3
wdt <- function(wd, t) {
  if        (wd == "Sat" & t == "winter") {
    "Sat - winter"
  } else if (wd == "Sat" & t == "spring") {
    "Sat - spring"
  } else if (wd == "Sat" & t == "summer") {
    "Sat - summer"
  } else if (wd == "Sat" & t == "fall") {
    "Sat - fall"
  } else {as.character(wd)}
}
daily <- daily %>%
  mutate(specific = as.factor(map2_chr(wday, term, wdt)))

daily_specific <- lm(n ~ specific, data = daily)

daily %>%
  gather_predictions(daily.mod, daily_specific, daily.mod2) %>%
  ggplot(aes(x = wday)) +
  geom_boxplot(aes(y = n)) + 
  geom_point(aes(y = pred), col = "red", size = 3) +
  facet_grid(model ~ term)
    #as this graph shows, the specific model (just created)
      #fares bettern than the naive first model without terms ONLY in predicting Saturdays
      #is worse than the model with every combination of wday and term except for Saturdays (where they are even)
daily %>%
  gather_residuals(daily.mod, daily_specific, daily.mod2) %>%
  ggplot(aes(date, abs(resid), col = model)) + geom_line(alpha = 0.6)
#the above argument is supported by the plotted residuals

  #4
    #TBD

  #5
daily <- daily %>%
  mutate(month = month(date, label = TRUE))

daily_wday_month <- lm(n ~ wday * month, data = daily)

daily %>%
  gather_predictions(daily_wday_month, daily.mod2) %>%
  ggplot(aes(x = wday)) +
  geom_boxplot(aes(y = n)) + 
  geom_point(aes(y = pred), col = "red", size = 3) +
  facet_grid(model ~ month)

daily %>%
  gather_residuals(daily_wday_month, daily.mod2) %>%
  ggplot(aes(date, abs(resid), col = model)) + geom_line(alpha = 0.6)

    #Both graphs suggest that there are some data, each respective model fits better
    #The main argument against the more complex model is that it might suffer from overfitting.

  #6
daily_splines <- lm(n ~ wday + ns(date, 5), data = daily)

daily %>%
  gather_residuals(daily_splines, daily.mod2) %>%
  ggplot(aes(date, abs(resid), col = model)) + geom_line(alpha = 0.6) + theme_bw()
    #The splines model is worse. One reason is that it seems to struggle to handel the weekday pattern.
    #This is probably because it tries to account for these with by adjusting the highly flexible date variable


  #7
library(viridis)
flights %>%
  mutate(wday = wday(time_hour, label = TRUE)) %>%
  group_by(wday, hour) %>%
  summarise(avg_dist = mean(distance)) %>%
  ggplot() +
  geom_tile(aes(x = wday, y = hour, fill = avg_dist)) +
  scale_fill_viridis()
    #This plot reveals some interesting patterns:
      #1. not relevant to the task: but the average distance varies along a day:
        #flights leaving at 24 are on average very far (might be influenced by outliers)
        #flights around noon (11 - 16) seem to be shorter.
      #2 relevant to the task: flights at Saturday and Sunday evening seem to be a little longer
        #than during the rest of the week (very weak relationship though)

    #This plot filters out flights after 22:00
flights %>%
  mutate(wday = wday(time_hour, label = TRUE)) %>%
  group_by(wday, hour) %>%
  summarise(avg_dist = mean(distance)) %>%
  filter(hour < 22, hour > 3) %>%
  ggplot() +
  geom_tile(aes(x = wday, y = hour, fill = avg_dist)) +
  scale_fill_viridis()

  #8
library(forcats)

montosun <- function(x) {
  fct_relevel(x, levels(x)[-1])
}
    #applied to graph from previous exercise
flights %>%
  mutate(wday = montosun(wday(time_hour, label = TRUE))) %>%
  group_by(wday, hour) %>%
  summarise(avg_dist = mean(distance)) %>%
  filter(hour < 22, hour > 3) %>%
  ggplot() +
  geom_tile(aes(x = wday, y = hour, fill = avg_dist)) +
  scale_fill_viridis()



# Chapter 25 - Many models
library(modelr); library(tidyverse); library(gapminder); library(broom)


# 25.2.5 ------------------------------------------------------------------

  #1
by_country <- gapminder %>%
  mutate(year2 = year - mean(year)) %>%
  group_by(country, continent) %>% 
  nest()

slm <- function(df) {
  lm(lifeExp ~ year2, data = df)
}

elm <- function(df) {
  lm(lifeExp ~ poly(year2, 2), data = df)
}

(by_country <- by_country %>%
    mutate(slm_model = map(data, slm),
           elm_model = map(data, elm),
           resid = pmap(.l = list(data, slm_model, elm_model), .f = gather_residuals),
           pred =  pmap(.l = list(data, slm_model, elm_model), .f = gather_predictions),
           slm_acc =   map(slm_model, broom::glance),
           elm_acc =   map(elm_model, broom::glance)))

(slm_glanced <- unnest(by_country, slm_acc, .drop = TRUE) %>%
    arrange(r.squared))

bad_fits <- slm_glanced %>%
  filter(r.squared < 0.2)

preds <- unnest(by_country, pred)
resids <- unnest(by_country, resid)

inner_join(gapminder, bad_fits, by = "country") %>%
  left_join(preds, by = c("country", "year")) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = pred, col = model), lty = 2) +
  geom_line(aes(y = lifeExp.x)) +
  facet_wrap(~ country)
    #visually, the quadratic model captures the development in 5 out of the 6 extraordinary countries better
    #Only the genocide in Rwanda can't be captured properly by the quadratic model.
      #There is virtually no difference between the 2 models for Rwanda

resids %>%
  filter(abs(resid) < 8) %>%
  ggplot(aes(resid, col = model)) +
  geom_freqpoly(binwidth = 0.1)
    #futhermore, the quadratic model seems to have lower tails when it comes to the distribution of residuals
resids %>%
  mutate(modelcountry = paste(model, country)) %>%
  ggplot(aes(x = year, y = resid, group = modelcountry, col = model)) +
  geom_ref_line(h = 0) + 
  geom_line(alpha = 0.5) +
  facet_wrap(~ continent)
    #the residuals seem to be lower especially in Africa and Asia
resids %>%
  group_by(continent, model, year) %>%
  summarise(med_resid = median(resid)) %>%
  ggplot(aes(x = year, y = med_resid, col = model)) +
  geom_ref_line(h = 0) + 
  geom_line() +
  facet_wrap(~ continent)
    #the median residual is much more well-balanced
      #In summary, the quadratic model seems to fit the data better, especially in Africa and Asia


  #2
elm_glanced <- unnest(by_country, elm_acc, .drop = TRUE)

elm_glanced %>%
  ggplot(aes(x = continent, y = r.squared)) + geom_jitter()
    #this shoes that the r squared is still very bad for one country. The one point represents Rwanda
arrange(elm_glanced, r.squared)

library(ggbeeswarm)
elm_glanced %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()

elm_glanced %>%
  ggplot(aes(r.squared, col = continent)) + geom_freqpoly(binwidth = 0.05, alpha = 0.5)

elm_glanced %>%
  mutate(r_sq = cut(r.squared,
                 breaks = c(0, 0.5, 0.9, 0.95, 0.99, 1),
                 labels = c("bad", "fair", "good", "very good", "excellent"))) %>%
  ggplot(aes(x = continent, fill = r_sq)) + geom_bar(position = "dodge")

  #3
unnest(by_country, slm_acc) %>%
  filter(r.squared < 0.2) %>%
  unnest(data) %>%
  ggplot(aes(year, lifeExp, col = country)) + geom_line()
    #now applied to quadratic model
unnest(by_country, elm_acc) %>%
  filter(min_rank(r.squared) <= 5) %>%
  unnest(data) %>%
  ggplot(aes(year, lifeExp, col = country)) + geom_line()



# 25.4.5 ------------------------------------------------------------------

  #1
library(stringr)
head(str_split(sentences, " "))
head(str_extract_all(sentences, "s"))
head(str_locate_all(sentences, "s"))
head(str_match_all(sentences, "s"))
map(mpg, summary) %>%
  enframe()

  #2
quantile(mpg$hwy)
summary(mpg$hwy)
range(mpg$hwy)

  #3
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>% 
  unnest()
    #the specified quantile is missing. quantile() usually returns it as name
quantile(mtcars$mpg) %>%
  names()
    #That's not helpful here because the names are lost when unnesting

  #4
mtcars %>% 
  group_by(cyl) %>% 
  summarise_each(funs(list))
    #The code returns a dataframe with one column specifying the cylinders and
    #other columns containing lists of summary information for each variable



# 25.5.3 ------------------------------------------------------------------

  #1
    #might e.g. be useful to determine where an unusual amount of errors has occured

  #2
    #a typical dataframe usually contains 
      #a lot of doubles and integers (some augmented to factors)
      #some logicals, characters and dates/times
    #lists are different because they can contain any combination of vector types


gapminder %>%
  ggplot(aes(x = year, y = lifeExp, group = country)) + geom_line()

nz <- filter(gapminder, country == "New Zealand")
nz.mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz.mod) %>%
  ggplot(aes(year)) +
  geom_point(aes(y = lifeExp)) +
  geom_line(aes(y = pred))

nz %>%
  add_residuals(nz.mod) %>%
  ggplot(aes(x = year, y = resid)) + geom_ref_line(h = 0) + geom_line()

broom::glance(nz.mod)





unnest(by_country, slm_resid) %>%
  ggplot(aes(x = year, y = resid)) +
  geom_line(aes(group = country), alpha = 0.3) +
  #geom_smooth(se = F) +
  facet_wrap(~ continent)

(glanced <- unnest(by_country, slm_acc, .drop = TRUE) %>%
  arrange(r.squared))

glanced %>%
  ggplot(aes(x = r.squared, col = continent)) + geom_freqpoly(binwidth = 0.1)

glanced %>%
  ggplot(aes(x = continent, y = r.squared)) + geom_jitter(width = 0.5)

bad_fits <- glanced %>%
  filter(r.squared < 0.2)

preds <- unnest(by_country, slm_pred)



gapminder %>%
  group_by(country, continent) %>% 
  nest()

gapminder %>%
  nest(year, lifeExp, pop, gdpPercap)
library(stringr)
sentences %>%
  as_tibble() %>%
  mutate(x2 = str_split(value, pattern = " ")) %>%
  unnest

probs <- seq(0, 1, by = 0.2)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs),
            q = list(summary(mpg, probs = probs )))



df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type = map_chr(x, typeof))

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest()
