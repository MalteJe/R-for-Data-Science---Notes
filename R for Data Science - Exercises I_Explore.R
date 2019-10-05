#Hierarchy (in order to ease searching via Strg + f):

# Part I - xxx
# Chapter 1 - Introduction
# 1.1.3


# Part I - Explore
# Chapter 3 - Data Visualisation
library(tidyverse)
#3.2.4
  #1
ggplot(data = mpg)
  #2
dim(mpg)
  #3
?mpg  #drv is a factor specifying the type of wheel drive
  #4
ggplot(data = mpg) +
  geom_point(aes(x = cyl, y = hwy))
  #5
ggplot(data = mpg) +
  geom_point(aes(x = drv, y = class))
    #graph is not useful because points are stacked on top of each other. It only shows
    #which combinations of wheel drive type and class exist and don't exist.

#3.3.1
  #1
ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy, col = "blue")) #blue is not a variable and thus, can't
    #be mapped to an aesthetic. Instead, the colour should be specified outside aes()
  #2
str(mpg) #manufacturer, model, (year), (cyl), trans, drv, fl and class are categorical
  #3
ggplot(data = mpg) +
  geom_point(aes(x = cyl, y = hwy, col = cty, size = cty))
    #colour scale, and size scale are allowed
    #continuous variables can't be mapped to shape
  #4
ggplot(data = mpg) +
  geom_point(aes(x = cyl, y = hwy, col = cty, size = cty))
    #both mappings will be shown in the graph. However, it's not really sensible to do that
  #5
ggplot(data = mpg) +
  geom_point(aes(x = cty, y = hwy, stroke = cyl), shape = 21)
    #not very sensible
  #6
ggplot(data = mpg) +
  geom_point(aes(x = cyl, y = hwy, col = cty >20))
    #ggplot creates a binary variable that is mapped to the corresponding aesthetic

#3.5.1
  #1
ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~cty)

ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~cyl)
  #a single plot for every single value of the continuous variable is created
  #here, the second plot might be sensible, the first isn't

  #2
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl)) +
  facet_grid(drv ~ cyl)
    #the empy plots represent combinations of wheel drive type and cylinders that don't exist in the dataset
  #3
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
    #creates three facets on top of each other
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
    #creates four facets next to each other
      #the '.' acts as factor with only a single occurence (so only a single row / column is created in the graph)
  #4
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
    #advantage of a faceted plot: enhanced analysis of single class is easier
    #disadvantage of a faceted plot: comparisons among classes are more difficult
    #the more observations are added to a dataset, the harder it gets to distinguish
    #different colours in a color-separated single plot
      #--> the higher the number of observations: the more advantagous facetting becomes
      #--> the higher the number of categories:   the more advantagous facetting becomes
  #5
?facet_wrap
    #nrow and ncol determine the number of rows/columns in the final plot
    #facet_gird doesn't have that option because the rows and numbers are determined by the inputted variables
    #other options include e.g. harmonized scaling of the axes
  #6
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)
    #the canvas is in a typical wide format.
    #Thus, there is more horizontal space to place the variable with more levels

#3.6.1
  #1
    #Either a smoothing line (geom_smooth) or an actual line linking the data points (geom_line)
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_line()
    #Typically, lines are useful just for time series data
  #2
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
  #3
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(show.legend = F)
    #removes the legend from the right corner
  #4
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = TRUE)
?geom_smooth
    #it gives confidence intervals based on standard errors ('se')
  #5
    #the graphs won't be different because the mappings are equal.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()
    #in the first case the global mappings are distributed to the local geoms
ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
    #in the second case the mappings are specified locally. 
  #6
    #a
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = F)
    #b
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = F)
    #c
ggplot(mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point() +
  geom_smooth(se = F)
    #d
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(col = drv)) +
  geom_smooth(se = F)
    #e
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(col = drv)) +
  geom_smooth(aes(lty = drv), se = F)
    #f
ggplot(mpg, aes(x = displ, y = hwy, fill = drv)) +
  geom_point(color = "white", shape = 21, size = 2.5, stroke = 2.5)

#3.7.1
  #1
?stat_summary
    #the default geom is 'pointrange'
?geom_pointrange
ggplot(data = diamonds) +
  geom_pointrange(aes(x = cut, y = depth), stat = "summary",
                  fun.ymin = min, fun.ymax = max, fun.y =median)
  #2
?geom_col
    #geom_col doesn't count observations but uses stat "identity" to take values directly
    #from a column. Thus, requires specifying a y aesthetic
      #main difference: uses stat "identity" instead of "count"
datatemp <- data.frame(quality = c("Nice", "very Nice", "Bad"), y = c(20,30,40))
ggplot(data =datatemp) +
  geom_col(aes(x = quality, y = y))
  #3
    #https://jrnold.github.io/r4ds-exercise-solutions/data-visualisation.html
  #4
?stat_smooth
    #stat smooth is essentially a prediction function and computes
      #a predicted value y for every x
      #a minimum and maximum value given a particular confidence level
      #standard errors (not displayed by default)
ggplot(data = diamonds, aes(x = carat, y = depth)) +
  stat_smooth()
      #control parameters are method, formula and na.rm
  #5
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))
    #by default, 5 groups are assumed (because cut has 5 different values).
    #Thus, the stat 'prop' computes how often e.g. the value 'Fair' occurs in the group 'Fair'
    #Naturally, the occurences of 'fair' in a group filtered for 'fair' is 100%
    #So the groups need to be re-specified to be only 1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
    #here, the proportion of a particular value (e.g. Fair) for the whole dataset is calculated
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..count.. / sum(..count..)))


#3.8.1
  #1
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
    #The graph suffers from overplotting. Single points may very well represent multiple observations
    #To tackle this problem one could
      #use jittering
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0.25, height = 0.25)
      #use an alpha level
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(alpha =0.4, size = 2)
      #do both
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(alpha = 0.4, size = 1.5, width = 0.25, height = 0.25)
  #2
?geom_jitter
    #jittering is controlled by width & height
  #3
?geom_count
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()
    #geom_count adjusts the size of single points on the canvas based on their count
    #as jittering, it tackles the problem of overplotting.
  #4
?geom_boxplot
    #the default position is 'dodge'
ggplot(data = mpg) +
  geom_boxplot(aes(x = drv, y = hwy, col = class)) #each value is plotted exactly at it's exact position

#3.9.1
  #1
ggplot(data = diamonds) +
  geom_bar(aes(x = cut, y = ..count.., fill = cut)) +
  coord_polar()
  #2
?labs
    #labs change or add titles, descriptions and legends to the graph
ggplot(mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(y = "mileage", x = "engine litres",
       title = "fuel efficiency on highways by engine litres grouped by wheel drive",
       subtitle = "data: 'diamonds' from ggplot2-package")
  #3
?coord_quickmap
    #quickmap is a faster, approximated version of map
  #4
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()
    #coord_fixed ensures that the line is 45 degrees


# Chapter 4 - Workflow: basics
#4.4
  #1
    #my_varlable is different from my_variable which was assigned before
  #2
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)
  #3
    #helpbar with shortcuts appears

# Chapter 5 - Data Transformation
library(tidyverse);library(nycflights13)

#5.2.4
  #1
    #a)
filter(flights, arr_delay >=120)
    #b)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, dest %in% c("IAH", "HOU"))
    #c)
airlines
filter(flights, carrier %in% c("UA", "DL", "AA"))
    #d)
filter(flights, month %in% 7:9)
filter(flights, month == 7 | month == 8 | month == 9)
    #e)
filter(flights, dep_delay <= 0 & arr_delay >= 120)
    #f)
filter(flights, dep_delay >= 60 & arr_delay +30 < dep_delay)
    #g)
filter(flights, dep_time <= 600 | dep_time == 2400)
  #2
?between
    #filters for all values between the 2 specified boundaries
filter(flights, between(month, 7,9))
  #3
filter(flights, is.na(dep_time) == T)
    #delay and arrival time are not available. These might represent cancelled flights
  #4
NA^0;is.na(NA^0) #not NA because verything to the power of 0 equals 1
NA | TRUE #???
FALSE & NA #???
NA * 0; is.na(NA * 0) # is NA like most mathematical operations with NAs


#5.3.1
  #1
arrange(flights, desc(is.na(dep_time)), dep_time)
  #2
arrange(flights, desc(arr_delay))
arrange(flights, dep_delay)
  #3
flights %>%
  mutate(ground_speed = distance/air_time) %>%
  arrange(desc(ground_speed))
  #4
arrange(flights, desc(distance))
arrange(flights, distance)


#5.4.1
  #1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, 4, 6, 7, 9)
select(flights, starts_with("dep"), starts_with("arr"))
select(flights, contains("dep_"), contains("arr_"), -(contains("sched")))
select(flights, ends_with("_delay"), ends_with("_time"), -starts_with("sched"), -air_time)
  #2
select(flights, flight, flight)
    #duplicate columns are removed
  #3
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
  #4
select(flights, contains("TIME"))
    # 'contains' is not case sensitive. It can be specified the other way around
select(flights, contains("TIME", ignore.case = FALSE))  #no columns are returned


#5.5.2
  #1
flights %>%
  mutate(dep_time.n = dep_time %/% 100 * 60 + dep_time %% 100) %>%
  mutate(sched_dep_time.n = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %>%
  select(contains("dep_time"), everything())
  #2
flights %>%
  select(contains("time")) %>%
  mutate(air_time.n = arr_time - dep_time)
    #there is a deviation between the actual airtime and the calculated airtime because the calculation doesn't account
    #for the format (100)

flights %>%
  select(contains("time")) %>%
  mutate(dep_time.n = dep_time %/% 100 * 60 + dep_time %% 100) %>%
  mutate(arr_time.n = arr_time %/% 100 * 60 + arr_time %% 100) %>%
  mutate(difference = arr_time.n - dep_time.n)
    #still misbehavior due to difference between 'just flight time' and 'departing from and arriving at parking position'
  #3
(checkdata <- flights %>%
  select(contains("dep")) %>%
  mutate(dep_time.n = dep_time %/% 100 * 60 + dep_time %% 100) %>%
  mutate(sched_dep_time.n = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %>%
  mutate(dep_delay.n = dep_time.n - sched_dep_time.n) %>%
  mutate(equivalent = dep_delay  == dep_delay.n))

filter(checkdata, equivalent == F)
    #seems to work for most cases, but not for all
  #4
?min_rank
flights %>%
  select(contains("dep"), everything()) %>%
  mutate(delay_rank = min_rank(desc(dep_delay))) %>%
  select(delay_rank, everything()) %>%
  arrange(delay_rank)
  #5
1:3 + 1:10
    #1:3 creates a vector of length 3 and 1:10 creates a vector of length 10
    #in order to add them up R repeats the values of the first vector until all values in the second vector have been added with some number
    #one could also calculate the total sum of all numbers in both vectors:
sum(1:3) + sum(1:10)
  #6
?Trig
    #sinus, cosinus, tangens, acosinus, asinus, atangens as well as others are supported

#5.6.7
  #2
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
    #a
not_cancelled %>% count(dest)
      #alternative (using summarise)
not_cancelled %>%
  group_by(dest) %>%
  summarise(n = length(dest))
    #b
not_cancelled %>% count(tailnum, wt = distance)
      #alternative (using summarise)
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance)) 

  #3
flights %>%
  filter(xor(is.na(dep_delay), is.na(arr_delay))) %>%
  arrange(arr_delay)
    #There seems to be a number of flights where the arrival delay is missing but departure and arrival time exist
flights %>%
  filter(xor(is.na(dep_time), is.na(arr_time))) %>%
  arrange(arr_time)
    #Additionally, there are some flights with a departure time but without an arrival time
    #So the most important variables to determine whether a flight has been cancelled or not seems to be the actual departure time
    #(unless there are flights included that departed but never arrived)
    #So a more appropriate filter might be
flights %>%
  filter(!is.na(dep_time))
  #4
flights %>%
  group_by(year, month, day) %>%
  summarise(n = n(),
            canc = sum(is.na(dep_time))) %>%
  ggplot(aes(x = n, y = canc)) + geom_point()

flights %>%
  filter(is.na(dep_time)) %>%
  group_by(year, month, day) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_histogram()
      # There are some heavy outliers. These might be due to troubles at the airport
flights %>%
  group_by(year, month, day) %>%
  summarise(ttl_flights = n(),
            cld_flights = sum(is.na(dep_time))) %>%
  mutate(perc = cld_flights / ttl_flights) %>%
  ggplot(aes(x = perc)) +
  geom_histogram()
    #looks very similar which indicates there is no clear pattern among weekdays
flights %>%
  group_by(year, month) %>%
  summarise(ttl_flights = n(),
            cld_flights = sum(is.na(dep_time))) %>%
  mutate(perc = cld_flights / ttl_flights) %>%
  ggplot() +
  geom_col(aes(x = month, y = perc))
    #there are some differences between month. February seems to have been the worst
flights %>%
  group_by(year, month, day) %>%
  summarise(ttl_flights = n(),
            cld_flights = sum(is.na(dep_time)),
            avg_delay = mean(dep_delay, na.rm = T)) %>%
  mutate(perc = cld_flights / ttl_flights) %>%
  filter(perc < 0.5) %>%   #filtering to exclude two heavy outliers
  ggplot(aes(x = perc, y = avg_delay)) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw()
    #The average delay seems to be related to the percentage (or total) of cancelled flights.
    #There seem to be some days where the airports operations don't function entirely perfect.
    #Another explanation might be bad weather.
  #5
flights %>%
  filter(!is.na(dep_time)) %>%
  group_by(carrier) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = T),
            avg_dep_delay = mean(dep_delay, na.rm = T)) %>%
  gather("type", "avg_delay", -carrier) %>%
  ggplot(aes(x = carrier, y = avg_delay, group = type, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip()
    #from this, it looks like F9 has the worst arrival delays. Note that some airlines make a lot of ground in flight
  #6
?dplyr::count()
    #sort orders the counts of the groups
flights %>%
  filter(!is.na(dep_time)) %>%
  count(year, month, day, sort = T)
    #e.g. to find the day with most departures

#5.7.1
  #1
    #TBD
  #2
flights %>%
  filter(!is.na(dep_time)) %>%
  group_by(tailnum) %>%
  summarise(avg_delay = mean(dep_delay), count = n()) %>%
  arrange(desc(avg_delay))
    #Plane N844MH has the worst average delay. But it only has one flight.
    #Amending the query above to planes that had at least 50 flights:
flights %>%
  filter(!is.na(dep_time)) %>%
  group_by(tailnum) %>%
  summarise(avg_delay = mean(dep_delay), count = n()) %>%
  filter(count >=50) %>%
  arrange(desc(avg_delay))

flights %>%
  filter(!is.na(dep_time)) %>%
  group_by(tailnum) %>%
  summarise(ttl_delay = sum(dep_delay), count = n()) %>%
  arrange(desc(ttl_delay))
    #N15910 has the worst records when all delay is combined

(tmp <- flights %>%
  group_by(tailnum) %>%
  summarise(scheduled_flights = n(),
            cancelled_flights = sum(is.na(dep_time)),
            delayed_flights = sum(dep_delay > 0 , na.rm = T)) %>%
  mutate(canc_del_flights = cancelled_flights + delayed_flights,
         perc_can_del = canc_del_flights / scheduled_flights,
         perc_del = delayed_flights / scheduled_flights,
         perc_can = cancelled_flights / scheduled_flights) %>%
    filter(scheduled_flights >= 20))


arrange(tmp, desc(cancelled_flights))
arrange(tmp, desc(delayed_flights))
arrange(tmp, desc(canc_del_flights))
arrange(tmp, desc(perc_can))
arrange(tmp, desc(perc_del))
arrange(tmp, desc(perc_can_del))
  #3
flights %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  select(dep_hour, everything()) %>%
  group_by(dep_hour) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = T),
            avg_arr_delay = mean(arr_delay, na.rm = T)) %>%
  gather("type", "delay", -dep_hour) %>%
  ggplot(aes(x = dep_hour, y = delay, fill = type)) + geom_col(position = "dodge") +theme_bw()
    #in order to avoid delays, one might be inclined to depart early

  #4
flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(ttl_delay = sum(arr_delay)) %>%
  arrange(desc(ttl_delay))

flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(flight) %>%
  summarise(n = n(),
            ttl_delay = sum(arr_delay),
            ttl_flight_time = sum(air_time)) %>%
  mutate(perc_delay = ttl_delay / ttl_flight_time) %>%
  arrange(desc(perc_delay))

  #5
flights %>%
  filter(!is.na(dep_time)) %>%
  arrange(origin, year, month, day, dep_time) %>%
  mutate(prev_delay = lag(dep_delay)) %>%
  group_by(prev_delay) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(y = avg_delay, x = prev_delay)) + geom_point()

  #6
(tmp <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>%
  mutate(min_time = min(air_time)) %>%
  mutate(diff = air_time - min_time) %>%
  select(diff, air_time, min_time, dest, everything()) %>%
  filter(mean(diff) > 40))
  ggplot(data = tmp, aes(x = diff)) + geom_histogram() + facet_wrap(~dest)
    #The shortest flight to MSP looks suspicious
tmp %>%
  filter(dest == "MSP") %>%
  arrange(diff)
    #Said flight was half an hour faster than any other flight to that destination during that year
  #7
dests <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(airlines = n_distinct(carrier)) %>%
  filter(airlines > 1)
    #these are all destinations that are served by at least 2 carriers
flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  filter(dest %in% dests$dest) %>%
  group_by(carrier) %>%
  summarise(arrivals = n_distinct(dest)) %>%
  arrange(desc(arrivals))

# Chapter 7 - Exploratory Data Analysis
library(tidyverse)
#7.3.4
  #1
plotdata <- diamonds %>%
  mutate(id = 1:nrow(diamonds)) %>%
  select(x,y,z, id) %>%
  gather("dimension", "size", -id)

to.plot <- ggplot(plotdata, aes(x = size)) +
  geom_histogram()

to.plot +
  facet_wrap(~ dimension, nrow = 1) # creates several plots: one for each value of the variable
    #this graph suggests that there are some unusual values. In order to obtain them:
to.plot +
  facet_wrap(~dimension, nrow = 1) +
  coord_cartesian(ylim = c(-1, 50))
    #there are some zero values in all variables. Moreover, y & z have some suspiciously high values. Filter for all of them
diamonds %>%
  filter(x == 0 | y == 0 | z == 0  | y > 20 | z > 20) %>%
  arrange(desc(y))
    #a typical combination is existing x and y value with a z value of '0'. This might reflect some super thin diamonds
      #that are rounded to '0'. We will keep them in the dataset interpreting them as very thin diamonds.
    #The remaining entries are
diamonds %>%
  filter(x == 0 | y == 0 | y > 20 | z > 20)
    #Some observations have two or three '0's. These are most likely data entry errors and will be excluded from here on.
    #3 diamonds are super sizeable in one dimension. This seeme to reflect a data entry error as well and leads to exclusion
filtered <- diamonds %>%
  filter(x != 0, y != 0, y < 20, z < 20)  #filter out the data entry errors

plotdata.f <- filtered %>%
  mutate(id = 1:nrow(filtered)) %>%
  select(x,y,z, id) %>%
  gather("dimension", "size", -id)

(to.plot <- ggplot(plotdata.f, aes(x = size)) +
  geom_histogram())
    #this shows all dimensions in a single plot now
to.plot +
  facet_wrap(~ dimension, nrow = 1) # creates several plots: one for each value of the variable
    #grouped by dimension
to.plot +
  facet_wrap(~dimension, nrow = 1) +
  coord_cartesian(ylim = c(-1, 50))  #shows the outliers downside which were not excluded (might not be reasonable after all)

ggplot(plotdata.f, aes(x = size, col = dimension)) + geom_freqpoly(binwidth = 0.1)
    #x and y variable are very similar in their distribution. Z has a lower mean
cor(select(filtered, x, y, z))
    #there is a very significant relationship between x, y and z
ggplot(filtered, aes( x = x, y = y)) +
  geom_point() +
  geom_smooth(col = "blue") +
  geom_abline(aes(intercept = 0, slope = 1), col = "red") +
  theme_bw()
    #x and y are almost always the same or a very similar size (red line: 45 degree line | blue line: smoothing method)
ggplot(filtered, aes( x = x, y = z)) +
  geom_point() +
  geom_smooth(col = "blue") +
  geom_abline(aes(intercept = 0, slope = 1), col = "red") +
  theme_bw()
    #The relationship between x and z also seems to be linear. However, z is smaller than y.
    #The observations with z = 0 really seem to be out of the trend. In retrospect they probably should be excluded
    #Only two observations are on the 45 degrees line. These might be data entry errors where the y value was registered as z value
ggplot(filtered, aes( x = y, y = z)) +
  geom_point(aes(col = cut),alpha = 0.35, size = 0.35) +
  geom_smooth(col = "blue", se = F) +
  geom_abline(aes(intercept = 0, slope = 1), col = "red") +
  theme_bw()
    #the relationship between y and z is very similar to the one between x and z
      #This makes sense because in most cases te values of x and y are basically the same
    #Moreover, it seems that the better the cutting quality is, the closer the indiidual points are to the trend line.
      #This suggests that 'good cutting' results in a rather symmetric diamond
    #In summary, the data suggests that diamonds are usually symmetric in the sense that two dimensions are almost equal ins isze
    #the third dimension (here: z) probably reflects height and is smaller in size

  #2
ggplot(filtered, aes(x = price)) +
  geom_histogram(binwidth = 100)
    #I: the distribution is skewed to the right which makes sense because very big and expensive diamonds are rare
    #II: There are few diamonds for a very low price (< ~500)
    #III: There seems to be a particular price almost no diamonds are sold
      #expanding on II:
filtered %>%
  filter(price < 750) %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 20)
min(filtered$price)
        #The minimum price for diamonds in the dataset is 326. From there on the frequency of higher prices is increasing first
      #expanding on III
filtered %>%
  filter(price < 5000) %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 50)
        #There seem to be no diamons sold for 1500 for some reason.

    #Benford Analysis
benford.distribution <-  data.frame(digit = 1:9,bf.prediction = log10(1+1/1:9))
filtered$price.fd <- as.factor(str_sub(filtered$price, start = 1, end = 1))
ggplot() +
  geom_bar(data = filtered, aes(x = price.fd, y = ..prop.., group = 1)) +
  geom_line(data=benford.distribution,aes(x=digit,y= bf.prediction), size=1.3, color="red")
      #Seems to be roughly in line with expectations by Benford's law. Values of 2 and 3 are slightly under-represented
      #and values of 4 to 9 are ever so slightly over represented. This might be due to prcing strategies

  #3
filtered %>%
  filter(between(carat, 0.99, 1.01)) %>%
  count(carat)
    #There are only very few diamonds with 0.99 carat. The reason might be psychological.
    #It is easier to sell a 1 carat diamond than a 0.9 carat diamond. 

  #4
?coord_cartesian
?xlim
    #coord_cartesian calculates all values from the whole data and zooms in to the specified range afterwards
    #xlim and ylim don't compute values outside the required data range
demo <- ggplot(filtered, aes(x = price))
demo + geom_histogram(binwidth = 1000)

demo + geom_histogram(binwidth = 1000) +
  coord_cartesian(xlim = c(0,10000))

demo +geom_histogram(binwidth = 1000) +
  xlim(c(0,10000))
    #the bars that overlap with the bins whose range includes values outside the specified limits are not visible due to NA's

#7.5.5.1
  #1
names(flights)
pltdata <- flights %>%
  mutate(cancelled = is.na(dep_delay),
         sched_dep_hour = sched_dep_time %/% 100,
         sched_dep_min = sched_dep_time %% 100,
         sched_dep_hour2 = sched_dep_hour + sched_dep_min / 60)
ggplot(pltdata, aes(x = sched_dep_hour2, y = ..density.., col = cancelled)) + geom_freqpoly(binwidth = 1)
    #cancelled flights seem to occur more frequently later during the day and spike at 18:00
ggplot(pltdata, aes(x = cancelled, y = sched_dep_hour2)) + geom_boxplot()
    #same story

  #2
names(diamonds)
filtered %>%
  ggplot(aes(x = carat, y = price)) + geom_point() + geom_smooth() + theme_bw()
    #There seems to be a strong, not necessarily linear relationship between carat and price
filtered %>%  ggplot(aes(x = cut, y = price)) + geom_boxplot()
    #The relationship between cut quality and price seems not very strong.
    #For some reason, the worst quality seems to indicate a higher price.
filtered %>%  ggplot(aes(x = color, y = price)) + geom_boxplot()
    #again, the trend is counterintuitive. Diamonds with worse color (j) seem to be more expensive
filtered %>%  ggplot(aes(x = clarity, y = price)) + geom_boxplot()
    #Clarity doesn't seem to have much of an impact (though again, worst quality seems to be more expensive)
filtered %>%  ggplot(aes(x = depth, y = price)) + geom_point()
    #no obvious relationship betweeen depth and price
filtered %>%  ggplot(aes(x = table, y = price)) + geom_point()
    #no obvious relationship
filtered %>%  ggplot(aes(x = x, y = price)) + geom_point() + geom_smooth(se = F)


      #size seems to be the best indicator of price. Relationship between size and cut:
filtered %>%  ggplot(aes(x = cut, y = x)) +
    geom_boxplot()
filtered %>% ggplot(aes(x = x, y = ..density.., col = cut)) + geom_freqpoly(binwidth = 0.2)
    #for some reason a bad cut is associated with a bigger size
    #Since size is a good predictor for price, well cut diamonds tend to be cheapter

  #3
    #Don't install ggstance!
  #4
library(lvplot)
?geom_lv
filtered %>%  ggplot(aes(x = cut, y = price)) + geom_lv() +coord_flip()
    #the lv-geom shows the distribution of a variable by varying the width of a 'tree'.
    #It shows that again, that there seems to be a right tail skewness.
    #Also, differences between cut classes aren't as obvious as in other graphics.

  #5
ggplot(filtered) +
  geom_histogram(aes(x = price), binwidth = 500) + 
  facet_wrap( ~ cut, nrow = 5)
    #shows the difference between classes nicely. Analysis within classes harder (especially those that are smaller)
ggplot(filtered) +
  geom_freqpoly(aes(x = price, col = cut, y = ..density..), binwidth = 5000)
    #comparisons within a class very difficult
?geom_violin
ggplot(filtered) +
  geom_violin(aes(x = cut,y =  price), position = "dodge")
    #gives quite a lot of information about a single class. Doesn't display mean or median.
    #Comparisons among classes a bit more difficult
  #6
    #Skipped

#7.5.2.1
  #1
ggplot(filtered, aes(y = color, x = cut)) + geom_count()
filtered %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = color, y = cut, fill = prop)) + geom_tile()

filtered %>%
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(prop = n/ sum(n)) %>%
  ggplot(aes(x = color, y = cut, fill = prop)) + geom_tile()
  
  #2
    #naive plot
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(dest, month) %>%
  summarise(count = n(),
            avg_delay = mean(dep_delay)) %>%
  ggplot() +
  geom_tile(aes(x = factor(month), y = dest, fill = avg_delay))

    #filtering for only the biggest airports
airports <- flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(dest) %>%
  summarize(count = n()) %>%
  filter(count > 5000)
flights %>%
  filter(dest %in% airports$dest,!is.na(dep_delay)) %>%
  group_by(dest, month) %>%
  summarise(count = n(),
            avg_delay = mean(dep_delay)) %>%
  ggplot() +
    geom_tile(aes(x = factor(month), y = dest, fill = avg_delay))
      #delay seems to be highest during summer month and december. Also, some destinations seem to have higher departure delays

    #re-leveling
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(dest, month) %>%
  summarize(count = n(),
            avg_delay = mean(dep_delay, na.rm = T)) %>%
  ungroup() %>%
  mutate(dest = reorder(dest, avg_delay)) %>%
  ggplot() + geom_tile(aes(x = factor(month), y = dest, fill = avg_delay))

    #from solution manual
library(viridis)
flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%
  filter(n() == 12) %>%
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")

  #3
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))
    #The first graph makes better use of the available space
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(y = color, x = cut)) +
  geom_tile(mapping = aes(fill = n)) 
    #The second graph is a little worse because it puts the variable with more levels on the more limited y axis


#7.5.3.1
  #1
      #The first thing to consider would be which variable should be binned. The following graphs reveal, that carat is more sensible
filtered %>%
  filter(carat < 2.5) %>%
  ggplot() +
    geom_freqpoly(aes(x = price, col = cut_width(carat, width = 1)), binwidth = 200)
filtered %>%
  filter(carat < 2.5) %>%
  ggplot() +
  geom_freqpoly(aes(x = carat, col = cut_width(price, width = 5000)), binwidth = 0.05)
      #CHoosing cut_width requires deciding about the binning parameter. To keep things vivid and clearly, to many bins should be avoided.
?cut_number
filtered %>%
  ggplot() +
  geom_freqpoly(aes(x = price, col = cut_number(carat, n = 4)), binwidth = 200)
      #Choosing cut_number leads to the question of how many intervals should be used. 
      #In both cases, the binwidth could be tweaked. The default (30 bins) is often sensible but more bins might reveal more insights
filtered %>%
  ggplot() +
  geom_freqpoly(aes(x = price, col = cut_number(carat, n = 4)), binwidth = 200)

  #2
filtered %>%
  ggplot() +
  geom_boxplot(aes(x = price, y = carat, group = cut_number(price, n = 8)))
    #advantage of this visualization is that the carat outliers don't really impede the readability of the graph
    #disadvantage is that the higher stacked groups (low price) are very small and contain many outliers
filtered %>%
  ggplot() +
  geom_boxplot(aes(x = price, y = carat, group = cut_width(price, 2000)), varwidth = T)
    #this is easier to read and also gives an indication about the amount of observations within each bin.
    #However, the borders within each bin aren't that visible. One might want switch coordinates or include colors to tackle that
filtered %>%
  ggplot() +
  geom_boxplot(aes(x = cut_number(price, n = 8), y = carat)) + coord_flip()
filtered %>%
  ggplot() +
  geom_boxplot(aes(x = price, y = carat, col = cut_width(price, 2000, boundary = 0)),
               varwidth = T) +
  scale_color_discrete(name = "price", labels = c("0 - 2000", "2000 - 4000", "4000 - 6000", "6000 - 8000", "8000 - 10000",
                                                  "10000 - 12000", "12000 - 14000", "14000 - 16000", "16000 - 18000", "> 18000"))

    #Alternative visualiations are freqpolies. Here filtering out the rare high-caraters is sensible
filtered %>%
  filter(carat < 2.5) %>%
  ggplot() +
  geom_freqpoly(aes(x = carat, col = cut_number(price, n = 4)), binwidth = 0.1)
filtered %>%
  filter(carat < 2.5) %>%
  ggplot() +
  geom_freqpoly(aes(x = carat, col = cut_width(price, width = 5000)), binwidth = 0.1)

  #3
    #We measure size by carat (as it apparently contains length, width and depth)
to_plot <- filtered %>%
  mutate(very_large = carat > 2,
         small = carat < 0.3) %>%
  filter(very_large == TRUE | small == TRUE) %>%
  ggplot(aes(x = price))
to_plot + geom_histogram(binwidth = 100) + facet_wrap(~ very_large, nrow = 2)
to_plot + geom_freqpoly(aes(col = very_large), binwidth = 100)
    #The price distribution isn't really surprising. Very large diamonds are more expensive than small diamonds.
    #However, there is a lot more variation. Maybe small diamonds aren't screened in much detail while 
    #large diamonds are screened for cut quality, color, clarity more intensely.

  #4
    #One of the easiest visualizations would be a scatterplot colored by cut:
filtered %>%
  filter(carat < 2.5) %>%
  ggplot(aes(x = carat, y = price, col = cut, fill = cut)) +
    geom_point(alpha = 0.1) +
    theme_bw()
      #This is not super clear and also takes a lot of time.
filtered %>%
  filter(carat < 2.5) %>%
  ggplot(aes(x = cut_width(carat, 0.5, boundary = 0), y = price), varwidth = TRUE) +
  geom_violin() +
  scale_x_discrete(labels = c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2","2 - 2.5")) +
  xlab("carat") +
  facet_grid(cut ~ 1)

filtered %>%
  filter(carat < 2.5) %>%
  ggplot(aes(x = cut_width(carat, 0.5, boundary = 0), y = price, col = cut), varwidth = TRUE) +
  geom_boxplot(position = "dodge", varwidth = T) +
  scale_x_discrete(labels = c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2","2 - 2.5")) +
  xlab("carat")

  #5
ggplot(data = filtered) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
ggplot(data = filtered) +
  geom_boxplot(aes(x = cut_width(x, 2, boundary = 0), y = y))

