#Hierarchy (in order to ease searching via Strg + f):

# Part I - xxx
# Chapter 1 - Introduction
# 1.1.3

# Part II - Wrangle
# Chapter 10 - Tibbles

#10.5
library(tidyverse)
vignette("tibble")
  #1
mtcars
    #when printing a dataframe, more rows than fit the window will be printed in the console
mpg
    #when printing a tibble, the printing will be adjusted to the size of the console window
str(mtcars);str(mpg)
    #Checking the structure reveals the type of a data frame. Technically, tibbles are data frames but also tibbles.
  #2
df <- data.frame(abc = 1, xyz = "a")
df$x
    #this returns the column even though it's not a precise match. In order to avoid mistakes this shouldn't return anything
df[, "xyz"]
    #Character strings are converted to vectors automatically. This may or may not be sensible.
df[, c("abc", "xyz")]

tb <- tibble(abc = 1, xyz = "a")
tb$x
    #In contrast, tibbles require you to be precise with column names
tb[, "xyz"]
    #This returns another tibble instead of just a vector. Also, the column remains as string column
tb[, c("abc", "xyz")]
    #Conveniently, the dimensions of the tibble and the type of each column are printed
  #3
var <- "model"
mpg[[var]]
  #4
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`)))
annoying
    #i)
annoying$`1`
annoying[,1]
    #ii)
ggplot(annoying, aes(`1`, `2`)) + geom_point()
    #iii)
(annoying <- annoying %>%
  mutate(`3` = `2` / `1`))
    #iv)
rename(annoying, one = `1`, two = `2`, three = `3`)
  #5
?tibble::enframe
enframe(c(a = 5, b = 7))
    #converts vectors where each observation is names into a two-column tibble
  #6
?print.tbl
    #the option n_extra controls for how many extra column names are printed


# Chapter 11 - Data import

#11.2.2
  #1
    #Separating with `|`is not common. I doubt there is an explicit readr function for this.
    #So I would choose the customisable option
#read_delim("filepath", delim = "|")
  #2
?read_csv
    #They have all arguments in common as they're both just special cases of read_delim:
      #col_names to indicate the (non-) existance of a header
      #col_types to manually specify the imported column types
      #locale (???)
      #na to import particular values as NAs
      #quoted_na to import quoted NAs as NAs
      #quote to specify quote character
      #trim_ws to trim whitespace
      #n_max to define maximum number of rows to be imported
      #guess_max to specify number of rows that should be used to guess column types
      #progress to enable a progress bar
  #3
?read_fwf
    #col_positions determines the field length for columns. To create this specification use:
      #fwf_wmpty, fwf_widths, fwf_positions
  #4
    #the argument quote specifies what character is used to denote quotes
read_delim("x,y\n1,'a,b'", delim = ",", quote = "'")
  #5
read_csv("a,b\n1,2,3\n4,5,6")
    #the first line (head line) only has 2 values whereas the following lines have 3
    #When reading, the third values of the tail lines are ignored
read_csv("a,b,c\n1,2\n1,2,3,4")
    #values within each line differ (head = 3, first data line = 2, second data line = 4)
    #missing values are created when data line doesn't contain enough values
    #values in columns exceeding the number of values in the head line are ignored
read_csv("a,b\n\"1")
    #quotation marks are misleading (3 of them?)
read_csv("a,b\n1,2\na,b")
    #nothing necessarily wrong with this.
read_csv("a;b\n1;3")
    #delimiter is a semicolon. Use read_csv2 instead:
read_csv2("a;b\n1;3")

#11.3.5
  #1
?locale
    #What arguments are important depends on the type of data
      #for dates: date_names, date_format, time_format, tz (timezone)
      #for doubles: decimal_mark, grouping_mark
      #for characters: encoding
  #2
parse_double(c("1.000.0", "1.005", "100.5"), locale = locale(decimal_mark = ".", grouping_mark = "."))
    #there is an error message. No parsing happens.
parse_number(c("1.000,4", "1234,5"), locale = locale(decimal_mark = ","))
    #parse_number changes the grouping mark to '.' which is useful for European denoted numbers
parse_number(c("1.000,4", "1234,5"), locale = locale(grouping_mark = "."))
    #parse_number changes the decimal mark to ',' which is useful for numbers denoted in the European style
  #3
?locale
    #date_format interprets many date format which is useful when they are not noted ISO compliant
parse_date("20.01.2010", locale = locale(date_format = "%d.%m.%Y"))
    #time_format
parse_time("21-32-12", locale = locale(time_format = "%H-%M-%S"))
  #4
mylocale = locale(date_names = "de", date_format = "%d.%m.%Y", time_format = "%H:%M:%S",
                  decimal_mark = ",", grouping_mark = ".", tz = "CET", encoding = "UTF-8")
parse_number(c("2.000", "12,92", "1.000,8"), locale = mylocale)
parse_date("24.08.1995", locale = mylocale)
parse_time("12:00:00", locale = mylocale)
  #5
?read_csv
    #read_csv2 uses ';' as delimiter between entries, read_csv uses ','
read_csv("a,b\n1,3")
read_csv2("a;b\n1;3")
  #6
    #Typical European encodings are UTF-8, ISO 8859-1 to 8859-15, cp1250, cp 1251 & cp1252
    #Typical Asian encodings are GB2313, EUC-KR, cp1251, Shift-JIS
  #7
?strptime
d1 <- "January 1, 2010"; parse_date(d1, format = "%B %d, %Y")
d2 <- "2015-Mar-07"; parse_date(d2, format = "%Y-%b-%d")
d3 <- "06-Jun-2017"; parse_date(d3, format = "%d-%b-%Y")
d4 <- c("August 19 (2015)", "July 1 (2015)"); parse_date(d4, format = "%B %d%*%Y)")
d5 <- "12/30/14"; parse_date(d5, format = "%m/%d/%y") # Dec 30, 2014
t1 <- "1705"; parse_time(t1, format = "%H%M")
t2 <- "11:15:10.12 PM"; parse_time(t2, format = "%H:%M:%OS %p")


# Chapter 12 - Tidy Data

#12.2.1
  #1
table1
    #Table 1 contains one record for each country and year. Both, cases and population are stored as individual columns
    #Table 1 is tidy because (i) every variable variable has its own column, (ii) each observation has its own row & (iii) each value has its own cell
table2
    #Table 2 contains 2 records for each country and year. There is a column giving the count for both 'cases' and 'population'. It relies on the specification in another column: type which gives either 'cases' or 'population'
    #Table 2 isn't tidy because (i) cases and population share a column and (ii) every observation has 2 rows
table3    
#Table 3 contains one record for each country and year. Cases and population are given in a single cell. The values are seperated by a slash within the cell.
    #Table 3 isn't tidy because (i) cases and population share a column and (iii) the single values for cases and population share a cell.
table4a; table4b
    #Tables 4a & 4b partition the dataset in two tables: one for 'cases' and one for 'population'. Both contain one record for each country and two more columns for each year.
    #Tables 4a & 4b aren't tidy because (i) a column for year doesn't exist while there are two columns for country and (ii) the two records 1999 and 2000 share one row for each country

  #2
table2a <- table2 %>%
  filter(type == "cases") %>%
  rename(cases = count) %>%
  select(country, year, cases)
table2b <- table2 %>%
  filter(type == "population") %>%
  rename(population = count) %>%
  select(country, year, population)
tibble(country = table2a$country, year = table2a$year, cases = table2a$cases, population = table2b$population) %>%
  mutate(rate = cases / population * 10000)

table4a2 <- table4a %>%
  gather("year", "cases", -country)
table4b2 <- table4b %>%
  gather("year", "population", -country)
tibble(country = table4a2$country, year = table4a2$year, cases = table4a2$cases, population = table4b2$population) %>%
  mutate(rate = cases / population * 10000)

  #3
ggplot(table2a, aes(year, cases)) +
  geom_point(aes(col = country)) +
  geom_line(aes(group = country), col = "grey50")
    #the data needs to be transformed into the tidy format first

#12.3.3
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
    #comparing the original dataset 'stocks' with the double-transformed one results in two differences
      #1.  The variables are ordered differently. This is because during the first spread-transformation only the variable 'half' remains and the years are concatenated after.
          #During the second transformation the new key and value variables are ordered behind the variable 'half'.
          #The reordering can be reversed using the select verb
      #2.  The variable 'year' turns from a double to a character.
          #This happens because gather uses the column names to fill the values of the new key variable.
          #More often than not column names are strings and not numbers, so this behavior is natural.
          #One might use as_double() or parse_double() to reverse the type to a double
?spread
    #For spreading, a True set 'convert'-argument will try to handle the data type of the newly generated columns after the transformation
newstocks <- tibble(
    date = seq(as.Date("2010-01-01"), as.Date("2010-06-01"), length.out = 6),
    return = c(seq(1, 2, length.out = 3), "yes", "no", "no"))
newstocks %>%
  spread(date, return)
    #Without spreading all columns are characters (the most general type)
newstocks %>%
  spread(date, return, convert  = TRUE)
    #with convert = TRUE, appropriate columns are transformed into doubles and integers

?gather
    #For gathering, a True set 'convert'-argument will handle the data type of the new key column
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`, convert = TRUE)
    #with convert = TRUE, the value column is converted into an integer type

#2
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
    #the code fails because 1999 and 2000 are not identified as columns. Instead backticks should be used:
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

#3
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people
spread(people, key, value)
    #Spreading isn't possible because for the newly generated cell representing Phillip Woods' age, two values exist
    #Apparently there are two Phillip Woods in this data, one being 45 years of age and one 50 years.
    #To solve this, one could introduce ID's to distinguish between the two Phillips'.
people2 <- tribble(
  ~name,             ~key,    ~value,    ~id,
  #-----------------|--------|--------|-----
  "Phillip Woods",   "age",       45,      1,
  "Phillip Woods",   "height",   186,      1,
  "Phillip Woods",   "age",       50,      2,
  "Jessica Cordero", "age",       37,      3,
  "Jessica Cordero", "height",   156,      3
)
spread(people2, key, value)

  #4
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg
    #Male and Female are not variables so they need to be gathered. This results in all variables being columns: pregnant, gender & count.
preg %>%
  gather("gender", "count", -pregnant)


#12.4.3
  #1
?separate
    #extra defines the behavior when there are more separators than columns
(to.many <- tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")))
separate(to.many, x, c("one", "two", "three"))
      #default: 'warn' truncates additional information and gives a warning
separate(to.many, x, c("one", "two", "three"), extra = "drop")
      #'drop' behaves like warn except that it supresses the warning
separate(to.many, x, c("one", "two", "three"), extra = "merge")
      #'merge' will transforms complete original vector. Additional characters are written into the last column 
      #'
    #fill defines the behavior when there are less separators than columns
(to.few <- tibble(x = c("a,b,c", "d,e", "f,g,i")))
separate(to.few, x, c("one", "two", "three"))
      #default: 'warn' fills with 'NA's' from the right and gives an appropriate warning
separate(to.few, x, c("one", "two", "three"), fill = "right")
      #'right' behaves like warn except that it supresses the warning
separate(to.few, x, c("one", "two", "three"), fill = "left")
      #'left' fills with 'NA's' from the left without giving a warning

  #2
?separate
?unite
    #the remove argument determines whether or not the original columns are kept or not.
    #You might want to keep them to check the separating/uniting behavior in detail.

  #3
?separate
?extract
    #extract is more flexible than separate because it uses regular expressions
?unite
    #unite only allows for separation by sign, because it standardises the new column.
    #There is only one way (one seperator) to combine the input columns


#12.5.1
  #1
?spread
    #Spreading: the fill argument defines how implicit and explicit missing values shall be replaced with
(nstocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
))
spread(nstocks, qtr, return)               #default is NA
spread(nstocks, qtr, return, fill = -Inf)  #alternatively any other value
?complete
    #Completing: The fill argument takes a list where for every column a replacement can be specified
complete(nstocks, year, qtr, fill = list(return = -Inf))

  #2
?fill
    #.direction determines in which direction to fill (from top to bottom or from bottom to top)
(persons <- tibble(name = c("Malte", NA, "Jonah", NA),
                  measure = c("height", "width", "height", "width"),
                  value = c(182, 40, 187, 43)))
fill(persons, name, .direction = "down")
fill(persons, name, .direction = "up")

#12.6.1
  #1
who %>%
  select(-iso2, -iso3) %>%
  gather("key", "value", -country, -year) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, into = c("newvold", "type", "sex_age"), sep = "_") %>%
  select(-newvold) %>%
  separate(sex_age, into = c("sex", "age_group"), sep = 1)
    #when not removing NA's in the gather step, there are about 330,000 more rows. Depending on what
    #NA's represent this is reasonable but it doesn't really hurt to include them in the first step
    #There are no implicit missing values in the original dataset.
    #When using na.rm, there are implicit missing values.
    #NA should represent that there is no records for a particular constellation. (i.e. no one measured)
    #Zeros represent zero that there is a record of zero cases for a particular constellation (i.e. someone tried to measure)

  #2
who %>%
  select(-iso2, -iso3) %>%
  gather("key", "value", -country, -year, na.rm = T) %>%
  separate(key, into = c("newvold", "type", "sex_age"), sep = "_") %>%
  filter(newvold == "newrel")
    #When neglecting the string replacement, the values are displaced in wrong columns

  #3
who %>%
  count(country)
    #there are 219 countries in the country column
who %>%
  count(country, iso2, iso3)
    #there are still only 219 combinations of country, iso2 and iso3

  #4
plotdata <- who %>%
  select(-iso2, -iso3) %>%
  gather("key", "value", -country, -year, na.rm = T) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, into = c("newvold", "type", "sex_age"), sep = "_") %>%
  select(-newvold) %>%
  separate(sex_age, into = c("sex", "age_group"), sep = 1) %>%
  filter(year >=1995) %>%
  group_by(country) %>%
  mutate(ttl_cases = sum(value)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(ttl_cases))) %>%
  filter(rank <= 6) %>%
  group_by(country, year, sex) %>%
  summarise(cases = sum(value)) %>%
  arrange(desc(cases))



ggplot(plotdata, aes(year, cases)) +
  geom_line(aes(col = country)) +
  facet_wrap( ~ sex, nrow = 2) +
  theme_bw()

ggplot(plotdata, aes(year, cases)) +
  geom_col(aes(fill = country)) +
  facet_wrap(~ sex, nrow = 2)

ggplot(plotdata, aes(year, cases)) +
  geom_col(aes(fill = sex)) +
  facet_wrap(~ country) +theme_bw()

ggplot(plotdata, aes(year, cases)) +
  geom_line(aes(col = country, lty = sex)) +
  theme_bw()



# Chapter 13 - Relational data
library(tidyverse)
library(nycflights13)
#13.2.1
  #1
    #To determine the routes, origin and destination for each flight are needed
      #This information is present in the flights table
    #To draw the routes one needs latitude & longitude of the airports
      #This information is available in the airports table
    #If one was inclined to break it down by airplane, the planes table might required

  #2
    #airports and weather are connected by the variables origin (weather) & faa (airports)
    #accordingly the two variables should be connected in the diagram

  #3
    #The column 'origin' should be renamed because it would then reflect origin and destination.
    #Thus, the realtionship with the flights table would change.
    #Both, origin and destination from the flights table would be linked to the new weather variable

  #4
    #The new table should include an indicator for year, month and day.
    #Additionally at least one column must represent some measure of how inclined people are to fly during that day
    #The primary key would be the combination of year, month and date.
    #These three variables also represent the links to the other tables (flights & weather)


#13.3.1
  #1
(nflights <- flights %>%
  mutate(id = row_number()) %>%
  select(id, everything()))
  #2
    #1
library(Lahman); ?Batting
as.tibble(Batting) %>%
  count(playerID, yearID, stint) %>%
  filter(n> 1)
    #2
install.packages("babynames")
library(babynames); ?babynames
babynames %>%
  count(year, sex, name) %>%
  filter(nn > 1)
    #3
library(nasaweather); ?atmos
atmos %>%
  count(lat, long, year, month) %>%
  filter(n>1)
    #4
library(fueleconomy); ?vehicles
vehicles %>%
  count(id) %>%
  filter( n > 1)
    #5
?diamonds
      #to me it looks like that there is not a natural combination of unique keys here.
      #By trial and error there is probably one to find.
      #But adding an incredibly high amount of new records would ensure that no combination is unique
      #Thus, I pledge to include an id
diamonds %>%
  mutate(id = row_number()) %>%
  count(id) %>%
  filter(n > 1)

  #3
    #Batting, Master & Salaries
?Batting
as.tibble(Batting) %>%
  count(playerID, yearID, stint) %>%  #primary key: playerID, yearID & stint
  filter(n> 1)
?Master
as.tibble(Master) %>%
  count(playerID) %>%  #primary key: playerID
  filter(n>1)
?Salaries
as.tibble(Salaries) %>%
  count(playerID, yearID, teamID) %>% # primary key: playerID, yearID, teamID
  filter(n>1)

    #Master, Managers & AwardsManagers
?Managers
as.tibble(Managers) %>%
  count(playerID, yearID, inseason) %>%
  filter(n > 1)
?AwardsManagers
as.tibble(AwardsManagers) %>%
  count(playerID, awardID, yearID) %>%
  filter(n>1)

#13.4.6
  #1
library(maps)
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, col = avg_delay)) +
  borders("state") +
  geom_point(size = 2) +
  coord_quickmap()
  #2
flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  select(year:alt) %>%
  rename(origin_lat = lat, origin_name = name, origin_lon = lon, origin_alt = alt) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(year:alt) %>%
  rename(dest_name = name, dest_lat = lat, dest_lon = lon, dest_alt = alt) %>%
  select(contains("origin"), contains("dest"))

  #3
flights %>%
  filter(!is.na(dep_delay)) %>%
  left_join(planes, by = "tailnum") %>%
  rename(year_built = year.y) %>%
  group_by(year_built) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(year_built, avg_delay)) +
  geom_col()

  #4
flights4 <- flights %>%
  filter(!is.na(dep_delay)) %>%
  left_join(weather) %>%
  select(dep_delay, temp:visib)
flights4 %>%
  filter(!is.na(temp)) %>%
  mutate(temp_bins = cut_width(temp, width = 5, boundary = 5)) %>%
  group_by(temp_bins) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = temp_bins, y = avg_delay))
    #apparently very high temperatures lead to higher delays

flights4 %>%
  filter(!is.na(dewp)) %>%
  mutate(dewp_bins = cut_width(dewp, width = 5, boundary = -15)) %>%
  group_by(dewp_bins) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = dewp_bins, y = avg_delay))
    #very high and very low dewpoints lead to higher delays

flights4 %>%
  filter(!is.na(humid)) %>%
  mutate(humid_bins = cut_width(humid, width = 5, boundary = 0)) %>%
  group_by(humid_bins) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = humid_bins, y = avg_delay))
    #high humidity is associated with higher delays

flights4 %>%
  filter(!is.na(wind_dir)) %>%
  mutate(dir_bins = cut_width(wind_dir, width = 90, boundary = 0)) %>%
  group_by(dir_bins) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = dir_bins, y = avg_delay))
    #no real relationship with wind direction

flights4 %>%
  filter(!is.na(wind_speed)) %>%
  mutate(speed_bins = cut_width(wind_speed, width = 1, boundary = 0)) %>%
  group_by(speed_bins) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = speed_bins, y = avg_delay))
    #high wind speed typically increases delays

flights4 %>%
  filter(!is.na(visib)) %>%
  group_by(visib) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot() + 
  geom_col(aes(x = factor(visib), y = avg_delay))
    #higher visibility makes flights leave early

  #5
library(maps)
library(viridis)
flights %>%
  filter(!is.na(dep_delay), year == 2013, month == 6, day  %in% c(12, 13)) %>%
  group_by(dest, day) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, col = avg_delay, size = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  facet_wrap(~day, nrow = 2) +
  scale_colour_viridis()
    #flights to the East coast were highly delayed on the 13th (contrasted to 1 day earlier)
    #There was a heavy storm and a lot of flights were cancelled and delayed that day


#13.5.1
  #1
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE) %>%
  left_join(airlines, by = "carrier")
    #Unknown planes belong mostly to Envoy Air and American Airlines
  #2
busy_planes <- flights %>%
  count(tailnum, sort = TRUE) %>%
  filter(n >= 100, !is.na(tailnum))

flights %>%
  semi_join(busy_planes, by = "tailnum")
  #3
fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))
  #4
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(year, month, day) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  mutate(avg_delay_lead = lead(avg_delay)) %>%
  gather("delay", "type", -year, -month, -day) %>%
  group_by(year, month, day) %>%
  summarise(avg_delay_2days = mean(type, na.rm = T)) %>%
  arrange(desc(avg_delay_2days))
    #the first 48 hours of average delays have been at the 07.03.2013 and the subsequent day.
weather %>%
  filter(month == 3, between(day, 7, 10)) %>%
  group_by(origin, year, month, day) %>%
  summarise(avg_wind_speed = mean(wind_speed),
            avg_wind_gust = mean(wind_gust),
            avg_visib = mean(visib))
    #On the 07.03 the wind speed seems rather high. Moreover, on the 08.03 the visibility was really low.
    #The subsequent days (09.03 and 10.03 are printed for comparisons)

  #5
anti_join(flights, airports, by = c("dest" = "faa")) %>%
  count(dest)
    #This anti_join suggests that the airports BQN, PSE, SJU and STT are not in the airports table
anti_join(airports, flights, by = c("faa" = "dest"))
    #This anti_join returns all airports that haven't been a destination of any flight
      #departing from one of the three NY airports during 2013.
    #There are probably some military, private and out of service airports included

  #6
(double_planes <- flights %>%
  count(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(nn>1, !is.na(tailnum)))
    #There are a handful of planes operated by two airlines. These will now be examined further

planes %>%
  semi_join(double_planes, by = "tailnum") %>%
  left_join(flights, by = "tailnum") %>%
  count(tailnum, carrier) %>%
  spread(carrier, n)
    #This suggests that there is a partnership between 9E and EV and one between DL and FL.
    #They either share planes or one occasionally lends aircrafts to the other.



# Chapter 14 - Strings
library(tidyverse)
library(stringr)
#13.2.1
  #1
?paste
paste("This", "is", "a", "test"); paste0("This", "is", "a", "test")
    #paste concatenates many strings to one and allows for specifying a separation character.
    #paste0 does the same but concatenates without any separation character
str_c("This", "is", "a", "test", sep = " "); str_c("This", "is", "a", "test")
    #str_c is the stringr equivalent to paste/paste0 (control via 'sep' argument)
str_c("Test", NA); paste("Test", NA); paste0("Test", NA)
    #paste/paste0 convert NA into "NA" by default. str_c returns a real NA instead

  #2
?str_c
chars <- c("Exemplary", "Test")
str_c(chars, chars, sep = " ", collapse = "-")
    #sep specifies the character that is used to separate strings from different arguments
      #sep is only relevant when there is more than one argument passed to str_c
    #collapse specifies the character that is used to separate strings within a vector
      #by default, strings within the same vector are not concatenated
      #collapse is only relevant when at least one vector is among the arguments passed to str_c
  #3
demo <- c("Demo", "NDemo")
first <- round(str_length(demo)/2 + 1/4)
second <- ceiling(str_length(demo)/2 + 1/4)
str_sub(demo, first, second)
    #This code ensures that for even number characters both middle characters are printed out
  #4
?str_wrap
demo2 <- ("This is a long text to demonstate the 'functionality' of the str_wrap command which might be useful when a dataset contains a lot of text that isn't formatted properly. Str_wrap formats the text in a way that single text rows aren't too long which makes the whole thing more readable.")
writeLines(demo2)
writeLines(str_wrap(demo2, width = 40))
    #str_wrap includes word_wraps in long strings.
  #5
?str_trim
?str_pad
str_trim("  hi  ", side = "both")
    #str-trim removes spaces from character strings
str_pad("hi", width = 10, side = "both", pad = " ")
    #str-pad attaches spaces (or other characters) to a string until a specified string length is reached

  #6
random_function <- function(x) {
  library(stringr)
  l <- length(x)
  last <- tail(x, 1)
  x <- x[-l]
  str_c(str_c(x, collapse = ", "), last, sep = ", and ")
}
random_function(c("a", "b", "c")); random_function(c("Düsseldorf", "Seoul"))
random_function("OnlyOne"); random_function(c("a", "b", "c", "d", "e", "f"))


#14.3.1.1
  #1
demo4 <- "\\"; writeLines(demo4)
#str_view(demo4, "\")
    #the above code doesn't work because the backslash escapes the second double quotes.
      #Thus R doesn't think the string has ended and waits for additional input
      #For that reason the code is hidden so it doesn't ruin the rest of the script
str_view(demo4, "\\")
#str_view(demo4, "\\\")
    #again, the second double quote is escaped by the last backslash
str_view(demo4, "\\\\")
    #only this works

  #2
demo5 <- "\"'\\" ; writeLines(demo5)
str_view(demo5, "\"'\\\\")
  #3
matched_patterns_demo <- c(".a.b.cdef", "......", "%.4.!.?", "ab.cd.ef"); writeLines(matched_patterns_demo)
str_view(matched_patterns_demo, "\\..\\..\\..")  #string representation can be viewed as second argument to the left
    #the pattern matches patterns like .a.b.c or %.4.!.?

#14.3.2.1
  #1
demo6 <- "$^$"; writeLines(demo6)
str_view(demo6, "\\$\\^\\$")
  #2
    #a)
str_view(words, "^y", match = TRUE)
    #b)
str_view(words, "x$", match = TRUE)
    #c)
str_view(words, "^...$", match = TRUE)
    #d)
str_view(words, "^.......", match = TRUE)

#14.3.3.1
  #1
    #a)
str_view(words, "^[aeiou]", match = TRUE)
    #b)
str_view(words, "^[^aeiou]+$", match = TRUE)
    #c)
str_view(words, "(^|[^e])ed$", match = TRUE)
    #d)
str_view(words, "ing$|ise$", match = TRUE)

  #2
str_view(words, "[^c]ie", match = TRUE)
str_view(words, "[^c]ei", match = TRUE)
    #this shows that typically, ie comes after letters that are not 'c'. There is only one exception.

str_view(words, "cei", match = TRUE)
str_view(words, "cie", match = TRUE)
    #This shows that there are actually more examples of 'cie' than 'cei' in the words sample
    #The rule can't really be verified

  #3
str_view(words, "q[^u]", match = TRUE)
str_view(words, "q", match = TRUE)
    #Yes, q is always followed by a 'u' in this sample

  #4
str_view(words, "ise", match = TRUE)

  #5
demo7 <- c("01575-1294972", "0170-168948", "123-456-789")
str_view(demo7, "^0\\d\\d\\d*-\\d\\d\\d\\d")


#14.3.4.1
  #1
demo8 <- c("aaabbbccc", "aabbccdd", "abcde")
    #The {m,n} equivalent to ? is {0,1}
str_view_all(demo8, "aa(a)?b")        #'?' matches 0 or 1 occurences of last character
str_view_all(demo8, "aa(a){0,1}b")
    #The {m,n} equivalent to + is {1,}
str_view_all(demo8, "aab+")
str_view_all(demo8, "aab{1,}")
    #The {m,n} equivalent to * is {0,}
str_view_all(demo8, "^aa*")
str_view_all(demo8, "^aa{0,}")

  #2
demo9 <- c("123", "abc", "d{e}f", "g{hl}m", "n{}o", "x01234-56-78x", "123-456-789", "\\\\\\\\"); writeLines(demo9)
    #a)
      #matches anything
str_view_all(demo9, "^.*$")
    #b)
      #matches curly brackets that contain at least one character
str_view_all(demo9, "\\{.+\\}")
    #c)
      #matches a code with four digits, then 2 digits and then another 2 digits, each chunk separated by '-'
str_view_all(demo9, "\\d{4}-\\d{2}-\\d{2}")
    #d)
      #matches 4 consecutive backslashes
str_view_all(demo9, "\\\\{4}")

  #3
    #a)
str_view_all(words, "^[^aeiou]{3}", match = TRUE)
    #b)
str_view_all(words, "[aeiou]{3}", match = TRUE)
    #c)
str_view_all(words, "([aeiou][^aeiou]){2,}", match = TRUE)

  #4
    #https://regexcrossword.com/challenges/beginner

#14.3.5.1
  #1
    #a)
demo10 <- c("aaabbcc", "anna", "noabbano")
      #matches any string where a particular character appears three times in a row

str_view_all(demo10, "(.)\\1\\1")
    #b)
      #matches a string where a first character is followed by a double character, followed by the first character again
str_view_all(demo10, "(.)(.)\\2\\1")
    #c)
      #matches a combination of 2 letters appearing two times in a row
str_view_all(words, "(..)\\1", match = TRUE)
    #d)
      #matches a specific character, followed by any character, followed by the specific character, followed by any character, followed by the specific character again
str_view_all(words, "(.).\\1.\\1", match = TRUE)
    #e)
      #matches a combination of three characters that at some point occurs again in reversed order
str_view_all(words, "(.)(.)(.).*\\3\\2\\1", match = TRUE)

  #2
    #a)
str_view_all(words, "^(.).*\\1$", match = TRUE)
    #b)
str_view_all(words, "(..).*\\1", match = TRUE)
    #c
str_view_all(words, "(.).*\\1.*\\1", match = TRUE)


#14.4.1.1
  #1
    #a)
str_subset(words, "^x|x$")
c( str_subset(words, "^x"), str_subset(words, "x$"))
    #b)
str_subset(words, "^[aeiou].*[^aeiou]$")
starts_with_vowel   <- str_detect(words, "^[aeiou]")
ends_with_consonant <- str_detect(words, "[^aeiou]$")
words[starts_with_vowel & ends_with_consonant]
    #c)
str_subset(words, "f")
conta <- str_detect(words, "a")
conte <- str_detect(words, "e")
conti <- str_detect(words, "i")
conto <- str_detect(words, "o")
contu <- str_detect(words, "u")
words[conta & conte & conti & conto & contu]
        #There are no words in this sample that contain all 5 vowels

  #2
vowel_count <- str_count(words, "[aeiou]")
words[vowel_count >= max(vowel_count)]
    #There are 8 words with 5 vowels
vowel_perc <- vowel_count / str_length(words)
words[vowel_perc >= max(vowel_perc)]
    #There is only "a" with 100% vowels
words[dense_rank(desc(vowel_perc)) <=2]
    #2 more words contain 75% vowels


#14.4.2.1
  #1
regex_simple <- "red|orange|blue|yellow|green|purple"
regex_adv <- "[ \\.](red|orange|blue|yellow|green|purple)[ \\.]"
regex_clever <- "\\b(red|orange|blue|yellow|green|purple)\\b"
str_view_all(sentences, regex_simple, match = TRUE)
    #as can be seen, red often appears in the text as part of a longer word.
str_view_all(sentences,regex_adv, match = TRUE)
    #Problem seems to be gone
str_view_all(sentences, regex_clever, match = TRUE)
    #same result
matches_simple <- str_detect(sentences, regex_simple); sum(matches_simple)
matches_adv <- str_detect(sentences, regex_adv); sum(matches_adv)
matches_clever <- str_detect(sentences, regex_clever); sum(matches_clever)
    #There seem to be a lot of mismatches
sentences[matches_simple == T & matches_adv == F]
    #All 28 sentences where red was falsely identified as color in the simple regex 

  #2
    #a)
(first_word <- str_extract(sentences, "^[^ ]*")); any(is.na(first_word))
      #Every first word is extracted. There is no NA in the vector (so presumably everything went well).
    #b)
      #Alternative 1:
ending_ing <- str_detect(sentences, "[^ ]*ing[ \\.]"); sum(ending_ing)
str_extract_all(sentences[ending_ing], "[^ ]*ing", simplify = T) #only one column so no sentence contains two words ending in 'ing'.
      #Alternative 2:
ending_in_ing <- str_detect(sentences, "\\b[A-Za-z]*ing\\b")
str_extract_all(sentences[ending_in_ing], "\\b[A-Za-z]*ing\\b", simplify = T)
      #They differ in the handling of the sample "king's"
sentences[ending_ing == F &  ending_in_ing == T]

    #c)
      #heuristic: ends with 's' end has more than 3 letters (unfortunately this still includes a lot of verbs such as 'makes')
contains_plural <- str_detect(sentences, "\\b[A-Za-z]{3,}s\\b")
str_extract_all(sentences[contains_plural], "\\b[A-Za-z]{3,}s\\b", simplify = T)


#14.4.3.1
  #1
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
             "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
numbers2 <- numbers %>%
  c(str_to_title(numbers)) %>%
  str_c(collapse = "|")
numbers3 <- str_c("\\b(", numbers2, ")\\b \\b([^ ]+)\\b" ); writeLines(numbers3)

contains_number <- str_detect(sentences, numbers3)
sentences[contains_number] %>%
  str_match_all(numbers3)

  #2
apost <- "\\b([^ ]+)\\b'\\b([^ ]+)\\b"

contains_apost <- str_detect(sentences, apost)
sentences[contains_apost] %>%
  str_match(apost)


#14.4.4.1
  #1
toreplace <- "This/is/a/string"; writeLines(toreplace)
str_replace_all(toreplace, "/", "\\\\")
  #2
replacements <- c(
  "A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
  "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j",
  "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o",
  "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t",
  "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y",
  "Z" = "z"
)
str_replace_all(sentences, pattern = replacements) %>% head()
  #3
(changed <- str_replace_all(words, "(^.)(.*)(.$)", "\\3\\2\\1"))
intersect(changed, words)
    #There are 45 words that definitely are still words when first and last letter are swapped.

str_split(sentences, " ", simplify = T) %>% head()


#14.4.5.1
  #1
to_split <- "apples, pears, and bananas"
str_split(to_split, boundary("word"))
str_split(to_split, ", (and)?", simplify = T)
  #2
    #Splitting by " " returns unwanted punctuation marks
str_split(to_split, " ", simplify = T)
str_split(to_split, boundary("word"), simplify = T)
  #3
str_split(to_split, "")
    #Every single character is returned as an individual element
?str_split
    #This is equivalent to setting boundary("character")
str_locate(words, "ab") %>% head()

str_view_all(sentences, regex("it's", ignore_case = T), match = T)


#14.5.1
  #1
demo11 <- c("\\test", "\\newtest");writeLines(demo11)
      #using regex
str_view_all(demo11, "\\\\")
      #using fixed
str_view_all(demo11, fixed("\\"))

  #2
str_split(sentences, boundary("word"), simplify = T)  %>%
  as.tibble() %>%
  gather() %>%
  mutate(word = str_to_lower(value)) %>%
  count(word, sort = T) %>%
  filter(word != "")

#14.7.1
    #TBD


# Chapter 15 - Factors
#15.3.1
  #1
library(tidyverse); library(forcats)
gss_cat %>% ggplot(aes(rincome)) + geom_bar()
    #the default plot is hard to read because of the high amount of included factors.
    #This can be tackled with a coordinate flip
gss_cat %>%
  ggplot(aes(rincome)) + geom_bar() + coord_flip()
    #More intuitively, higher incomes should be displayed on top (relevel)
  #2
count(gss_cat, relig, sort = T)
    #Protestant is the most common religion
count(gss_cat, partyid, sort = T)
    #Independent is the most common party affiliation
  #3
gss_cat %>%
  filter(denom != "No answer", denom != "Don't know", denom != "No denomination", denom != "Not applicable") %>%
  count(relig)
    #denom only applies to Protestants
gss_cat %>% ggplot() + geom_bar(aes(x = relig, fill = denom)) + coord_flip()
gss_cat %>% ggplot() + geom_count(aes(denom, relig))


#15.4.1
  #1
?gss_cat
    #tv hours measures the hours per day watching TV
gss_cat %>%
  count(tvhours) %>%
  arrange(desc(tvhours))
    #22 occurences of 24 hours of TV watching. This might be a data entry error or survey takers misunderstanding the question.
      #one could filter these out or just calculate the median instead of the mean; e.g.:
gss_cat %>%
  filter(!is.na(tvhours)) %>%
  group_by(partyid) %>%
  summarise(meantv = mean(tvhours),
            mediantv = median(tvhours))
      #for some levels, the difference between mean and median is sizeable.
  #2
str(gss_cat)
attach(gss_cat)
    #There are 6 factors:
levels(marital)    #kind of arbitrary
levels(race)       #arbitrary
levels(rincome)    #principled: ordered by income
levels(partyid)    #principled: ordered by strength of affiliation to republicans/democrats
levels(relig)      #arbitrary
levels(denom)      #arbitrary
detach(gss_cat)
  #3
gss_cat %>%
  group_by(rincome) %>%
  summarise(median_age = median(age, na.rm = T)) %>%
  mutate(rincome = relevel(rincome, "Not applicable")) %>%
  ggplot(aes(median_age, rincome)) + geom_point() + theme_bw()
    #The y-axis displays the factors from bottom to top in ascending order.
    #Thus, the first value 'Not applicable' is at the bottom.


#15.5.1
  #1
levels(gss_cat$partyid)
gss_cat %>%
  mutate(partyid2 = fct_collapse(partyid,
                              "Rep" = c("Strong republican", "Not str republican"),
                              "Dem" = c("Strong democrat", "Not str democrat"),
                              "Ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                              "Other" = c("No answer", "Don't know", "Other party"))) %>%
  count(year, partyid2) %>%
  group_by(year) %>%
  mutate(prop =  n/ sum(n)) %>%
  ggplot(aes(year, prop, col = fct_reorder2(partyid2, year, prop))) + geom_line()
    #Trends of decreasing number of republican and increasing number of independent voters

  #2
levels(gss_cat$rincome)
    #you can collapse into 'unknown', 'high income', 'medium income' and 'low income' or any other arbitrarily chosen levels; e.g.
gss_cat %>%
  mutate(rincome2 = fct_collapse(rincome,
                             "Low Income" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
                             "Medium Income" = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999"),
                             "High Income" = c("$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more"),
                             "Unknown" = c("No answer", "Don't know", "Refused", "Not applicable")),
         partyid2 = fct_collapse(partyid,
                                 "Rep" = c("Strong republican", "Not str republican"),
                                 "Dem" = c("Strong democrat", "Not str democrat"),
                                 "Ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                                 "Other" = c("No answer", "Don't know", "Other party"))) %>%
  ggplot(aes(rincome2, fill = partyid2)) + geom_bar()


# Chapter 16 - Dates
library(tidyverse); library(lubridate); library(nycflights13)
#16.2.4
  #1
ymd(c("2010-10-10", "bananas"))
    #the valid date is parsed correctly. The invalid date is translated as NA.
    #A warning message appears
  #2
?today
?now
today("CET"); today("NZ")
now("CET"); now("NZ")
    #The timezone argument determines what timezone should be considered when returning the current date
    #This is important when working with different operating systems across various locations
Sys.timezone()  #returns the system's timezone
head(OlsonNames())    #returns all known timezones to the OS
  #3
d1 <- "Januar 1, 2010"
mdy(d1)
d2 <- "2015-Mar-07"
ymd(d2)
d3 <- "06-Jun-2017"
dmy(d3)
d4 <- c("August 19 (2015)", "Juli 1 (2015)")
mdy(d4)
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)


#16.3.4
  #1
date_time100 <- function(year, month, day, hourmin) {
  hour <- hourmin %/%100
  min <- hourmin %% 100
  date <- make_datetime(year, month, day, hour, min)
  return(date)
}

flightsn <- flights %>%
  filter(!is.na(dep_delay), !is.na(sched_dep_time), !is.na(arr_delay), !is.na(sched_arr_time)) %>%
  mutate(dep_time2 = date_time100(year, month, day, dep_time),
         sched_dep_time2 = date_time100(year, month, day, sched_dep_time),
         arr_time2 = date_time100(year, month, day, arr_time),
         sched_arr_time2 = date_time100(year, month, day, sched_arr_time))

flightsn %>%
  mutate(month = month(dep_time2, label = T)) %>%
  ggplot(aes(hour)) +
  geom_bar() +
  facet_wrap( ~ month)
    #a trend can't be derived visually from this
library(viridis)
flightsn %>%
  mutate(y.day = yday(dep_time2)) %>%
  select(y.day, hour) %>%
  count(y.day, hour) %>%
  ggplot(aes(y.day, hour, fill = n)) +
           geom_tile() +
  scale_fill_viridis(option = "D")
    #there isn't much to take away from this graph either. Two observations are that
      #there was one day (maybe two) without flights before 9 am and after 16pm
      #there is an increase in flights departing between 8 and 9 am close to the end of the year
  
  #2
flightsn %>%
  select(dep_time2, sched_dep_time2, dep_delay) %>%
  mutate(dep_delay2 = as.integer((dep_time2 - sched_dep_time2) / 60)) %>%
  count(dep_delay == dep_delay2)
    #there are differences in 1199 cases

flightsn %>%
  select(dep_time2, sched_dep_time2, dep_delay) %>%
  mutate(dep_delay2 = as.integer((dep_time2 - sched_dep_time2) / 60)) %>%
  filter(dep_delay != dep_delay2) %>%
  mutate(absol = abs(dep_delay) + abs(dep_delay2))
    #There appear to be some flights that wer scheduled to depart late but took off early.
    #the original dep_delay variable counts them as delay
    #the proper substraction counts these as early departures

  #3
flightsn %>%
  select(air_time, dep_time2, arr_time2, origin) %>%
  mutate(air_time2 = arr_time2 - dep_time2,
         diff = air_time2 - air_time) %>%
  filter(diff > -1000) %>%
  ggplot(aes(x = diff, col = origin)) + geom_freqpoly(binwidth = 5)
  group_by(origin) %>%
  summarise(avg_diff = mean(diff))
    #the differene might be related to timezones?
  
  #4
flightsn %>%
  group_by(hour) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(hour, avg_delay)) + geom_col()
    #average delay increases up until 8 pm. Then it decreases.


  #5
    #chance of delay is interpreted as departure delay > 0
flightsn %>%
  mutate(delayed = (dep_delay > 0),
         wday = wday(dep_time2, label = T)) %>%
  group_by(wday) %>%
  summarise(n = n(),
            perc_delayed = sum(delayed)/n) %>%
  ggplot(aes(wday, y = perc_delayed)) + geom_col()
    #Saturday is the day where most flights are not delayed

flightsn %>%
  mutate(delayed = (dep_delay > 0),
         wday = wday(dep_time2, label = T)) %>%
  ggplot(aes(wday, fill = delayed)) + geom_bar() + scale_fill_manual(values=c("blue", "red"))

  #6
tibble(value = union_all(diamonds$carat, as.numeric(flights$sched_dep_time)),
       type = union_all(rep("carat", nrow(diamonds)), rep("sched_dep_time", nrow(flights)))) %>%
  ggplot(aes(value)) + geom_freqpoly(bins = 1000) + facet_wrap(~type, scales = "free", nrow = 2) + theme_bw()
    #they are similar in the sense that particular values appear much more often than others so the distribution 'spikes'
      #e.g. carat: 0.5, 1 & 1.5  and sched_dep_time 6:00, 6:30 & 7:00

  #7
flightsn %>%
  mutate(delayed = (dep_delay > 0),
         minute = minute(dep_time2)) %>%
  group_by(minute, delayed) %>%
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = minute, y = avg_delay, col = delayed)) + geom_line() + scale_color_manual(values=c("blue", "red"))
    #when filtering out the early flights, the average delay is rather consistent across minutes.
    #Thus, the observation that flights leaving in minute 20-30 or 50-60 is merely a reflection of the fact that most flights depart around full or half hours.


#16.4.5
  #1
    #dmonth as a duration is imprecise because months have different number of days (and thus, seconds).
    #month as a period is more intuitive

  #2
flightsn %>%
  select(dep_time2, arr_time2, sched_arr_time2) %>%
  mutate(overnight = (dep_time2 > arr_time2),
         arr_time3 = arr_time2 + days(overnight * 1),
         sched_arr_time3 = sched_arr_time2 + days(overnight * 1)) %>%
  filter(overnight == T)
    #days(overnight * 1) can be parsed into different operations
      #a) overnight * 1 --> overnight is a logical vector.
          #A logical TRUE is stored as a 1 and a logical TRUE is stored as a 0.
          #So the whole expression is 1 for overnight flights and 0 for non-overnight flights
      #b)days(x) --> this command converts a number into an object of class 'period'
          #Objects of class 'period' make it more intuitive & convenient to do mathematical operations with dates

  #3
today() %>%
  floor_date(unit = "years") + months(0:11)

  #4
age <- function(bday = dmy("24.08.1995")) {
  bday %--% today() %/% years(1)
}

  #5
(today() %--% (today() + years(1))) / months(1)
    #TBD

      