library(readxl); library(haven); library(stringr); library(stringi); library(forcats); library(lubridate); library(modelr); library(broom); library(feather); library(purrr); library(magrittr); library(gapminder); library(tidyverse)
install.packages("package", repos='http://cran.us.r-project.org')
#########  Table of Content
    #{jump to line via Alt + Shift + G}
#I      - import/readr      ~ line 20
#II     - tidyr             ~ line 100
#III    - stringr           ~ line 170
#IV     - forcats           ~ line 235
#V      - lubridate         ~ line 255
#VI     - dplyr             ~ line 300
#VII    - joining           ~ line 360
#VIII   - ggplot            ~ line 400
#IX     - Modelling         ~ line 500
#X      - export            ~ line 550
#XI     - Programming       ~ line 600

setwd("C:/Users/psymo/OneDrive/Studium/Statistik/Daten")
remove(list = ls())

###############################################  I import/readr  ###############################################
  #csv's          ~ line 30
  #Excel          ~ line 42
  #SAS/Stata/SPSS ~ line 50
  #others         ~ line 60
  #parsing        ~ line 75
                              # jump to line with {Alt+Shift+G}

#import csv's with automatic column specification
data <- read_csv("Filepath",       #read_csv2 for semicolon seperated files | filename can be an URL
                 skip = 0,         #skip n rows of metadata and descriptions
                 na = c(".","NA"), #all `.`s are imported as NAs
                 guess_max = 1000, #specifies how many rows are used to guess column type
                 col_names = TRUE, #alternatively a vector to allocate a name to each column
                 n.max = Inf)      #limit number of rows to read
?read_csv
#import csv's with manual column specification
read_csv("Filepath", col_types = cols(v1 = col_character(), v2 = col_character))
      #alternatively col_types can be specified via a string character "cindlDTt?-" where each string represents a different type ( e.g. c=character; i = integer)

#import Excel:
library(readxl); data <- read_excel("pers?nlicher Studienverlaufsplan.xlsx",
                                    sheet = 1,
                                    col_names =TRUE, skip = 0, n_max = Inf)




#import SAS/STATA/SPSS:
library(haven)
data <- read_sas("SAS-filename.sas7bdat");data <- read_dta("STATA-filename.dta");data <-read_sav("SPSS-filename.sav")
#Mind factors in imports
data$factor_column <- as_factor(data$factor_column)
  #order factors by different variable
data$factor.variable <- reorder(factor.variable, variable.to.order.by, FUN = mean) #alternatives e.g. FUN = median

#import anything:
data <- read_delim("Filename",delim =",",col_names = TRUE, col_types = NULL,
           skip = 0, n_max = Inf)   #alternatively import can ignore observations at the start and/or be limited

#fixed width files:  read_fwf | fwf_empty | fwf_width | fwf_position | read_table
#import from Databases library(DBI);?DBI
#JSON/hierarchical data: library(jsonlite);?Jsonlite
#read_Apache style log files: read_log()
#other formats: https://cran.r-project.org/doc/manuals/r-release/R-data.html

#for speed:
library(data.table); data <- fread("Income1.csv")



#Parsing into special types of vectors | FOR IMPORTING DATA: CHANGE parse_* TO col_*
  #best guess:
guess_parser("1995-08-24"); parse_guess("1995-08-24")

  #logicals & characters
parse_logical(vector, na = "9999") #na argument specifies which values shall be treated as NAs
parse_character("Rick Astley", locale = locale(encoding = "UTF-8"))  #if not UTF-8 --> guess_encoding("filepath" | vector)

  #numbers
parse_integer(c("1", "2", "-1", "na"), na = c("-1", "na"))
parse_double(c("4", "5.3", "9999"), na = "9999",
             locale = locale(decimal_mark = "."))  #locale to change country specific settings
parse_number(c("5,4", "32.567,1", "It's $4", "300%", "-"), na = "-",
             locale = locale(decimal_mark = ",", grouping_mark = "."))
    #number extracts the number from strings

  #factors
parse_factor(c("apple", "banana", "apple", "0"),
             levels = c("apple", "banana"), ordered = FALSE, na = "0")

  #dates and times
parse_date(c("01.10.2010"), format = "%d.%m.%Y")  #default is "2010-10-01"
parse_datetime("2010-10-01T060520"); parse_time("06:05:20")

#extracting the values that caused problems
newvec <- parse_*(oldvec); problems(newvec)



###############################################  II tidyr  ###############################################
#Basics/Glimpse       ~ line 110
#Gather & Spread      ~ line 120
#column manipulation  ~ line 135
#Missing Values       ~ line 145
#Outliers             ~ line 160
                            # jump to line with {Alt+Shift+G}

#3 Rule for a dataset to be tidy
  #i.   Each variable must have its own column
  #ii.  Each observation must have its own row
  #iii. Each value must have its own cell

#dataframe to tibble
tiblle <- tbl_df(dataframe)

# inspection
glimpse(data);summary(data);head(data);hist(data$variable);plot(data$variable,data$variable)

#tidy data
  #format change:
    #wide--> long (useful for columns that are not variables; e.g. one column for each year --> transformation via Key-Value pairs)
longdata <- gather(data, key = "Key-Variable" , #Name of the new key-Variable, e.g. Years
       value = "value-Variable",                #Name of the new Value-column, i.e. what the values in each cell actually represent
       variables.to.gather,                     #see variable selection via verb 'Select'
       convert = FALSE,                         #if TRUE, new value columns type is guessed
       na.rm = FALSE)                           #if TRUE, resulting rows with missing values will be removed

    #long --> wide (useful for readability)
widedata <- spread(data, key = Key-Variable, #e.g. Years, each value of that variable will be assigned its own column )
                   value = Value-variable,   #the variable that represents what is actually measured
                   convert = FALSE)          #if TRUE, all new columns data types are guessed

  #one column --> two columns (useful when one column contains more than one information, e.g. year & months)
separate(data, column_to_separate,
         into = c("firstnewcolumnname","second_new_column_name"),
         sep = "-",           #alternatively: separation-digit (e.g. 5 to split after 5th digit or -5 to split after 5th digit from right)
         convert = FALSE)     #by default, new columns will be characters; for numbers, convert == TRUE is advisable

  #two columns --> one column
unite(data, new_column_name, old_column_name1, old_column_name2, sep = "")
             #to specify old column names: see variable selection via verb 'Select'

#fill downward
fill(data, column, .direction = "down")  #carries last observation forward (downward)

#Detecting/working with NA's
any(is.na(data));sum(is.na(data))  #show if & and how many NA's are in the dataframe
complete.cases(data)    #shows which observations are 'complete' without NA's
newdata <- na.omit(data) #removes all rows with missing data entirely
mutate(data, variable1 = ifelse(condition, NA, variable1)) #replaces unusual variables with NA's; more complex: case_when
?case_when
NA; NA_integer_; NA_real_; NA_character_    #Beware of different atomic vectors

  #implicit missing values
    #uncover implicit missing values
complete(data, column1, column2, column3) #looks for all unique combinations of specified columns (eg. year, month, day)
                                          #If for a combination no row is found, one will be added with NA's for all other columns


#detecting/working with Outliers
boxplot(data)
hist(data$variable)







###############################################  III stringr  ###############################################
library(stringr)
#Obtain information about strings  ~ line 180
#Maniplating Strings               ~ line 190
#Regular Expressions               ~ line 205
                            # jump to line with {Alt+Shift+G}

?"'"   #lists how to implement 'escapes' as \n (newline), \t (tab) or \\ (backslash)
(char <- c("  düsseldorf  ", "SEOUL", "Bronx", "New Delhi", NA))    #RUN THIS: example string to demonstrate examples below
writeLines("\\")  #writeLines returns the actual string with the escape markers left out

  #obtain information about strings
str_length(char)                                               #returns length of each string
(char_n <- str_replace_na(char))                               #replaces NA with "NA"
str_detect(char, "üss")                                        #returns logical vector which identifies matches
str_count(char, "[Ss]")                                        #returns number of occurences
str_extract(char, "SE")                                        #variation: str_extract_all
    str_extract(str_subset(char, "SE"), "SE")                  #combine with subset to exclude NA's
str_subset(char, "üss")                                        #returns matches only
str_match(char, "([Ss])([A-Za-z]*)")                           #returns whole match as well as every component in match marked by brackets '()'
str_locate(char, "ü")                                          #gives exact positions of match (alternative: str_locate_all)

  #manipulating strings
str_sort(char_n); str_order(char_n)                            #more control with 'locale' argument
str_c(char, char) ; str_c("first", "second", sep = " and ")    #binds strings together
str_c(char_n, collapse = ",")                                  #bind strings within a vector
str_sub(char_n, 1,4); str_sub(char_n, -4,-1)                   #returns particular digits of a vector (- counts from right)
str_to_upper(char_n); str_to_lower(char_n); str_to_title(char_n) #more control with 'locale' argument
str_trim(char, side = "both")                                  #cuts whitespace 
str_pad(char ,width = 20,side = "right", pad="_")              #concatenates specified pad-character until width is reached
str_replace(char, pattern = "ü", replacement = "ue")           #alternative: str_replace_all
    str_replace_all(char, pattern = c("ü" = "ue", "x" = "XXX"))
str_split(char, boundary("word"), simplify = TRUE)             #boundary accepts: "character", "line_break", "sentence" and word
    str_split(char, " ", simplify = TRUE)

#REGULAR EXPRESSIONS (Regexps)
(charn <- c("  düsseldorf  ", "Seoul", "bronx-2X4", "alabama", "new.delhi", "rio\\de  janeiro"))  #RUN THIS: example string to demonstrate examples below
  #View matches of a regular expression in bottom right corner
str_view(charn, "r")              #matches first occurence of regular expression
str_view_all(charn, "a")          #matches all occurences of regular expression

str_view_all(charn, ".e..")       #'.' is a placeholder for any arbitrary character (except \n)
str_view_all(charn, "\\d")        #matches any digit
str_view_all(charn, "\\s")        #matches any whitespace (space, tabs, \n)
str_view_all(charn, "[xü]")       #matches any character within square brackets
str_view_all(charn, "[^ael]")     #matches anything except characters within square bracketsstr_view_all(charn, "\\.")        #matches literal '.' | equivalent for e.g. $ or ^ --> "\\$" and "\\^"
str_view_all(charn, "de.*")       #'*' matches 0 or more occurences of last character
str_view_all(charn, "s+")         #'+' matches 1 ore more occurences of last character
str_view_all(charn, "ss?")        #'?' matches 0 or 1 occurences of last character
str_view_all(charn, "s{2,3}")      #matches expression at least 2 but maximal 3 times
str_view_all(charn, "(e[aeiou])+")# parentheses amplify the validity of '+', '*', '?' & '{n,m} from single characters to whole expressions

str_view_all(charn, regex("s", ignore_case = T))  #removes case sensitivity
str_view_all(charn, "\\\\")       #matches literal '\'
str_view_all(charn, "[.]")        #alternative, more readable version, doesn't work for every character

str_view_all(charn, "^a")         #only matches at the beginning of the string
str_view_all(charn, "l$")         #only matches at the end of a string

#Even more control and functions with stringi
library(stringi); ?stringi




###############################################  IV forcats  ###############################################
library(forcats) #to handle factors
#Creating factors
factor(data, c("level1", "level2"))    #levels should be in appropriate order
#Parsing factor
parse_factor(c("apple", "banana", "apple", "0"),
             levels = c("apple", "banana"), ordered = FALSE, na = "0")
#checking factors levels
levels(factor)
#reorder (by other variable)
fct_infreq(factor)                                                   #reorders by number of occurences of factors
fct_rev(factor)                                                      #reverses factor levels
fct_reorder(factor, variable.to.reorder.by)                          #reorders by other variable (plots using 2 variables)
fct_reorder2(factor, variable.to.reorder.by, grouping.variable)      #sensible e.g. for line plots (with grouping variables --> 3 variables)
#relevel
fct_relevel(factor, "first level", "second level")      #doesn't need to include all levels, just puts mentioned ones at the front
fct_recode(factor, "new level1" = "old level1,",
           "new level1" = "old level2",                 #allows to summarise old groups to single new group
           "new level2" = "old level3")                 #alternatives: fct_collapse when summarising a lot of groups into fewer new ones  & fct_lump (quick & dirty)

###############################################  lubridate  ###############################################
library(lubridate) #to handle times, dates & timezones
today(); now(); Sys.timezone(); head(OlsonNames())
dmy("24.09.1996"); ymd(20190917) #others include myd, ydm, dym, mdy
dmy_hms("24.09.1996 - 13:24:22", tz = "CET")
to_convert <- tibble(year = c("2018", "2019"), month = c("08", "09"), day = c("20", "30"))
    make_date(to_convert$year, to_convert$month, to_convert$day)  #alternative: make_datetime
as_date(now()); as_datetime(today())   #switching between dates and datetimes

#access individual parts of a date
n <- now(); year(n); month(n); mday(n); yday(n); wday(n); hour(n); minute(n); second(n)
month(n, label = TRUE, abbr = TRUE); wday(n, label = TRUE, abbr = FALSE)

#round date
floor_date(vec, unit = "week")    #alternatives: round_date, ceiling_date

#manipulate dates
year(date.vec) <- 2019  #also: month, day, hour... | alternatively via update()

#Time Spans
    #using the class 'duration' that always breaks down time differences in seconds
as.duration(today() - dmy("16.09.2019"))
dseconds(5); dminutes(5); dhours(5); ddays(5); dweeks(5); dyears(5); str(dseconds(1))
now() + dhours(2) - dweeks(2)
    #using the class 'periods' that works with "human dates" (days, month...)
seconds(5); minutes(5); hours(5); days(5); weeks(5); months(5); years(5); str(seconds(1))



















###############################################  VI dplyr  ###############################################

#filter the observations
filter(data, condition1, condition2... ,
       variable %in% c(val1, val2), # variable is either value 1 or value 2 (or value 3...)
       between(variable, lower, upper), # variable is between lower and upper value (only sensible for continuous variable)
       xor(condition1,condition2), #exclusive OR: Either condition 1 is met OR condition 2 (not both!)
       is.na(variable)) #preserves NAs after applying condition

#order the observations
arrange(data, variable.to.order.by, desc(second.variable.to.order.by), third...) # default: ascending || for descending order: desc(variable)

#select the columns
select(data, new_first_column, new_second_column,...,
       1:10,                                                     #selects first 10 columns
       contains("abc"), starts_with("abc"), ends_with("xyz"),    #selects columns based on their given names
       matches("regular expression"),                            #selects columns based on a regular expression
       one_of(character_vector))                                 #selects all columns from names in the character  vector
select(data, -(1:10), -variable_toexclude)                       #selects all columns except those specified with -

select(data, new_first_column, new_second_column, everything())  #REORDERS: selects all columns but those explicitly specified are now in first place
rename(data, new_variable_name = old_variable_name)              #keeps all variables, just RENAMES the specified ones

purrr::keep(data, is.numeric)                                    #keep columns based on condition
purrr::discard(data, is.factor)                                  #remove columns based on condition

#add new variables
mutate(data, newvariablename = mathematical_operation, secondnewvariable = second_mathematical_operation,
       new.v3 = old.v %/% 100, new.v4 = old.v %% 100,      #returns integer division and remainder (in the example 5 & 40 from 540 )
       new.v5 = old.v > 1000,                              #returns logical vector with 1 for those observation where check evaulates to TRUE
       new.v6 = lead(old.v, 1), new.v7 = lag(old.v, 1),    #gives lags and leads
       new.v8 = cumsum(old.v), new.v9 = cummin(old.v))     #gives cumulated sum/minimum - other functions are cummax, cummean, cumprod
transmute(data, newvariablename = mathematical_operation, ...) #like mutate but just returns the new variables

#Summarise data
summarise(data, aggregated.variable = aggregate.function)   #e.g. mean(old.variable, na.rm = TRUE)

#grouping | summarizing
data %>%
  group_by(variable1.to.group.by, variable2.to.group.by) %>%
  summarise(aggregated.variable = aggregate.function,                            #below there are examples of aggregate functions
            count = n(), nv = n_distinct, nv = sum(other.variable),              #returns the number of (distinct) observations /the sum within each group
            nv = mean(other.variable),                                           #returns the mean of another variable for each group
            nv = min(ov), nw = max(ov), nv = median(ov), nv = quantile(ov, q),   #returns minimum, maximum, median, quantile for each group
            nv = sd(ov), nv = IQR(ov), nv = mad(ov),                             #returns standard deviation, Interquartile Range, median absolute deviation
            nv = mean(ov[condition]),                                            #subsetting: Returns some aggregate function given that a condition holds
            nv = n_something = sum(ov > 0), perc_something = mean(ov > 0))       #get sum or percentage of something

#shortcut for counting
data %>%
  count(variable, sort = FALSE)  #%>%
  #count(cut_width(variable, 0.5))    #to segment a continuous variable into different bins








###############################################  VII Joining  ###############################################

#Joining
  #standard join
left_join(primary_dataset, secondary_dataset,
          by = "unique_key",              #unique_key can be a single variable or a combination of variables
          suffix = c("_suffix_primary_dataset", "_suffix_secondary_dataset"))  #attaches to same named variables

left_join(primary_dataset, secondary_dataset,
          by = c("firstkey" = "secondkey"))   #in case the keys don't match

  #joining only full matches, i.e. only observations that are available in both data sets are displayed
inner_join(primary_dataset, secondary_dataset, by = "unique_key")

    #full join, i.e. all observations from both data sets are displayed (produces most NA's)
full_join(first_dataset, second_dataset, by = "unique_key")

  #identify full matches without joining data from the second data frame
semi_join(primary_dataset, secondary_dataset, by= "unique_key")

  #identify observations that don't match with the secondary dataset
anti_join(primary_dataset, secondary_dataset, by= "unique_key")

#combining equal data sets
  #gives all observations from both datasets but doesn't duplicate them
union(first_dataset, second_dataset)

  #gives only observations that appear in both datasets
intersect(first_dataset, second_datset)

  #gives observations that appear only in the first dataset, not in the second
setdiff(first_dataset, second_datset)


#apply verbs to more than one dataset
reduce(list_of_dataframes, verb)  #useful e.g. for union, intersects or joins
    #alternative: accumulate



###############################################  VIII ggplot2  ###############################################

#General Template               ~ line 415
#Scatterplot                    ~ line 425
#Bar Plot                       ~ line 440
#Histogram                      ~ line 450
#Lines (Time Series)            ~ line 460
#recommended Geoms by data type ~ line 470
#create color palette           ~ line 490
                                          # jump to line with {Alt+Shift+G}





#general TEMPLATE
ggplot(data = DATA) + 
  GEOM_FUNCTION(aes(MAPPINGS), stat = STAT, position = POSITION) +
  COORDINATE_FUNCTION + #coord_flip / coord_polar / coord_cartesian {to manually determine mins and maxs of x or y axis}
  labs(x = "x-Axis", y = "y-Axis", title = "Title", subtitle = "Subtitle", caption = "Caption") +
  geom_hline()   +  #reference line (alternatively geom_vline, geom_rect for rectangles, geom_segment for an arrow)
  scale_y_continuous(breaks = seq(0, 100, by = 10))  +  #scale_x_continuous, scale_x_discrete, scale_x_date | hide scale with labels = NULL
  scale_color_brewer() +   #ideal for colorblind follks | for continuous: use scale_color_viridis from viridis package
  FACET_FUNCTION + #facet_wrap / facet_grid
  theme(legend.position = "bottom") #suppress legend with "none" | furhter arguments via guides() along with guide_legend and guide_colorbar


#Scatterplot
ggplot(data,aes(x=variable, y=variable, color=categorical.variable, size=variable)) + #shape, fill (only with shape between 21&25)
  geom_point(position = jitter, size=2, shape = 1, alpha = 0.7) +  #color, fill, other position: identity (default)
  #scale_x_log10() +    #scales x-variable in logarithmic form
  #scale_y_log10() +
  #facet_wrap(~ variable, nrow = 1) +  # creates several plots: one for each value of the variable |facet_grid
  #ggtitle("title name") + xlab("name for x-Axis") + ylab("name for y-Axis") +
  #geom_smooth() +     #adds a line to a scatterplot (e.g. linear regression)
  #scale_fill_manual ("legend name", values=c("#E41A1C", "#377EB8"), labels=c("first.name", "second.name"))
  theme_bw()




#bar plot
ggplot(data,aes(x=categorical, fill=categorical)) +
  geom_bar(aes(y = ..count..), position="dodge") + #other positions: stack (default), fill
  #geom_bar(aes(y = count.variable)) # if the counted values are already calculated in the data
  #coord_flip()  + #changes x- and y-axis
  theme_bw()
      #alternatively: geom_col if y values are already computed



#Histogram
ggplot(data,aes(x=continuous)) +   #for density aes(y=..density..)
  geom_histogram(bins=15,fill="#101090") + #other positions are possible, but not necessarily very useful
  #coord_cartesian(ylim = c(-1, 100)) + #in order to find outliers in large datasets
  theme_bw()
    #for different categorical histograms use e.g. geom_freqpoly




#Lines (Time Series)
ggplot(data,aes(x=time_variable,y=continuous_variable)) +  #col = categorical requires long data format (i.e. the same year appears in several rows)
  geom_line() +
  #geom_rect(aes(xmin=single value or vector,xmax = single value or corresponding vector, ymin= -Inf, ymax = Inf),
  #inherit.aes = FALSE, alpha = 0.2) #fill argument changes the color of the rectangles

  
  
  
  
#two-dimensional geoms:
  #for two continuous variables
geom_point(data, aes(x = continuous1, y = continuous2), alpha = 1)  #good for small data sets
geom_bin2d(data, aes(x = continuous1, y = continuous2), bins = c(30,30)) # good to prevent overplotting / big data sets
geom_boxplot(data, aes(x = continuous2bebinned, y = continuous1,      #transform variable on x axis to a categorical variable
                       group = cut_width(continuous2bebinned, width = 0.2),  #0.2 represents the specified binwidth | alternatviely: cut_number()
                       varwidth = T))                 #so thatwidth of boxplots depends on amount of observations within a bin

  #for two categorical variables
geom_count(data, aes(x = categorical, y = categorical))
geom_bar(data, aes(x = categorical, fill = categorical), position = "stack")
data %>%
  count(categorical1, categorical2) %>%
  ggplot(aes(x = categorical1, categorical2, fill = n)) + geom_tile()

  #for continuous and categorical variable
geom_boxplot(data, aes(x = categorical, y = continuous))
geom_freqpoly(data, aes(x = continuous, color = categorical, y = ..count.., binwidth = 100)) #alternatively: y = ..density..
geom_violin(data, aes(x = categorical, y = continuous))

#create color palettes:
col.pal <- colorRampPalette(c("#800000","#0000FF"))  #here red to blue
munsell::plot_hex(col.pal(9)) # Quick and dirty plot
  #scale_fill_manual(values=col.pat(9)) +  #adjust number to the number of categories e.g. in a bar plot with position= "fill"





# IX - Modelling ---------------------------------------------------------------
library(modelr); options(na.action = na.warn)
    #modelr wraps around base r but makes modeling functions pipeable

  #generate all possible combinations of variables (base r alternative: expand_grid)
data_grid(data, variable1, variable2, variable3)
    #when all combinations are to much, for continuous variable: use seq_range
data_grid(data, x1 = seq_range(variable1, 10, pretty = TRUE),   #pretty gives nice, rounded values
                x2 = seq_range(variable2, 10, trim = NULL))     #trim cuts off outliers (put percentage as argument instead of NULL)
                                                                    #opposite to trim: expand

  #add predictions and residuals to a dataframe
data %>%
  add_predictions(regression_results) %>%   #regression results e.g. from lm or glm
  add_residuals(regression_results)
      #alternative for more than one model: spread_predictions / gather_predictions
      #spread_residuals / gather_residuals

  #when formula technique is not supported:
model_matrix(mpg, cty ~ hwy + displ)

#turn many models into tidy data frames
library(broom)
(demo <- list(mod1 = lm(hwy ~ cty, data = mpg),
              mod2 = lm(cty ~ hwy, data = mpg)) %>%
    enframe())
  #glance to obtain summary statistics
demo %>%
  mutate(model.summary =  map(value, glance)) %>%
  unnest(model.summary)
  #tidy to obtain coefficient matrix
mod1 = lm(hwy ~ cty + trans, data = mpg)
tidy(mod1)
  #augment to attach extra values to data frame that was used to create the model
augment(mod1, mpg) %>%
  as.tibble()














# X - Exporting Files -----------------------------------------------------


#writing tables
library(feather); write_feather(data.frame, "filepath") #recommended; read with read_feather
write.csv(data.frame, "filepath", append = FALSE) #write_excel_csv() for csv's to be read with Excel
write_rds(data.frame, "filepath")  #R's custom RDS format


#writing plots
ggsave("filename.png", width = unit(10, "cm"), height = unit(6, "cm"))







































# XI - Programming --------------------------------------------------------
library(magrittr); library(tidyverse); library(purrr)

which()  #gives position of TRUE in a logical vector

  #if statements
?`if`
case_when()   #for chained if statements
?cut          #use cut() to make continuous variable discrete

#TRUE and FALSE
    # with &&: all conditions must evaluate to TRUE
if (TRUE && TRUE)  {"condition is TRUE"} else {"condition is FALSE"}
if (TRUE && FALSE) {"condition is TRUE"} else {"condition is FALSE"}
    #with ||: only one conditions must evaluate to TRUE
if (TRUE || FALSE) {"condition is TRUE"} else {"condition is FALSE"} 

    #evaluate TRUE and FALSE for a whole vector
all_true <- c(T, T, T); all_false <- c(F, F, F); mixed <- c(T, F)
any(mixed); !any(mixed); any(all_false)
all(all_true); !all(all_true); all(mixed)

    #check for identity of a whole vector
identical(0, "0")   #identical is very strict
?dplyr::near        #less strict


#Functions
  
  #Is the function PIPEABLE? | should the function be PIPEABLE?
    #e.g. use invisible in last line when just printing information
?invisible  #output doesn't get printed out
  
  #...
    #let ... be an argument so it can be passed down to functions called in further functions
coefficients <- function(...) {
  lm(...)$coefficient
}
coefficients(carat ~ x, data = diamonds)

  #Stop functions
    #stop if something specific occurs
wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean(c(1,2), c(1,2,3))

    #stop if something is not in the format it should be:
?stopifnot

#vectors
  #check vector type: is_logical; is_integer; is_double; is_numeric; is_character; is_atomic; is_list, is_vector (which includes lists)
  #also: is_scalar_logical / is_scalar_integer ... check for type and that the length is one
#naming vectors
purrr::set_names(vector, c("x","y", "z", "a", "b"))

  #subsetting
x <- c("a" = 1, "b" = 2, "c" = 3, "d" = NA)
    #using integers
x[c(1,3,3,3,3,3,3)] ; x[-2]
    #using logical vector (returns only TRUE's)
x[is.na(x)]
    #using names
x[c("a", "d", "d", "d", "d")];
    #using [[]] to extract a single element while dropping any name

    #subsetting lists
li <- list(a = 1:5, b = list(1, TRUE, "shitty"), c = "This is a test")
      #using [] - works with integer, logical and character vectors but always returns a (sub-) list.
li["a"] ; li[-2]
      #using [[]] to remove a level of hierarchy
li[["a"]]; li[[2]]
      #using $
li$c

    #check attributes of a vector
attributes(tibble(1))

#Iteration

  #for loop TEMPLATE
res <- vector("double", ncol(data))     # 1. alternatives: "integer", "character"... | numeric(0)
for (i in seq_along(data)) {            # 2. sequence
  res[[i]] <- median(data[[i]])        # 3. body
}
    #when length of single outputs is unknown:
means <- c(-10,0,10)
res <- vector("list", length(means))

for (i in seq_along(res)) {
  n <- runif(1, 1,10) %>%
          round()
  res[[i]] <- rnorm(n, means[[i]])
  
}
str(res) ; unlist(res) #alternatives to unlist:
                          #stricter (just works when all lists are doubles): purrr:flatten_dbl()
                          #for character vectors: str_c(res, collapse = "")
                          #to bind rows: dplyr::bind_rows(res)

#wrappers
  #base R: apply, lapply, tapply
  #purrr: map, map_lgl, map_int, map_dbl, map_chr

  #iterate over 2 vectors: map2, map2_lgl, map2_int, map2_dbl & map2_chr
  #iterate over more than 2 vectors: create a list or df comprising all vectors and use pmap()
  #Advanced: apply different functions for each iteration: invoke_map

  #keep and discard to select columns
discard(iris, is.factor)
    #safely in order to look atfor which elements the loop failed to apply the respective function
demo <- list(100, TRUE, "a")
map(demo, safely(log))
map(demo, safely(log)) %>% transpose()
    #alternatives to safely:
      #possibly() to substitute errors with e.g. NA
      #quietly() captures printed output, warnings & messages instad of errors

  #don't return but invoke side effects: walk(), walk2() & pwalk()
  #very specific: some(), every(), detect(), detect_index(), head_while(), tail_while()


  #apply verbs to more than one dataset
reduce(list_of_dataframes, verb)  #useful e.g. for union, intersects or joins
    #alternative: accumulate




# Nested Data Frames ------------------------------------------------------

  #1. Create Nested Data Frames
    #a) using nest()
data %>%
  group_by(variable1, variable2) %>%    #variable1 and 2 will remain a column in the nested df
  nest()
data %>%
  nest(variable3, variable4)   #variable3 and 4 will be hidden in the df in the list column
      #see select for variable selection (~line 310)

    #b) using enframe() when you want to 'kinda transpose' existing lists
mpg %>%
  split(mpg$cyl) %>%
  enframe()

    #c) using functions that return lists
as.tibble(sentences) %>%
  mutate(words = str_split(value,pattern =  " "))
    
    #d) from listed summaries
mpg %>%
  group_by(cyl) %>%
  summarise(summ = list(summary(hwy)))  #important that the rhs must be listed by list()



  #2. apply functions and create intermediate lists in nested data frame
    #usually, you'll need map, map2 or pmap to do this
      #Example:
        #Creating nested df
library(gapminder)
by_country <- gapminder %>%
  mutate(year2 = year - mean(year)) %>%
  group_by(country, continent) %>% 
  nest()
        #defining functions that will be applied to every row in the nested df
slm <- function(df) {
  lm(lifeExp ~ year2, data = df)
}
elm <- function(df) {
  lm(lifeExp ~ poly(year2, 2), data = df)
}
        #applying functions to every row of the nested df
(by_country <- by_country %>%
    mutate(slm_model = map(data, slm),
           elm_model = map(data, elm),
           resid = pmap(.l = list(data, slm_model, elm_model), .f = gather_residuals),
           pred =  pmap(.l = list(data, slm_model, elm_model), .f = gather_predictions)))

  #3. unnest
        #for the examples execute code above to obtain 'by_country'
    #a) using unnest()
by_country %>%
  unnest(resid, pred)
      #the above code
        #keeps all vector columns
        #unnests the specified list columns (must be same length)
        #deletes the not specified list columns
by_country[,1:3] %>%
  unnest()
      #the above code kees all vector columns and unlists all list columns

    #b) using mutate with map_lgl, map_int, map_dbl or map_chr to add a vector column
by_country[,1:4] %>%
  mutate(length = map_chr(slm_model, typeof))


?end
#########################################################
q()
