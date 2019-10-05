#Hierarchy (in order to ease searching via Strg + f):

# Part I - xxx
# Chapter 1 - Introduction

# 1.1.3 -------------------------------------------------------------------



# Part V - Communicate
# Chapter 28 - Graphics for Communication
library(tidyverse); library(viridis)


# 28.2.1 ------------------------------------------------------------------
library(ggbeeswarm)
  #1
ggplot(mpg, aes(cyl, cty)) +
  geom_beeswarm(aes(col = drv)) +
  labs(x = "Cylinders", y = "City Miles per Gallon",
       title = "Cars with less cylinders have better fuel efficiency",
       subtitle = "Four wheel drives have the worst",
       caption = "Data from fueleconomy.gov?") +
  scale_color_discrete("Drive Type", labels = c("4 Wheel Drive", "Front Drive", "Rear Drive"))

  #2
    #a) display a smoothing line for every class
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(col = class)) +
  geom_smooth(se = F, aes(col = class))
    #b) remove 2 seaters from analysis
mpg %>%
  filter(class != "2seater") %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(col = class)) +
  geom_smooth(se = F)
    #c) make an extra graph for sports cars
mpg %>%
  mutate(sport = (class == "2seater")) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(col = class)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~sport)

set.seed(Sys.time())
  #3
    #added a title and renamed x and y variables at the end of the function
    #added seed functionality and subtitle that prints a seed
arma <- function(alpha = NULL, beta = NULL, length = 100, deviation = 1, drift = 0, times = 1, seed = Sys.time()) {
  library(tidyverse)
  set.seed(seed)
  lags.a <- ifelse(is.matrix(alpha), ncol(alpha), length(alpha))
  lags.b <- ifelse(is.matrix(beta) , ncol(beta ), length(beta ))
  lags <- max(lags.a, lags.b)
  
  plotdata <- matrix(nrow = length * times, ncol = 3)
  plotdata[,1] <- rep(1:length,times)
  
  m.data <- matrix(nrow = (lags + length), ncol = 2)
  m.data[1:lags, ] <- 0
  if (length(alpha) == 0) {alpha <- 0}
  if (length(beta ) == 0) {beta  <- 0}
  beta <-  matrix(rep(t(beta ), length.out = times * lags.b), nrow = times, byrow = T)
  alpha <- matrix(rep(t(alpha), length.out = times * lags.a), nrow = times, byrow = T)
  deviation <- rep(deviation, length.out = times)
  descr <- NULL
  
  for (j in 1:times) {
    plotdata[(1 + length * (j-1) ) : (length * j),
             3] <- rep(j, length)
    errors <- rnorm(length,0, deviation[j])
    m.data[-(1:lags),2] <- errors
    
    for (i in (lags + 1): (lags + length)) {
      vec.a <- m.data[(i-lags.a) : (i-1), 1]
      cum.propensity.a <- sum(rev(vec.a) * alpha[j,])
      if (is.na(cum.propensity.a)) {cum.propensity.a <- 0}
      
      vec.b <- m.data[(i-lags.b) : (i-1), 2]
      cum.propensity.b <- sum(rev(vec.b) *  beta[j,])
      if (is.na(cum.propensity.b)) {cum.propensity.b <- 0}
      
      m.data[i,1] <- cum.propensity.a + cum.propensity.b + m.data[i,2] + drift
    }
    plotdata[(1 + length * (j-1) ) : (length * j),
             2] <- m.data[-(1:lags),1]
    
    descr <- append(descr, paste0("Run: ", j,
                                  "\nDeviation: ", deviation[j],
                                  "\nAR-Lag(s): ", toString(alpha[j,]),
                                  "\nMA-Lag(s): ", toString( beta[j,]),"\n" ))
    
  }
  colnames(plotdata) <- c("Time", "Y", "run")
  plotdata <- plotdata %>%
    as_tibble() %>%
    mutate(run = as.factor(run))
  
  if(lags.a == 0) {
    header <- paste0("MA(", lags.b)
  } else if (lags.b == 0) {
    header <- paste0("AR(", lags.a)
  } else {
    header <- paste0("ARMA(", lags.a,",",lags.b)
  }
  
  
  graph <-   ggplot(plotdata,aes(x=Time,y=Y, col = run)) + 
    geom_abline(slope = drift, intercept = 0, color = "grey") +
    geom_line(size = 0.8) +
    theme_bw() +
    labs(x = "Time Variable", y = "Value",
         title = str_c("Simulation of ", times, " ", header, ") processes"),
         subtitle = str_c("Used Seed: ", seed)) +
    scale_color_discrete(name = paste0(header,") Process\n"),
                         labels = descr)
  
  return(graph)
}
arma(alpha = 1, beta = c(1, 0.5), length = 1000, deviation = 1:4, times = 4)



# 28.3.1 ------------------------------------------------------------------

  #1
corners <- tribble(~displ, ~hwy, ~label,        ~vjust,   ~hjust,
                   Inf,    Inf, "Top Right",    "top",    "right",
                   Inf,   -Inf, "Bottom Right", "bottom", "right",
                   -Inf,   Inf, "Top Left",     "top",    "left",
                   -Inf,  -Inf, "Bottom Left",  "bottom", "left")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_text(data = corners, aes(label = label, vjust = vjust, hjust = hjust))

  #2
?annotate
    #annotate allows to specify the position directly without using aesthetics mappings
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  annotate("text", x = max(mpg$displ), y = max(mpg$hwy), label = "Top Right")

  #3
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_text(data = corners, aes(label = label, vjust = vjust, hjust = hjust)) +
  facet_wrap(~drv)
    #by default, the texts are displayed in all graphs.
    #In order to individualise the text in each facet, the facetting variables must be passed to the annotation data
corners_12 <- cbind(rbind(corners, corners, corners), drv = rep(c("4", "f", "r"), each = 4)) %>%
  mutate(label = str_c(label, drv, sep = " "))

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_text(data = corners_12, aes(label = label, vjust = vjust, hjust = hjust)) +
  facet_wrap(~drv)

  #4
?geom_label
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_label(data = corners, aes(label = label, vjust = vjust, hjust = hjust),
             label.padding = unit(0.8, "lines"), label.size =  1, label.r = unit(1, "lines"),
             alpha = 0.5, color = "blue")
    #label.padding adjusts the 'whitespace' above, under, left & right to the text body 
    #label.r controls the amoount of rounding of the label boxes
    #label.size controls the size of the label border
    #Moreover, all aesthetics can also be passed as arguments: e.g. alpha & colour

  #5
?arrow
    #TBD



# 28.4.4 ------------------------------------------------------------------

  #1
df <- tibble(x = rnorm(10000), y = rnorm(10000))
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()
    #because hex uses fill to depict the count, scale_fill_gradient should be used
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

  #2
?scale_color_manual
    #THe first argument ist always ...  

  #3
presidential %>%
  mutate(id = 33 + row_number(),
         mid =  start + (end - start)/2 ) %>%
  ggplot(aes(start, y = 0, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = 0)) +
  geom_label_repel(aes(x = mid, y = 0, label = name), alpha = 0.5) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue")) +
  scale_x_date("Year", breaks = presidential$start, date_labels = "'%y") +
  scale_y_continuous(NULL, labels = NULL) +
  labs(title = "Presidential Terms 1953-2017", caption = "Data: presidential dataset (Source unknown)") +
  theme_bw()

  #4
ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
