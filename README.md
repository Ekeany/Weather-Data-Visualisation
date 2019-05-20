Data Visualization Assignment 2
================
Eoghan Keany
14 February 2019

Introduction
------------

This project deals with the visualisation of daily air quality measurements from the city of New York. This data set was obtained from the New York State Department of Conservation (ozone data) and the National Weather Service (meteorological data). The original data set contained 6 variables and with 154 observations spanning from May 1st, 1973 to September 30th 1973. A sub set of this dataset was chosen for this project that contained six variables with 92 observations spanning from May 1st, 1973 to July 31st 1973. The six variables are as follows:

-   Ozone: Mean ozone levels measured in parts per billion from 13:00 to 15:00 hours at Roosevelt Island.
-   Solar radiation: Mean Solar radiation from 8:00 to 12:00 hours at Central Park measured in Langleys (only the visible part of the spectrum of light was considered).
-   Wind: Average wind speed in miles per hour from 7:00 and 10:00 hours at LaGuardia Airport
-   Temperature: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport.
-   Temporal data: represented by two columns containing the day and month of a measurements origin.

#### Pre-processing

the dataset contained a total of 31 missing values all located in the ozone column this a rather large percentage of the ozone data therefore by substituting a single value, whether that be the mean, median etc.., will underestimate the variability of the data. However as this project is concerned with the visualisation of data these values were filled using the median of the ozone column. This method has an advantage over the mean when dealing with outliers, which are a regular occurrence in meteorological data not to mention 1970's instrumentation. A new column was also created that contained the temporal data in a date format as this format is required for ggplot.

``` r
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(lattice)

# Replaces the nan values in the dataframe with the median value
f=function(x){
  x[is.na(x)] =median(x, na.rm=TRUE)
  return(x)
}

MyData <- read.csv(file="//fs2/18234602/Desktop/airqualityv2.csv", header=TRUE, sep=",")

Filled_df = data.frame(apply(MyData,2,f))
Filled_df$Date<- seq(as.Date("2018/05/01"), by = "day", length.out = nrow(Filled_df))
question_4 <- Filled_df
Filled_df$Month <- NULL
Filled_df$Day   <- NULL
```

### Question One.

The first plot created was of the monthly average measurements. By taking the average value of each month it makes it much easier to visualize the trends in the data as a function of time.To plot this data, the inital datset had to be grouped by month and then converted to a "tidy" data format. A simple bar chart was then employed to visualize the grouped data, however initially a simple line plot was created. The sloped lines joining the data points allows the brain to comprehend the notion of increasing or decreasing values easier than comparing static heights seperated by distance. Although there was an issue with the axis values which made it unclear that the data was in fact the monthly averages, therfore the bar chart was used instead.

``` r
Grouped <- Filled_df %>% 
  group_by(month=floor_date(Date, "month")) %>%
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = funs(mean="mean"))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## please use list() instead
    ## 
    ## # Before:
    ## funs(name = f(.)
    ## 
    ## # After: 
    ## list(name = ~f(.))
    ## This warning is displayed once per session.

``` r
Plottable_grouped <- Grouped %>% gather(Measurement, Values_, Ozone_mean:Temp_mean)


ggplot(data=Plottable_grouped, aes(month, y = Values_, fill=Measurement))+ 
  geom_col(position = "dodge",colour = "black") + 
  scale_x_date(labels = date_format("%B"))+
  labs(title="Monthly Mean Measurements", subtitle="Assignment 2", y="Measurement", x="Month")+
  scale_fill_brewer(palette="Dark2")+ theme_bw() 
```
<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/pressure-1.png">
</p>

``` r
ggplot(data=Plottable_grouped, aes(month, y = Values_, color =Measurement))+ 
  geom_line() + 
  labs(title="Monthly Mean Measurements", subtitle="Assignment 2", y="Measurement", x="Month")+
  scale_fill_brewer(palette="Dark2")+ theme_bw() 
```
<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/pressure-2.png">
</p>

### Question Two.

The Diverging bar chart places a greater emphasis on the variation of the data from its mean value. To create this chart the tidy dataset used previously was gathered and mutated to contain an extra column that contained an indicator value that implied if a row measurement was above or below the three month average measurement therefore two classes could be visualised by assigning a different feature for each. Also the three month mean values were subtracted from their relevant measurements, creating either a negative or positive value that would then appeare above or below the x\_axis in the diverging bar chart. Only the temperature and ozone levels were selected for this plot. The columns were also placed horizontally as when they are vertically aligned it suggests that values below the mean are negative or undesirable.

``` r
Temp_gathered <- Filled_df %>%  mutate(Ozone = Ozone - mean(Ozone),Temp = Temp-mean(Temp)) %>%
  select(Date,Ozone,Temp) %>% gather(Measurement, Values_, Ozone:Temp)

gathered <- transform(Temp_gathered, Postion_With_Mean = ifelse(Measurement== 'Ozone' & Values_ > 0, "Above",
                      ifelse(Measurement== 'Ozone' & Values_ < 0,"Below",
                      ifelse(Measurement== 'Temp' & Values_ > 0,"Above","Below"))))

xx <- ggplot(gathered, aes(x=Date, y=Values_, fill=Postion_With_Mean))+
  geom_col(position="identity", col = "black",size=0.1)+
  coord_flip() +
  facet_wrap(~Measurement)+scale_fill_manual(values=c("#50486D","#FFA373"))+
labs(title="Diverging Bar Chart", subtitle="Assignment 2", y="y", x="Month")+ theme_bw() 
xx
```

<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_2-1.png">
</p>

### Question Three.

The slope chart normalizes each column, which helps with the task of visually comparing the changes between the monthly average values over the three year period. By joining each data point with a line the brain subconsciously follows the direction of the established pattern in this case the line making it easier to understand the change in height. From the given example the colours were initially green and red, however these were changed as these colours subconsciously infer either a positive or negative conation with either decreasing or increasing slopes.To create the graph the average value of each measurment from May to July was caluclated and dived by the maximum value over the three month period for their respective measurement. An extra column indicating if the measurments were increasing or decreasing was also added to the dataframe to allow for the use of colour.

``` r
Grouped$Ozone_mean <- Grouped$Ozone_mean/(max(Filled_df$Ozone))
Grouped$Wind_mean <- Grouped$Wind_mean/(max(Filled_df$Wind))
Grouped$Temp_mean <- Grouped$Temp_mean/(max(Filled_df$Temp))

transposed <- t(Grouped)
transposed = transposed[-1,]
colnames(transposed) <- c("May", "June", "July")
transposed <- cbind(Class = rownames(transposed), transposed)
question_3 <- as_data_frame(transposed)
```

    ## Warning: `as_data_frame()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
question_3[, c(2,3,4)] <- sapply(question_3[, c(2,3,4)], as.numeric)
question_3$Change <- c("blue","red","blue")
question_3$Class <- c("Temperature","Wind Speed","Ozone Level")

p <- ggplot(question_3) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  geom_vline(xintercept=3, linetype="dashed", size=.1) +
  labs(title="Slope Chart", subtitle="Assignment 2", y="Monthly Average") +  # Axis labels
  xlim(.5, 3.5) + 
  ylim(0,(1.1*(max(question_3$`May`,question_3$`June`, question_3$`July`)))) 
 
p <- p + geom_segment(aes(x=1, xend=2, y=`May`, yend=`June`, col=Change), size=.75, show.legend=F)
p <- p + geom_segment(aes(x=2, xend=3, y=`June`, yend=`July`, col=Change), size=.75, show.legend=T)+
  scale_color_manual(labels = c("Increasing", "Decreasing"),values = c("blue"="lightblue", "red"="indianred3"))
p <- p + geom_text(label = question_3$Class, y=question_3$`May`, x=rep(1, NROW(question_3)), hjust=1.1, size=3.5)
p <- p + geom_text(label="May", x=1, y=1.1*(max(question_3$`May`, question_3$`June`,question_3$`July`)), hjust=1.2, size=5)  # title of left line
p <- p + geom_text(label="June", x=2, y=1.1*(max(question_3$`May`, question_3$`June`,question_3$`July`)), hjust=-0.1, size=5)  # title of reight line
p <- p + geom_text(label="July", x=3, y=1.1*(max(question_3$`May`, question_3$`June`,question_3$`July`)), hjust=-0.1, size=5)  # title of reight line

p <- p + theme_bw() 
p
```

<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_3-1.png">
</p>

### Question Four.

The three plots chosen to represent the distributions per month of each measurement were a strip plot, density plot and a frequency polygon plot. Each graph was faceted my measurement and coloured by month. The frequency polygon was the most confusing graph to understand due to the overlap of lines the x axis making it hard to distinguish between the different monthly distributions. On the other hand the density plot clearly indicates the overlap between various months when a low alpha value is applied. The strip plot also clearly defines the differences between the monthly distributions however as it uses intensity and position it is not processed as quickly in comparison with the area attribute from the density and ploygon plots.

``` r
question_4 <- transform(question_4, Month = ifelse(Month== 5, "May",
                        ifelse(Month== 6,"June","July"))) 

Question4_Gathered <- question_4 %>% select(Month,Ozone,Temp,Wind,Date) %>% gather(Measurement, Values_, Ozone:Wind)

ggplot(Question4_Gathered, aes(x=Values_, y=Month)) + 
  geom_point(aes(col=Month), alpha=0.3, size = 3, show.legend=T) +
  coord_fixed(ratio=5)+facet_grid(Measurement ~ .)+
  labs(title="Monthly Distributions", subtitle="Assignment 2", y="", x="Measurement Value")+ theme_bw() 
```

<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_4-1.png">
</p>

``` r
ggplot(Question4_Gathered, aes(x=Values_)) + 
  geom_freqpoly(binwidth=4,aes(color=Month),size = 1, alpha = 0.4)+
  geom_freqpoly(binwidth=4, aes(color="Total"), size = 0.2, alpha = 0.3) +
  facet_grid(Measurement ~ .)+
  scale_colour_manual(values= c("#ca0020","#0571b0","#00ba38","#000000" )) +
  labs(title="Monthly Distributions", subtitle="Assignment 2", y="Count", x="Measurement Value")+theme_bw() 
```

<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_4-2.png">
</p>


``` r
ggplot(Question4_Gathered) + 
  geom_density(aes(x = Values_, fill = Month), alpha = 0.3 ) +
  labs(title="Monthly Distributions", subtitle="Assignment 2", y="Count", x="Measurement Value")+
  facet_grid(Measurement ~ .) + theme_bw() 
```
<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_4-3.png">
</p>

### Question Five

The temporal aspect of the data was represented by a time series plot using an area plot faceted upon measurement.Other graphs were considered such as a calender plot however the time series plot is simple and intutive. Each month was highlighted by a unique colour and a loess trend line was also applied to highlight the motion of the measurements over time.

``` r
ggplot(Question4_Gathered, aes(x = Date, y = Values_)) + 
  geom_area(aes(color = Month, fill = Month), 
            alpha = 0.5, position = position_dodge(0.8))+
  geom_smooth(method = "loess", color = "black", size = 0.5)+
  labs(title="Time Series Plot", subtitle="Assignment 2", y="Measurment Values", x="Date")+
  facet_grid(Measurement ~ .) + theme_bw() 
```

<p align="center">
  <img width="672" height="480" src="/figure-markdown_github/chunk_5-1.png">
</p>

Conclusion
----------

It is clearly evident from the data that as the months pass the temperature and ozone levels are increasing, whereas the wind speed is decreasing. This makes sense as New York is situated in the northern hemisphere as the months progress more solar radiation will enter the atmosphere thus increasing temperature and ozone levels. Also more stable temperatures cause less variation in barometric pressure therefore less severe wind. In my understanding of the data this aspect was best represented by the time series plot with the regression line and the initial plot where the data was compressed to monthly averages and plotted. This is due to the Gestalt principle of continuity were the eye is naturally drawn to follow the direction of an established pattern rather than deviate from it. The distribution plots didn't have the same intuition despite their areas being easily comparable. The fact is that their areas were overlapping and not in their correct sequence making it harder to interpret the temporal aspect of the data.

References
----------

-   <https://nuigalway.blackboard.com>
-   <https://ggplot2.tidyverse.org>
