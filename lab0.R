#Exersize 1
arbuthnot$girls

#Exersize 2
print('There is a consistent upwards trend in the number of girls baptized over the given time
      frame aside form a decade long drop from appoximately 1650 to 1660. One would speculate
      that some event or series of events occurred during this time to artificially drop
      the number of girls being baptized.')

#Exersize 3
plot(arbuthnot$year, arbuthnot$boys/(arbuthnot$boys+arbuthnot$girls), type = 'l')
print('The proportion of boys born during the measured time frame varies from year to year but is consistently just around 0.51 and never below 0.5')

#On Your Own
source('http://www.openintro.org/stat/data/present.R')

#1
print(max(present$year))
print(min(present$year))
dim(present)
names(present)

#2
summary(present$boys + present$girls)
summary(arbuthnot$boys + arbuthnot$girls)

#3
plot(present$year, present$boys / (present$boys + present$girls), type='l')
all(present$boys > present$girls)
#Yes, all the years have a greater proportion of boys than girls however the present data shows a significantly more consistent proportion when compared the relative volatility of the arbuthnot data.

plot(seq(1,length(arbuthnot[,1])), arbuthnot$boys/(arbuthnot$boys+arbuthnot$girls), type='l', col='blue')
lines(seq(1,length(present[,1])), present$boys / (present$boys + present$girls), type='l', col='red')

#4
present[present$boys + present$girls == max(present$boys + present$girls),]$year
