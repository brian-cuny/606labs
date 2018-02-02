#use ggplots

library(ggplot2)
source("http://www.openintro.org/stat/data/cdc.R")

#Execise 1
nrow(cdc) #cases
ncol(cdc) #variables
lapply(cdc[,], FUN=unique)
#categorical: genhlth, hlthplan, smoke100, gender
#discrete: height, weight, wtdesire, age

#Exercise 2
#include measures of spread
#fivenum, IQR

summary(cdc$height)
IQR(cdc$height)

summary(cdc$age)
IQR(cdc$age)

table(cdc$gender) #9569 males
table(cdc$exerany)
nrow(cdc[cdc$genhlth == 'excellent',])/nrow(cdc) #0.23285

#Exercise 3
mosaicplot(table(cdc$gender, cdc$smoke100), shade=TRUE, main='Relationship between gender and smoking habits', xlab = 'Gender', ylab='Smoking Habit')
#women make up slightly more than half of the sample and are less likely to have smoked 100 cigarettes in their lifetime then men

#Exercise 4
under23_and_smoke <- subset(cdc, age < 23 & smoke100 == 1)


#Exsercise 5
#This box plot shows a comparison between BMI and respondents description of their health. It could be helpful in determining if their is a relationship between a respondents weight and whether or not they consider themselves healthy.
bmi <- (cdc$weight / cdc$height^2)*703
level_data <- factor(x=cdc$exerany, levels=0:1, labels = c('No Exercise Plan', 'Exercise Plan'))
ggplot(cdc, mapping=aes(x=level_data, y=bmi)) + geom_boxplot() + xlab('Group') + ylab('BMI') + ggtitle('Comparison of BMI between Different Exercise Groups')
#I plotted BMI against whether the respondant is on an exercise plan. I would anticipate that the BMI of people who exercise would be lower than those who do not but would also have more upper outliers. THis may seem paradoxical but it would make sense that people who exercise would broadly be comprised of people concerned about their health and weight along with those who were put on health plans by doctors in an attempt to bring their weight down.
#The figure mostly supports this interpretation. In addition, the plot shows fewer exercising respondents without severly low BMI as well. This supports the idea that people who exercise are perhaps more concience of their weight (both high and low)

#On your Own

#1
ggplot(cdc, aes(x=weight, y=wtdesire)) + geom_point() + geom_abline(slope=1,intercept=0, color='red') + geom_point(data=cdc[cdc$weight>230 & cdc$wtdesire>cdc$weight,], color='green') + ggtitle('Current Weight vs. Desired Weight') + xlab('Weight') + ylab('Desired Weight')
plot(cdc$weight, cdc$wtdesire, col=ifelse(cdc$weight > 230 & cdc$wtdesire > cdc$weight,'green','black'))
lines(1:700, 1:700, col='red', type='l')
#There is a strong positive correlation between the two variables. That is, people appear to base the weight they desire off of their current weight. This makes sense as it is common to hear people makes statements such as, "I'd like to drop 20 pounds."
#The red line indicates equal weight and wtdesire and thus all people below the line would like to lose weight, while all people above would like to gain weight. It matches expectations that the number of people that would like to gain weight would depreciate as weight grows. (Only appoximately 7 people above 220 wish to gain more weight)

#2
cdc$wtdiff <- cdc$wtdesire - cdc$weight

#3
typeof(cdc$wtdiff[0])
#wdiff is discrete data. An integer
#If wdiff is 0 that means the person is already at their desired weight. If the number is negative, this represents the amount of weigh the person wishes to lose and if it is positive, that represents the weight the person wishes to gain.

#4
ggplot(cdc, aes(x=wtdiff)) + geom_histogram(aes(y=..count../sum(..count..)),bins = 200) + stat_function(fun=dnorm, color='red', args=list(mean=mean(cdc$wtdiff), sd=sd(cdc$wtdiff)), show.legend=FALSE) + ggtitle('Distribution of Desired Weight Change') + xlab('Weight Difference') + ylab('Relative Frequency')

his <- hist(cdc$wtdiff, breaks=200)
xfit <- seq(min(cdc$wtdiff), max(cdc$wtdiff))
yfit <- dnorm(xfit, mean=mean(cdc$wtdiff), sd=sd(cdc$wtdiff))
yfit <- yfit*diff(his$mids[1:2]*length(cdc$wtdiff))
lines(xfit, yfit, col='blue')
#Normal curve thanks to Peter Dalgaard

summary(cdc$wtdiff)

#Upon initial observation the data is heavily centered just below 0. This would indicate that most people want to "lost a couple of pounds". This is a common sentiment heard from those wishing to lose weight. The mean and median are closely located at -10 and -14.59 again backing up that sentinment of "wanting to lose a few pounds".
#The data appears slight skewed towards losing weight. This is more easily noticable with the normal curve added to the historgram. 

#However the data is hard to process due to a number of extreme outliers. A boxplot supports the idea of a number of extreme outliers

boxplot(cdc$wtdiff)

#I created a data set with these outliers removed
meanDiff <- mean(cdc$wtdiff)
iqrDiff <- IQR(cdc$wtdiff)
cdc$bmi <- bmi
cdc$cat <- ''
cdc[cdc$bmi < 18.5,]$cat <- 'Under'
cdc[cdc$bmi >= 18.5 & cdc$bmi < 24.9,]$cat <- 'Normal'
cdc[cdc$bmi >=24.9 & cdc$bmi < 29.9,]$cat <- 'Over'
cdc[cdc$bmi >= 29.9,]$cat <- 'Obese'
cdcNoOutliers <- cdc[cdc$wtdiff > meanDiff-1.5*iqrDiff & cdc$wtdiff < meanDiff+1.5*iqrDiff,]

boxplot(cdcNoOutliers$wtdiff)
orig_plot <- ggplot(cdcNoOutliers, aes(x=wtdiff)) + geom_histogram(aes(y=..count../sum(..count..), fill=cat),bins = 15) + ggtitle('Distribution of Desired Weight Change') + xlab('Weight Difference') + ylab('Relative Frequency')
orig_plot + stat_function(fun=dnorm, color='red', args=list(mean=mean(cdcNoOutliers$wtdiff), sd=sd(cdcNoOutliers$wtdiff)), show.legend=FALSE) + labs(fill='Legend')



#The histogram with the outliers removes shows a much clearer picture. Most people indicate wanting to lose a few pounds with the data being more heavily skewed towards losing weight.


#5
summary(cdcNoOutliers[cdcNoOutliers$gender == 'm',]$wtdiff)
summary(cdcNoOutliers[cdcNoOutliers$gender == 'f',]$wtdiff)

ggplot(cdcNoOutliers, mapping=aes(x=gender, y=wtdiff)) + geom_boxplot() + xlab('Gender') + ylab('Weight Diff') + ggtitle('Comparison of Ideal Weight Change Between Men and Women')


#Comparing the mean and median from the summary data along with side-by-side box plots supports the notion that women tend to want to lose more weight than men. Interestingly both genders had an equal proportion (1/4) who wanted to gain weight and the distribution of is also equal between 0 and 10 pounds.
#This might indicate that men are in general more happy with their weight or it may simply mean that men are less likely to report that they are unhappy with their weight.


#6
meanWeight <- mean(cdc$weight)
sdWeight <- sd(cdc$weight)
plot(density(cdc$weight), main='Density Distribution of Weights')
abline(v=meanWeight)
lowerBound <- meanWeight-sdWeight
upperBound <- meanWeight+sdWeight
abline(v=lowerBound, col='red')
abline(v=upperBound, col='red')

densityData <- density(cdc$weight, from=lowerBound, to=upperBound)
polygon(c(lowerBound,densityData$x,upperBound), c(0,densityData$y,0), col='red')

#Modified from r-graph-gallery.com number 162

nrow(cdc[cdc$weight >= lowerBound & cdc$weight <= upperBound,])/nrow(cdc)



