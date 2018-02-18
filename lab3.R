library(tidyverse)

load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\Labs\\bdims.RData')

mdims <- subset(bdims, sex == 1)
fdims <- subset(bdims, sex == 0)

#Exercise 1
ggplot() + geom_histogram(data=mdims, aes(x=wgt, fill=sex), alpha=0.5) + 
  geom_histogram(data=fdims, aes(x=wgt, fill=sex), alpha=0.5) +
  labs(x='Weight (kg)', y='Count', title='Comparison of Weights between Men and Women') +
  scale_fill_manual(name='sex', values=c('red', 'blue'), labels=c('Women', 'Men'))

summary(mdims$wgt)
sd(mdims$wgt)
summary(fdims$wgt)
sd(fdims$wgt)

#Men weigh more than women and have a larger standard deviation and IQR indicating that men's weights vary more than womens. Both distributions look roughly normal, but further analysis would be required to know for certain.

#Exercise 2
fhgtmean <- mean(fdims$hgt)
fhgtsd <- sd(fdims$hgt)

ggplot(data=fdims, aes(x=hgt)) + geom_histogram(aes(y=..density..), fill='red', alpha=0.5, bins=8) + 
  stat_function(fun=dnorm, args=list(mean=fhgtmean, sd=fhgtsd)) + 
  labs(x='Height (cm)', y='Density', title='Height Distribution of Women with Normal Curve')

#Yes, the data appears to closely resemble the normal curve.

#Exsercise 3
par(mfrow=c(2,1))
qqnorm(fdims$hgt)
qqline(fdims$hgt)

sim_norm <- rnorm(n = length(fdims$hgt), mean = fhgtmean, sd = fhgtsd)
qqnorm(sim_norm)
qqline(sim_norm)

#No, all the points do not fall on the line. The real data appears to have more variations than the simulated data but stays pretty close to the line. Both deviate at the extremes although in different directions.


#Exercise 4
qqnormsim(fdims$hgt)

#Yes, the data looks similar to the simulated data. The highest and lowest extremes deviate a bit but represent a very small portion of the overall data.

#Exercise 5
qqnorm(fdims$wgt)
qqline(fdims$wgt)

qqnormsim(fdims$wgt)

#No. The data does not appear similar. The actual data has many more extreme values both on the high end and low end. It is logical to say that women's weight in this data is not normally distributed.

#Exercise 6

#Question 1: What is the probability that a randomly selected young adult female is shorter than 5 feet (152.4 cm)?

pnorm(q=152.4, mean=fhgtmean, sd=fhgtsd)
sum(fdims$hgt <= 152.4) / length(fdims$hgt)

#Question 2: What is the probability that a randomly selected young adult female weights more than 80kgs?

pnorm(q=80, mean=fdims$wgt %>% mean(), sd=fdims$wgt %>% sd(), lower.tail=FALSE)
sum(fdims$wgt > 80) / length(fdims$wgt)

#The calculation on height was more accurate. This makes sense as the height distribution is more roughly normal than the weight distribution.


#On Your Own
par(mfrow=c(2,1))
bii.di.formated <- ((fdims$bii.di) - mean(fdims$bii.di)) / sd(fdims$bii.di)
hist(bii.di.formated, breaks=13)
qqnorm(bii.di.formated)
qqline(bii.di.formated)

#bii.di matches with Q-Q Plot B. There are too many lower outliers for the data to be normal.

par(mfrow=c(2,1))
elb.di.formated <- ((fdims$elb.di) - mean(fdims$elb.di)) / sd(fdims$elb.di)
hist(elb.di.formated, breaks=13)
qqnorm(elb.di.formated)
qqline(elb.di.formated)

#elb.di matches with Q-Q Plot C. This data is the most normally distributed and thus should match with the Q-Q plot closest to a straight line.

par(mfrow=c(2,1))
age.formated <- ((fdims$age) - mean(fdims$age)) / sd(fdims$age)
hist(age.formated, breaks=13)
qqnorm(age.formated)
qqline(age.formated)

#age matches with Q-Q Plot D. The data is heavily skew right and that is reflected in the Q-Q Plot.

par(mfrow=c(2,1))
che.de.formated <- ((fdims$che.de) - mean(fdims$che.de)) / sd(fdims$che.de)
hist(che.de.formated, breaks=13)
qqnorm(che.de.formated)
qqline(che.de.formated)

#che.de matches with Q-Q Plot A. The data has the most upper outliers and this is reflected in the upper values of the Q-Q Plot.

#2
#Age is usually rounded to the nearest year and thus is likely to result in many repeated values. 
#Female elbow diameter is unlikely to be rounded but perhaps there are just not that many variations in elbow diameter when compared to other measurements. 

#3. 
qqnormsim(fdims$kne.di)

#The data is not normally distributred and appears to be skew right.

ggplot(data=fdims, aes(x=kne.di)) + geom_histogram(aes(y=..density..), fill='red', alpha=0.5, bins=13) + 
  stat_function(fun=dnorm, args=list(mean=fdims$kne.di %>% mean(), sd=fdims$kne.di %>% sd())) + 
  labs(x='Knee Diameter', y='Density', title='Knee Diameter Distribution of Women with Normal Curve')




