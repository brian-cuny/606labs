library(tidyverse)

load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\Labs\\ames.RData')
load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\Labs\\bdims.RData') #for qqnormsim

set.seed(100)

area <- ames$Gr.Liv.Area
price <- ames$SalePrice

qqnorm(area)
qqline(area)

#Exercise 1: According to the qqnorm plot the data is right skewed with a long upper tail

samp1 <- sample(area, 50)

par(mfrow=c(2,1))
hist(samp1)
qqnorm(samp1)
qqline(samp1)

summary(samp1)
#Exercise 2: The data appears much more normal than in the original.

mean(samp1)

samp2 <- sample(area, 50)

mean(samp2)

#Exercise 3: The means are roughly similar. Both are close to the population mean. As the number of elements in the sample increases the mean will trend towards to the true population mean.

#Exercise 4: There are 5000 elements in sample_mean50, each one a mean of a SRS of 50 randomly selected elements all from the same population. The sampling distribution is normal centered about the population mean. If the number of samples were increased, the resulting distribution will be even more normal and centered even more closely to the true population mean.

sample_means_small <- rep(0, 100)
for(i in 1:100){
  sample_means_small[i] <- sample(area, 50) %>% mean()
}
sample_means_small

#Exercise 5: There are 100 elements in sample_means_small each one represents the mean of a SRS of 50 elements from the population area. 


#Exercise 6: The centers are all of similar value, near the true population mean. The spread decreases as the sample size increases.


#On Your Own

price.sample <- sample(price, 50)
mean(price.sample)

#1: The mean of the sample is $185859.4. This is also our estimate of the population mean.

sample_means50 <- map(rep(0, 5000), ~sample(price, 50) %>% mean()) %>% unlist()

hist(sample_means50, breaks=40)

mean(price)

#2: The sample distribution is normal centered about $180605.5. My single sample gave a population mean of $185859.4. The sampling distribution gave a mean of $180605.5 and the true population means is $180796.1

sample_means150 <- map(rep(0, 5000), ~sample(price, 150) %>% mean()) %>% unlist()

par(mfrow=c(2,1))
xlimits <- range(sample_means50)
hist(sample_means50, breaks=40, xlim=xlimits)
hist(sample_means150, breaks=40, xlim=xlimits)

mean(sample_means150)
#3: The sampling distributions are incredibly similar. This would indicate that the additional size of the sample (from 50 to 150) did not appreciably alter the sampling distribution. This is supported by the fact that the mean is $180818.7, very similar the previous predictions.

#4: The sampling distribution with the larger samples has the smaller spread. If we were concerned about the accuracy of our prediction, we should tend towards as large as a sample as is feasible. The smaller spread increases the likleyhood of an accurate prediction.


















