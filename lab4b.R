download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

set.seed(10000)

#Exercise 1
population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

summary(samp)

hist(samp)

#The average house is about 1450 square feet. The distribution is slightly skew right. The typical house is between 1036 and 1664 square feet. Typical in thise case is the IQR or the middle 50% of homes by size.


#Exercise 2

#I would not expect it to be identical. There are many different samples of 60 from a population of nearly 3000. However, I would anticipate that most distributions would be very similar to mine. 60 elements out of a population 3000 is a very good sample size and will accurately represent the data within reason. Thus, the data will be close, but not identical.

#Exercise 3
sample_mean <- mean(samp)
se <- sd(samp) / sqrt(60)
lower <- sample_mean - 1.96*se
upper <- sample_mean + 1.96*se

c(lower, upper)

#We must have selected at least 30 elements from a population via a SRS. 

#Exercise 4

#This means that it can be stated with 95% confidence that the true population proportion is in the stated range of given values. It does NOT mean that there is a 95% chance of the population mean being in the given range.

#Exercise 5

mean(population)

#Yes, the population is captured in the 95% confidence interval that I generated.

#Exercise 6

#I would expect appoximately 95% of the calculated confidence intervals to contained the population mean. The confidence interval is an estimate that tries to encompass 95% of all possible calculated sample means. Thus, we would expect about 95% of the calculated means to contain the populatio mean.

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

for(i in 1:50){
  samp <- sample(population, n)
  samp_mean[i] <- mean(samp)
  samp_sd[i] <- sd(samp)
}

lower_vector <- samp_mean - 1.96*samp_sd / sqrt(n)
upper_vector <- samp_mean + 1.96*samp_sd / sqrt(n)

#On Your Own

#1

plot_ci(lower_vector, upper_vector, mean(population))

#1 48 out of 50 confidence intervals contain the true population mean. 96% of the calculated confidence intervals contain the population mean. This proportion of not exactly the same as the 95% confidence interval. It would be impossible to hit that number exactly as there would be either 96 or 98 % matching. 
#There will always be a small amount of variation between the theoretical value and the expected value. The expected value will trend towards the theoretical value until every possible sample has been generated.


#2. 99% 2.576

#3. 

lower_vector <- samp_mean - 2.576*samp_sd / sqrt(n)
upper_vector <- samp_mean + 2.576*samp_sd / sqrt(n)

plot_ci(lower_vector, upper_vector, mean(population))

#49 out of 50 or 98% of the confidence intervals matched the true value. Both values were incredibly close to their theoretical values indicating that a sample of 50 confidence intervals gives a strong indication as to the performance of each interval. 




