# 1
# Create a population that represents the rolling of a six-sided die. Simulate
# the random experiment of taking samples of size 5 from this population and
# calculating the sample mean. Save the resulting sampling distribution as
# a list Die05 . Repeat this for samples of size 10, 20 and 40 to obtain lists
# # Die10 , Die20 and Die40 .

dice = c(1,2,3,4,5,6)
dice05 = sample(dice, 5, replace = TRUE)
dice10 = sample(dice, 10, replace = TRUE)
dice20 = sample(dice, 20, replace = TRUE)
dice40 = sample(dice, 40, replace = TRUE)

# a) Find the mean of these lists.

mean(dice05)
library(Hmisc)
describe(dice05,type=2)

sd(dice05, na.rm=TRUE)/sqrt(length(dice05[!is.na(dice05)])) 
#or

calculateStandardDeviationError(dice05)
# 3
# Assume the population standard deviation is q = 25. Compute the standard
# error of the mean, qX , for sample sizes of 50, 100, 150 and 200. What can
# you say about the size of the standard error of the mean as the sample size is
# increased?
standDev<-25
standDev/sqrt(50)
standDev/sqrt(100)
standDev/sqrt(150)
standDev/sqrt(200)

# 4
# The Automobile Association gave the average price of unleaded petrol in
# Sweden as 14.63 Swedish Krona (SK) per litre in June 2012. Assume this
# price is the population mean, and that the population standard deviation is
# q = 1 SK.

# a)
# What is the probability that the mean price for a sample of 30 petrol
# stations is within 0,25 SK of the population mean?

p <- diff(pnorm(c(-0.25,0.25)*sqrt(30)))
# b)
# What is the probability that the mean price for a sample of 50 petrol
# stations is within 0,25 SK of the population mean?
p2 <- diff(pnorm(c(-0.25,0.25)*sqrt(50)))

# c)
# What is the probability that the mean price for a sample 100 petrol
# stations is within 0,25 SK of the population mean?
p3 <- diff(pnorm(c(-0.25,0.25)*sqrt(100)))

# d)
# Would you recommend a sample size of 30, 50 or 100 to have at
# least a 0,95 probability that the sample mean is within 0,25 SK of the
# population mean?

# Recommend n = 100, because
# the probabilities in (a) and (b) are below 0.95, in (c) above 0.95.

# 5
# According to Golf Digest, the average score for male golfers is 95 and the
# average score for female golfers is 106. Use these values as population
# means. Assume that the population standard deviation is q = 14 strokes
# for both men and women. A simple random sample of 30 male golfers and
# another simple random sample of 45 female golfers are taken.


# a) Sketch the sampling distribution of X for male golfers.

sd = 14
nMale = 30
nFemale = 45
devitation = sd/sqrt(nMale) #richtig

averageOfAll = (95*nMale + 106*nFemale)/(nMale+nFemale)



p = diff(rnorm(nMale, mean = averageOfAll, sd = devitation))
sd(p)
#?????????????????

#b) What is the probability that the sample mean is within 3 strokes of the
# population mean for the sample of male golfers?

p3 <- diff(rnorm(nMale, mean = 14.63, sd = devitation))



# 6
# Advertisers contract with Internet service providers and search engines to
# place ads on websites. They pay a fee based on the number of potential
# customers who click on their ads. Unfortunately, click fraud - i.e. someone
# clicking on an ad solely for the purpose of driving up advertising revenue -
#   has become a problem. Forty per cent of advertisers claim they have been a
# victim of click fraud. Suppose a simple random sample of 380 advertisers is
# taken to learn about how they are affected by this practice.

# a) What is the probability the sample proportion will be within +-0.04 of
# the population proportion experiencing click fraud?

lengthSample = 380 

p <- diff(pnorm(c(-0.004,0.004)*sqrt(lengthSample)))

sol = 1+ (pi*(1-pi)/lengthSample)

sol = (pi*(1-pi)/lengthSample)
sqrt(sol)

#?????????????????

