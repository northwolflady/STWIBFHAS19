library(openxlsx)
schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')

attach(schoolData)
# 1
# Based on many years’ experience we can say that sd = 10 for the distribution
# of maths marks. Hence construct a 95% confidence interval for the the mean
# maths marks based on the sample maths .

sig = 10
xbar = mean(maths)
n = length(maths)
z = abs(qnorm(0.025))
me = z*sig/sqrt(n)
xbar-me
xbar+me

# 2
# A survey of 750 university students found they were paying on average Euro 108
# per week in accommodation costs. Assume the population standard deviation
# for weekly accommodation costs is Euro 22.

# a) Construct a 90 per cent confidence interval estimate of the population
# mean.

s = 22
n = 750
xbar = 108
alpha = 1-0.9
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(n)

xbar-ME
xbar+ME

# b)Construct a 95 per cent confidence interval estimate of the population
# mean.

s = 22
n = 750
xbar = 108
alpha = 1-0.95
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(n)

xbar-ME
xbar+ME

# c) Construct a 99 per cent confidence interval estimate of the population
# mean.
s = 22
n = 750
xbar = 108
alpha = 1-0.99
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(n)

xbar-ME
xbar+ME

# d) Discuss what happens to the width of the confidence interval as the confidence
# level is increased. Does this result seem reasonable? Explain
# 
# Die Breite des Intervalls nimmt zu. Um mehr Sicherheit zu haben, 
# den wahren population mean in das Intervall einzubeziehen, muss das Intervall breiter sein.

# 3
# Assuming the sample of hair lengths from the data sheet is representative of
# the appropriate population.

# a) Find a 95% confidence interval for the  mean length of hair for each
# of the genders.

hairM = hair[gender=='M']

xbar = mean(hairM)
n = length(hairM)
s = sd(hairM)
t = abs(qt(0.025,n-1))
me = t*s/sqrt(n)
xbar-me
xbar+me

#Why is this wrong? lösungen falsch?
hairF = hair[gender=='F']
xbar = mean(hairF)
n = length(hairF)
s = sd(hairF)
t = abs(qt(0.025,n-1))
me = t*s/sqrt(n)
xbar-me
xbar+me

#Why is this wrong? lösungen falsch?


# b) There are more girls than boys so one could expect that the margin of
# error for the girls is smaller than that for the boys. In this data set that
# is not the case. Explain why not.

# Es gibt mehr Variation in den Haarlängen der Mädchen 
# als für die Jungen, wie in den zugehörigen Standardabweichungen angegeben. 
detach(schoolData)
# 4
# Thirty fast-food restaurants including McDonald’s and Burger King were
# visited. During each visit, the customer went to the drive-through and ordered
# a basic meal such as a burger, fries and drink. The time between pulling up
# to the order kiosk and receiving the filled order was recorded. The times in
# minutes for the 30 visits are given in the file Fastfood.xlsx.

fastFoodData = read.xlsx(xlsxFile = './FastFood.xlsx')
attach(fastFoodData)

# a) Provide a point estimate of the population mean drive-through time at
# fast-food restaurants.

mean(Time)
sd(Time)

# b) At 95 per cent confidence, what is the margin of error?

xbar = mean(Time)
n = length(Time)
s = sd(Time)
t = abs(qt(0.025,n-1))
me = t*s/sqrt(n)
xbar-me
xbar+me
#vielleicht ist die Lösung falsch, denn sie ist 3.8. Das entspricht dem Mean und nicht dem Margin of Error

# c) What is the 95 per cent confidence interval estimate of the population
# mean?

xbar = mean(Time)
n = length(Time)
s = sd(Time)
t = abs(qt(0.025,n-1))
me = t*s/sqrt(n)
xbar-me
xbar+me

# d) Discuss skewness that may be present in this population. What suggestion
# would you make for a repeat of this study?

# Es gibt eine leichte positive Schiefe in diesem Datensatz. Eine grössers Sample wäre gut.
detach(fastFoodData)
attach(schoolData)
# 5
# Find an appropriate 90% confidence interval for the proportion of girls who
# have blue eyes.

girlsWithBlueEyes = eye[gender=='F' & eye=='Blue']
n = length(eye[gender=='F'])
pHat = length(girlsWithBlueEyes)/n
alpha <- 1-0.9
z <- qnorm(alpha/2)

ME = abs(z)*sqrt((pHat*(1-pHat))/n)

pHat-ME
pHat+ME

# 6
# In a sample of 162 companies, 104 reported profits that beat prior estimates,
# 29 matched estimates, and 29 fell short of prior estimates.
length = 162
# a) What is the point estimate of the proportion that fell short of estimates?
pHat = 29/length
# b)Determine the margin of error and provide a 95 per cent confidence
# interval for the proportion that fell short of estimates.
alpha = 1-0.95
z = -qnorm(alpha/2)
me = z* sqrt((pHat*(1-pHat)/length))

pHat-me
pHat+me

# c) How large a sample is needed if the desired margin of error is 0.05?

me = z * sqrt((pHat*(1-pHat)/length))
#dies ist die formel, nun stellen wir die um, dass wir lenght nicht wissen müssen und me wird durch 0.05 ersetzt
newLength = (pHat* (1-pHat)) / (0.05/z)^2 

# 7
# A well-known bank credit card firm wishes to estimate the proportion of
# credit card holders who carry a non-zero balance at the end of the month and
# incur an interest charge. Assume that the desired margin of error is 0.03 at
# 98 per cent confidence.

# a) How large a sample should be selected if it is anticipated that roughly
# 70 per cent of the firm’s cardholders carry a non-zero balance at the
# end of the month?

me = 0.03
alpha <- 1-0.98
z <- qnorm(alpha/2)
pHat = 70/100 #wir wissen, dass es 70% sind
me = z * sqrt((pHat*(1-pHat)/length))
newLength = (pHat* (1-pHat)) / (me/z)^2 

# b) How large a sample should be selected if no planning value for the
# proportion could be specified?
# ????????????????????????????????????????????

# 8 How many observations at least would be needed such that the margin of
# error for the pendulum timings will be less than 0.05?
attach(schoolData)

#Erste überlegung: Margin of Error soll genau 0.05 sein 
me <- z*sqrt((phat*(1-phat))/n)

0.05 <- z*sqrt((phat*(1-phat))/n)
pHat = length/length(pendulum)

#???????????????????????????????????????????????

# 9
# We saw above that the margin of error for the proportion of girls with blue
# eyes is approximately 26%. How large should the sample be at least if we
# wanted a margin or error less than 5%?

girlsWithBlueEyes = eye[gender=='F' & eye=='Blue']
n = length(eye[gender=='F'])
pHat = length(girlsWithBlueEyes)/n
alpha <- 1-0.9
z <- qnorm(alpha/2)

ME = abs(z)*sqrt((pHat*(1-pHat))/n)

(Me * abs(z))^2 = (pHat*(1-pHat))/n

n = (pHat*(1-pHat)) / (0.05 * abs(z))^2

#???????????????????????????????????????????????????????????????















