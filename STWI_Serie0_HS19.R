library(openxlsx)
data = read.xlsx(xlsxFile = './STWI_Serie0_HS19.xlsx')

# 1
# a) If a coin is tossed 10 times and each time it comes up
# heads, explain whether or not you would accept this coin as being
# a fair coin.

#Fair heisst im bezug auf ne Münze, dass die Warscheinlichkeit
#50% => 1/2 ist.

pValue = (1/2)^10

#the p value is small, below 0.05 => unfair

# b) Technology enables a user to unlock their mobile telephone
# simply by having their fingerprint read by their phone. Although
# the technology is good, it can make errors. Explain what it means
# in this situation for a Type II error to have happened.

# Ein Fehler 2. macht keine Aktion wenn die Aktion ausgeführt werden soll
# Ergo => Handy wird nicht entsperrt, falls der Finger auf den Button gelegt wird
# Bei dem Beispiel wäre ein Fehler 1. Art fatal => denn ein Handy würde dann mit einem Falschen Finger
# entsperrt werden.

# c) Below is the probability density function related to the variable
# T that represents the time it takes for people to get through
# airport security. The mean is 15 minutes and standard deviation is
# 6 minutes. Sketch on the same axes as the given graph (so that a
# fair comparison can be made) the distribution of T: the sampling
# distribution of T for samples of size 10.

#schaue in den OD-Lösungen => Zeichnunge in R sind schwierig

# 2
# The following questions pertain to the file STWI_SERIE0_FS19.xlsx
# and the data Rochartis. Please include in your solutions what you
# type into your calculator

# a) Classify the following variables according to their type:
# i) alcohol
# ordinal & discret

# ii) before
# metric & continous
# iii) dizzy
# nominal & discrete

# b) Describe the distribution of the ages of the people who
# took part in the experiment, including an appropriate sketch.

attach(data)
hist(age)
mean(age)
sd(age)
# c) How many men older than 24 years have torkisol levels
# before the experiment lower than 18 units?

length(person[gender=="M" & before<18 & age>24])

# 3
# The following questions pertain to the file STWI_SERIE0_FS19.xlsx
# and the data Rochartis. Please include in your solutions what you
# type into your calculator.
# Assume that the data of before are representative of the population
# and that it is normally distributed.

# a) What is the probability that a randomly selected person
# from the population will have torkisol levels between 18 and
# 20 units?

mean = mean(before)
sd = sd(before)
pnorm(20,mean,sd)-pnorm(18,mean,sd)

# b) What is the probability that a simple random sample of
# 32 people will have a mean torkisol level less than 18 units?

#mean bleibt

sdNew = sd/sqrt(32)
pnorm(18,mean,sdNew)

# c) Give an appropriate confidence interval for the proportion
# of people who suffered from dizziness as a side-effect of having
# taken the drug.

pHAt = length(dizzy[dizzy==1])/length(dizzy)
marginOfError = 1.96 * sqrt((pHAt*(1-pHAt))/length(dizzy)) #1.96 is the z value

pHAt-marginOfError
pHAt+marginOfError

# 4
# The following questions pertain to the file STWI_SERIE0_FS19.xlsx
# and the data Rochartis. Please include in your solutions what you
# type into your calculator.

# a) It is claimed that the new drug can reduce the torkisol level
# by at least 2 units. Create and apply an appropriate hypothesis
# test to test whether the claim can be believed

# H1:Mdif <= 2
# H0:Mdiff > 2
t.test(before-after,mu = 2, alternative = "greater")
#Hier finden wir den pValue = 0.04532, dieser wert ist kleiner als 0.05 => signifikant => akzeptiert

# b) Rochartis claim that after taking their new drug, the level
# of torkisol in the blood should on average be lower than 17.4 units.
# Test this claim by way of an appropriate hypothesis test.

# H1 Mdif <17.4
# H0 Mdif >= 17.4

t.test(after, alternative = "less", mu = 17.4)
#p-value = 0.03761 => lower than 0.05 => we accept the claim

# c) Explain whether there is an association between the level
# of the torkisol at the beginning of the trial and the age of the
# participant, include an appropriate sketch.

#Beide sind sowohl metrisch wie continous
cor(before,age)

plot(age,before, type = "p") 
abline(lm(age~before))
#es existiert eine kleine negaive assozation 


# 5
# It is known that 4% of the population have the genetic defect known as
# torkisolisis. Rochartis have further developed a simple saliva test that
# can indicate whether a person has this defect or not. In their research,
# the test was positive for 95% of people who had torkisolisis but also it
# was positive for 8% of people who did not have torkisolisis.

# a) What is the probability that from 5 randomly selected
# people, there is at least one person who suffers from torkisolisis?

1-(1-0.04)^5 #Erste 1 sind 100% probanten, Zweite 1, da 96% keine Krankheit haben => man kehrt es um

# b) What is the probability that someone who tests positive
# in the saliva test actually has torkisolisis?

#probability of sick P(S)
pSick = 0.04
#probabilty of sick & tested P(T|S)
# P(T&S)=P(T|S) * P(T) = 0.95 * 0.04 = 0.038
pTestedAndSick = 0.95 

#probability of tested but not sick P(T| -S) 
#P(T & -S) = P(T|-S) * P(-S) = 0.08 * (1-0.04) = 0.0768
pTestedNotSick = 0.08

pST = 0.038/(0.038+0.0768)

# c) The time it takes the saliva test to return a result follows a
# uniform distribution with a mean of 6 minutes and is never longer
# than 10 minutes. What is the probability that the test returns a
# result in less than 5 minutes?

