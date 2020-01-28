#FS19
library(openxlsx)
postOfficeData = read.xlsx(xlsxFile = './STWI_FS19_Exam.xlsx')
attach(postOfficeData)

# 1
# Classify the following three variables from postOffice.

# i) postcode
#dicrete & nominal

# ii) weight
# metric & continous

# iii) date
#discrete & ordinal

# b) Calculate the skewness of the variable weight from postOfficeData
weight.z = (weight-mean(weight))/sd(weight)
lengthWeight = length(weight)
skewnessHeight = lengthWeight/((lengthWeight-1)*(lengthWeight-2))*sum(weight.z^3)
#2.549

# c) Use postOffice to find whether or not there an association 
# between the weight of a letter sent internaionally and the price 
# that is needed to be paid for it to be sent? Include both and 
# appropriate diagram and a numerical measure.
internationalWeight = weight[class=="I" & price != "_"]
internationalPrice = price[class=="I" & price != "_"]
typeof(internationalPrice) #R interpretiert es als character
internationalPrice = as.numeric(internationalPrice) #wir konvertieren es in Numeric
cor(internationalWeight,internationalPrice) #cor = -0.023

plot(internationalPrice,internationalWeight, type = "p") 
abline(lm(internationalPrice~internationalWeight))

#Es gibt keinen Assozation zwischen den Variabeln 

# 2
# A letter that is sent with via B Class will 70% of the time be delivered
# in exacly tow days, 20% of the time it will be delivered in exacly three days and 
# in 10% of the time in one day.

# a) What is the mean and standard deviation for the 
# number of days needed to deliver a B Class letter?

days = c(2,3,1)
percentage = c(0.7,0.2,0.1)
mean = sum(days*percentage) #2.1
sample = c(1)
sample = c(sample,2,2,2,2,2,2,2)
sample = c(sample,3,3)

sd(sample) #0.568

# b) If four letters are sent via B Class, what is the prbability 
# that at leas one of them will be delivered in exactly 3 days?

#0.7 +0.1 = 0.8 => die chance dass, er nicht in 3 tagen ankommt is 80%

1-(0.8)^4 #0.59

# c) The post office would like to know whether the proportions of mail sent by A Class is
# any different to tht proportion sent by B Class. Create an appropiate hypothesis test and
# use the data postOffice to test whther or not this is the case.

# H0 P(A) = P(B)
# H1 P(A)>P(B) or P(A)<P(B)

pHatA = length(class[class == "A"])/length(class)
pHatB = length(class[class == "B"])/length(class)
lengthA = length(class[class == "A"])
lengthB = length(class[class == "B"])
test =prop.test(x=lengthA,n=length(class), p = 0.5)
test$p.value

prop.test(x=lengthA,n=lengthB, p = 0.5) #Dies beweisst, dass sie nicht im verhältniss 0.5 zu einander stehen

#Ja sie sind verschieden

# 3

# a) Twint would like to know whether their app is being used
# more than it was the previous year, wehn it was being used
# for 2% of Post office payments. Create an appropiate hypothesis test
# and use postOffice to test wehther or not this is the case.

lengthTwint = length(payment[payment=="Twint"])
pHatTwint = lengthTwint/length(payment)

#H0 P(Twint)<= 0.2
#H1 P(Twint) > 0.2

prop.test(x=lengthTwint,length(payment),p=0.2, alternative = "less")

#Ja, ist mehr gebraucht

# b) Using postOffice give an appropriate confidence interval
# for the price for a letterssent that used Twint as the payment mehthod.
paymentTwint = payment[payment=="Twint"]
s = sd(price[payment=="Twint"])
n = length(paymentTwint)
xbar = mean(as.numeric(price[payment=="Twint"]))
alpha = 1-0.95
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(n)
xbar-ME
xbar+ME


# c) On what date was teh heavies letter sent that 
# used Twint as the payment mehtod from postOffice?
maxWeight = max(weight[payment=="Twint"])
date[payment=="Twint" & weight == maxWeight]
#19.01.2019


# 4
# The Post Iffice use an automated process to read handwritten post-codes.
# Interestingly, British people write the number 7 similar to how
# a Swiss would write the number 1.Unfortunately, this can confuse
# the technology leading to letters from Britain being delyed in the system.
# 
# When a postcode does not contain the number 7, the only 2% of the letters are delayed.
# When a postcode does contain the number 7, then as musch as 10% of the letters are delayed. It is also
# known that 5% of all letters contain the numer 7 in the postcode.

# a) What is the probability that a letter is delayed?

pContain7 = 5/100
pDelayed = 2/100
pContain7Delayed = 10/100

#Wie ist die Warscheinlicheit, dass keine 7 vorkommt.
pContainsZero7 = 1-pContain7
#Wie hoch ist die Warscheinlichkeit, das keine 7 vorkommt, der brief aber verspätet ist
# P(pDelayed|!pContain7) => P(pDelayed ∩ pContainsZero7) = pDelayed * pContainsZero7
pDelayedWithout7 = pDelayed * pContainsZero7

#Wie hoch ist die Warscheinlichkeit, das eine 7 vorkommt, der brief aber verspätet ist
# P(pContain7Delayed|pContain7) => P(pContain7Delayed ∩ pContain7) = pContain7Delayed * pContain7
pDelayWith7 = pContain7Delayed * pContain7

pDelay = pDelayedWithout7 + pDelayWith7


#b) If a letter is deleyed, what is the probability that it conatins the number 7 in it's postcode?
pDelayWith7/pDelay

# c) What proportion of the letters in postOffice are sent
# tot the region Graubünden that is, those with poscodes that begin with 7?

allLetters = length(postcode)
max(postcode) #so finden wir heraus, ob es grössere as 10'000 hat
min(postcode[postcode!=0])#so finden wir heraus, ob es kleinere as 1'000 hat
gbLetters = length(postcode[postcode>=7000 & postcode <8000])

gbLetters/allLetters

# 5

# a) Sketch an appropriate diagram to represent how the number of letters sent varies on
# a day to day basis through a week.

Monday = length(weekday[weekday=="Monday"])
Tuesday = length(weekday[weekday=="Tuesday"])
Wednesday = length(weekday[weekday=="Wednesday"])
Thursday = length(weekday[weekday=="Thursday"])
Friday = length(weekday[weekday=="Friday"])
Saturday = length(weekday[weekday=="Saturday"])
Sunday = length(weekday[weekday=="Sunday"])

allDays = c(Monday,Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
barplot(allDays,xlab = "Days",ylab = "Number of Letters", names.arg = c("Monday","Tuesday"," Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Some Post Office branches offer a courier service that will try to deliver
# letters as quick as possible as long as the address is within a radius of 10km
# from the branch. In fact, their deliveries have a mean time of 40 minutes with
# a standard deviation of 15 minutes.


# b) If a company uses this service once a day for five days, what is the
# probability that the mean delivery time is greater than 45 mins?

mean = 40
sd = 15
sampleSize = 5
sampleStandardError = sd/sqrt(sampleSize)

pnorm(45,mean=40,sd=sampleStandardError, lower.tail = FALSE) 

# c) The Post Office would like to market a maximum time
# needed when using this service. When a delivery takes longer than this time,
# the Post Office can budget to offer compensation to 5% of such deliverie,
# then what maximum time should they then dadvertise?


qnorm(0.95,mean,sd) #64.67 Minutes => 65 minutes




#FS19 N
# 1 At the end of the semester all 34 participants of a university statistics
# course were invited to complete an online feeback survey. Ten of the 
# students completed the survey. The second statement of the survey was:
# How would you rate the diffculty of the course? to wich the possible
# responses were considered as numerical values ranging from 1 to 4. Of 
# the ten responses, the mean was 2.1 with a standard deviation of 0.7.

# a) Explain whether or not the sample is representative?
# Nein, es ist nicht representativ. Denn die restlichen Studenten würden gezwungen werden
# die umfrage zu verfollständigen. Somit kann man sample und population nicht mit einander vergleichen.

# b) Give and appropriate of 95% confidence interval for the difficulty of the course.

s = 0.7
n = 10
xbar = 2.1
alpha = 1-0.95
z = qnorm(alpha/2)
ME = abs(z)*s/sqrt(n)
xbar-ME
xbar+ME

# c) What is the minimal sample size that would lead to 95% confidence interval
# for the difficulty of the course to have a margin of error no larger than 0.2.

marginOFError = abs(z)*s/sqrt(n)
marginOFError*sqrt(n) = abs(z)*s
sqrt(n) = abs(z)*s / marginOFError
n = ((abs(z)*s) / marginOFError)^2

n = ((abs(z)*s) / 0.2)^2

# 2
# The following graph represents a normal distribution that has a mean of 50 and a standard deviation of 10.

# a) What is the probability that a randomly selkected value from this distrubution 
# is between 35 and 62?
1-pnorm(q =62,mean=50,sd=10,lower.tail = FALSE) - pnorm(q =35,mean=50,sd=10,lower.tail = TRUE) 

# b) If a sample of size 40 is randomly selected from this distribution,
# what is the probability that the sample mean will be below 48.

1-pnorm(q =48,mean=50,sd=10/sqrt(40),lower.tail = FALSE)
pnorm(q =48,mean=50,sd=10/sqrt(40),lower.tail = TRUE)

# c) sketch on the same axes above, a graph that represents
# a normal distribution with a mean of 79 and standard deviation of 20.
# Explain your answer.

c = rnorm(500,mean = 79,sd =20 )
barplot(c)

d = rnorm(500,mean = 50,sd =10 )
barplot(d)

#normal, bei 70 zentriert, mehr verteilt

# 3
# The police are using a new radar system to identify cars thath are speeding
# on the motorways. In tests, when a car was speeding, the radar system indicated 
# 95% of the time that the car was indeed speeding. In further testts, when a car was not speeding,
# the radard systen said 0.5% of the time that the car was speeding. Based on national statistics
# the police estimated that where they plan to install the radar system, 2% of the cars are speeding.

# a) What is the probability that at least one car is speeding from the first three cars of the day?
carNotSpeeding = 1-0.02
nCars = 3
1-(carNotSpeeding)^nCars

# b) What is the probability that a car was speeding, given that the radar 
# said that it was speeding?

carSpeeding = 2/100
indicated =95/100
carNotSpeedingButIndicated = 0.5/100


calculate = function(a,b,pANotB){
  a1 = pAB = a * b #A1
  b1 = pNotAB = b-pAB #B1
  c1 = b
  a2 = pANotB * (1-b)
  c2 =1-b #C2
  b2 = c2-a2
  a3 = a1 + a2
  b3 = b1 + b2
  c3 = 1
  
  return(c(a1,b1,c1,a2,b2,c2,a3,b3,c3))
  
}
result = calculate(indicated,carSpeeding,carNotSpeedingButIndicated)
print(result)

carSpeedingAndIndicated = result[1]
indicatedFull = result[7]

carSpeedingAndIndicated/indicatedFull

# c) Waht is the porbability that a car was not speeding, given
# that the radar said that it was not speeding?
result[5]/result[8]


# 4
# The following questions pertain to the file STWI_NExam.xlsx and the data Newborns.

# a) If one person is selected randomly form the whole world, as represented by this datam was is the probability
# that the person is from China?

newBornData = read.xlsx(xlsxFile = './STWI_NEXAM_FS19.xlsx')
attach(newBornData)

fullPopulation = sum(population)
populationChina = population[country =="China"]

populationChina/fullPopulation

# b) What proportion of countries have a gni below 2000 per individual?

gnis =  gni[gni !="_"]
gnisWithData =  length(gni[gni !="_"])

gniBelow2000 = length(gni[as.numeric(gni)<2000 & gni !="_"])

gniBelow2000/length(population)

# c) Calculate the mean GNI of the three countrie whose data is displayed in the table below.

weight<-c(874742,5413971,65696689)
xvar<-c(3680,48420,42420)
sum(xvar*weight)/sum(weight)

# 5
# The following questions pertain to the file STWI_NExam.xlsx and the data Newborns.

# a) Explain whether there is an association between
# the percentage of skilled attendants present at a birth and the
# first day mortaility rate.
attendantFiltered = as.numeric(attendant[mortality!="_" & attendant!="_"])
mortalityFiltered = as.numeric(mortality[mortality!="_" & attendant!="_"])

cor(attendantFiltered,mortalityFiltered)

plot(attendantFiltered,mortalityFiltered, type = "p") 
abline(lm(mortalityFiltered~attendantFiltered))

#Left skewed, small negative association

# b) Explain whether rhere is an association between the GNI of a country
# and the first day mortality rate.

gniFiltered = as.numeric(gni[mortality!="_" & gni!="_"])
mortalityFiltered = as.numeric(mortality[mortality!="_" & gni!="_"])

cor(gniFiltered,mortalityFiltered)

plot(gniFiltered,mortalityFiltered, type = "p") 
abline(lm(mortalityFiltered~gniFiltered))


#Not linear, so cor does nothing say. 

# c) Its claimed that European countries have in general a lower mortality rate than the USA
# Verify whether this claim can be be accepted or not using an appropirate hypothesis test.

europeMortality = as.numeric(mortality[europe==1 & mortality !="_"])
usaMortality = as.numeric(mortality[country=="United States"])
mean = mean(europeMortality)
sd = sd(europeMortality)


#H0 = m <= usa
#H1 m > usa

t.test(x = europeMortality,mu = 3,alternative = "less")
#we accept the claim => p.value = 2.295 * 10^-9 => this is less than 0.05

#Serie 0 HS19
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

#P(t<5) = (5 - (6-(10-6)))* (1/(10-(6-(10-6)))) = (5-2)*1/8 = 3/8 = 0.375

pT = (5 - (6-(10-6)))* (1/(10-(6-(10-6))))


#HS 2016

# 1. The following questions pertain to the file STWI_EXAM_FS16.tns and
# the data Swiss Historical Data.
library(openxlsx)
data = read.xlsx(xlsxFile = './HS2016.xlsx')
attach(data)

# a) A historical researcher would like to use this data to help
# understand how Switzerland as a whole was at around the year
# 1888. Explain whether or not, this data can be considered statistically
# valid for such a research project.

#Nein, nicht representativ, es hat nur 47 Städte und alle im französchischen teil

# b) We call a province Catholic if the percentage catholic
# is greater than 50%. Which Catholic province has the lowest percentage
# of infant mortality?

catholicMin = min(infant_mortality[catholic>50])
province[infant_mortality==catholicMin]


# c) Is it true that the fertility measure for Catholic provinces
# are on average larger than the fertility measure for non Catholic
# provinces? Solve this question with help of an appropriate hypothesis
# test.

catholicFerility = fertility[catholic>50]
nonCatholicFerility =  fertility[catholic<=50]
#H0 P(catholic)<= P(non)
#H1 P(catholic) > P(non)

t.test(x = catholicFerility,mu = mean(nonCatholicFerility),alternative = "greater")
t.test(x = nonCatholicFerility,mu = mean(catholicFerility),alternative = "less")

#Mit einem pValue wert von 0.0035 kann man davon ausgehen, dass die aussage nicht stimmt

# 2.
# a ) We consider the following extension to a sub table of the
# original data set, where the population’s size is given for the four
# indicated provinces.

# Province Catholic (%) Population
# Moutier   34          7629
# Broye     90          30035
# Gruyere   97          2090
# Aigle     8           9771

# Calculate the percentage of Catholic inhabitants from these four
# provinces.


catholicPeople = 0.34 * 7629 + 0.9 * 30035 +0.97*  2090 + 0.08 * 9771
allPeople = 7629 + 30035 +2090 + 9771
catholicPeople/allPeople


# b) Describe graphically the distribution of the variable catholic,
# including an explanation why the distribution has the shape it
# has.
hist(catholic)
mean(catholic) #the top of the courve

# c) Let us assume that the variable catholic is a population
# from which we can create random samples of size 20. Sketch on the
# same axes as for part (b) the sampling distribution of the sample
# mean for such samples

#Mean should be the same => 41
sd = sd(catholic)/sqrt(20) #=> 9.325

# 3. Dan has been living in Switzerland for a while now and has gained the
# ability to identify Swiss regional accents. Let us say that when a person
# from Canton Bern speaks, Dan will correctly identify that the person
# does indeed come from Canton Bern with a probability of 70%. When
# a person comes from elsewhere in Switzerland, 30% of the time Dan
# will still, incorrectly, think that the person comes from Canton Bern.
# Federal statistics show that of the 8 379 477 Swiss population, 1 009 418
# live in Canton Bern.

# a) On the train to work, Dan takes a seat among three other
# people who we may assume are all Swiss. What is the probability
# that at least one of the three does not come from Canton Bern?

indeedBern = 70/100
indeedRestAsBern = 30/100

peopleOfSwitzerland = 8379477
peopleOfBern = 1009418

percentageOfBern = peopleOfBern/peopleOfSwitzerland

percentageNotBern = 1-percentageOfBern
1-(percentageOfBern)^3 # 0.99

# b) Behind Dan, he can hear the mobile telephone conversation
# of a fellow passenger. Dan believes that this person comes from
# Canton Bern. What is the probability that the person really does
# indeed come from Canton Bern?

danIndeedBernAndTrue = indeedBern * percentageOfBern
danIndeedNotBernButIsBern = percentageOfBern - danIndeedBernAndTrue 
danIndeedBernAndItItsFalse = (1-percentageOfBern) * indeedRestAsBern

danIndeedBernAndTrue / (danIndeedBernAndTrue+danIndeedBernAndItItsFalse) #0.242



# 4. Historical Walks AG is a small company that specialises in walking tours
# around Swiss cities. Each walking tour involves a small group of people:
# 5% of the time that group is an individual; 10% of the time it is
# group of two people. 15% of the time it is a group of three people
# the percentages are the same for groups of four (15%), five and six.
# whereas 4% of the time the groups contain seven or more peope.

# 1   5%
# 2   10%
# 3   15%
# 4   22%
# 5   22%
# 6   22%
# >=7 4%

# a) What is the probability that a group contains exactly five people?
# 0.22

# b) What is the probability that a group will contain seven people given that there are already
# six people in the group?

# 6 => 22%
# 7 => 4%
4/(22+4)


# c) The company create a survey to find out how their customers rate their tours.
# Each customer can give a raiting ranging from 0 (meaning terrible) to 10
# (meaning super). We may asume that the standard deviation for the popluation is 3.2.
# Their idea is to collect enough data such that the margin of error of the reuslting
# 99% confidence interval is no larger thatn 1 point. Whatr is the minimum sample size
# that archives their wishes?

alpha = 1-0.99
z = qnorm(alpha/2)
s = 3.2
marginOFError = abs(z)*s/sqrt(n)
marginOFError*sqrt(n) = abs(z)*s
sqrt(n) = abs(z)*s / marginOFError
n = ((abs(z)*s) / marginOFError)^2

n = ((abs(z)*s) / 1)^2
#=> 68 people


# 5
# a) Explain whether there is an association between education
# and fertility. Your answer should include an appropriate numerical
# measure, an appropriate graphical representation and an
# appropriate conclusion.


cor(education,fertility)

plot(education,fertility, type = "p") 
abline(lm(fertility~education))

# b) 
# Assuming that the variable fertility is normally distributed,
# what is the probability that a randomly selected province
# from the population of all Swiss provinces, has a fertility measure
# between 80 and 90?

1-pnorm(q =90,mean=mean(fertility),sd=sd(fertility),lower.tail = FALSE) - pnorm(q =80,mean=mean(fertility),sd=sd(fertility),lower.tail = TRUE) 

# c)
# Although no data is given for Bern, it is commonly believed
# that 10% of the draftees from Bern received an education
# beyond primary school. A report from the time mentioned a football
# match that took place among a group of 22 draftees from
# Bern, yet within that group five of the draftees had received an
# education beyond primary school. Devise an appropriate hypothesis
# test to test whether this football group gives evidence against
# what is commonly believed.

sampleSize = 22
beyondPrimarySchool = 10/100
beyondPrimaryCount = 5
pHat = 5/22
prop.test(x=10,n=100, p = pHat)



