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

#0.7 +0.1 = 0.8 => die chance dass, er nicht in 3 tagen ankommt is 0.8%

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




        