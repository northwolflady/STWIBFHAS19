library(openxlsx)
schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')
attach(schoolData)

# 1
# catorgorise the following variables as being either discrete or continuous:
# Therorie:
# Diskrete und kontinuierliche Zufallsvariablen:
#   
# Eine Variable ist eine Größe, deren Wert sich ändert. 
# 
# Eine diskrete Variable ist eine Variable, deren Wert durch Zählen erhalten wird.
# 
# Beispiele: 
# - Anzahl der anwesenden Schüler
# 
# - Glaskugelanzahl
# 
# - Anzahl der Köpfe beim Wenden von drei Münzen
# 
# - Schuljahr
# 
# Eine stetige Variable ist eine Variable, deren Wert durch Messung ermittelt wird.
# 
# Beispiele: 
# - Höhe der Schüler in der Klasse
# 
# - Gewicht der Schüler in der Klasse
# 
# - Schulwegzeit
# 
# - Klassenunterschied
# 
# 
# Eine Zufallsvariable ist eine Variable, deren Wert ein numerisches Ergebnis eines Zufallsphänomens ist.
# 
# 
# ▪ Eine Zufallsvariable wird mit einem Großbuchstaben bezeichnet
# 
# ▪ Die Wahrscheinlichkeitsverteilung einer Zufallsvariablen X sagt aus, welche Werte von X möglich sind und wie die Wahrscheinlichkeiten diesen Werten zugeordnet werden
# 
# ▪ Eine Zufallsvariable kann diskret oder kontinuierlich sein
# 
# 
# Eine diskrete Zufallsvariable X hat eine zählbare Anzahl von möglichen Werten.
# 
# Beispiel: X sei die Summe zweier Würfel.


#a) height = continous
#b) mother = continous
#c) maths = continous
#d) french = discret
#e) siblings = discret
#f) present = continous

# 2
# Find the probability function for the random variable S that represents the number of siblings.

max(siblings)
length(siblings)
length(siblings[siblings<=0])/length(siblings)
length(siblings[siblings>0 & siblings<2])/length(siblings) #Probabilty lenght of customized / lenght of all
length(siblings[siblings>1 & siblings<3])/length(siblings)
length(siblings[siblings>2 & siblings<4])/length(siblings)
length(siblings[siblings>3 & siblings<5])/length(siblings)
length(siblings[siblings>4 & siblings<6])/length(siblings)

# => with STWIBFH:
calculateConditionalProbabilty(length(siblings[siblings<=0]),length(siblings))

# 3
# Calculate the expected value and the standard deviation of for the random variable siblings.
sd(siblings)
calculateStandardDeviation(siblings)

sib = 0:5
prob = c(0.05,0.58,0.16,0.11,0.05,0.05) # the values of number 2, rounded on 2 digits
mean(prob)
#or
sum(sib*prob)

# 4
# Suppose we know home mortgage rates for 12 Danish lending institutions.
# Assume that the random variable of interest is the number of lending institutions
# in this group that offers a 30-year fixed rate of 1:5 per cent or less.
# What values may this random variable assume?

# Values from 1 to 12

#5
# To perform a certain type of blood analysis, lab technicians must perform
# two procedures. The first procedure requires either 1 or 2 separate steps, and
# the second procedure requires either 1, 2 or 3 steps.

# a) List the experimental outcomes associated with performing the blood analysis.

# 2 * 3 = 6 => S={(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)}

#b) If the random variable of interest is the total number of steps required
# to do the complete analysis (both procedures), show what value the
# random variable will assume for each of the experimental outcomes

# =>   (1,1) => 1+1 , (1,2) => 1+2 = 3

# 6
# The table below summarizes the joint probability distribution for the percentage
# monthly return for two ordinary shares 1 and 2. In the case of share 1,
# the percentage return X has historically been -1, 0 or 1. Correspondingly,
# for share 2, the percentage return Y has been -2, 0 or 2.

# a) Determine E(X), E(Y ), V ar(X) and V ar(Y )

# Diese Tabelle zeigt die gemeinsame Wahrscheinlichkeit, monatlich Rendite in Prozent
# 

#   Share 2,Y
#       -2  0  0 
#   -1 0.1 0.1 0.0   # 0.1+0.1 = 0.2
#   0  0.1 0.2 0.0   # 0.1 + 0.2 = 0.3
#   1  0.0 0.1 0.4   # 0.1 + 0.4 = 0.5   
#
#   0.1+0.1 = 0.2
#   0.1+0.2+0.1 = 0.4
#   0.4 = 0.4
#-1,0,1 ist share 1,x

colMinus2 = 0.2
colZero = 0.4
col2 = 0.4
rowMinus1 = 0.2
rowZero = 0.3
row1 = 0.5

eX = -1 * rowMinus1 + 0 * rowZero + 1 * row1
x = -1 * 0.2 + 0 * 0.3 + 1 * 0.5 # same 

varX = (-1 - eX)^2 * rowMinus1 + (0-eX)^2 * rowZero + (1-eX)^2 * row1
varX = (-1-0.3)^2 * 0.2 + (0-0.3)^2 * (0.3) + (1-0.3)^2 * 0.5 # same 

eY = -2 * colMinus2 + 0* colZero + 2 * col2
y = -2 * 0.2 + 0* 0.4 + 2 * 0.4

varY = (-2 - eY)^2 * 0.2 + (0-eY)^2 * 0.4 + (2 - eY)^2 * 0.4 

varY = (-2 - y)^2 * 0.2 + (0-y)^2 * 0.4 + (2 - y)^2 * 0.4 

# b) Determine the correlation coefficient between X and Y

allData = c(0.1,0.1,0,0.1,0.2,0,0,0.1,0.4)
x = c(-1,0,1)
y = c(-2,0,2)
fullNumber = 0
count = 1
countX = 1
countY = 1
#in dieser schlaufe werden alle 9 rechnungen durchgeführt
while(count<=9){
  if(countX==4){ #der 4. index existiert nicht, somit kann man zurück zum ersten und yIndex um 1 erhöhen
    countX = 1
    countY = countY + 1
  }
  newNumber = (x[countX]-0.3)* (y[countY]-0.4)* allData[count] # 
  fullNumber = fullNumber + newNumber
  countX = countX+1
  count = count +1
}

fullNumber / (sqrt(varX)*sqrt(varY)) #lösung, mega mühsam!!! Auch bei dieser Aufgabe abweichung zur Lösung

#c) What do you deduce from (b)?

#Es besteht ein Zusammenhang => Siehe unten


# Exactly –1. A perfect downhill (negative) linear relationship
# 
# –0.70. A strong downhill (negative) linear relationship
# 
# –0.50. A moderate downhill (negative) relationship
# 
# –0.30. A weak downhill (negative) linear relationship
# 
# 0. No linear relationship
# 
# 
# 
# +0.30. A weak uphill (positive) linear relationship
# 
# +0.50. A moderate uphill (positive) relationship
# 
# +0.70. A strong uphill (positive) linear relationship
# 
# Exactly +1. A perfect uphill (positive) linear relationship


#7
# Suppose we are interested in bidding on a piece of land and we know there
# is one other bidder. The seller announced that the highest bid in excess of
# Euro 10 000 will be accepted. Assume that the competitor’s bid X is a random
# variable that is uniformly distributed between Euro 10 000 and Euro 15 000.


# a) Suppose you bid Euro 12 000. What is the probability that your bid will be accepted?

min = 10000

#x is random between 10'000 and 15'000
# I bid 12'000

my = 12000-10000
other = 15000-10000

probailty = my/other
#or
calculateConditionalProbabilty(my,other)


#b) Suppose you bid e14 000. What is the probability that your bid will be accepted?

myNew = 14000-10000
probailtyB = myNew/other

#c) What amount should you bid to maximize the probability that you get the property?

#you should bid the max. => 15'000

#d) Suppose you know someone who is willing to pay you e16 000 for the
# property. Would you consider bidding less than the amount in part (c)?
#   Why or why not?

(10000 + 16000)/2


# 8
# We assume that the data fruit is representative of the actual population
# of weights for this such fruit. Hence we can assume that the weights are
# normally distributed with a mean of 119 g and standard deviation of 7,2 g.

#a) What is the probability that a randomly selected piece of fruit is somewhere between 110 g and 130 g.

mean = 119
sd = 7.2
pnorm(130,119,7.2)-pnorm(110,119,7.2)
#or
calculateProbabilityWithStandardDeviatioAndMeanForRange(7.2,119,110,130)

#b) At which weight will 75% of these fruits be heavier than?
qnorm(0.25,119,7.2)
#or 
calculateAtWhichNumberAreXPercentBigger(7.2,119,75)
calculateAtWhichNumberAreXPercentBigger(7.2,119,0.75)

#9
# The Attendance at a rock concert is normally distributed with a mean of
# 28 000 persons and a standard deviation of 4000 persons. What is the probability,
# that:
# a) more than 28 000 persons will attend?

sd = 4000
mean = 28000
value = 28000
pnorm(mean,value,sd)
#or
calculateProbabiltyThatMoreThanValue(4000,28000,28000)

#b) less than 14 000 persons will attend?
sd = 4000
mean = 28000
value = 14000
1-pnorm(mean,value,sd)

#or
calculateProbabiltyThatLessThanValue(4000,28000,14000)

#c) between 17 000 and 25 000 persons will attend?
pnorm(25000,28000,4000)-pnorm(17000,28000,4000)
#or
calculateProbabiltyThatBetweenRange(4000,28000,17000,25000)

# d) Suppose the number who actually attended was x and the probability of
# achieving this level of attendance or higher was found to be 5%. What
# is x?
qnorm(0.95,28000,4000)
#or
calculateAtWhichNumberAreXPercentBigger(4000,28000,5)

# 10
# A company has been involved in developing a new pesticide. Tests show that
# the average proportion, p, of insects killed by administration of x units of
# the insecticide is given by p = P(X <= x) where the probability P(X <= x)
# relates to a normal distribution with unknown mean and standard deviation.

# a) Given that x = 10 when p = 0,4 and that x = 15 when p = 0,9,
# determine the dose that will be lethal to 50 per cent of the insect
# population on average.

# 1: x = 10, p = 0.4
# 2: x = 15, p = 0.9

#Question : p=0.5 what is x?
# q norm needs mean
# p norm needs mean


lowQnorm = qnorm(0.4)
highQnor = qnorm(0.9)

# low => 10 - M / sd = qnorm(0.4) => 10-M = sd*qnorm(0.4)
# high => 15 - M / sd = qnorm(0.9) => 15-M = sd*qnorm(0.9)

# high - low => 15-5 -M -(-M) = sd * qnorm(0.9) - sd * qnorm(0.4)
# => 5 = sd * qnorm(0.9) - sd * qnorm(0.4)
# => 5 = (qnorm(0.9)-qnorm(0.4) ) * sd
# => sd = 5/(qnorm(0.9)-qnorm(0.4))
sd = 5/(qnorm(0.9)-qnorm(0.4)) #sd
#=> 10 - M/sd =  qnorm(0.4) => qnorm(0.4)*sd = 10-M
# => M = qnorm(0.4)*sd -10
mean = 10-qnorm(0.4)*sd

# b) If a dose of 17.5 units is administered to each of 100 insects, how many
# will be expected to die?
pnorm(17.5,mean = mean,sd=sd)
