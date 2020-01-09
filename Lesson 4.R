# 1 Consider the experiment of tossing a coin three times.
# a) Develop a tree diagram for the experiment.
coin<-c('H','T')
sample(coin,size = 3, replace = TRUE)
# b) List the experimental outcomes.
# Random comninations of H & T with the size of 3

#c) What is the probability for each experimental outcome?
1/2*1/2*1/2 # => 1/8
#better => probability^size
(1/2)^3

#2 
# A company that franchises coffee houses conducted taste tests for a new
# coffee product. Four blends were prepared, then randomly chosen individuals
# were asked to taste the blends and state which one they liked best. Results of
# the taste test for 100 individuals are given.

#a) Define the experiment being conducted. How many times was it repeated?

# 4 => Blens , 100 => people

# b) Prior to conducting the experiment, it is reasonable to assume preferences
# for the four blends are equal. What probabilities would you
# assign to the experimental outcomes prior to conducting the taste test?
#   What method did you use?

#???????????????????????????????????????? hat auch keine lösungen



#3 
# Consider the experiment of rolling a pair of dice. Suppose that we are
# interested in the sum of the face values showing on the dice.
dice<-c(1:6)
#a) How many sample points are possible?How many sample points are possible?
length(dice)^2

#b) List the sample points.
row1<-c(2:7)
row2<-c(3:8)
row3<-c(4:9)
row4<-c(5:10)
row5<-c(6:11)
row6<-c(7:12)
rowCombination = c(row1,row2,row3,row4,row5,row6);
matrix(rowCombination,nrow=6,byrow=TRUE)
#with => combn
combn(x=rowCombination,1)

#c) What is the probability of obtaining a value of 7?
library(MASS)
amountOfSeven = print(length(which(rowCombination==7)))
fractions(amountOfSeven/(6*6))

#d) What is the probability of obtaining a value of 9 or greater?

amountHigherEight = print(length(which(rowCombination>8)))
fractions(amountHigherEight/(6*6))

#e) Because each roll has six possible even values (2, 4, 6, 8, 10 and 12)
# and only five possible odd values (3, 5, 7, 9 and 11), the dice should
# show even values more often than odd values. Do you agree with this
# statement? Explain.

evenNumbers = c(2,4,6,8,10,12)
oddNumbers = c(3,5,7,9,11)

ammountOfEvenNumbers = length(which(rowCombination %in% evenNumbers))
ammountOfOddNumbers = length(which(rowCombination %in% oddNumbers))
p.even = ammountOfEvenNumbers/36
p.odd = ammountOfOddNumbers/36
fractions(p.even)
fractions(p.odd)
#Es ist genau gleich gross, da es zwar mehr verschieden geraden nummern hat, aber zB. die 12 nur mit 6+6 gewürfelt werden kann
# und zb die 11 mit 5+6 (das kann 2mal passieren)

#f) What method did you use to assign the probabilities requested?
#schaue oben => Classical. A probability of 1/36 is assigned to each experimental outcome.

#4 
# Suppose that a manager of a large apartment complex provides the following
# subjective probability estimates about the number of vacancies that will exist
# next month.

#a) No vacancies.
paste(round((0.1)*100,digits=3),"%",sep="")
#b) At least four vacancies.
paste(round((0.15)*100,digits=3)+round((0.1)*100,digits=3),"%",sep="")
#c) Two or fewer vacancies
paste(round((0.3+0.15+0.1)*100,digits=3),"%",sep="")


# 5
# Here we will create our own Monte Carlo simulations. Open a new document.
# We need to set-up the calculators to be different to each other with respect
# to their random number generation. To do this we set a new randSeed
# in a calculator page. Now open a new spreadsheet page where we will
# generate our data.

#Keine Lösungen

# 6
# A survey of magazine subscribers showed that 45.8 per cent rented a car
# during the past 12 months for business reasons, 54 per cent rented a car
# during the past 12 months for personal reasons, and 30 per cent rented a car
# during the past 12 months for both business and personal reasons.

#a) What is the probability that a subscriber rented a car during the past 12 months for business or personal reasons?

businessPercentage = 45.8
personalPercentage = 54
combinedPercentage = 30

businessOnly = 45.8-30
personalOnly = 54-30

businessOrPersonal = businessPercentage + personalOnly
paste(round((businessOrPersonal),digits=3),"%",sep="")

#b) What is the probability that a subscriber did not rent a car during the past 12 months for either business or personal reasons?

notRent = 100-businessOrPersonal
paste(round((notRent),digits=3),"%",sep="")


# 7
# Employee A does the finishing work on 40% of the output at a clothing
# factory, employee B does 35% and employee C 25%. Of the items finished
# by employee A, 10% are defective, 20% of the items finished by B are
# defective and 30% of those finished by C.

employeeA = 40
employeeB = 35
employeeC = 25

employeeADefective = 10
employeeBDefective = 20
employeeCdefective = 30

# a) If an item is selected at random from the large number of items finished
# by the factory during the day, what is the probability it is defective?
# What is the probability it is non-defective?

outputA = employeeA * ((100-employeeADefective)/100)
outputB = employeeB * ((100-employeeBDefective)/100)
outputC = employeeC * ((100-employeeCdefective)/100)
#completOutput = outputA+outputB+outputC -1 => this is the fix
completOutput = outputA+outputB+outputC
# Ich habe 81.5 erhalten, die Lösungen sagen 80.5. fehler?????

#b) What is the probability that a defective item was finished by C?

fullChangeOfDefective = 100-80.5

defectByA = 40-outputA
defectByB = 35-outputB
defectByC = 25-outputC

fullDefect = defectByA + defectByB + defectByC

# howBigIsTheChanceOfC = defectByC/(fullDefect+1) => this is the fix 
howBigIsTheChanceOfC = defectByC/(fullDefect)

#c) What is the probability an item is defective and was finished by C?

fullChangeOfDefective * defectByC/(fullDefect+1)
#(fullChangeOfDefective * defectByC/(fullDefect+1))+1 => fix

# d) If an item is non-defective, what is the probability it was finished by B?

chanceOfNonDefective = 80.5
outputB/chanceOfNonDefective

# e) What is the probability an item is non-defective and/or finished by C?

#1-(outputC)/(chanceOfNonDefective)+0.1 => this is the fix auch hier fehlen 10%
1-(outputC)/(chanceOfNonDefective)

#f) What is the probability an item is not finished by A or C, and is not defective?
# => logik: es muss durch B fertigtgestellt werden outputB sind nur die ganzen kleider

chanceOfB = outputB/100

# 8
# A sample of convictions and compensation orders issued at a number of
# Manx courts was followed up to see whether the offender had paid the
# compensation to the victim. Details by gender of offender are as follows:

# Offender gender   Paid in full  Part paid   Nothing paid
#           Male    754           62          61
#           Female  157           7           6

#a) What is the probability that no compensation was paid?

fullConvictionsMale = 754 + 62 + 61
fullConvictionsFemale = 157 + 7 + 6

fullConvictions = fullConvictionsMale + fullConvictionsFemale

notPaid = 61 + 6
chanceOfNotPaid = notPaid/fullConvictions
paste(round((chanceOfNotPaid*100),digits=2),"%",sep="")


#b) What is the probability that the offender was not male given that compensation was part paid?

chanceOfNotMale = fullConvictionsFemale/fullConvictions #hier wird berechnet wie gross die chance ist, dass es kein mann war
chanceOfNotMalePartPaid = 62/fullConvictions #die chance dass es ein mann war der nur ein teil bezahlt hat von der population

chanceFemalePartPaid = 7/fullConvictionsFemale

chanceOfNotMale*chanceFemalePartPaid  #das sollte die lösung sein => aber nicht sicher

69/fullConvictions #die möglichkeit, dass es zum teil gezahlt wird 
#???????????????????????????????????????????????????

# 9
# The prior probabilities for events A1, A2, and A3 are P(A1) = 0.20,
# P(A2) = 0.50 and P(A3) = 0.30. The conditional probabilities of event
# B given A1, A2, and A3 are P(B | A1) = 0.50, P(B | A2) = 0.40 and
# P(B | A3) = 0.30.

#die warscheinlichkeiten von a1,a2 und a3
a1.p = 0.2
a2.p = 0.5
a3.p = 0.3
# die abhängigen warscheinlichkeiten von a1,a2 und a3
b.a1.p = 0.3
b.a2.p = 0.4
b.a3.p = 0.3

# conditional probability: P(A|B) = P(A∩B)/P(B)
# wichtig ist dass P(A|B) != P(B|A) ist => Reihenfolge ist wichtig
#a) Compute P(B∩A1), P(B∩A2) and P(B∩A3).
# Beweis: 
# Wir wissen P(B|A1) = 0.5 
# P(B∩A1)/P(A1) = 0.5 => angewendet: P(A|B) = P(A∩B)/P(B)
# P(B∩A2)/P(A2) = 0.4
# P(B∩A3)/P(A3) = 0.3
# weiter wissen wir P(A ∩ B) = P(A) * P(B) => daraus folgt:
# P(B ∩ A1) = P(B) * P(A1)
# => P(B ∩ A1) = P(B) * 0.2
# => (P(B) * 0.2)/0.2 = 0.5 => P(B)=0.5 => Tada, es wird alles gekürzt bis auf P(B)
b.p.a1 = 0.5 #Achtung, diese P(B) zählt nur im Fall auf A1!!!!! 
b.p.a1 * a1.p
0.4* a2.p
0.3 * a3.p


#b) Apply Bayes’ theorem to compute the posterior probability P(A2 | B).
#This required argument is the prior probability of A, or Pr(A). In this case A2 => 
#This required argument is the conditional probability of B given A or Pr(B|A),
#and is known as the data, evidence, or likelihood.

#wichtig ist, dass Bayes’ theorem nicht mit einem wert auskommt, deshalb auch die summe
PrA = c(0.2,0.5,0.3)  # P(A1)=0.2,P(A2)=0.5,P(A1)=0.3
PrBA = c(0.5,0.4,0.3) # P(B|A1)=0.5,P(B|A4)=0.4,P(B|A3)=0.3
PrAB = (PrBA * PrA) / sum(PrBA * PrA)

#Ein Beispiel mit einem Wert, damit erhält man immer 1!, dies ist der grund wieso aufgabe b eigentlich aufgabe c sein muss
PrA = c(0.2)  
PrBA = c(0.5) 
PrAB = (PrBA * PrA) / sum(PrBA * PrA)


