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
prop.test(x=100,n=10, p = pHat)

