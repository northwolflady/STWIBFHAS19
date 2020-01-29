schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')
attach(schoolData)

# 1
# It is claimed that boys tend to be taller than girls. Formulate and evaluate an
# appropriate statistical test.

heightMale = height[gender=='M']
heightFemale = height[gender=="F"]

test <- t.test(x=heightMale, y=heightFemale, alternative = "greater")
test$p.value

#0.0003 => ja es stimmt, ist signifikant


# 2
# It is claimed that there is a difference between boys and girls concerning
# their maths marks. Formulate and evaluate an appropriate statistical test.

mathMale = maths[gender=='M']
mathFemale = maths[gender=="F"]

test <- t.test(x=mathMale, y=mathFemale, alternative = "two.sided")
test$p.value

# 3
# It is claimed that girls are more generous than boys. Formulate and evaluate
# an appropriate statsitical test based on relevant data from the data set.
present.total = siblings*present #how much in CHF is spent on a present for one sibling
present.m = present.total[gender=='M']
present.f = present.total[gender=="F"]

test <- t.test(x=present.m, y=present.f, alternative = "less")
t.test(present.f,present.m,alternative='greater')
test$p.value

# P-value<=0.05: strong evidence against the null hypothesis.
# P-value> 0.05; indicates weak evidence against the null hypothesis
# da 0.1214 grösser als 0.05 ist, weisst das auf einen schwachen Beweis hin
# Achtung, da in der Lösung das Resultat 0.026 ist, ist es ein starker Beweist

# 4
# It is claimed that the second time someone did the reaction test their reaction
# speed increased. Formulate and evaluate an appropriate statistical test.

delta = reaction1-reaction2
t.test(delta, mu=0, alternative='greater')

# 5
# It is claimed that there is a difference between the proportion boys with
# brown eyes and girls with brown eyes. Formulate and evaluate an appropriate
# statistical test.

boysBrowEyes = length(eye[gender=="M" & eye=="Brown"])
girlsBrowEyes = length(eye[gender=="F" & eye=="Brown"])

boysEyes = length(eye[gender=="M"])
girlsEyes = length(eye[gender=="F"])

prop.test(x=c(boysBrowEyes,girlsBrowEyes),n=c(boysEyes,girlsEyes),alternative='two.sided')

# 6
# Periodically, Merrill Lynch customers are asked to evaluate Merrill Lynch
# financial consultants and services. Higher ratings on the client satisfaction
# survey indicate better service, with 7 the maximum service rating. Independent
# samples of service ratings for two financial consultants in the Dubai
# office are summarized here. Consultant A has ten years of experience while
# consultant B has one year of experience. Use alpha = 0.05 and test to see
# whether the consultant with more experience has the higher population mean
# service rating.

# 
# Consultant A Consultant B
# n1 = 16       n2 = 10
# x1 = 6.82     x2 = 6.25
# s1 = 0.64     s2 = 0.75

# a)State the null and alternative hypotheses.

#H0 <= 0 H1 >0

# b) What is the p-value? #Laut dem Dozent so korrekt (obwohl die lösung 0.0318 wäre)
n1 <- 16
xbar1 <- 6.82
s1 <- 0.64

n2 <- 10
xbar2 <- 6.25
s2 <- 0.75

s <- sqrt((s1^2)/n1+(s2^2)/n2)
t <- ((xbar1-xbar2)-0)/s

df <- n1+n2
1-pt(t,df)

# c) What is your conclusion?

# p-value < 0.05, reject H0. The consultant with more experience has a higher
# population mean rating.


# 7
# A survey was made of Book-of-the-Month-Club members to ascertain
# whether members spend more time watching television than they do reading.
# In file TVread.xlsx is data of a sample of 15 respondents who
# provided their data on weekly hours of television watching and weekly hours
# of reading. Using a 0.05 level of significance, can you conclude that Bookof-
#   the-Month-Club members spend more hours per week watching television
# than reading?

detach(SchoolData)
TVRead <- read.xlsx(xlsxFile='/...TVRead.xlsx', sheet='Data', colNames=TRUE)
attach(TVRead)

t.test(Television,Reading,alternative="greater")

# 8
# In a test of the quality of two television commercials, each commercial was
# shown in a separate test area six times over a one-week period. The following
# week a telephone survey was conducted to identify individuals who had seen
# the commercials. Those individuals were asked to state the primary message
# in the commercials. The following results were recorded.

#                             Commercial A Commercial B
# Number who saw commercial   150           200
# Number who recalled message 63            60

# Use alpha = 0.05 and test the hypothesis that there is no difference in the recall
# proportions for the two commercials.

pHatA = 63/150
pHatB = 60/200

prop.test(c(63,60),c(150,200),alternative = "two.sided")

# With a p-value of 0.02683 (0.0198 in the solutions), reject H0. There is a difference between the recall
# rates for the two commercials.

# 9
# A large car insurance company selected samples of single and married male
# policyholders and recorded the number who made an insurance claim over
# the preceding three-year period.

# Single policy holders         Married policyholders
# n1 = 400                      n2 = 900
# Number making claims = 76     Number making claims = 90


prop.test(c(76,90),c(400,900),alternative = "two.sided")

# With a p-value effectively zero, we reject H0 and conclude that there is a
# difference between claim rates.
