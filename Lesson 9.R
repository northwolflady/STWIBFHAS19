schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')

attach(schoolData)
# 1
# The sports teacher claim that the mean height of the boys is at least 175 cm.
# Does this sample of boys’ heights support that claim?
maleHeights = heigh[gender=="M"]

test <- t.test(x=maleHeights, alternative = 'greater', mu=175)
test$p.value

# Mit einem t-Test finden wir den nicht signifikanten p-Wert von 0.055. Daher scheitern wir an
# die Null-Hypothese ablehnen. Die Stichprobe kann keinen Beweis für die Behauptung bieten.

# 2
# It is claimed that the mean distance pupils need to get to school is less than
# 15 km. Does the sample distance offer evidence?

test <- t.test(x=distance, alternative = "less", mu=15.0)
test$p.value

# Mit einem t-Test finden wir den p-Wert bei 0.015. Dies gilt als signifikant, aber
# es ist nah dran! In den Lösungen steht 0.04. Jedoch hat man beim t-test nicht viel spielraum für fehler
# die Lösungen könnten falsch sein. 

# 3
# According to the Swiss demographics, the Swiss fertility rate is 1.33 children
# per woman. Does the data set support this claim?

#In der Schweiz hat man 1.33 Kinder 

children = siblings + 1 #Man muss sich selbst mitzählen

test = t.test(x=children, mu=1.33)
val = test$p.value

#Hier erhält man eine wert von ca 0.000169. Eine Annahme kann sein, dass die Geburtenrate grösser ist
#Jedoch sollte man beachten, dass ein Sample von 19 Messwerten alles andere als representativ ist.

# 4
# The French teacher claims that at least 75% of her pupils get a sufficient
# grade. Does this sample of french support that claim?
biggerThan4Length <- length(french[french>=4])
allLeng <- length(french)
test = prop.test(x = biggerThan4Length, n = length(french), p = 0.75, alternative = 'greater')
test$p.value
#Mit dem Proportions tests finden wir einen p-value von 0.0425 (Genau vertauscht mit der Lösung von Aufgabe 2)
# Im One Note ist auch der Wert 0.042 vermerkt

# 5
# Suppose that the mean length of the working week for a population of
# workers has been previously reported as 39.2 hours. We would like to take a
# current sample of workers to see whether the mean length of a working week
# has changed from the previously reported 39.2 hours.

#a) State the hypotheses that will help us determine whether a change
# occurred in the mean length of a working week.
#????????????????????

# b) Suppose a current sample of 112 workers provided a sample mean of
# 38.5 hours. Use a population standard deviation sd = 4.8 hours. What
# is the p-value?

mu0 = 39.2 #mean of all 
sig = 4.8 #sd
n = 112 #sample lenght
xbar = 38.5 #sample mean
pValue = 2*(pnorm(xbar,mu0,sig/sqrt(n))) #So stimmen die Lösungen, wichtig ist es mal 2 zu rechnen, da es 2 dieser bereiche gibt

# c) At alpha = 0.05, can the null hypothesis be rejected? What is your
# conclusion?

# Ein p-value von 0.05 wiederspricht nicht der Anahmen von H0 = 39.2. 
# Wir können nicht feststellen, dass sich die durchschnittliche Länge einer Arbeitswoche geändert hat.

# 6
# Fowler Marketing Research bases charges to a client on the assumption that
# telephone surveys can be completed in a mean time of 15 minutes or less
# per interview. If a longer mean interview time is necessary, a premium rate
# is charged. Suppose a sample of 35 interviews shows a sample mean of 17
# minutes. Use sd = 4 minutes. Is the premium rate justified?

# a) Formulate the null and alternative hypotheses for this application.

#H0 <= 15 and H1 > 15

# b) What is the p-value?

mu0 = 15 #mean of all 
sig = 4 #sd
n = 35 #sample lenght
xbar = 17 #sample mean
pValue = 2*(1-pnorm(xbar,mu0,sig/sqrt(n)))

#?????????????????????????????????????????????????????????

# c) At alpha = 0.01, what is your conclusion?

# p-value < 0.01; reject H0; the premium
# rate should be charged. 
#Ein p-value von 0.05 wiederspricht der Anahmen von H0 = 15.

# 7
# A popular pastime amongst football fans is participation in fantasy football
# competitions. Participants choose a squad of players and a manager, with the
# objective of increasing the valuation of the squad over the season. Suppose
# that at the start of the competition, the mean valuation of all available strikers
# was $4.7 million.

# a) Formulate the null and alternative hypotheses that could be used by a
# football pundit to determine whether midfielders have a higher mean
# valuation than strikers.

#H0 <= 4.7 H1 > 4.7


# b) Suppose a random sample of 30 midfielders from the available list had
# a mean valuation at the start of the competition of $5.80 million, with
# a sample standard deviation of $2.46 million. On average, by how
# much did the valuation of midfielders exceed that of strikers?
  
n = 30 #sample size
meanNew = 5.8 #new mean 
sd = 2.46 #standard deviation
oldMean = 4.7

exceed = meanNew - oldMean

# c) At alpha = 0.05, what is your conclusion?

#With a p-value of 0.01 we reject H0 at alpha = 0.05 and conclude that the mean
#valuation of midfielders is higher than that of strikers.

# 8
# Eagle Outfitters is a chain of stores specializing in outdoor clothing and
# camping gear. They are considering a promotion that involves sending
# discount coupons to all their credit card customers by direct mail. This
# promotion will be considered a success if more than 10 per cent of those
# receiving the coupons use them. Before going nationwide with the promotion,
# coupons were sent to a sample of 100 credit card customers

# a) Formulate hypotheses that can be used to test whether the population
# proportion of those who will use the coupons is sufficient to go national.

#H0 <= 10, H1 > 10

# b) The file Eagle.xlsx contains the sample data. Compute a point
# estimate of the population proportion.
detach(schoolData)
eagleData = read.xlsx(xlsxFile = './Eagle.xlsx')
attach(eagleData)

lengthYes = length(`Used.Coupon?`[`Used.Coupon?`=='Yes'])
lengthAll = length(`Used.Coupon?`)
pHat <- lengthYes/lengthAll
prop.test(x=lengthYes, n=lengthAll, p=0.10, alternative='greater', correct = FALSE)
test = prop.test(x=lengthYes, n=lengthAll, p=0.10, alternative='greater', correct = FALSE)
pValue = test$estimate
# c) Use alpha = 0.05 to conduct your hypothesis test. Should Eagle go national
# with the promotion?

newTest = prop.test(x=13,n = 100, p = 0.1,alternative = "greater")
newTest$p.value
#Der wert 0.20 nicht signifikant ist, empfiehlt sich nich national zu gehen!








