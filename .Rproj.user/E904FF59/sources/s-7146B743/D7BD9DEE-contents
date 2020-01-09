library(openxlsx)
schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')
attach(schoolData)



#1
# When appropriate, some of the following exercises are based on the data file
# STWI SchoolData.xlsx.
#a) How many girls are taller than the mean height of the boys?

meanHeightBoys = mean(height[gender=='M'])
meanHeightGirls = mean(height[gender=='F'])

countGirlsTallerThanMeanHeightOfBoys = length(height[gender=="F" & height > meanHeightBoys])
# b) Will this value above be similar to the number of boys who are shorter
# than the mean height of the girls?
countBoysSmallerThanMeanHeightOfGirls = length(height[gender=="M" & height < meanHeightGirls])

#2
# The wisdom of crowds says that the combined knowledge of a crowd is
# better than the knowledge of any one person. We can test this wisdom by
# way of the jar data. There were in fact 557 M&Ms in the jar.
# a) Find the mean jar value.
meanJar = mean(jar)
# b) Find the date of birth of the person who has made the best guess.
datesOfBirth = as.Date(dob,origin='1899-12-30')
datesOfBirth[abs(jar-557)==min(abs(jar-557))]
#abs verwandlet alles in absolute zahlen => -5 = 5
# c) How many individuals are closer to the actual jar value than they were
# to the class average?

#wir nehmen hier den mean als average
numbersPeopleCloserToActualJarValue = length(jar[abs(jar-557)<abs(jar-meanJar)])

#3 In three consecutive days, Dan earns 100, 110 and 120 CHF. What is the
# average earnings that Dan made per day?

dansEarning = c(100,110,120)
mean(dansEarning)

#4 In four consecutive years, Sarah’s investments have increased by 5%, 10%,
# 15% and 20%. What is the average percentage increase that Sarah’s investments
# have increased per year?
# hier nehmen wr die zahlen des wachstums : aus 100 wird 105. dann wird aus 100 110, aber 100 entspechen da ja schon den 105 ;)
#1/4 wird genommen, da es 4 investments sind
average = ((1.05*1.1*1.15*1.20)^(1/4) * 100)-100

#5 Consider a Formula 1 driver going round the same track three times. The
# first lap his speed was 150 kmh^-1, the second lap the speed was 130 km/h
# and the final lap was at a speed of 175 km/h. What is the average speed
# per lap?
# Anmerkung : kmh^-1 dient hier zur verwirrung. kein mensch schreibt das so
# kmh^-1 = km*h^-1 = km*1/h = km/h 
# 3 ist die Rundenanzahl
averagePerLap = 3/(1/150+1/130+1/175)

#6
# For the following variables compare the mean with the median. If they are
# not more or less the same, give an explanation for why this is not the case.
# a) jar
median(jar)
mean(jar)

# The mean is 403 whereas the median is 440. This could be due to a slight
# left skewness to the data, which the histogram does indeed indicate.
# Mean ist effektiv der Durchscnitt
# Median ist der Wert welcher in der Mitte liegt
# Man merke : mean<median => left skewed 

#b) fruit
median(fruit)
mean(fruit)
#Da hier die die Werte nahe bei einander liegen, kann man sagen es ist symetrisch 

#c) distance
#mean>median => right skewed
median(distance)
mean(distance)

#Achtung!!!!!!! das sind nur spekulationen! Grosse Ausreiser können dies abändern
#Angenommen die Normale Distanz vom Schulweg ist 500m-1km, jedoch kommt ein Schüler jeden Tag von ZH mit über 100km, dann stimmt das nicht

# 7
# Before calculating, which value would you expect to be higher: the mean or
# the median of cash?

# Similar to the distance argument above, we only need an outlier of someone
# holding a lot of cash and the data would be right skewed, pushing the mean to be
# higher than the median.



#8 What is the mean cost of a present for a sibling for a member of this class?

onlySibling = present[siblings>=1]/siblings[siblings>=1]
mean(onlySibling)

#????????????????????????????


#9 Find the range of the following lists:
#a) height
rangeHeight = range(height)
diffRange = rangeHeight[2]-rangeHeight[1]
#b) maths
rangeMaths = range(maths)
diffRange = rangeMaths[2]-rangeMaths[1]
# c) cash 
rangeCash = range(cash)
diffRange = rangeCash[2]-rangeCash[1]

#10 Find the interquartile range of the following lists:
#a) height
quantile(height,0.75)-quantile(height,0.25)
#or
calculateInterquantile(height)               
#b) maths
quantile(maths,0.75)-quantile(maths,0.25)
#c) cash
quantile(cash,0.75)-quantile(cash,0.25)

#11 Find the standard deviation (as a sample) of the following lists:
#a) height 
sqrt(sum((height-mean(height))^2)/(length(height)-1))
sd(height)
#b) maths 
sqrt(sum((maths-mean(maths))^2)/(length(maths)-1))
#c) cash 
sqrt(sum((cash-mean(cash))^2)/(length(cash)-1))

#12 Verify whether the empirical rule holds for the following data sets:
# Ungefähr 68% der Daten liegen innerhalb einer Standardabweichung des Mittelwertes
# (oder zwischen dem Mittelwert - einmal die Standardabweichung und dem Mittelwert
#   + 1 mal die Standardabweichung). 
# In der mathematischen Notation wird dies dargestellt als: μ ± 1σ 

# Interval 1: [mean-1*sd,mean+1*sd] => 68
# Interval 2: [mean-2*sd,mean+2*sd] => 95
# Interval 3: [mean-3*sd,mean+3*sd] => fast alle
#a) height 

interval = c(mean(height)-sd(height),mean(height)+sd(height))
lenghtHeight = length(height)
lenghtHeightInteval1 = length(height[height>=interval[1] & height<=interval[2]])
lenghtHeightInteval1/lenghtHeight*100

interval2 = c(mean(height)-2*sd(height),mean(height)+2*sd(height))
lenghtHeightInteval2 = length(height[height>=interval2[1] & height<=interval2[2]])
lenghtHeightInteval2/lenghtHeight*100

interval3 = c(mean(height)-3*sd(height),mean(height)+3*sd(height))
lenghtHeightInteval3 = length(height[height>=interval3[1] & height<=interval3[2]])
lenghtHeightInteval3/lenghtHeight*100

calculateEmpiricalRule <- function(values) {
  lenghtValues = length(values)
  
  interval = c(mean(values)-sd(values),mean(values)+sd(values))
  lengthInterval1 = length(values[values>=interval[1] & values<=interval[2]])
  first = lengthInterval1/lenghtValues*100
  
  interval2 = c(mean(values)-2*sd(values),mean(values)+2*sd(values))
  lengthInterval2 = length(values[values>=interval2[1] & values<=interval2[2]])
  second = lengthInterval2/lenghtValues*100
  
  interval3 = c(mean(values)-3*sd(values),mean(values)+3*sd(values))
  lengthInterval3 = length(values[values>=interval3[1] & values<=interval3[2]])
  third = lengthInterval3/lenghtValues*100
  
  return(c(first,second,third))
}

# b) frutit
calculateEmpiricalRule(fruit)
# c) pendulum
calculateEmpiricalRule(pendulum)
# d) reaction1
calculateEmpiricalRule(reaction1)


#13 Calculate the coefficient of variation of the following data sets
# a) height

sd(height)/mean(height)*100
#or
calculateCoefficientOfVariation(height)
# b) maths
sd(maths)/mean(maths)*100
# c) cash
sd(cash)/mean(cash)*100

#14
# Intercontinental Hotels Group (IHG) owns and runs several chains of hotels,
# including Holiday Inn, in 100-plus countries across the world. The data
# found in IHG.xlsx shows customer ratings (out of 5) for Holiday Inn
# hotels in 25 large European cities (ihg.com, March 2015).

ihg = read.xlsx(xlsxFile = './IHG.xlsx')
attach(ihg)
#a) Compute the mean and median.
mean(Customer.ratings)
median(Customer.ratings)

# b) Explain whether or not, it would be better to use the mean over the
# median as the measure of central location for these data?
# Da es keine offensichtlichen Ausreißer gibt,
# ist der Mittelwert die geeignetere Maßnahme.

# c) Compute the first and third quartiles as well as the IQR.
first = quantile(Customer.ratings,0.25)
last = quantile(Customer.ratings,0.75)
iqr = quantile(Customer.ratings,0.75)-quantile(Customer.ratings,0.25)

#d) Compute the 85th percentile.
quantile(Customer.ratings, 0.85)


#15
# Police records show the following numbers of daily crime reports for a
# sample of days during the winter months and a sample of days during the
# summer months.
# Winter: 18 20 15 16 21 20 12 16 19 20
# Summer: 28 18 24 32 18 29 23 38 28 18

winter = c(18,20,15,16,21,20,12,16,19,20)
summer = c(28,18,24,32,18,29,23,38,28,18)

#a) Compute the range and interquartile range for each period.

rangeWinter = max(winter) - min(winter)
range(winter)
quantile(winter,0.75)-quantile(winter,0.25)

rangeSummer = max(summer) - min(summer)
range(summer)
quantile(summer,0.75)-quantile(summer,0.25)
IQR(summer)

# b) Compute the variance and standard deviation for each period.

var(winter)
sd(winter)

var(summer)
sd(summer)

# c) Compute the coefficient of variation for each period.
sd(winter)/var(winter)
sd(summer)/var(summer)

# d) variability
#summer ist sid grösser

#16 Verify the mean and standard deviation (as a sample) of the z-scores for the following lists:
# a) height
height.z = (height-mean(height))/sd(height)
sd(height.z)

# b) fruit
fruit.z = (fruit-mean(fruit))/sd(fruit)
sd(fruit.z)

#c) house
house.z = (house-mean(house))/sd(house)
sd(house.z)
# z-Scores messen die relative Anordnung von Datenelementen innerhalb eines 
# Datensatz.  Manchmal wird er auch der standardisierte Wert genannt. sd(z-score) ist immer 1

#17 Calculate the skewness for the following lists:

# a) height
lengthHeight = length(height)
skewnessHeight = lengthHeight/((lengthHeight-1)*(lengthHeight-2))*sum(height.z^3)
skewnessHeight2 = calculateSkewness(height)
# b) fruit
lengthFruit = length(fruit)
fruit.z = (fruit-mean(fruit))/sd(fruit)
skewnessFruit = lengthFruit/((lengthFruit-1)*(lengthFruit-2))*sum(fruit.z^3)
# c) distance
lenghtDistnace = length(distance)
distance.z = (distance-mean(distance))/sd(distance)
skewnessDistance = lenghtDistnace/((lenghtDistnace-1)*(lenghtDistnace-2))*sum(distance.z^3)
# d) cash
lenghtCash = length(cash)
cash.z = (distance-mean(cash))/sd(cash)
skewnessCash = lenghtCash/((lenghtCash-1)*(lenghtCash-2))*sum(cash.z^3)
# e) hair of girls
hairGirls = hair[gender=='F']
lenghtHairGirls = length(hairGirls)
hairGirls.z = (hairGirls-mean(hairGirls))/sd(hairGirls)
skewnessHairGirls = lenghtHairGirls/((lenghtHairGirls-1)*(lenghtHairGirls-2))*sum(hairGirls.z^3)

#18 Create side by side boxplots to compare the following pairs of data:
# a) hair gender
boxplot(hair~gender,horizontal = TRUE)
# b) distance transport
boxplot(distance~transport, horizontal = TRUE)
# c) fruit eye
boxplot(fruit~eye, horizontal = TRUE)

#19 Compare the following pairs of data by way of a scatter plot, a line of best fit and the correlation coefficient:
# a) height and foot
par(mfcol=c(1,1))

plot(height, foot, main="Scatterplot Height Foot", xlab="Height ", ylab="Foot") 
abline(lm(foot ~ height))
# b) hair and foot
plot(hair, foot, main="Scatterplot Hair Foot", xlab="Hair ", ylab="Foot") 
lines(cor(hair,foot), col="blue") # lowess line (x,y) 
abline(lm(foot ~ hair))
# c) mother and father
plot(mother, father, main="Scatterplot Mother Father", xlab="Mother ", ylab="Father")
lines(cor(mother,father), col="blue") # lowess line (x,y) 
abline(lm(mother ~ father))

#20 Here we want to consider the association between height and hair with respect to the gender.
#a) Calculate the correlation between height and hair.
correlationHairGender = cor(height,hair)
# wie kleiner, desto kürzer sind die haare

# b) Consider part a but now restrict the data only to the girls. Would you
# expect the correlation to be stronger or weaker? Check your intuition
# by way of calculation.

correlationHairGenderGirls = cor(height[gender=='F'],hair[gender=="F"])
#immer noch stark abhängig

# c) Consider part a but now restrict the data only to the boys. Would you
# expect the correlation to be stronger or weaker? Check your intuition
# by way of calculation.
correlationHairGenderBoys = cor(height[gender=='M'],hair[gender=="M"])
#komplett in die gegenrichtung

# Das Simpson-Paradoxon (auch simpsonsches Paradoxon oder Simpson’sches Paradoxon, benannt nach Edward Hugh Simpson)
# ist ein Paradoxon aus der Statistik. Dabei scheint es, dass die Bewertung verschiedener Gruppen unterschiedlich ausfällt,
# je nachdem ob man die Ergebnisse der Gruppen kombiniert oder nicht. Dieses Phänomen tritt oft bei statistischen
# Auswertungen in den Sozialwissenschaften und in der Medizin auf. 
# Das Simpson-Paradoxon ist möglich, wenn mehrere Vierfeldertafeln mit einem Chancenverhältnis kleiner (größer)
# als 1 zu einer Gesamttafel zusammengefasst werden, die einen Chancenquotienten größer (kleiner) als 1 aufweist.

#21 What can be said about the formula for covariance if both lists are of the same data?
#Sie entspricht dann der Definition für die Abweichung. Das heißt: Sxx = Sx.

#22-24 => estimating of plots, lueg ds eifach ir unterlage noche

#25 In 2008, stock markets around the world lost value. The website www.owneverystock.com
# listed the percentage falls in stock market indices between the start of the
# year and the beginning of October. The data can be found in Stock.xlsx.
detach(schoolData)
stockData = read.xlsx(xlsxFile = './STOCK.xlsx')
attach(stockData)
#a) What are the mean and median percentage changes for these countries?
mean(stockData$`%.Fall`)
median(stockData$`%.Fall`)
#b) What are the first and third quartiles?
quantile(`%.Fall`,0.25)
quantile(`%.Fall`,0.75)

#c) Do the data contain any outliers? Construct a box plot.
boxplot(`%.Fall`) #no outliners

#d) What percentile would you report for Belgium?
# Belgium is just below Q3, so we can say it is just
# below the 75th percentile.

detach(stockData)

#26 Stock markets across the Eurozone tend to have mutual influences on each other. 
#The index levels of the German DAX index and the French CAC 40 index
#for the first 10 weeks of 2015 can be found in DAX CAC 2015.xlsx.
daxData = read.xlsx(xlsxFile = './DAX_CAC 2015.xlsx')
attach(daxData)
# a) Compute the sample correlation coefficient for these data.
cor(DAX[!is.na(DAX)],CAC[!is.na(CAC)])
# b) Are they poorly correlated, or do they have a close association?
#Sie haben eine sehr starke positive lineare Assoziation.
detach(daxData)