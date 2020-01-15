#Lesson 2########################################

install.packages("openxlsx")
library(openxlsx)
schoolData = read.xlsx(xlsxFile = './STWI_SchoolData.xlsx')

attach(schoolData)
#View(shoolData)

#1
# Find sum(cash) and sum(gender) and explain the difference.
sumCash = sum(schoolData$cash)
#sumGender = sum(shoolData$gender) => typ error

# 2
# Find height/100 and interpret what this data now represents.

heightInMeters = schoolData$height/100
#It represents the height given in metres rather than in the original centimetres.

# 3
# Find reaction2-reaction1 and interpret what this data now represents.

reaction = schoolData$reaction2-schoolData$reaction1

# It represents the gain in reaction time between the first and second attempt,
# where negative values mean there has been an improvement.

# 4
# Find siblings*present and interpret what this data now represents. What
# would the sum of this data set then represent?

siblingPresent = schoolData$siblings*schoolData$present
#The list is the total cost per person in buying presents for siblings, hence the
# total sum 599:5 is the total value spent on buying presents.

# 5
# Find the frequency distribution of transport

frequencyOfTransport = schoolData$transport

frequencyOFBike = length(which(frequencyOfTransport=='Bike'))
frequencyOFBus = length(which(frequencyOfTransport=='Bus'))
frequencyOFCar = length(which(frequencyOfTransport=='Car'))
frequencyOFTrain = length(which(frequencyOfTransport=='Train'))

# 6
# Find the frequency distribution of the data height for bins of width 5 cm
# from 155 cm to 185 cm.


heights = schoolData$height
selectedHeights = heights[heights>155 && heights<185]

bins = 5 * 0:6 + 155 #Schnitte in 5cm abstand

heightsFrequency = table(cut(selectedHeights,bins))
View(heightsFrequency)

# 7
# Find the cumulative frequency the data height for the same bins as of the
# previous question.

heightsFrequencyCumsum = cumsum(heightsFrequency)
View(heightsFrequencyCumsum)

# 8
# Create appropriate histograms for the following variables:
#a) fruit
hist(schoolData$fruit)
#b) pendulum
hist(schoolData$pendulum)

# 9
# Consider the variable height .

#a) Plot an appropriate histogram.

hist(height)
# b) Create two sublists of height , one for each gender. Then plot noth
# histograms on the same scale.

maleHeights = height[gender=='M']
femaleHeights =  height[gender=='F']
#To get the same scale we need to use xlim=c(min(height),max(height)),
# as arguments to hist . To get both plots together as one plot we need
#to place par(mfcol=c(2,1)) before we call the two hist s.
par(mfcol=c(2,1))
xLim = c(min(height),max(height))
hist(x=maleHeights,xlim = xLim)
hist(femaleHeights,xlim = xLim)

# 10
# Consider the variable hair .
# a) Plot an appropriate histogram.
hist(hair)
maleHair = hair[gender=='M']
femaleHair = hair[gender=='F']

par(mfcol=c(2,1))
xLim = c(min(hair),max(hair))
hist(x=maleHair,xlim = xLim)
hist(femaleHair,xlim = xLim)

#10
# Create an appropriate histogram for cash and state whether the data is
# skewed.

hist(cash)

#The histogram shows the data to be right skewed. People with plenty of
# cash can only be balanced by people with negative cash, which cannot be.

#11
# Create height.m the heights of the boys and height.f the heights of the
# girls. How many girls are taller than the shortest boy?


shortestBoy = sort.int(maleHeights)[1]
#or => min(maleHeights)
girsHigherThanShortestBoy = height[gender=='F' & height > shortestBoy]
howManyGirlsAreTaller = length(girsHigherThanShortestBoy)

# 13
# Create foot.m the feet size of the boys and foot.f the feet size of the
# girls. Does the tallest boy have the largest feet for the boys? How about for
# the girls?

height.m = height[gender=='M']
height.f = height[gender=='F']

foot.m = foot[gender=="M"]
foot.f = foot[gender=="F"]

hasTallesBoyBiggestFeet = which.max(height.m) == which.max(foot.m)
hasTalleGirlBiggestFeet = which.max(height.f) == which.max(foot.f)

# 14
# How many boys are taller then their father? How many girls are taller than
# their mother?

boysBigerThanFather = length(height[gender=='M' & height>father])

girlsBiggerThanMotther = length(height[gender=='F' & height>mother])

#15
# Although it is no longer the case, recruits to the British police force needed
# to be of a minimum height: 173 cm and 163 cm, for men and women respectively.
# How many people in our data are tall enough to have been able to join
# the British police force?

britishPolice = height[height>=173 & gender=='M' | height>=163 & gender=='F']
length(britishPolice)

# 16
# The date format is the original Excel file looks like yyyy-mm-dd but when
# we view this in R we see that it is in fact just a number. This number represents
# the number of days since the origin set in Excel of 1899-12-30. To
# obtain the actual dates in R we need as.Date(dob), origin='1899-12-30' .
# Create a list that contains the age of the pupils with respect to the number of
# days assuming that today is January 1, 2020. Use the list to find the number
# of days between the births of the eldest and youngest person.

datesOfBirths = as.Date(schoolData$dob,origin="1899-12-30")

youngest = max(datesOfBirths)
oldest = min(datesOfBirths)
differenceInDays = max(schoolData$dob)-min(schoolData$dob)

# 17
# Create a list month that gives the month of each person’s birth date. How
# many people are born in April?

months = format(datesOfBirths,"%m")
length(which(months=='04'))
#strtoi => converts string to integer 
hist(strtoi(months))

# 18
# Make a cross tabulation with the two variables: gender and eye .
table(gender,eye)
# 19
# Create a scatter plot and comment on the type of relationship between the
# following pairs of variables:
# a) heigth and hair
plot(height~hair)
# b) height and foot
plot(height~foot)
# c) height and fruit
plot(height~fruit)

#20
# Recently, management at Oak Tree Golf Course received a few complaints
# about the condition of the greens. Several players complained that the greens
# were too fast. Rather than react to the comments of just a few, the Golf
# Association conducted a survey of 100 male and 100 female golfers. The
# survey results are summerised here.

# a) Combine these two cross-tabulations into one with ’male’ and ’female’
# as the row labels and the column labels ’too fast’ and ’fine’. Which
# group shows the highest percentage saying the greens are too fast?

maleGolfers <- matrix(c(10,40,25,25),ncol=2,byrow=TRUE)
colnames(maleGolfers) <- c("Too Fast","Fine")
rownames(maleGolfers) <- c("Under 15","15 or more")
maleGolfers <- as.table(maleGolfers)
maleGolfers

femaleGolfers <- matrix(c(1,9,39,51),ncol=2,byrow=TRUE)
colnames(femaleGolfers) <- c("Too Fast","Fine")
rownames(femaleGolfers) <- c("Under 15","15 or more")
femaleGolfers <- as.table(femaleGolfers)
femaleGolfers


maleTooFast = maleGolfers[1] + maleGolfers[2]
maleFine = maleGolfers[3] + maleGolfers[4]

femaleTooFast = femaleGolfers[1] + femaleGolfers[2]
femaleFine = femaleGolfers[3] + femaleGolfers[4]

crossTable = matrix(c(maleTooFast,maleFine,femaleTooFast, femaleFine),ncol=2,byrow=TRUE)
colnames(crossTable) <- c("Too Fast","Fine")
rownames(crossTable) <- c("Male","Female")
crossTable <- as.table(crossTable)
crossTable


# b)
# Refer to the initial cross-tabulations. For those players with low handicaps
# (the better players), which group 
# (male or female shows the highest percentage saying the greens are too fast?)

maleBetterPlayerTooFast = maleGolfers[1]
maleBetterPlayerFine = maleGolfers[3]
maleBetterPlayerAll = maleBetterPlayerFine + maleBetterPlayerTooFast

(maleBetterPlayerTooFast/maleBetterPlayerAll)*100

femaleBetterPlayerTooFast = femaleGolfers[1]
femaleBetterPlayerFine = femaleGolfers[3]
femaleBetterPlayerAll = femaleBetterPlayerFine + femaleBetterPlayerTooFast

(femaleBetterPlayerTooFast/femaleBetterPlayerAll)*100

# c)
# Refer to the initial cross-tabulations. For those players with higher
# handicaps (the less good players), which group (male or female shows
# the highest percentage saying the greens are too fast?)

maleBetterPlayerTooFast = maleGolfers[2]
maleBetterPlayerFine = maleGolfers[4]
maleBetterPlayerAll = maleBetterPlayerFine + maleBetterPlayerTooFast

(maleBetterPlayerTooFast/maleBetterPlayerAll)*100

femaleBetterPlayerTooFast = femaleGolfers[2]
femaleBetterPlayerFine = femaleGolfers[4]
femaleBetterPlayerAll = femaleBetterPlayerFine + femaleBetterPlayerTooFast

(femaleBetterPlayerTooFast/femaleBetterPlayerAll)*100

# d)
# 
# What conclusions can be drawn about the preferences of men and
# women concerning the speed of the greens? Are the conclusions you
# draw from part (a) as compared with parts (b) and (c) consistent?
# Explain any apparent inconsistencies.

# At first, (a), it appears as though women are complaining more, but in fact
# when we include the extra information of the ability it turns out that it is the men.
# The inconsistency happens because there is some relationship between gender and
# ability: most of the good players are male.

#Lesson 3 ##########################################################

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

#Lesson 4 ##############################################################

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


#Lesson 5 ##############################################################################
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

#Lesson 7 #############################################################################

# 1
# Create a population that represents the rolling of a six-sided die. Simulate
# the random experiment of taking samples of size 5 from this population and
# calculating the sample mean. Save the resulting sampling distribution as
# a list Die05 . Repeat this for samples of size 10, 20 and 40 to obtain lists
# # Die10 , Die20 and Die40 .

dice = c(1,2,3,4,5,6)
dice05 = sample(dice, 5, replace = TRUE)
dice10 = sample(dice, 10, replace = TRUE)
dice20 = sample(dice, 20, replace = TRUE)
dice40 = sample(dice, 40, replace = TRUE)

# a) Find the mean of these lists.

mean(dice05)
library(Hmisc)
describe(dice05,type=2)

sd(dice05, na.rm=TRUE)/sqrt(length(dice05[!is.na(dice05)])) 
#or

calculateStandardDeviationError(dice05)
# 3
# Assume the population standard deviation is q = 25. Compute the standard
# error of the mean, qX , for sample sizes of 50, 100, 150 and 200. What can
# you say about the size of the standard error of the mean as the sample size is
# increased?
standDev<-25
standDev/sqrt(50)
standDev/sqrt(100)
standDev/sqrt(150)
standDev/sqrt(200)

# 4
# The Automobile Association gave the average price of unleaded petrol in
# Sweden as 14.63 Swedish Krona (SK) per litre in June 2012. Assume this
# price is the population mean, and that the population standard deviation is
# q = 1 SK.

# a)
# What is the probability that the mean price for a sample of 30 petrol
# stations is within 0,25 SK of the population mean?

p <- diff(pnorm(c(-0.25,0.25)*sqrt(30)))
# b)
# What is the probability that the mean price for a sample of 50 petrol
# stations is within 0,25 SK of the population mean?
p2 <- diff(pnorm(c(-0.25,0.25)*sqrt(50)))

# c)
# What is the probability that the mean price for a sample 100 petrol
# stations is within 0,25 SK of the population mean?
p3 <- diff(pnorm(c(-0.25,0.25)*sqrt(100)))

# d)
# Would you recommend a sample size of 30, 50 or 100 to have at
# least a 0,95 probability that the sample mean is within 0,25 SK of the
# population mean?

# Recommend n = 100, because
# the probabilities in (a) and (b) are below 0.95, in (c) above 0.95.

# 5
# According to Golf Digest, the average score for male golfers is 95 and the
# average score for female golfers is 106. Use these values as population
# means. Assume that the population standard deviation is q = 14 strokes
# for both men and women. A simple random sample of 30 male golfers and
# another simple random sample of 45 female golfers are taken.


# a) Sketch the sampling distribution of X for male golfers.

sd = 14
nMale = 30
nFemale = 45
devitation = sd/sqrt(nMale) #richtig

averageOfAll = (95*nMale + 106*nFemale)/(nMale+nFemale)



p = diff(rnorm(nMale, mean = averageOfAll, sd = devitation))
sd(p)
#?????????????????

#b) What is the probability that the sample mean is within 3 strokes of the
# population mean for the sample of male golfers?

p3 <- diff(rnorm(nMale, mean = 14.63, sd = devitation))



# 6
# Advertisers contract with Internet service providers and search engines to
# place ads on websites. They pay a fee based on the number of potential
# customers who click on their ads. Unfortunately, click fraud - i.e. someone
# clicking on an ad solely for the purpose of driving up advertising revenue -
#   has become a problem. Forty per cent of advertisers claim they have been a
# victim of click fraud. Suppose a simple random sample of 380 advertisers is
# taken to learn about how they are affected by this practice.

# a) What is the probability the sample proportion will be within +-0.04 of
# the population proportion experiencing click fraud?

lengthSample = 380 

p <- diff(pnorm(c(-0.004,0.004)*sqrt(lengthSample)))

sol = 1+ (pi*(1-pi)/lengthSample)

sol = (pi*(1-pi)/lengthSample)
sqrt(sol)

#?????????????????

#Lesson 8 ##########################################################################
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





#Lesson 9 ##########################################################################
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
#Der wert 0.20 nicht signifikant ist, empfiehlt sich nich national zu gehen

#Lesson 10 ##########################################################################
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

detach(schoolData)
tvData = read.xlsx(xlsxFile = './TVRead.xlsx')
attach(tvData)

full = Television / Reading #werte > 1 lesen mehr 
#?????????????????????????? machi speter, bi jetzt z müed

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