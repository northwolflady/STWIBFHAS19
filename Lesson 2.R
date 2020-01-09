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
