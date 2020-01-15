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

c = rnorm(50,mean = 79,sd =20 )
barplot(c)

d = rnorm(50,mean = 50,sd =10 )
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