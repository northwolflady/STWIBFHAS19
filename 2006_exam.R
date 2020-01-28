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
