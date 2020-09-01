#!/usr/bin/env Rscript

# Linear programming example using the graphical method


#Joanne wants to buy x oranges and y peaches from the store. She must buy at least 5 oranges and the number of oranges must be less than twice the number of peaches. An orange weighs 150 grams and a peach weighs 100 grams. Joanne can carry not more than 3.6 kg of fruits home.
#https://www.onlinemathlearning.com/linear-programming-example.html

# Identify constraints and target optimisation function
# Always solve for the y axis, whatever that is, in this case it's p
# o >= 5 (constraint)
# o < 2p (constraint) -> p > 0/2
# o150 + p100 <= 3600 (constraint) -> o1.5 + p <= 36 -> p <= 36 - o1.5
# max fruit = o + p (target to optimise)

o = c(1,100)
p = c(1,100)
plot(o, p, type='n')

#abline(v=0, lty=1) # o>=0
#abline(h=0, lty=1) # p>=0 both made irrelevant by later functions
abline(v=5, lty=2) # o>=5
abline(0, 2, lty=2) #o<2p
abline(36, -1.5, lty=2) #p<=36-o*1.5 

# The graph appears to have intersections at:
points(5,28)
points(10,20)
# Because o5 + p28 > o10 + p20, then the optimum is the former