#!/usr/bin/env Rscript

# Linear programming example using the graphical method


#Joanne wants to buy x oranges and y peaches from the store. She must buy at least 5 oranges and the number of oranges must be less than twice the number of peaches. An orange weighs 150 grams and a peach weighs 100 grams. Joanne can carry not more than 3.6 kg of fruits home.
#https://www.onlinemathlearning.com/linear-programming-example.html

# Identify variables
# Those are oranges(o) and peaches(p)

# Identify constraints
# Always solve for the y axis, whatever that is, in this case it's p
# o >= 5 (constraint)
# o < 2p (constraint) -> p > o2
# o150 + p100 <= 3600 (constraint) -> o1.5 + p <= 36 -> p <= 36 - o1.5

# Identify and objective function
# max fruit = o + p (objective function)


# IS THERE A BETTER WAY TO DO THIS?
get_intersection = function(y_int_1, gradient_1, y_int_2=NULL, gradient_2=NULL, vertical=NULL) {
    # This is expected to work for linear functions only. Quadratics etc are not gauranteed to have an x intercept
    if (is.null(vertical) && (is.null(y_int_2) || is.null(gradient_2))) {
        stop("Given y_int_2 and gradient_2, vertical must be NULL")
    }
    if (!is.null(vertical) && (!is.null(y_int_2) || !is.null(gradient_2))) {
        stop("Given vertical, then y_int_2 and gradient_2 must be NULL")
    }
    x = (y_int_2 - y_int_1) / (gradient_1 - gradient_2)
    y_1 = gradient_1 * x + y_int_1
    y_2 = gradient_2 * x + y_int_2
    if (round(y_1, 10) == round(y_2, 10)) {
        point = c(x, y_1)
        return (point)
    } else {
        print("No intersection") 
    }
}
#Tests
print(get_intersection(36, -1.5, vertical=5))

o = c(1,100)
p = c(1,100)
plot(o, p, type='n')

#abline(v=0, lty=1) # o>=0
#abline(h=0, lty=1) # p>=0 both made irrelevant by later functions
abline(v=5, lty=2) # o>=5
abline(0, 2, lty=2) #o<2p
abline(36, -1.5, lty=2) #p<=36-o*1.5 

print(get_intersection(y_int_1=36, gradient_1=-1.5, y_int_2=0, gradient_2=2))

# The graph appears to have intersections at:
points(5,28)
points(10,20)
# Because o5 + p28 > o10 + p20, then the optimum is the former