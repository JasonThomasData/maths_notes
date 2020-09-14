#!/usr/bin/env Rscript

get_intersection = function(y_int_1, gradient_1, y_int_2=NULL, gradient_2=NULL) {
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

# In a zero sum game
# There are two players, min and max
#
#                max
#
#                B_1  B_2
#                y_1  y_2
# min   A_1 x_1  3    -1
#       A_2 x_2  -2   4

# The choices for min are {A_1, A_2} and for max are {B_1, B_2}
# The probabilities to assign are {x_1, x_2} for min and {y_1, y_2} for max
# The probability of x_2 is (1 - x_1) and for y_2 is (1 - y_1)
# For example, min could choose to play A_1 at 0.3 and A_2 at 0.7, depending on what max is likely to do

min_linear_choices=data.frame(row.names=c("opponent_choice","y_int","gradient"))
min_linear_choices = rbind(min_linear_choices, data.frame(opponent_choice="B_1", y_int=-2, gradient=4))
min_linear_choices = rbind(min_linear_choices, data.frame(opponent_choice="B_2", y_int=4, gradient=-3))

x_axis = c(0,10)
y_axis = c(0,10)
optimal_minimum = get_intersection(-2, 4, 4, -3)
plot(x_axis, y_axis, type='n', main=sprintf("Optimal %s is %#3f", "x_1", optimal_minimum[1]))
abline(v=0)
abline(h=0)
# Min decision for max choosing B_1
# x_1 * 3 + (1 - x_1) * -2 = 3x_1 - 2(1 - x_1) = -2 + 4x_1
abline(-2, 4, lty=1)
# Min decision for max choosing B_2
# x_1 * -1 + (1 - x_1) * 4 = -1x_1 + 4(1 - x_1) = 4 - 3x_1
abline(4, -3, lty=1)
points(x=optimal_minimum[1], y=optimal_minimum[2], type = "p")

x_axis = c(0,10)
y_axis = c(0,10)
optimal_maximum = get_intersection(-1, 4, 4, 2)
plot(x_axis, y_axis, type='n', main=sprintf("Optimal %s is %#3f", "y_1", optimal_maximum[1]))
abline(v=0)
abline(h=0)
# Max decision for min choosing A_1
# y_1 * 3 + (1 - y_1) * -1 = 3*y_1 - 1(1 - y_1) = -1 + 4x_1
abline(-1, 4, lty=1)
# Max decision for min choosing A_2
# y_1 * -2 + (1 - y_1) * 4 = -2*y_1 + 4(1 - y_1) = 4 + 2x_1
abline(4, 2, lty=1)
points(x=optimal_maximum[1], y=optimal_maximum[2], type = "p")
