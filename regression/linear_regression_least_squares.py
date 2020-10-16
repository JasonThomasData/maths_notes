#!/usr/bin/env python3

example_dataset = [
    {
        "x": 1,
        "y": 2
    },
    {
        "x": 3,
        "y": 3
    },
    {
        "x": 5,
        "y": 6
    },
    {
        "x": 5,
        "y": 4
    },
    {
        "x": 5,
        "y": 7
    },
    {
        "x": 6,
        "y": 7
    },
    {
        "x": 8,
        "y": 7
    },
    {
        "x": 8,
        "y": 8
    },
    {
        "x": 9,
        "y": 7
    }
]

def mean(dataset, key):
    total = 0
    count = 0
    for datum in dataset:
        total += datum[key]
        count += 1
    return total / count

def deviations_from_mean(mean, dataset, key):
    deviations = []
    for datum in dataset:
        deviation = datum[key] - mean
        deviations.append(deviation)
    return deviations

def product_of_lists(list1, list2):
    products = []
    for num1, num2 in zip(list1, list2):
        products.append(num1 * num2)
    return products

def square_each_element(original_list):
    squares = []
    for num in original_list:
        squares.append(num ** 2)
    return squares 

def gradient(x_deviations, y_deviations):
    x_y_deviations_product = product_of_lists(x_deviations, y_deviations)
    x_deviations_squared = square_each_element(x_deviations)
    return sum(x_y_deviations_product) / sum(x_deviations_squared)

def intercept(mean_y_value, gradient, mean_x_value):
    return mean_y_value - (linear_gradient * mean_x_value)

mean_x_value = mean(example_dataset, "x")
mean_y_value = mean(example_dataset, "y")

x_deviations = deviations_from_mean(mean_x_value, example_dataset, "x")
y_deviations = deviations_from_mean(mean_y_value, example_dataset, "y")

# https://en.wikipedia.org/wiki/Regression_analysis "Linear Regression"
linear_gradient = gradient(x_deviations, y_deviations)
y_intercept = intercept(mean_y_value, linear_gradient, mean_x_value)

print("Given this dataset:")
for datum in example_dataset:
    print("({},{})".format(datum["x"], datum["y"]))

print("The linear regression for this dataset is : \n y = {0} * x + {1}".format(linear_gradient,
    y_intercept))
