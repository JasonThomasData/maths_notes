#!/usr/bin/env python3

import math

example_dataset = [1,2,3,4,7,8,13,20]

number_of_data = len(example_dataset) 

arithmetic_mean = sum(example_dataset)/number_of_data
print("Arithmetic mean:", arithmetic_mean)

deviations_from_mean = []
for elem in example_dataset:
    deviation = elem - arithmetic_mean
    deviations_from_mean.append(deviation)
print("Deviations from mean:", deviations_from_mean)

print("Sum of deviations will always be zero:", sum(deviations_from_mean))

deviations_squared = []
for elem in deviations_from_mean:
    deviation_squared = (elem - arithmetic_mean)**2
    deviations_squared.append(deviation_squared)
sum_of_squares = sum(deviations_squared)
print("Total sum of squares (TSS) will be more useful:", sum_of_squares)

population_variance = sum_of_squares/number_of_data
print("The average deviation, squared AKA population variance:", population_variance)

print("This is still squared, so to undo that, take the root to find population standard deviation:", math.sqrt(population_variance))

print("This is the same as taking the root of the sum of squares, and then finding the average from the root of n:", math.sqrt(sum_of_squares)/math.sqrt(number_of_data))
print("This simply means, that since all terms were squared, then all terms must have their root found")

sample_variance = sum_of_squares/number_of_data-1
print("For sample standard deviation,:", math.sqrt(sample_variance))
