#!/usr/bin/env python3
import sys
import matplotlib.pyplot as plt

#Using the example from - https://en.wikipedia.org/wiki/Logistic_regression

[
  {
    "Hours": 0.5,
    "Pass": 0
  },
  {
    "Hours": 0.75,
    "Pass": 0
  },
  {
    "Hours": 1,
    "Pass": 0
  },
  {
    "Hours": 1.25,
    "Pass": 0
  },
  {
    "Hours": 1.5,
    "Pass": 0
  },
  {
    "Hours": 1.75,
    "Pass": 0
  },
  {
    "Hours": 1.75,
    "Pass": 1
  },
  {
    "Hours": 2,
    "Pass": 0
  },
  {
    "Hours": 2.25,
    "Pass": 1
  },
  {
    "Hours": 2.5,
    "Pass": 0
  },
  {
    "Hours": 2.75,
    "Pass": 1
  },
  {
    "Hours": 3,
    "Pass": 0
  },
  {
    "Hours": 3.25,
    "Pass": 1
  },
  {
    "Hours": 3.5,
    "Pass": 0
  },
  {
    "Hours": 4,
    "Pass": 1
  },
  {
    "Hours": 4.25,
    "Pass": 1
  },
  {
    "Hours": 4.5,
    "Pass": 1
  },
  {
    "Hours": 4.75,
    "Pass": 1
  },
  {
    "Hours": 5,
    "Pass": 1
  },
  {
    "Hours": 5.5,
    "Pass": 1
  }
]

# Important is the log-ods https://en.wikipedia.org/wiki/Logitn which is otherwise the inverse of the logistic function
# Odds is defined as P(Y|X) / (1 - P(Y|X)) "the chances in favor of the event to the chances against it"
# Log odds is (see in LaTeX) = \log_{b}(\frac{P(Y|X)}{1-P(Y|X)})

# Important is the logistic function, for any x, \frac{1}{1+e^{-x}}
# Both functions have extremes that approach limits of 0 and 1
# This video is good https://www.youtube.com/watch?v=NmjT1_nClzg



def get_next_population(growth_factor, previous_population):
    #logistic map can be rewritten as rx-rx^{2} (function in LaTeX)
    #I'm not sure if Python respects distributive law so this is simpler
    return (growth_factor * previous_population) - (growth_factor * (previous_population ** 2)) 

def get_population_set(growth_factor, starting_population, intervals):
    interval_set = []
    population_set = []
    growth_for_interval = starting_population 
    interval_set.append(0)
    population_set.append(starting_population)
    for i in range(1, intervals+1):
        growth_for_interval = get_next_population(growth_factor, growth_for_interval)
        interval_set.append(i)
        population_set.append(growth_for_interval)
    return interval_set, population_set

if __name__ == "__main__":

    message = '''You must pass a file name and three numeric values, like:
    `./logistic_map.py starting_population growth_factor intervals`
    eg. `./logistic_map.py 0.7 2.1 15` for a population of 0.7, growth factor of 2.1, over 15 intervals
    where `0 <= starting_population <= 1` and `0 <= growth_factor <= 4`'''
    try:
        assert len(sys.argv) == 4
        starting_population = float(sys.argv[1])
        assert starting_population >= 0 and starting_population <= 1 # 1 - x provides this constraint
        growth_factor = float(sys.argv[2])
        intervals = int(sys.argv[3])
    except AssertionError:
        print(message)
        sys.exit()
    except TypeError:
        print(message)
        sys.exit()

    interval_set, population_set = get_population_set(growth_factor, starting_population, intervals)
    plt.plot(interval_set, population_set, linewidth=1)
    plt.title('Population size = {}% of maximum, growth factor {} per interval'.format(starting_population, growth_factor))
    plt.xlabel('Interval')
    plt.ylabel('Population')
    plt.show()
