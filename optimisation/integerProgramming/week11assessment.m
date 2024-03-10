%% MATH2390 Week 11 Hand in Lab
%% Jason Thomas s3907634
%% Question 1

% First, let's model the situation and then later convert the model into 
% the appropriate format. The seperation of steps will help avoid mistakes.

% Let C_{ij} be the 6x4 matrix of financial costs given already, loaded up
% Let CombinationsEq_{ij} be the appropriate 6x4 constraint matrix for
% equalities
% Let CombinationsInEq_{ij} be the appropriate 6x4 constraint matrix for
% inequalities

% a)
%   Forall j, CombinationsEq_{1j} - CombinationsEq_{3j} = 0

% b)
%   CombinationsInEq_{21} - CombinationsInEq_{51} <= 0

% c)
%   CombinationsEq_{43} + 
%   CombinationsEq_{44} + 
%   CombinationsEq_{53} + 
%   CombinationsEq_{54} = 0

% d)
%   Forall j, CombinationsInEq_{2j} + CombinationsInEq_{6j} <= 1

% e)
%   C_{*,1} <= 300

% There are two additional constraints, that are in the initial paragraph: 
% let's call them constraints f, g; 

% f) 
%   Forall j, forall i, sum over CombinationsEq_{ij} = 4

% g) 
%   This one forces locations to have at most one project, see Question 2b
%   code to understand the logic

%% Question 2 a)

load("data_platypus.mat");

CostL = reshape(transpose(Cost), [1, 24]);

%% Question 2 b)

type question2b.m

[Aeq, beq, A, b] = question2b(CostL);

%% Question 2 c)

type question2c.m

x = question2c(CostL, Aeq, beq, A, b);

%% Question 2 d)

% The way to interpret the result is to get the optimal values (currently 
% a vector) back to the order it was. It is ordered such that each location
% iterates through the four years. So therefore, convert it to a 
% 4x6 matrix, and then transpose it to get this back to the original order 
% locations x years

locationsYears = transpose(reshape(x, [4,6]));
disp(locationsYears);

% The optimum solution is to build 4 projects: 

% Each of the constraints is met: 
% - a) Gold Coast and Airlee Beach will not proceed
% - b) Brisbane doesn't start on year 1
% - c) Melbourne and Sydney are built but not years 3 or 4
% - d) Brisbane starts year 2 and Perth year 1
% - e) Sydney and Perth in year 1 are < $300M
% - f) all locations have no more than one project
% - g) there are 4 projects scheduled
