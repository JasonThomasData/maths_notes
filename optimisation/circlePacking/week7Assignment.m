%% MATH2390 Week 7 Hand in Lab
%% Jason Thomas s3907634
%% Question 1

clear;

type Three_circles.m

%% Question 2

type LinearConstraints.m
[A, b] = LinearConstraints();

%% Question 3

type Neg_Areas.m

%% Question 4

options = optimoptions('fmincon','Display','iter','GradObj','on',...
    'GradConstr','on','PlotFcns',{@optimplotx, @optimplotfval});
[rx, fval] = fmincon(@Neg_Areas, zeros(9,1), A, b,...
[], [], zeros(9),[inf; inf; inf; 5; 5; 5; 3; 3; 3],...
@Three_circles, options);

%% Question 5

rectangleDim = [5, 3];

type maximalArea.m
maximalArea(rx, rectangleDim);

type checkGeometry.m
checkGeometry(rx(1:3), rx(4:6), rx(7:9), rectangleDim);

% To me, this looks successful
