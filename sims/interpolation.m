#!/usr/bin/env octave

% Given n cartesian coordinates, form a polynomial with n coefficients
% The polynomial will have its like terms collected

% 5 points
x = [1,2,3,4,5];
y = [2,-1,4,3,-1];
% z*x^4 + z*x^3 + z*x^2 + z*x^1 + z*x^0;

% Then, for each x substituted into the polynomial, to make n polynomials,
% the [nxn] size linear system becomes:
%  1   , 1   , 1   , 1 , 1 
%  2^4 , 2^3 , 2^2 , 2 , 1
%  3^4 , 3^3 , 3^2 , 3 , 1
%  4^4 , 4^3 , 4^2 , 4 , 1
%  5^4 , 5^3 , 5^2 , 5 , 1

A = [1,1,1,1,1; 2^4,2^3,2^2,2,1; 3^4,3^3,3^2,3,1; 4^4,4^3,4^2,4,1; 5^4,5^3,5^2,5,1];
Z = A\y';
disp(Z); % 0.7083, -9.4167, 42.7917, -76.0833, 44.0000
% Then, z in Z will be sustituted into the polynomial
function y = P(x)
    y = 0.7083*x^4 - 9.4167*x^3 + 42.7917*x^2 - 76.0833*x^1 + 44*x^0;
end

U = 0.5:0.1:5.5;
V = zeros(1,length(U));
for ii =1:length(U)
    V(ii) = P(U(ii));
end
plot(x,y,'o',U,V,'-');

% TODO, what does \ actually do here? How does it solve the system? Is it a
% least square solution?
% TODO, is this the same as the Lagrange Polynomial?

input("Press ENTER to exit")