#!/usr/bin/env octave

resolution = 50;
xDimension = linspace(-5,5,resolution);
yDimension = linspace(-5,5,resolution);
zSurface = ( sin(xDimension)-sin(xDimension/2)+cos(yDimension)' );
mesh(xDimension, yDimension, zSurface);
hold on;

function [x, y] = lineSearchInOneDirection(xIndex,yIndex,zSpace, stepSize)
    % https://en.wikipedia.org/wiki/Line_search
    % For it to be coordinate descent, do one at a time
    lowestXIndex = xIndex;
    lowestYIndex = yIndex;
    lowestZ = zSpace(xIndex,yIndex);
    for ii =[-1,0,1]
        for jj =[-1,0,1]
            % we want to ensure the direction is x XOR y
            if ~((ii==0 && jj~=0) || (ii~=0 && jj==0))
                continue;
            end
            potentialXIndex = xIndex+ii;
            potentialYIndex = yIndex+jj;

            % Ignore attempts to get values that are out of bounds
            if potentialXIndex < 1 || potentialXIndex > size(zSpace,1) || potentialYIndex < 1 || potentialYIndex > size(zSpace,1)
                continue;
            end
            potentialZ = zSpace(potentialXIndex, potentialYIndex); 

            % What happens if both are equal? We should make it a coin flip from the two equal lowestZ
            if potentialZ < lowestZ 
                lowestXIndex = potentialXIndex;
                lowestYIndex = potentialYIndex;
                lowestZ = potentialZ;
            end
        end
    end
    x = stepSize*(lowestXIndex-xIndex);
    y = stepSize*(lowestYIndex-yIndex);
end

function [xIndex,yIndex,z] = getNextPos(xIndex, yIndex, zSurface, xChange, yChange)
    xIndex = xIndex + xChange;
    yIndex = yIndex + yChange;
    z = zSurface(xIndex,yIndex);
end

stepCount = 1;
xIndex = 25;
yIndex = 30;
z = zSurface(xIndex,yIndex);
zBuffer = 0.1;
pathX = [xDimension(xIndex)];
pathY = [yDimension(yIndex)];
pathZ = [(z+zBuffer)];

lastPos = [xIndex,yIndex,z];

finished = 0;
while ~finished
    stepSize = ceil(resolution/(stepCount*6));
    [xChange, yChange] = lineSearchInOneDirection(xIndex, yIndex, zSurface, stepSize);
    [xIndex,yIndex,z] = getNextPos(xIndex, yIndex, zSurface, xChange, yChange);
    if [xIndex, yIndex, z] == lastPos
        finished = 1;
    end
    lastPos = [xIndex, yIndex, z];
    pathX = [pathX xDimension(xIndex)];
    pathY = [pathY yDimension(yIndex)];
    pathZ = [pathZ (z+zBuffer)];
    stepCount = stepCount + 1;
end

plot3(pathY,pathX,pathZ, '-r', 'LineWidth', 3.0);
title(sprintf("This took %d steps before reaching the local minimum.\n", stepCount));
hold off

input("Press ENTER to exit");
