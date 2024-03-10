function maximalArea(rx, rectangleDim)
    circleArea = pi*rx(1)^2 + pi*rx(2)^2 + pi*rx(3)^2;
    rectangleArea = rectangleDim(1)*rectangleDim(2);

    fprintf("Circles have area: %f\n", circleArea);
    fprintf("Circles fill %f of rectangle\n", circleArea/rectangleArea);
end
