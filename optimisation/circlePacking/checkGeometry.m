function checkGeometry(r, x, y, rectangleDim)
    % you should be able to look and see if the optimal solution makes sense
    
    polySides = 1000; % close enough to a circle
    
    figure

    rectangle('Position', [0, 0, rectangleDim]);

    hold on;
    axis equal;
    
    circle1 = nsidedpoly(polySides, 'Center', [x(1), y(1)], 'Radius', r(1));
    plot(circle1, 'FaceColor', 'r');
    circle2 = nsidedpoly(polySides, 'Center', [x(2), y(2)], 'Radius', r(2));
    plot(circle2, 'FaceColor', 'r');
    circle3 = nsidedpoly(polySides, 'Center', [x(3), y(3)], 'Radius', r(3));
    plot(circle3, 'FaceColor', 'r');
    
    hold off;
end
