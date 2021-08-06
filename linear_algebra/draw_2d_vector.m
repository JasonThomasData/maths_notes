function draw_2d_vector(varargin)
    components = varargin{1};
    if (nargin == 1)
        origin = zeros(size(components))
    else
        origin = varargin{2};
    end

    #if (nargin > 2)
    #    error("At most, pass the vector components and the vector origin as two vectors");
    if (!isvector(components))
        error("A vector's components must be a vector");
    else if (nargin == 2 && size(components) != size(origin))
        error("A vector's components and origin must be vectors of the same size");
    end

    quiver(origin(1), origin(2), components(1), components(2));
    hold on;
end
