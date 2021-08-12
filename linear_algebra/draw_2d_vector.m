function draw_2d_vector(varargin)
## -*- texinfo -*-
## @deftypefn draw_d2_vector(@var{head})
## @deftypefnx draw_d2_vector(@var{head}, @var{tail})
## Receives either a head to draw, or a head and a tail. Both must be points in a 2d linear space.
## @end deftypefn

    head = varargin{1};
    if (nargin == 1)
        tail = zeros(size(head));
    else
        tail = varargin{2};
    end

    if (!isvector(head))
        error("A vector's head must be a point");
    elseif (size(head) != size(tail))
        error("A vector's head and tail must be points of the same degree");
    end

    arrow = quiver(tail(1), tail(2), head(1), head(2));
    set(arrow, "linewidth", 5);
    #set(arrow, "maxheadsize", 0.05);
    hold on;
end
