x=[0.1,0.5,0.45,0.3,0.3,0.9,0.3,0.2,0.8];
y=[0.4,0.1,0.5,0.6,0.3,0.8,0.9,0.1,0.9];
tri=delaunay(x,y);
trimesh(tri,x,y);
hold on;
plot(x,y,"*","markersize",18);