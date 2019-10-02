function [p] = rotate_around_point(pold, a, b, phi) 

translation = [1 0 a; 0 1 b; 0 0 1];
rotation = [cos(phi) -sin(phi) 0; sin(phi) cos(phi) 0; 0 0 1];
reverse_translation = [1 0 -a; 0 1 -b; 0 0 1];

p = translation * rotation * reverse_translation * pold;