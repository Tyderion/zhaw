function [p1, p2, p3, p4] = rotate_tuple(p1_o, p2_o, p3_o, p4_o, a, b, phi) 

p1 = rotate_around_point(p1_o, a, b, phi);
p2 = rotate_around_point(p2_o, a, b, phi);
p3 = rotate_around_point(p3_o, a, b, phi);
p4 = rotate_around_point(p4_o, a, b, phi);