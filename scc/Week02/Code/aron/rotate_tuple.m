function [p1new, p2new, p3new, p4new] = rotate_tuple(p1old, p2old, p3old, p4old, a, b, phi)
    p1new = rotate_around_point(p1old, a, b, phi);
    p2new = rotate_around_point(p2old, a, b, phi);
    p3new = rotate_around_point(p3old, a, b, phi);
    p4new = rotate_around_point(p4old, a, b, phi);
end 