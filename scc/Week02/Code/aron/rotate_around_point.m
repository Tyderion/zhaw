function pnew = rotate_around_point(pold, a, b, phi)
    translation = [1, 0, a; 0, 1, b; 0, 0, 1];
    rotation = [cos(phi), -sin(phi), 0; sin(phi), cos(phi), 0; 0, 0, 1];
    negative_translation = [1, 0, -a; 0, 1, -b; 0, 0, 1];
    
    pnew = translation * rotation * negative_translation * pold;
end 

