function [thing] = rotate_thing_around(obj, center, phi)
    % ROTATE_AROUND   rotate object around the given center point
    % Rotate center
    thing.center = rotate_around_point(obj.center, center(1), center(2), phi);
    % Rotate all corners
    [thing.topL, thing.topR, thing.botR, thing.botL] = rotate_tuple(obj.topL, obj.topR, obj.botR, obj.botL, center(1), center(2), phi);
end