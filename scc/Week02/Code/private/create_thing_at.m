function [thing] = create_thing_at(center, size) 
% CREATE_THING_AT   create a thing at the center point with the given size
thing.center = [center(1); center(2); 1];
thing.topL = [center(1) - size/2; center(2) - size/2; 1];
thing.topR = [center(1) - size/2; center(2) + size/2; 1];
thing.botR = [center(1) + size/2; center(2) + size/2; 1];
thing.botL = [center(1) + size/2; center(2) - size/2; 1];
end