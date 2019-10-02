function [thing] = spin_thing(thing, phi) 
% SPIN   spin thing around its own center
[thing.topL, thing.topR, thing.botR, thing.botL] = rotate_tuple(thing.topL, thing.topR, thing.botR, thing.botL, thing.center(1), thing.center(2), phi);
end