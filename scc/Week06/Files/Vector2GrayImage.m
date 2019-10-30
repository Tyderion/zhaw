function [I] = Vector2GrayImage(S, r, s)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here  
    I = zeros(s, r);
    i = 1; 
    for row=1:r
        for col=1:s
            I(col, row) = S(i);
            i = i + 1;
        end
    end
end


