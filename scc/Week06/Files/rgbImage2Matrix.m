function [M] = rgbImage2Matrix(I)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

    h = size(I, 1);
    w = size(I, 2);
    
    M = zeros(h*w, 3);
    i = 1; 
    for row=1:w
        for col=1:h
            M(i, :) = I(col, row, :);
            i = i + 1;
        end
    end
end


