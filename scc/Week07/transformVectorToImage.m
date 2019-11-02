function [M] = transformVectorToImage(V, height, length)
%TRASNFORMVECTORTOIMAGE Summary of this function goes here
%   Detailed explanation goes here
M = reshape(V, [height, length]);
end

