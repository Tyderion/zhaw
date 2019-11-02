function [V] = transformImageToRowVector(M)
%TRANSFORMIMAGETOROWVECTOR Summary of this function goes here
%   Detailed explanation goes here
V = reshape(M', [], 1)'; 
end

