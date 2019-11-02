function [vecCompressed] = compressPCAVector(vec, m, vEigen)
%COMPRESSPCAVECTOR Summary of this function goes here
%   Detailed explanation goes here
vecCompressed = vEigen * (vEigen' * (vec-m)) + m;
end

