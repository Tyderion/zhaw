function [m] = getMean(dataMat)
%GETMEAN Summary of this function goes here
%   Detailed explanation goes here
m = mean(sum(dataMat, 1) / size(dataMat, 1), 1);
end

