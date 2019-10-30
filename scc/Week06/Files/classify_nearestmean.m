function [alpha] = classify_nearestmean(A,M,k)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

r = size(A,1);
d = size(A,2);
N = r/k;
means = zeros(k,d);
alpha = zeros(size(M,1),1);
for i = 1:k
    T = A(N*(i-1)+1:N*i,:);
    means(i,:) = mean(T);
end
dist_to_mean = zeros(k,1);
for i = 1:size(M,1)
    P = M(i,:);
    for j = 1:k
        dist_to_mean(j) = distance(P,means(j,:));
    end;
    [~, l] = min(dist_to_mean);
    alpha(i) = l;
end;


