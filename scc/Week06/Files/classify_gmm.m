function [alpha] = classify_gmm(A,M,k)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

r = size(A,1);
d = size(A,2);
N = r/k;
means = zeros(k,d);
alpha = zeros(size(M,1),1);
probDensity = zeros(size(M,1), k);
for i = 1:k
    T = A(N*(i-1)+1:N*i,:);
    means(i,:) = mean(T);
    covMatrix = cov(T);
    [~, p] = chol(covMatrix);
    probDensity(:,i) = mvnpdf(M, means(i,:), covMatrix);
end
disp(means)
for i = 1:size(M,1)
    [~, I] = max(probDensity(i, :));
    alpha(i) = I;
end