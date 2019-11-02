function [V] = getEigenvectors(dataMat,k)
%GETEIGENVECTORS Computes the k first eigenvectors of dataMat
%   Detailed explanation goes here

covariance = cov(dataMat);
eigenValues = eigs(covariance, k);
[eigenVectors, ~] = eig(covariance);
[~, indexes] = sort(eigenValues);
nEigenvectors = size(indexes, 1);
V = zeros(size(eigenVectors, 1), nEigenvectors);
for i=1:nEigenvectors
    V(:, i) = eigenVectors(:, indexes(i));
end
disp(V);
end

