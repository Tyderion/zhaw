function [ICompressed] = compressPCAImage(I, nEigenvectors, V, mean)
%COMPRESSPCAIMAGE Summary of this function goes here
%   Detailed explanation goes here
imageAsVec = transformImageToColumnVector(I);
meanAsVec = transformImageToColumnVector(mean);
consideredEigenvectors = V(:, 1:nEigenvectors);
compressedVector = compressPCAVector(imageAsVec, meanAsVec, consideredEigenvectors);
ICompressed = transformVectorToImage(compressedVector, size(I, 1), size(I, 2));
end

