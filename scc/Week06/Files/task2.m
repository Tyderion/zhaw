load("M1.mat")
load("A1.mat")
classify_gmm(A1, M1, 3)

load("M2.mat")
load("A2.mat")
% This does not work due cov matrix not being positive definite.
% It seems to work for the actual image as well as example 1
classify_gmm(A2, M2, 3)


% plot(A1(:, 1), A1(:, 2))