%% Task 2 prerequisities
load('trainMatrixEx3.mat');
load('trainLabelsEx3.mat');

[probTokensSpam,probTokensNonSpam] = doTrainingForTask2(trainLabelsEx3, trainMatrixEx3);

%% Optional Task

load('tokenArray.mat');
[ratios, order] = sort(probTokensSpam ./ probTokensNonSpam, 'descend');
disp('======== Optional ========')
disp('Tokens with the highest indication of spam:')
for i=1:5
    disp(tokenArray(order(i)))
end