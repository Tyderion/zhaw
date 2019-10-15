
%% Case 1
load('trainMatrixEx1.mat');
load('trainLabelsEx1.mat');

[probTokensSpam,probTokensNonSpam] = doTrainingForTask1(trainLabelsEx1, trainMatrixEx1);
print(1, probTokensSpam,probTokensNonSpam)


%% Case2 2
load('trainMatrixEx2.mat');
load('trainLabelsEx2.mat');

[probTokensSpam,probTokensNonSpam] = doTrainingForTask1(trainLabelsEx2, trainMatrixEx2);
print(2, probTokensSpam,probTokensNonSpam)

%% Helpers
function print(exercise, probTokensSpam,probTokensNonSpam) 
fprintf('========Case %d========\n', exercise)
fprintf('probTokensSpam =\t')
fprintf('%.3f ', probTokensSpam)
fprintf('\nprobTokensNonSpam =\t');
fprintf('%.3f ', probTokensNonSpam)
fprintf('\n\n\n')
end
