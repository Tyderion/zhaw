%% Case 1
load('trainMatrixEx1.mat');
load('trainLabelsEx1.mat');
load('testLabelsEx1.mat');
[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask1(trainLabelsEx1, trainMatrixEx1);


load('testMatrixEx1.mat');
guess = doTest(testMatrixEx1, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx1.mat');
errorRate = computeErrorRate(testLabelsEx1, guess);
print(1, guess, errorRate)

%% Case 2
load('trainMatrixEx2.mat');
load('trainLabelsEx2.mat');

[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask1(trainLabelsEx2, trainMatrixEx2);
load('testMatrixEx2.mat');
guess = doTest(testMatrixEx2, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx2.mat');
errorRate = computeErrorRate(testLabelsEx2, guess);
print(2, guess, errorRate)

%% Case 3
load('trainMatrixEx3.mat');
load('trainLabelsEx3.mat');

[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask1(trainLabelsEx3, trainMatrixEx3);
load('testMatrixEx3.mat');
guess = doTest(trainMatrixEx3, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx3.mat');
errorRate = computeErrorRate(testLabelsEx3, guess);
print(3, guess, errorRate)

%% Testing
function [output] = doTest(matrix, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam)

    % Bayes Step 3 & 4 in one go
    p_expectedSpam = prod((probTokensSpam .^ matrix), 2) .* pSpam ;
    p_expectedNonspam = prod((probTokensNonSpam .^ matrix), 2) .* pNonSpam;
    
    % Step 5
    % Part 1: Calculate P(a_single_word):
    docCount = size(matrix, 1);
    totalWords = sum(sum(matrix));
    % Sum each column and then divide elementwise by total number of words
    p_each_word = (ones(1, docCount) * matrix) ./ totalWords;
    
    % Part 2: Calculate P(words_in_doc) = product of p_each_word of the
    % words that are in the doc (e.g. ignore zeros)
    all_wordprobs = (matrix .* p_each_word);
    % Set all elements that are 0 to 1 for the multiplication to ignore
    % them
    all_wordprobs(~all_wordprobs) = 1; 
    p_words_in_doc = prod(all_wordprobs, 2);
    
    % Part 3: Calculate P(Spam | words_in_doc)
    p_doc_is_spam = p_expectedSpam ./ p_words_in_doc ;
    % Part 3: Calculate P(NotSpam | words_in_doc)
    p_doc_is_not_spam = p_expectedNonspam ./ p_words_in_doc;
    
    % Step 6 Compare 
    output = p_doc_is_spam > p_doc_is_not_spam;
end
%% Helpers
function print(testCase, guess, errorRate) 
fprintf('========Case %d========\n', testCase)
disp("guess' =")
disp(guess')
fprintf('errorRate =\t %.3f', errorRate);
fprintf('\n\n\n')
end