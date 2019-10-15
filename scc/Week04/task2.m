%% Use Laplacian Smoothing

%% Case 1
load('trainMatrixEx1.mat');
load('trainLabelsEx1.mat');
[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask2(trainLabelsEx1, trainMatrixEx1);

printTraining(1, probTokensSpam,probTokensNonSpam)

load('testMatrixEx1.mat');
guess = doTestForTask2(testMatrixEx1, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx1.mat');
errorRate = computeErrorRate(testLabelsEx1, guess);
printTesting(guess, errorRate)


%% Case 2
load('trainMatrixEx2.mat');
load('trainLabelsEx2.mat');

[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask2(trainLabelsEx2, trainMatrixEx2);
printTraining(2, probTokensSpam,probTokensNonSpam)

load('testMatrixEx2.mat');
guess = doTestForTask2(testMatrixEx2, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx2.mat');
errorRate = computeErrorRate(testLabelsEx2, guess);
printTesting(guess, errorRate)

%% Case 3
load('trainMatrixEx3.mat');
load('trainLabelsEx3.mat');

[probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask2(trainLabelsEx3, trainMatrixEx3);
printTraining(3, probTokensSpam,probTokensNonSpam)

load('testMatrixEx3.mat');
guess = doTestForTask2(testMatrixEx3, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam);

load('testLabelsEx3.mat');
errorRate = computeErrorRate(testLabelsEx3, guess);
printTesting(guess, errorRate)



%% Testing
function [output] = doTestForTask2(matrix, probTokensSpam, probTokensNonSpam, pSpam, pNonSpam)
    % Bayes Step 3 & 4 in one go
    p_expectedSpam = sum((-log(probTokensSpam) .* matrix), 2) - log(pSpam);
    p_expectedNonspam = sum((-log(probTokensNonSpam) .* matrix), 2) - log(pNonSpam);
    
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

%% Helper Functions
function printTraining(exercise, probTokensSpam,probTokensNonSpam) 
fprintf('========Case %d========\n', exercise)
fprintf('probTokensSpam =\t')
fprintf('%.4f ', probTokensSpam)
fprintf('\nprobTokensNonSpam =\t');
fprintf('%.4f ', probTokensNonSpam)
end

function printTesting(guess, errorRate) 
disp("guess' =")
disp(guess')
fprintf('errorRate =\t %.4f', errorRate);
fprintf('\n\n\n')
end
