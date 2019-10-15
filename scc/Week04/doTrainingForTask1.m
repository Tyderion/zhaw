%% Basic training
function [probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask1(labels, matrix)     
    % Bayes Prediction Step 1 pSpam, pNonSpam
    spamDocs = sum(labels);
    allDocs = size(labels, 2);
    nonSpamDocs = allDocs - spamDocs;
    
    pSpam = spamDocs / allDocs;
    pNonSpam = nonSpamDocs / allDocs;

    % Bayes Prediction Step 2 
    % Training the filter
    
    % "Count" number of words in documents that are spam
    spamWords = labels * matrix;
    % "Count" number of words in documents that are not spam by inverting
    nonSpamWords = not(labels) * matrix;
    
    probTokensSpam = spamWords ./ sum(spamWords);
    probTokensNonSpam = nonSpamWords ./ sum(nonSpamWords);
end