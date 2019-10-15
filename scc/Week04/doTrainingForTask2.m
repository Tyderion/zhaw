%% Training
function [probTokensSpam,probTokensNonSpam, pSpam, pNonSpam] = doTrainingForTask2(labels, matrix)     
    % Bayes Prediction Step 1 pSpam, pNonSpam
    spamDocs = sum(labels);
    allDocs = size(labels, 2);
    nonSpamDocs = allDocs - spamDocs;
    
    pSpam = spamDocs / allDocs;
    pNonSpam = nonSpamDocs / allDocs;

    % Bayes Prediction Step 2 
    % Training the filter
    
    % "Count" number of words in docs that are spam + 1
    spamWords = labels * matrix + 1;
    % "Count" number of words in docs that are not spam + 1 by inverting
    nonSpamWords = not(labels) * matrix + 1;
    
    probTokensSpam = spamWords ./ sum(spamWords);
    probTokensNonSpam = nonSpamWords ./ sum(nonSpamWords);
end