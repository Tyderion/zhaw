
task2()
%% Optional Task

load('tokenArray.mat');
[ratios, order] = sort(probTokensSpam ./ probTokensNonSpam, 'descend');
disp('======== Optional ========')
disp('Tokens with the highest indication of spam:')
for i=1:5
    disp(tokenArray(order(i)))
end