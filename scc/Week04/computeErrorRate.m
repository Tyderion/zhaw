function [output] = computeErrorRate(correct, guess)
    output = sum(correct ~= guess) / length(correct);
end
