function [img] = experiment_replace_inner_part(originalImg, filteredImg)

topl =  [1 1];
botr =  size(originalImg);
midv = fix(botr(1)/2);
while originalImg(topl(1), midv) == 1
    topl = [topl(1)+1 topl(2)];
end
while originalImg(topl(1)+1, topl(2)) == 1
    topl = [topl(1) topl(2)+1];
end
while originalImg(botr(1), midv) == 1
    botr = [botr(1)-1 botr(2)];
end
while originalImg(botr(1)-1, botr(2)) == 1
    botr = [botr(1) botr(2)-1];
end

img = originalImg;
img(topl(1):botr(1), topl(2):botr(2)) = filteredImg(topl(1):botr(1), topl(2):botr(2));