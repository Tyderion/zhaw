function displayCompressionResult(IOriginal, IComp)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here


figure();
subplot(1,2,1);
imagesc(IOriginal);colormap('gray');
subplot(1,2,2);
imagesc(IComp);colormap('gray');



end

