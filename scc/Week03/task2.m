load_images();
% get inspiration from actual motion blur
% horizontalBlur = fspecial('motion', 20, 180);
blurVec(1:20) = 0.1;
horizontalBlur = blurVec*(1/sum(blurVec));

subplot(2, 2, 1);
imshow(Jetski);
subplot(2, 2, 2);
imshow(imfilter(Jetski, horizontalBlur));

diagVec = diag(blurVec)+ diag(blurVec(1:19)*0.3, 1) + diag(blurVec(1:19)*0.3, -1);

diagonalBlur = diagVec * (1 / sum(sum(diagVec)));
subplot(2, 2, 3);
imshow(Pinkflower);
subplot(2, 2, 4);
imshow(imfilter(Pinkflower, flip(diagonalBlur),'circular' ));
