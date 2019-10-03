load_images();


% Task 1
sharpener(1:3, 1:3) = -1/9;
sharpener(2,2) = 2-1/9;

subplot(3, 2, 1)
imshow(Monroe);
hold on
subplot(3, 2, 2)
imshow(imfilter(Monroe, sharpener));


blur(1:3, 1:3) = -1/9;
subplot(3, 2, 3);
imshow(Airplane);
subplot(3, 2, 4)
imshow(medfilt2(Airplane, [3 3]));


gradientx = [-1 0 1; -1 0 1; -1 0 1];
gradienty = [-1 -1 -1; 0 0 0; 1 1 1];
subplot(3, 2, 5);
imshow(Coins);
subplot(3, 2, 6)
imshow(imfilter(imfilter(Coins, gradientx), gradienty));

I1 = imfilter(Coins, gradientx);
I2 = imfilter(Coins, gradienty);
imshow(sqrt(I1.^2 + I2.^2));