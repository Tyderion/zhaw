Monroe = double(imread('Pics/monroe.png'))./255;
Monroe = rgb2gray(Monroe);

Airplane = double(imread('Pics/airplane.png'))./255;
Airplane = rgb2gray(Airplane);

Coins = double(imread('Pics/coins.png'))./255;
Coins = rgb2gray(Coins);

Jetski = double(imread('Pics/jetski.png'))./255;
Pinkflower = double(imread('Pics/pinkflower.png'))./255;

%% Task 1
sharpener(1:3, 1:3) = -1/9;
sharpener(2,2) = 2-1/9;

imshow(Monroe);
figure;
imshow(imfilter(Monroe, sharpener));

%% Task 2
imshow(Airplane);
figure;
imshow(medfilt2(Airplane));

%% Task 3
gradientY = [-1, -1, -1; 0, 0, 0; 1, 1, 1];
gradientX = [-1, -1, -1; 0, 0, 0; 1, 1, 1]';

imshow(Coins);
figure;
imshow(imfilter(imfilter(Coins, gradientX), gradientY));
I1 = imfilter(Coins, gradientX);
I2 = imfilter(Coins, gradientY);

imshow(sqrt(I1.^2 + I2.^2));