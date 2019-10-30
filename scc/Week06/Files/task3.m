
%% a) Testing the conversion
load("I.mat");
M = rgbImage2Matrix(I);
I = Vector2GrayImage([0.1; 0.5; 0.4; 0.6], 2, 2);

%% b) Applying classify_nearestmean and classify_gmm to test.png 
test = imread("test.png");
mask = imread("maskTest.png");
load("skindata.mat")
load("nonskindata.mat")
M = rgbImage2Matrix(test) ./255;

width = size(test, 2);
height = size(test, 1);

testVectorConversion = Vector2GrayImage(sum(M, 2)./3, width, height);
% Class 1 = nonskin, class 2 = skin -> -1 to have black/white
classes_nearestmean = classify_nearestmean([nonskindata; skindata], M, 2);
skin_nearestmean = Vector2GrayImage(classes_nearestmean-1, width, height); 
error_nearestmean = sum(skin_nearestmean ~= mask, 'all') / (width * height);

% Class 1 = nonskin, class 2 = skin -> -1 to have black/white
classes_gmm = classify_gmm([nonskindata; skindata], M, 2);
skin_gmm = Vector2GrayImage(classes_gmm-1, width,height); 
error_gmm = sum(skin_gmm ~= mask, 'all') / (width * height);

hold on
subplot(3, 1, 1), imshow(test), title("Original")
%subplot(4, 1, 2), imshow(testVectorConversion), title("Test Vector<->Image conversion")
subplot(3, 1, 2), imshow(skin_nearestmean), title("nearestmean")
ylabel(sprintf("err\n%.4f", error_nearestmean));
set(get(gca,'ylabel'),'rotation',0)
subplot(3, 1, 3), imshow(skin_gmm), title("gmm")
ylabel(sprintf("err\n%.4f", error_gmm));
set(get(gca,'ylabel'),'rotation',0)

