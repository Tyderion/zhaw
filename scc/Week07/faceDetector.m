function faceDetector(I, d, V, Mean)

% I: input image
% SizeX, SizeY: size of images in database
% Mean: mean face (as matrix)
[SizeX, SizeY] = size(Mean);


N = size(I,2)-SizeY+1; 
err = zeros(N,1);
%TODO: Use the sliding window approach and determine the error value for each window-position. 







%plot the given image and the corresponding error function
subplot(2,1,1);
imagesc(I);colormap('gray');
subplot(2,1,2);
plot(err,'.');


end

