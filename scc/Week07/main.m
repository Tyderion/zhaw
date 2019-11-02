global N_PERSON;
global N_EXPRESSION;
N_PERSON=20;
N_EXPRESSION=5;
nEigenfacesDisplayed = 10;
nEigenvaluesConsidered = 300; %upper bound for the number of eigenvectors considered in the compressions

displaySourceImages;
%Get eigenfaces and mean face
%Src is a matrix each row of which corresponds to an image
for personId=1:N_PERSON
    for expression=1:N_EXPRESSION
        A = loadAndScaleImage(personId, expression);
        SizeX=size(A,1);
        SizeY=size(A,2);
        B = transformImageToRowVector(A);   
        Src((personId-1)*N_EXPRESSION+expression, :) = B;
    end
end
[V] = getEigenvectors(Src, nEigenvaluesConsidered);
m = getMean(Src);
mf = transformVectorToImage(m, SizeX, SizeY);

%----------------------------------------------------

%Optional: Display eigenfaces and meanface
figure();imagesc(mf);colormap('gray');
pause;

[SizeX, SizeY] = size(mf);
for i=1:nEigenfacesDisplayed
    tmp= transformVectorToImage(V(:,i),SizeX,SizeY);
    Img(1:SizeX,(SizeY*(i-1))+1:(SizeY*i))=tmp;
end
figure();imagesc(Img);colormap('gray');
axis equal;
%different outcomes are possible (eigenvectors are only unique up to
%multiplication with -1)
pause;


%-----------------------------------------------------
%Application I: Face Compression

%an image inside the training set, with different levels of compression:
%use 300, 100 and 50, respectively eigenvalues 
IOrig = loadAndScaleImage(2,4);   %some image of the training set ((2,4) can be replaced by any tuple (i,j) with i <= 20, j <= 5)

%TODO: Compress IOrig using the corresponding number of eigenvalues and
%display the result. 





%an image that lies outside the training set
JOrig = loadAndScaleImage(21,6); %some image of the test set (can also be replaced by another one)

%TODO: Compress JOrig using 300 eigenvalues and display the result.



%---------------------------------------------------

%Application II: Face Detector

I=double(imread('FaceDetection.bmp'));
%TODO: call FaceDetector with the appropriate parameters (use 300 eigenvalues)
