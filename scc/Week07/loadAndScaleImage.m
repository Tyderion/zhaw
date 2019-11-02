function [I] = loadAndScaleImage(personId, expression)

imgScale = 0.6;

% Load face
I=imread(sprintf('s%i\\%i.pgm',personId,expression));
I=imresize(I,imgScale,'bicubic');
I = double(I);


