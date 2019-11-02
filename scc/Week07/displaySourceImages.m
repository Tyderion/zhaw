function displaySourceImages()
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

global N_PERSON;
global N_EXPRESSION;


clear Img;
for personId=1:N_PERSON
    for expression=1:N_EXPRESSION
        A = loadAndScaleImage(personId, expression);
        SizeX=size(A,1);
        SizeY=size(A,2);
        Img((SizeX*(expression-1))+1:(SizeX*expression),(SizeY*(personId-1))+1:(SizeY*personId))=A;
    end
end

figure();imagesc(Img);colormap('gray');
axis equal;
pause;


end

