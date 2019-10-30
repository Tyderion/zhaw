%% This task seems to be very close to task 3 or we misunderstood one of them.
% This applies classification to test.png
% It uses skindata / nonskindata for the matrix A in the classification functions (same as task3)
% It then additionally computes and displays error rate, false negative and
% false positive rate

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
[falsepos_nearestmean,falseneg_nearestmean ] = falseRates(skin_nearestmean, mask);

% Class 1 = nonskin, class 2 = skin -> -1 to have black/white
classes_gmm = classify_gmm([nonskindata; skindata], M, 2);
skin_gmm = Vector2GrayImage(classes_gmm-1, width,height); 
error_gmm = sum(skin_gmm ~= mask, 'all') / (width * height);
[falsepos_gmm,falseneg_gmm ] = falseRates(skin_gmm, mask);

figure('Renderer', 'painters', 'Position', [1000 100 600 900])
hold on
subplot(4, 1, 1), imshow(test), title("Original")
%subplot(4, 1, 2), imshow(testVectorConversion), title("Test Vector<->Image conversion")
subplot(4, 1, 2), imshow(skin_nearestmean), title("nearestmean")
ylabel(sprintf("err\n%.4f", error_nearestmean));
set(get(gca,'ylabel'),'rotation',0)
subplot(4, 1, 3), imshow(skin_gmm), title("gmm")
ylabel(sprintf("err\n%.4f", error_gmm));
set(get(gca,'ylabel'),'rotation',0)

ha = subplot(4, 1, 4)
labels =  {'Error Rate'; 'False Positives'; 'False Negatives'};
gmm = [error_gmm; falsepos_gmm; falseneg_gmm] ;
nearest = [error_nearestmean; falsepos_nearestmean;falseneg_nearestmean] ;
comparison = table( gmm, nearest, 'RowNames',labels);
displayTable(comparison, ha)

%% Compute False Rates (positive, negative)
function [fp, fn] = falseRates(computed, correct)
    fp = 0;
    fn = 0;
    for i=1:size(computed, 1)
        for j=1:size(computed,2)
            if computed(i, j) == 1 && correct(i, j) == 0
                fp = fp + 1;
            end
            if computed(i, j) == 0 && correct(i, j) == 1
                fn = fn + 1;
            end
        end
    end
    fp = fp / sum(computed == 1, 'all');
    fn = fn / sum(computed == 0, 'all');
end

%% Helper function to display a table in a specific subplot
% Display table in figure: https://ch.mathworks.com/matlabcentral/answers/254690-how-can-i-display-a-matlab-table-in-a-figure
% and https://ch.mathworks.com/matlabcentral/answers/313184-how-do-i-place-a-uitable-in-a-subplot-matlab-r2013a
function displayTable(T, ha) 
pos = get(ha,'Position');
un = get(ha,'Units');
delete(ha)
% Get the table in string form.
TString = evalc('disp(T)');
% Use TeX Markup for bold formatting and underscores.
TString = strrep(TString,'<strong>','\bf');
TString = strrep(TString,'</strong>','\rm');
TString = strrep(TString,'_','\_');
% Get a fixed-width font.
FixedWidth = get(0,'FixedWidthFontName');
% Output the table using the annotation command.
annotation(gcf,'Textbox','String',TString,'Interpreter','Tex',...
    'FontName',FixedWidth,'Units',un,'Position',pos);
end

