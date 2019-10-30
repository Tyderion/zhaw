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
function [false_positive_rate, false_negative_rate] = falseRates(computed, correct)
    % -1 = false negative, 0 = correct, 1 = false positive
    correct_grayscale = correct / 255;
    diff = uint8(computed) - correct_grayscale;
    true_negatives = sum(correct_grayscale == 0, 'all');
    true_positives = sum(correct_grayscale == 1, 'all');
    false_positives = sum(diff == 1, 'all');
    false_negatives = sum(diff == -1, 'all');
    
    false_positive_rate = false_positives / (false_positives + true_negatives);
    false_negative_rate = false_negatives / (false_negatives + true_positives);
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

