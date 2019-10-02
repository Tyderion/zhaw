function  moving_square()
%warmup example: point moving along diagonal line

x_step = 0.1; %increment of x-coordinate in one step
y_step = 0.05; %increment of y-coordinate in one step
n = 1000; %number of steps

cs_start = [3;4];  %coordinates of the center of the square (arbitrarily chosen value)
side_len = 8; %side length of the squaere

%determine the coordinates of the four vertices s1, s2, s3, s4 of the
%square 
s1_start = [cs_start(1) - side_len/2.0; cs_start(2) - side_len/2.0];
s2_start = [s1_start(1) + side_len; s1_start(2)];
s3_start = [s1_start(1) + side_len; s1_start(2) + side_len]; 
s4_start = [s1_start(1); s1_start(2) + side_len]; 


%initialization (from now an, use homogeneous coordinates)
cs = [cs_start; 1];
s1 = [s1_start; 1];
s2 = [s2_start; 1];
s3 = [s3_start; 1];
s4 = [s4_start; 1];

T = [1 0 x_step; 0 1 y_step; 0 0 1]; %translation matrix

for i = 1:n
    
    %update cs and s1, s2, s3, s4
    cs = T*cs;
    s1 = T*s1;
    s2 = T*s2;
    s3 = T*s3;
    s4 = T*s4;
    
    %plot
    clf; %delete previous plot
    hold on; %retain all upcoming plots in this iteration of the loop
    plot(cs(1),cs(2),'r.'); %plot center
    plot([s1(1) s2(1) s3(1) s4(1) s1(1)], [s1(2) s2(2) s3(2) s4(2) s1(2)], 'r');
    axis([-12, 120, -12, 120]); %keep axis limits (to avoid disturbing visual effects) 
    pause(0.0005); %pauses for 0.0005 sec

end

