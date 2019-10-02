function earth_sun2()

ce_start = [5; 0];
side_len = 4;
e1_start = [ce_start(1)-side_len/2; ce_start(2)-side_len/2];
e2_start = [ce_start(1)+side_len/2; ce_start(2)-side_len/2];
e3_start = [ce_start(1)+side_len/2; ce_start(2)+side_len/2];
e4_start = [ce_start(1)-side_len/2; ce_start(2)+side_len/2];

sun = [0; 0]; %coordinates of the sun

phi_rotate = pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
n = 1000; %number of iterations

ce = [ce_start; 1];
e1 = [e1_start; 1];
e2 = [e2_start; 1];
e3 = [e3_start; 1];
e4 = [e4_start; 1];

for i = 1:n

    %earth orbiting the sun
    ce = rotate_around_point(ce, sun(1), sun(2), phi_rotate);
    [e1, e2, e3, e4] = rotate_tuple(e1, e2, e3, e4, sun(1), sun(2), phi_rotate);
    
    %plot
    clf;
    hold on; 
    plot(ce(1), ce(2), 'o');
    plot(sun(1), sun(2), '*');
    plot([e1(1), e2(1), e3(1), e4(1), e1(1)], [e1(2), e2(2), e3(2), e4(2), e1(2)], 'r');
    axis([-10, 10, -10, 10]);
    axis equal;
    pause(0.001);
    
end

