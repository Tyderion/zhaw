function earth_sun_moon()

ce_start = [5; 0];
side_len = 1;
e1_start = [ce_start(1)-side_len/2; ce_start(2)-side_len/2];
e2_start = [ce_start(1)+side_len/2; ce_start(2)-side_len/2];
e3_start = [ce_start(1)+side_len/2; ce_start(2)+side_len/2];
e4_start = [ce_start(1)-side_len/2; ce_start(2)+side_len/2];

me_start = ce_start+2;
side_len_m = 0.5;
m1_start = [me_start(1)-side_len_m/2; me_start(2)-side_len_m/2];
m2_start = [me_start(1)+side_len_m/2; me_start(2)-side_len_m/2];
m3_start = [me_start(1)+side_len_m/2; me_start(2)+side_len_m/2];
m4_start = [me_start(1)-side_len_m/2; me_start(2)+side_len_m/2];

sun = [0; 0]; %coordinates of the sun

phi_rotate = pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
n = 1000; %number of iterations

ce = [ce_start; 1];
e1 = [e1_start; 1];
e2 = [e2_start; 1];
e3 = [e3_start; 1];
e4 = [e4_start; 1];

me = [me_start; 1];
m1 = [m1_start; 1];
m2 = [m2_start; 1];
m3 = [m3_start; 1];
m4 = [m4_start; 1];

for i = 1:n

    %earth orbiting the sun
    ce = rotate_around_point(ce, sun(1), sun(2), phi_rotate);
    [e1, e2, e3, e4] = rotate_tuple(e1, e2, e3, e4, sun(1), sun(2), phi_rotate);
    [e1, e2, e3, e4] = rotate_tuple(e1, e2, e3, e4, ce(1), ce(2), phi_rotate);
    
    %moon orbiting earth
    me = rotate_around_point(me, ce(1), ce(2), phi_rotate*10);
    [m1, m2, m3, m4] = rotate_tuple(m1, m2, m3, m4, ce(1), ce(2), phi_rotate*10);
    [m1, m2, m3, m4] = rotate_tuple(m1, m2, m3, m4, me(1), me(2), phi_rotate*10);
    
    %moon orbiting sun with same velocity that earth does (so reference
    %frame is kept)
    me = rotate_around_point(me, sun(1), sun(2), phi_rotate);
    [m1, m2, m3, m4] = rotate_tuple(m1, m2, m3, m4, sun(1), sun(2), phi_rotate);
    
    %plot
    clf;
    hold on; 
    plot(ce(1), ce(2), 'ro');
    plot(sun(1), sun(2), '*');
    plot(me(1), me(2), 'bx');
    plot([e1(1), e2(1), e3(1), e4(1), e1(1)], [e1(2), e2(2), e3(2), e4(2), e1(2)], 'r');
    plot([m1(1), m2(1), m3(1), m4(1), m1(1)], [m1(2), m2(2), m3(2), m4(2), m1(2)], 'b');
    axis([-10, 10, -10, 10]);
    %axis equal;
    pause(0.001);
    
end

