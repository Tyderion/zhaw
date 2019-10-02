function earth_sun1()

ce_start = [5; 0]; 

sun = [0; 0]; %coordinates of the sun

phi_rotate = pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
n = 1000; %number of iterations

ce = [ce_start; 1];

for i = 1:n

    %earth orbiting the sun
    ce = rotate_around_point(ce, sun(1), sun(2), phi_rotate);
    
    %plot
    clf;
    hold on; 
    plot(ce(1), ce(2), 'o');
    plot(sun(1), sun(2), '*');
    axis([-10, 10, -10, 10]);
    pause(0.001);   
    
end

