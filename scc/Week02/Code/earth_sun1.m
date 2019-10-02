function earth_sun1()

ce_start = [5; 0]; 

sun = [0; 0]; %coordinates of the sun

Phi_rotate =  pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
n = 1000; %number of iterations

ce = [ce_start; 1];


for i = 1:n

    %earth orbiting the sun

    ce = rotate_around_point(ce, sun(1), sun(2), Phi_rotate);
    
    %plot
    clf
    hold on
    % Earth
    plot(ce(1), ce(2), 'r.');
    %Sun
    plot(sun(1), sun(2), 'y*');
    
    set(gca,'Color','k')
    axis([-10, 10, -10, 10]);
    pause(0.0005);   
    
end;

