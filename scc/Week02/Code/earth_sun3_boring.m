function earth_sun3_boring()

ce_start = [5; 0]; 

sun = [0; 0]; %coordinates of the sun

Phi_rotate =  pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
n = 100; %number of iterations
ce = [ce_start; 1];

size = 1; % Rectangle size
% Rectangle Coords
rec1 = [ce_start(1) - size/2; ce_start(2) - size/2; 1];
rec2 = [ce_start(1) - size/2; ce_start(2) + size/2; 1];
rec3 = [ce_start(1) + size/2; ce_start(2) + size/2; 1];
rec4 = [ce_start(1) + size/2; ce_start(2) - size/2; 1];


for i = 1:n

    %earth orbiting the sun
    ce = rotate_around_point(ce, sun(1), sun(2), Phi_rotate);
    [rec1, rec2, rec3, rec4] = rotate_tuple(rec1, rec2, rec3, rec4, sun(1), sun(2), Phi_rotate);
    [rec1, rec2, rec3, rec4] = rotate_tuple(rec1, rec2, rec3, rec4, ce(1), ce(2), Phi_rotate);
    
    %plot
    clf
    hold on
    % Earth
    plot(ce(1), ce(2), 'r.');    
    line([rec1(1) rec2(1)], [rec1(2) rec2(2)], 'Color', 'red');
    line([rec2(1) rec3(1)], [rec2(2) rec3(2)], 'Color', 'red');
    line([rec3(1) rec4(1)], [rec3(2) rec4(2)], 'Color', 'red');
    line([rec4(1) rec1(1)], [rec4(2) rec1(2)], 'Color', 'red');
    
    %Sun
    plot(sun(1), sun(2), 'y*');
    
    
    set(gca,'Color','k')
    axis([-10, 10, -10, 10]);
    pause(0.0005);   
    
end;


