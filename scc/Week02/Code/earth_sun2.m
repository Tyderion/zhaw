function earth_sun3()

    % define centers
    center_earth_start = [5; 0]; 
    center_sun = [0; 0];

    % angle for earth orbiting the sun (arbitrarily chosen number)
    phi_rotate =  pi/180;
    % number of iterations
    n = 1000; 

    center_earth = [center_earth_start; 1];

    % rectangle size
    size = 1; 

    % rectangle coords
    rec1 = [center_earth(1) - size/2; center_earth(2) - size/2; 1];
    rec2 = [center_earth(1) - size/2; center_earth(2) + size/2; 1];
    rec3 = [center_earth(1) + size/2; center_earth(2) + size/2; 1];
    rec4 = [center_earth(1) + size/2; center_earth(2) - size/2; 1];


    for i = 1:n

        % earth orbiting the sun

        center_earth = rotate_around_point(center_earth, center_sun(1), center_sun(2), phi_rotate);
        [rec1, rec2, rec3, rec4] = rotate_tuple(rec1, rec2, rec3, rec4, center_sun(1), center_sun(2), phi_rotate);

        % plot init
        clf
        hold on
        % plot earth
        plot(center_earth(1), center_earth(2), 'r.');    
        line([rec1(1) rec2(1)], [rec1(2) rec2(2)], 'Color', 'red');
        line([rec2(1) rec3(1)], [rec2(2) rec3(2)], 'Color', 'red');
        line([rec3(1) rec4(1)], [rec3(2) rec4(2)], 'Color', 'red');
        line([rec4(1) rec1(1)], [rec4(2) rec1(2)], 'Color', 'red');
        % plot sun
        plot(center_sun(1), center_sun(2), 'y*');
        % plot settings
        set(gca,'Color','k');
        axis([-10, 10, -10, 10]);
        pause(0.0005);   
    end 
end

