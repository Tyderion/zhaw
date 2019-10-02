function earth_sun3()
    % define centers
    center_earth_start = [5; 0]; 
    center_sun = [0; 0];

    % angle for earth orbiting the sun (arbitrarily chosen number)
    phi_rotate =  pi/180;
    % number of iterations
    n = 1000; 

    % rectangle coords
    earth = create_thing_at(center_earth_start, 1);

    for i = 1:n
        % earth orbiting the sun
        earth = rotate_thing_around(earth, center_sun, phi_rotate);
        
        % spin things
        earth = spin_thing(earth, phi_rotate);
        
        % plot init
        clf
        hold on
        % plot earth
        draw_thing(earth, 'red')
        %plot sun
        plot(center_sun(1), center_sun(2), 'y*');
        % plot settings
        set(gca,'Color','k')
        axis([-10, 10, -10, 10]);
        pause(0.0005);
    end
end