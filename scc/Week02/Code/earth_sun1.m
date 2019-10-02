function earth_sun1()

    % define centers
    center_earth_start = [5; 0]; 
    center_sun = [0; 0];

    % angle for earth orbiting the sun (arbitrarily chosen number)
    phi_rotate =  pi/180;
    n = 1000; %number of iterations

    center_earth = [center_earth_start; 1];

    for i = 1:n

        % earth orbiting the sun
        center_earth = rotate_around_point(center_earth, center_sun(1), center_sun(2), phi_rotate);

        % plot init
        clf
        hold on
        % plot earth
        plot(center_earth(1), center_earth(2), 'r.');
        % plot sun
        plot(center_sun(1), center_sun(2), 'y*');
        % plot settings
        set(gca,'Color','k')
        axis([-10, 10, -10, 10]);
        pause(0.0005);  
    end

end

