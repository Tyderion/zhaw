function earth_sun_moon()
    earth_start = [5; 0];
    moon_start = [earth_start(1) + 2; earth_start(2)];
    sun_center = [0; 0]; %coordinates of the sun
    
    % angle for earth orbiting the sun (arbitrarily chosen number)
    phi_rotate =  pi/180;
    % angle for moon orbiting the earth (arbitrarily chosen number)
    phi_rotate_moon = phi_rotate*2; 
    % number of iterations
    n = 1000; 

    % create moon/earth
    earth = create_thing_at(earth_start, 1);
    moon = create_thing_at(moon_start, 0.5);

    for i = 1:n
        %earth orbiting the sun
        earth = rotate_thing_around(earth, sun_center, phi_rotate);

        %moon orbiting the earth
        moon = rotate_thing_around(moon, earth.center, phi_rotate_moon);

        %moon orbiting the sun (keep with earth)        
        moon = rotate_thing_around(moon, sun_center, phi_rotate);

        % spin things
        earth = spin_thing(earth, phi_rotate);
        moon = spin_thing(moon, phi_rotate_moon);

        % plot init
        clf
        hold on
        % plot earth/moon/sun
        draw_thing(earth, 'red')
        draw_thing(moon, 'green')
        plot(sun_center(1), sun_center(2), 'y*');
        % plot settings
        set(gca,'Color','k')
        axis([-10, 10, -10, 10]);
        pause(0.0005);
    end
end