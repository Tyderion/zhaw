function earth_sun_moon()
    earth_start = [5; 0];
    moon_start = [earth_start(1) + 2; earth_start(2)];
    sun = [0; 0]; %coordinates of the sun

    Phi_rotate =  pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
    Phi_rotate_moon = Phi_rotate*2; %angle for moon orbiting the earth (arbitrarily chosen number)
    n = 1000; %number of iterations

    % Create moon/earth
    earth = create_thing_at(earth_start, 1);
    moon = create_thing_at(moon_start, 0.5);

    for i = 1:n
        %earth orbiting the sun
        earth = rotate_thing_around(earth, sun, Phi_rotate);

        %moon orbiting the earth
        moon = rotate_thing_around(moon, earth.center, Phi_rotate_moon);

        %moon orbiting the sun (keep with earth)        
        moon = rotate_thing_around(moon, sun, Phi_rotate);

        % Spin things
        earth = spin_thing(earth, Phi_rotate);
        moon = spin_thing(moon, Phi_rotate_moon);

        %plot
        clf
        hold on
        draw_thing(earth, 'red')
        draw_thing(moon, 'green')
        plot(sun(1), sun(2), 'y*');

        % Plot settings
        set(gca,'Color','k')
        axis([-10, 10, -10, 10]);
        pause(0.0005);
    end
end