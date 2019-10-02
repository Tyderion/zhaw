function earth_sun3()
    ce_start = [5; 0]; 

    sun = [0; 0]; %coordinates of the sun

    Phi_rotate =  pi/180; %angle for earth orbiting the sun (arbitrarily chosen number)
    n = 300; %number of iterations

    % Rectangle Coords
    earth = create_thing_at(ce_start, 1);

    for i = 1:n
        %earth orbiting the sun
        earth = rotate_thing_around(earth, sun, Phi_rotate);
        
        % Spin things
        earth = spin_thing(earth, Phi_rotate);
        
        %plot
        clf
        hold on
        % Earth
        draw_thing(earth, 'red')
        
        %Sun
        plot(sun(1), sun(2), 'y*');

        set(gca,'Color','k')
        axis([-10, 10, -10, 10]);
        pause(0.0005);
    end
end