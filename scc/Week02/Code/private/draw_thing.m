function draw_thing(thing, color) 
    % DRAW   draw the thing with the corners connected and a dot in center
    plot(thing.center(1), thing.center(2), '.', 'Color', color); 
    plot([thing.topL(1), thing.topR(1)], [thing.topL(2), thing.topR(2)], 'Color', color);
    plot([thing.topR(1), thing.botR(1)], [thing.topR(2), thing.botR(2)], 'Color', color);
    plot([thing.botR(1), thing.botL(1)], [thing.botR(2), thing.botL(2)], 'Color', color);
    plot([thing.botL(1), thing.topL(1)], [thing.botL(2), thing.topL(2)], 'Color', color);
end