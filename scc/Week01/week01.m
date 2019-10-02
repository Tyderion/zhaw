% SCC Week 01 - Gabriel Nadler - Stephanie Bernhard
figure('Renderer', 'painters', 'Position', [300 200 1100 1200])
% Task 1.1
growth = 0.02; % b - m
t = [1800, 1850, 1900, 1950:5:2015];
p = [1, 1.262, 1.650, 2.525, 2.758, 3.018, 3.322, 3.682, 4.061, 4.440, 4.853, 5.310, 5.735, 6.127, 6.520, 6.930, 7.349];
subplot(3, 2, 1)
plot(t, p, '-o')
title('Task 1.1-1.3 <Population Growth>')
hold

t0 = 1960;
y0 = 3.018;
t = 1800:2200;
fn = @(t) y0 * exp(growth * (t - t0));

% Plot growth curve
d = arrayfun(fn, t);
plot(t, d)
ylim([0 15])
xlim([1800 2200])

% Plot growth curve by solving diff. equation
[t, d] = ode45(@(t, y) growth*y, [t0 2200], y0);
% This curve is on top of the previous one
plot(t, d);

%Task 1.3
g = 0.029;
p = 2.941 * 10^-3;
fn = @(t, y) (g - p*y) * y;

[t, d] = ode45(fn,[t0 2200], y0);
plot(t, d)

legend('population', 'Exponential Growth', 'ODE45 ', 'ODE45 modified', 'Location', 'southeast')
xlabel('year')
ylabel('population [B]')

% Task 1.4
subplot(3, 2, 2)
g = 0.01:0.01:0.05;
p = 0.001 : 0.001 : 0.005;
hold
for i=g
    for j=p
        [tx, dx] = ode45(@(t, y) (i - j*y) * y,[t0 2200], y0);
        plot(tx, dx)
    end
end
title('Task 1.4 <Verholst Model>')
xlabel('year')
ylabel('population [B]')
hold off

% Task 1.5
g1 = 0.5;
g2 = 0.8;
g3 = 0.008;
t = 0:40;
y0 = [ 50 30];

fns = @(t, y) [ 
    g1*y(1) - g3 * y(1) * y(2)
    -g2*y(2)+g3*y(1)*y(2) 
];

[t, d] = ode45(fns, t, y0);

subplot(3, 2, 3)
plot(t, d)
title('Task 1.5 <Time Series>')
legend('prey', 'predator', 'Location', 'north')
xlabel('time')
ylabel('population')

subplot(3, 2, 4)
plot(d(:,1),d(:,2))
title('Task 1.5 <Phase Space>')
ylabel('prey')
xlabel('predator')
% It forms roughly a circle, which means the populations are circling
% around a fixed point (where the populations stop fluctuating) without
% ever reaching it

% Task 1.6
g1 = 0.5;
g2 = 0.8;
g3 = 0.008;
g4=5 * 10^-4;
t = 0:200;
y0 = [ 50 30];


fns = @(t, y) [ 
    g1*y(1) - g3 * y(1) * y(2) - g4 * y(1)^2 
    -g2*y(2)+g3*y(1)*y(2) 
];

[t, d] = ode45(fns, t, y0);


subplot(3, 2, 5)
plot(t, d)
title('Task 1.6 <Time Series>')
legend('prey', 'predator')
xlabel('time')
ylabel('population')

subplot(3, 2, 6)
plot(d(:,1),d(:,2))
title('Task 1.6 <Phase Space>')
ylabel('prey')
xlabel('predator')
% Due to the additional term the populations approach and reach the fixed
% point instead of never settling like before
