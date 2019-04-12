%% Create structures with network connections and filenames

% For connections, rows indicate where connection originates FROM
% Columns indicate where connection goes TO

network0.connections = [0 0 0; 0 0 0; 0 0 0];
network0.filename = 'schem0.jpg';

network1.connections = [0 1 1; 0 0 0; 0 0 0];
network1.filename = 'schem1.jpg';

network2.connections = [0 0 1; 0 0 0; 0 1 0];
network2.filename = 'schem2.jpg';

network3.connections = [0 0 0; 0 0 1; 1 0 0];
network3.filename = 'schem3.jpg';

network4.connections = [0 0 0; 1 0 0; 0 0 0];
network4.filename = 'schem4.jpg';

network5.connections = [0 0 0; 0 0 1; 0 0 0];
network5.filename = 'schem5.jpg';

network6.connections = [0 0 0; 1 0 1; 0 0 0];
network6.filename = 'schem6.jpg';

network7.connections = [0 0 0; 1 0 0; 1 0 0];
network7.filename = 'schem7.jpg';

network8.connections = [0 1 0; 0 0 0; 0 1 0];
network8.filename = 'schem8.jpg';

network9.connections = [0 0 1; 1 0 0; 0 0 0];
network9.filename = 'schem9.jpg';

network10.connections = [0 1 0; 0 0 0; 0 0 0];
network10.filename = 'schem10.jpg';

network11.connections = [0 1 0; 0 0 0; 1 0 0];
network11.filename = 'schem11.jpg';

network12.connections = [0 0 0; 1 0 0; 0 1 0];
network12.filename = 'schem12.jpg';

network13.connections = [0 0 1; 0 0 0; 0 0 0];
network13.filename = 'schem13.jpg';

network14.connections = [0 0 0; 0 0 0; 0 1 0];
network14.filename = 'schem14.jpg';

network15.connections = [0 0 0; 0 0 0; 1 0 0];
network15.filename = 'schem15.jpg';

network16.connections = [0 1 1; 0 0 1; 0 0 0];
network16.filename = 'schem16.jpg';

network17.connections = [0 1 0; 0 0 1; 0 0 0];
network17.filename = 'schem17.jpg';

network18.connections = [0 0 1; 0 0 1; 0 0 0];
network18.filename = 'schem18.jpg';

network19.connections = [0 1 0; 0 0 0; 0 1 0];
network19.filename = 'schem19.jpg';

network20.connections = [0 1 0; 0 0 0; 1 1 0];
network20.filename = 'schem20.jpg';


%% concatenate
networkStructure = [network1 network2 network3 network4 network5 network6 network7 network8 network9 network10 network11 network12 ... 
    network13 network14 network15 network16 network17 network18];
