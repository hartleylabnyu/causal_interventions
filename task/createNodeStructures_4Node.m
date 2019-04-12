%% Create structures with network connections and filenames

% For connections, rows indicate where connection originates FROM
% Columns indicate where connection goes TO

network100.connections = [0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 0 0];
network100.filename = 'schem100.jpg';

network101.connections = [0 0 0 0; 1 0 1 1; 0 0 0 0; 0 0 0 0];
network101.filename = 'schem101.jpg';

network102.connections = [0 0 1 0; 1 0 0 1; 0 0 0 0; 0 0 0 0];
network102.filename = 'schem102.jpg';

network103.connections = [0 0 1 0; 1 0 0 0; 0 0 0 1; 0 0 0 0];
network103.filename = 'schem103.jpg';

network104.connections = [0 0 1 0; 1 0 0 0; 0 0 0 0; 0 0 0 0];
network104.filename = 'schem104.jpg';

network105.connections = [0 0 1 0; 1 0 0 0; 0 0 0 0; 0 0 1 0];
network105.filename = 'schem105.jpg';

network106.connections = [0 0 0 1; 1 0 1 0; 0 0 0 1; 0 0 0 0];
network106.filename = 'schem106.jpg';

network107.connections = [0 0 0 0; 1 0 1 0; 0 0 0 1; 1 0 0 0];
network107.filename = 'schem107.jpg';

% network108.connections = [0 0 1 1; 1 0 0 0; 0 0 0 0; 0 0 0 0];
% network108.filename = 'schem108.jpg';
% 
% network109.connections = [0 0 0 0; 0 0 1 0; 1 0 0 1; 0 0 0 0];
% network109.filename = 'schem109.jpg';
% 
% network110.connections = [0 0 0 1; 1 0 0 0; 0 0 0 0; 0 0 1 0];
% network110.filename = 'schem110.jpg';
% 
% network111.connections = [0 0 0 1; 0 0 0 0; 0 0 0 0; 0 0 0 0];
% network111.filename = 'schem111.jpg';
% 
% network112.connections = [0 0 0 1; 0 0 1 0; 1 0 0 0; 0 0 0 0];
% network112.filename = 'schem112.jpg';

network113.connections = [0 0 0 0; 0 0 1 0; 1 0 0 0; 0 0 1 0];
network113.filename = 'schem113.jpg';

%% rotated versions

network114.connections = [0 0 0 0; 0 0 0 1; 1 1 0 0; 0 0 0 0];
network114.filename = 'schem114.jpg';

network115.connections = [0 0 0 0; 0 0 0 1; 0 1 0 0; 1 0 0 0];
network115.filename = 'schem115.jpg';

network116.connections = [0 1 0 0; 0 0 0 0; 0 1 0 1; 1 0 0 0];
network116.filename = 'schem116.jpg';

network117.connections = [0 0 0 0; 1 0 0 1; 0 1 0 0; 0 0 0 0];
network117.filename = 'schem117.jpg';

network118.connections = [0 0 0 0; 0 0 0 0; 0 0 0 1; 1 1 0 0];
network118.filename = 'schem118.jpg';

network119.connections = [0 0 0 0; 1 0 0 0; 0 0 0 1; 0 1 0 0];
network119.filename = 'schem119.jpg';

network120.connections = [0 0 0 1; 0 0 0 0; 0 0 0 1; 0 1 0 0];
network120.filename = 'schem120.jpg';

network121.connections = [0 0 0 0; 0 0 0 0; 0 0 0 0; 1 1 1 0];
network121.filename = 'schem121.jpg';

network122.connections = [0 1 0 0; 0 0 0 0; 1 0 0 0; 0 0 1 0];
network122.filename = 'schem122.jpg';

network123.connections = [0 0 0 0; 0 0 0 0; 1 0 0 0; 0 0 1 0];
network123.filename = 'schem123.jpg';

network124.connections = [0 0 0 0; 1 0 0 0; 1 0 0 0; 0 0 1 0];
network124.filename = 'schem124.jpg';

network125.connections = [0 1 0 0; 0 0 0 0; 0 1 0 0; 1 0 1 0];
network125.filename = 'schem125.jpg';

network126.connections = [0 0 0 0; 0 0 0 0; 1 1 0 0; 0 0 1 0];
network126.filename = 'schem126.jpg';

network127.connections = [0 0 1 0; 0 0 0 0; 0 1 0 0; 1 0 0 0];
network127.filename = 'schem127.jpg';

network128.connections = [0 0 0 1; 0 0 1 0; 0 0 0 0; 0 1 0 0];
network128.filename = 'schem128.jpg';

network129.connections = [0 0 0 1; 0 0 0 0; 0 0 0 0; 0 1 0 0];
network129.filename = 'schem129.jpg';

network130.connections = [0 0 0 1; 0 0 0 0; 0 1 0 0; 0 1 0 0];
network130.filename = 'schem130.jpg';

network131.connections = [0 0 0 1; 0 0 0 0; 0 0 0 0; 0 1 1 0];
network131.filename = 'schem131.jpg';

network132.connections = [0 0 0 1; 0 0 0 0; 0 1 0 0; 0 0 1 0];
network132.filename = 'schem132.jpg';

network133.connections = [0 0 0 0; 0 0 0 0; 0 0 0 0; 0 0 1 0];
network133.filename = 'schem133.jpg';


%% concatenate
networkStructure = [network100 network101 network102 network103 network104 network105 network106 network107 network113 network114 network115 network116 network117 network118 network119 network120 network121 network122 network123 network124 network125 network126 network127 network128 network129 network130 network131 network132 network133];
