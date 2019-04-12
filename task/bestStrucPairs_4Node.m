%% List of the "best structure pairs"
% Read in "strucPairs.csv" which determines the pairs of images that should
% be presented.
strucPairs = readtable('strucPairs.csv', 'Delimiter', ',');

%take the ones in the three node condition
rows = strucPairs.nodeCondition == 4;
vars = {'nodeCondition', 'pairNumber', 'struc1Number', 'struc2Number'};
fourNodeStrucPairs = strucPairs(rows, vars);

%add new column to table with random numbers
fourNodeStrucPairs.randOrder = rand(height(fourNodeStrucPairs), 1);

%sort by random numbers
fourNodeStrucPairs = sortrows(fourNodeStrucPairs, 'randOrder');

%randomly swap the images in the pairs across the columns

%first, add new columns: struc1 and struc2
fourNodeStrucPairs.struc1 = fourNodeStrucPairs.struc1Number;
fourNodeStrucPairs.struc2 = fourNodeStrucPairs.struc2Number;

%determine which pairs to swap and add row to table
fourNodeStrucPairs.randomSwaps = round(rand(height(fourNodeStrucPairs),1));
fourNodeStrucPairs{fourNodeStrucPairs.randomSwaps == 1, 'struc1'} = fourNodeStrucPairs{fourNodeStrucPairs.randomSwaps == 1, 'struc2Number'};
fourNodeStrucPairs{fourNodeStrucPairs.randomSwaps == 1, 'struc2'} = fourNodeStrucPairs{fourNodeStrucPairs.randomSwaps == 1, 'struc1Number'};


%% turn first array into table with image names
rows = fourNodeStrucPairs.nodeCondition == 4;
vars = {'nodeCondition', 'pairNumber', 'struc1', 'struc2'};
strucTable_4 = fourNodeStrucPairs(rows, vars);

%add columns with image names
    
%image 1
for k = 1:height(strucTable_4)
    valuetofind1 = strucTable_4{k, 'struc1'}; %need to find the image name for the first structure in the table
    valuetofind2 = strucTable_4{k, 'struc2'};
    networkNumber1 = valuetofind1;
    networkNumber2 = valuetofind2;
    network1 = eval(['network', int2str(networkNumber1)]);
    network2 = eval(['network', int2str(networkNumber2)]);
    imageName1 = network1.filename;
    imageName2 = network2.filename;
    connections1 = network1.connections;
    connections2 = network2.connections;
    strucTable_4.im1{k} = imageName1;
    strucTable_4.im2{k} = imageName2;
    strucTable_4.connections1{k} = connections1;
    strucTable_4.connections2{k} = connections2;
end

