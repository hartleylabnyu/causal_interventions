%% List of the "best structure pairs"
% Read in "strucPairs.csv" which determines the pairs of images that should
% be presented.
strucPairs = readtable('strucPairs.csv', 'Delimiter', ',');

%take the ones in the three node condition
rows = strucPairs.nodeCondition == 3;
vars = {'nodeCondition', 'pairNumber', 'struc1Number', 'struc2Number'};
threeNodeStrucPairs = strucPairs(rows, vars);

%add new column to table with random numbers
threeNodeStrucPairs.randOrder = rand(height(threeNodeStrucPairs), 1);

%sort by random numbers
threeNodeStrucPairs = sortrows(threeNodeStrucPairs, 'randOrder');

%randomly swap the images in the pairs across the columns

%first, add new columns: struc1 and struc2
threeNodeStrucPairs.struc1 = threeNodeStrucPairs.struc1Number;
threeNodeStrucPairs.struc2 = threeNodeStrucPairs.struc2Number;

%determine which pairs to swap and add row to table
threeNodeStrucPairs.randomSwaps = round(rand(height(threeNodeStrucPairs),1));
threeNodeStrucPairs{threeNodeStrucPairs.randomSwaps == 1, 'struc1'} = threeNodeStrucPairs{threeNodeStrucPairs.randomSwaps == 1, 'struc2Number'};
threeNodeStrucPairs{threeNodeStrucPairs.randomSwaps == 1, 'struc2'} = threeNodeStrucPairs{threeNodeStrucPairs.randomSwaps == 1, 'struc1Number'};


%% turn first array into table with image names
vars = {'nodeCondition', 'pairNumber', 'struc1', 'struc2'};
strucTable = threeNodeStrucPairs(rows, vars);

%add columns with image names
    
for k = 1:height(strucTable)
    valuetofind1 = strucTable{k, 'struc1'}; %need to find the image name for the first structure in the table
    valuetofind2 = strucTable{k, 'struc2'};
    networkNumber1 = valuetofind1;
    networkNumber2 = valuetofind2;
    firstNetwork = eval(['network', int2str(networkNumber1)]);
    secondNetwork = eval(['network', int2str(networkNumber2)]);
    imageName1 = firstNetwork.filename;
    imageName2 = secondNetwork.filename;
    connections1 = firstNetwork.connections;
    connections2 = secondNetwork.connections;
    strucTable.im1{k} = imageName1;
    strucTable.im2{k} = imageName2;
    strucTable.connections1{k} = connections1;
    strucTable.connections2{k} = connections2;
end

