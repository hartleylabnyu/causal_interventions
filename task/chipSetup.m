%% CHIP TASK: MAIN SCRIPT %%
% Main control script %
clear all; close all; clc  %clear everything
rng('shuffle'); %shuffle the randomizer
Screen('Preference', 'SkipSyncTests', 1); %Skip synchronization tests

%% DEFINE TASK VARIABLES %%
% all these can be easily changed
numTrials = 40; %number of trials - should be set at 40
numPracTrials = 5; %number of practice trials - should be set at 5 
wireStrengthProb = .8;  %probability that a node turns on if there is a connection
schematicScreenTime = 2; %number of seconds the schematics should be on the screen
chipLoadTime = .2; %number of seconds it takes the chip to "activate"
s1 = 300; %size (in pixels) of schematic image (dimension1) 
s2 = 300; %size (in pixels) of schematic image (dimension2)

%% DEFINE NODE POSITIONS %%
% These coordinates are from the illustrator files. Later in the script,
% they get updated to account for the screen size / position on the screen
nodeCenters = [52, 301; 241, 61; 364, 363];%matrix of x-y positions of each node (on a 1920 x 1080 screen);
nodeCenters4 = [91, 385; 153, 133; 410, 133; 373, 394]; 
pracNodeCenters = [86, 251];
singleNodeCenters = [241, 61]; %for the tutorial


%% DEFINE IMPORTANT FILES AND DIRECTORIES %%
configTable = readtable('configCodes.csv', 'delim', ','); %table linking codes to image names
configTable4 = readtable('configCodes4.csv', 'delim', ','); %table linking 4-node codes to image names
configTable4.Properties.VariableNames = {'nodeClicked', 'node1', 'node2', 'node3', 'node4', 'filename'};
chipImDir = 'images/chipConfigs/jpgs/'; %directory with the images of the chips
schemImDir = 'images/schematics/jpgs/'; %directory with the images of the schematics

%% GET SUBJECT INFORMATION %%
subjectNumber = input('Enter subject number ');
cbNumber = input('Enter counter balance condition (1 or 2) ');

%% SET UP DATA FILE %%
filename = [int2str(subjectNumber), '_chipTask.txt'];

    while exist(filename, 'file') == 2 %check to see if the file exists
        subjectNumber = input('File already exists! Please enter a new subject number '); %reject the subject number if the file already exists
        filename = [int2str(subjectNumber), '_chipTask.txt'];
    end
        
fileID = fopen(filename, 'w');

% create a file with the following columns:

% 1. Participant ID
% 2. CB order (i.e. did they do this before or after the other task)
% 3. Trial number
% 4. True structure (image name)
% 5. Foil structure (image name)
% 6. True structure (image number)
% 7. Foil structure (image number)
% 8. Side of true structure (1 for left, 2 for right)
% 9. Node clicked
% 10. Reaction time for node clicked
% 11. Outcome image (image name)
% 12. Structure selected (1 for left, 2 for right)
% 13. Reaction time for structure selected
% 14. Confidence rating for structure selected
% 15. Reaction time for confidence rating
% 16. 3 or 4 node condition


formatSpec = '%f\t %f\t %f\t %s\t %s\t %f\t %f\t %f\t %f\t %d\t %s\t %f\t %d\t %f\t %d\t %f\n'; 

fileVars = {'SubID','CBOrder','TrialNum','TrueStrucImage','FoilStrucImage', 'TrueStrucNum', 'FoilStrucNum','SideTrueStruc',...
    'NodeClick','RTNodeClick','OutcomeImg','StrucSelected','RTStrucSelected',...
    'ConfRating','RTConfRating','NodeCondition'};

for fV = 1:length(fileVars)
    PrintType = '%s';
    PrintfV = cell2mat(fileVars(fV));
    fprintf(fileID, [PrintType '\t'], PrintfV);
end

fclose(fileID);

%% SET UP SCREEN %%

% Get the screen numbers
screens = Screen('Screens');

% Draw to the external screen if there is one
screenNumber = max(screens);

% Define black, white, and grey
white = WhiteIndex(screenNumber);
black = BlackIndex(screenNumber);
grey = white / 2;

% Open an on screen window
[window, windowRect] = PsychImaging('OpenWindow', screenNumber, black);

% Get the size of the on screen window
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

% Query the frame duration
ifi = Screen('GetFlipInterval', window);

% Get the centre coordinate of the window
[xCenter, yCenter] = RectCenter(windowRect);

% Define alpha blending mode so that transparent images are actually
% transparent
Screen(window,'BlendFunction',GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

%% INITIALIZE SOUNDS
InitializePsychSound(1); %initialize the sound
[pahandle]= audioInitialize(1,'beep.wav');
[pahandle2]= audioInitialize(1,'beep2.wav');
repetitions = 1;


%% SET UP KEYBOARD %%
% Define the keyboard keys that are listened for. We won't be using the
% keyboard in this task, so we only care about the escape key to quit.
KbName('UnifyKeyNames'); 
escapeKey = KbName('ESCAPE'); %this doesn't do anything right now, oops - need to build in easy escape later in the task

%% DEFINE IMAGES AND SCREEN LOCATIONS %%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the initial screen with the two schematics %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Define image locations
loc1 = [(screenXpixels/4 - (s1)/2 + 150) (screenYpixels/2 - (s2)/2) ((screenXpixels/4 - (s1)/2)+450) ((screenYpixels/2 - (s2)/2)+300)];
loc2 = [(screenXpixels/2 + (s1)/2 - 150) (screenYpixels/2 - (s2)/2) ((screenXpixels/2 + (s1)/2)+150) ((screenYpixels/2 - (s2)/2)+300)];

%assign image locations to true structure and foil structure (will get
%flipped later if needed)
trueLoc = loc1;
foilLoc = loc2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the screen where the participant makes their choice %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%import and resize the first practice configuration image
pracSchem14 = imread([schemImDir, 'schem7.jpg']);
pracSchem14 = imresize(pracSchem14, [200 200]);
pracSchem14Texture = Screen('MakeTexture', window, pracSchem14); %make into texture

%import and resize the practice configuration image
pracConfig = imread([chipImDir, 'practice1.jpg']);
pracConfig = imresize(pracConfig, [500 500]);
pracConfigTexture = Screen('MakeTexture', window, pracConfig); %make into texture

% import and resize the second practice configuration
prac2 = imread([chipImDir, 'practice2.jpg']);
prac2 = imresize(prac2, [500 500]);
prac2Texture = Screen('MakeTexture', window, prac2);  %make into texture

% import and resize the third practice configuration
prac3 = imread([chipImDir, 'practice3.jpg']);
prac3 = imresize(prac3, [500 500]);
prac3Texture = Screen('MakeTexture', window, prac3); %make into texture

% import and resize the practice schematic
pracSchem = imread([schemImDir, 'practiceSchem.jpg']);
pracSchem = imresize(pracSchem, [200 200]);
pracSchemTexture = Screen('MakeTexture', window, pracSchem);

%import and resize the baseline configuration image
baselineConfig = imread([chipImDir, 'config1.jpg']);
baselineIm = imresize(baselineConfig, [500 500]);
[bs1, bs2, bs3] = size(baselineIm);
baselineConfigTexture = Screen('MakeTexture', window, baselineIm); %make the image into a texture

%import and resize the baseline configuration image for the 4-node
%condition
baselineConfig4 = imread([chipImDir, '4_config1.jpg']);
baselineIm4 = imresize(baselineConfig4, [500 500]);
baselineConfigTexture4 = Screen('MakeTexture', window, baselineIm4); %make the image into a texture

%import and resize the node clicked images
node1im = imread([chipImDir, 'config2.jpg']);
node1im = imresize(node1im, [500 500]);
node1ConfigTexture = Screen('MakeTexture', window, node1im); %make the image into a texture

node2im = imread([chipImDir, 'config3.jpg']);
node2im = imresize(node2im, [500 500]);
node2ConfigTexture = Screen('MakeTexture', window, node2im); %make the image into a texture

node3im = imread([chipImDir, 'config4.jpg']);
node3im = imresize(node3im, [500 500]);
node3ConfigTexture = Screen('MakeTexture', window, node3im); %make the image into a texture

node1im4 = imread([chipImDir, '4_config18.jpg']);
node1im4 = imresize(node1im4, [500 500]);
node1ConfigTexture4 = Screen('MakeTexture', window, node1im4); %make the image into a texture

node2im4 = imread([chipImDir, '4_config2.jpg']);
node2im4 = imresize(node2im4, [500 500]);
node2ConfigTexture4 = Screen('MakeTexture', window, node2im4); %make the image into a texture

node3im4 = imread([chipImDir, '4_config10.jpg']);
node3im4 = imresize(node3im4, [500 500]);
node3ConfigTexture4 = Screen('MakeTexture', window, node3im4); %make the image into a texture

node4im4 = imread([chipImDir, '4_config26.jpg']);
node4im4 = imresize(node4im4, [500 500]);
node4ConfigTexture4 = Screen('MakeTexture', window, node4im4); %make the image into a texture



%import and resize the "processing" images
%image 1
[processingIm1, ~, alpha] = imread([chipImDir, 'processing1.png']);
processingIm1(:, :, 4) = alpha;
processingTexture1 = Screen('MakeTexture', window, processingIm1);

%image 2
[processingIm2, ~, alpha] = imread([chipImDir, 'processing2.png']);
processingIm2(:, :, 4) = alpha;
processingTexture2 = Screen('MakeTexture', window, processingIm2);

%image 3
[processingIm3, ~, alpha] = imread([chipImDir, 'processing3.png']);
processingIm3(:, :, 4) = alpha;
processingTexture3 = Screen('MakeTexture', window, processingIm3);

% define the locations of the schematics at the top of the screen
schemLoc1 = [(xCenter-300) 50 (xCenter-100) 250];
schemLoc2 = [(xCenter+100) 50 (xCenter+300) 250];

strucLocs = [(xCenter - 305) 50; (xCenter +105) 50];
pracSchemLoc = [(xCenter-100) 50 (xCenter+100) 250];

%define the location of the main picture of the chip configuration
configLoc = [(screenXpixels/2 - bs1/2) screenYpixels-(200+bs2) (screenXpixels/2 + bs1/2) screenYpixels-200]; 
nodeCenters = [nodeCenters(:,1) + configLoc(1), nodeCenters(:,2) + configLoc(2)];
nodeCenters4 = [nodeCenters4(:,1) + configLoc(1), nodeCenters4(:,2) + configLoc(2)];
pracNodeCenters = [pracNodeCenters(:,1) + configLoc(1), pracNodeCenters(:,2) + configLoc(2)];
singleNodeCenters = [singleNodeCenters(:,1) + configLoc(1), singleNodeCenters(:,2) + configLoc(2)];

% assign images to locations (will flip later if needed)
trueSchemLoc = schemLoc1; 
foilSchemLoc = schemLoc2; 


%% GENERATE TRIALS %%
% First generate the structures that link the configurations to their names
run createNodeStructures_3Node; %3 node structures
run createNodeStructures_4Node; %4 node structures


% Then create the tables with the image names
 run bestStrucPairs_3Node;
 run bestStrucPairs_4Node;


%This creates two tables, each with 20 trials: the 3-node & 4-node pairs
%to be run. The order of the trials within each set has already been randomized. 


% Add a vector of ones and twos to determine which is the true structure
trueStrucVec(1:round(numTrials/2)) = 1;
trueStrucVec(round(numTrials/2) + 1:numTrials) = 2;
trueStrucVec = trueStrucVec(randperm(length(trueStrucVec)));

% Add a vector of threes and fours to determine which trial type should be
% presented
trialTypes(1:5) = 3; %in every 10 trials, there will be 5 3-node trials and 5 4-node trials
trialTypes(6:10) = 4;
trialTypeVec = [trialTypes(randperm(length(trialTypes))) trialTypes(randperm(length(trialTypes))) trialTypes(randperm(length(trialTypes))) trialTypes(randperm(length(trialTypes)))];

%% INSTRUCTIONS SET 1 %%
addpath 'instructions'

for i = 1:11
instructionPicName = ['Slide', int2str(i), '.jpeg'];
I1 = imread(instructionPicName);
Screen('PutImage', window, I1); % put image on screen

% Flip to the screen
Screen('Flip', window);
KbStrokeWait;
end

%% PRACTICE CHIP %%

% Present baseline configuration %
Screen('DrawTexture', window, pracSchem14Texture, [], pracSchemLoc, 0);
Screen('DrawTexture', window, baselineConfigTexture,[],configLoc, 0);
Screen('Flip', window, [], 1);

%initialize trial variables
respToBeMade = true;
nodeClicked = [];
rt = [];
strucSelected = [];
selectRT = [];
tStart = GetSecs; %initialize RT
SetMouse(xCenter, yCenter, window);
ShowCursor(['hand']); %show the cursor


while respToBeMade == true %while they have not clicked on a node
    [mouseX, mouseY, buttons] = GetMouse(window); 
    if sum(buttons) > 0 %once they click
        %determine what node they clicked on 
        nodeClicked = find(singleNodeCenters(:,1) + 45 > mouseX & singleNodeCenters(:,1) - 45 < mouseX  & singleNodeCenters(:,2) + 44 > mouseY & singleNodeCenters(:,2) - 44 < mouseY);
        if isempty(nodeClicked) %if they didn't click on one, continue
            continue
        end
        rt = GetSecs - tStart; %record RT
        HideCursor();
        respToBeMade = false; %response no longer needed 
    end    
end

%present outcome
if nodeClicked == 1
    Screen('DrawTexture', window, node2ConfigTexture, [], configLoc, 0);
    Screen('DrawTexture', window, processingTexture1, [], configLoc, 0);
    Screen('Flip', window, [], 1); 
    t1 = PsychPortAudio('Start', pahandle, repetitions, 0, 1);
    WaitSecs(chipLoadTime);
    Screen('DrawTexture', window, processingTexture2, [], configLoc, 0);
    Screen('Flip', window, [], 1);
    t1 = PsychPortAudio('Start', pahandle, repetitions, 0, 1);
    WaitSecs(chipLoadTime);
    Screen('DrawTexture', window, processingTexture3, [], configLoc, 0);
    Screen('Flip', window, [], 1);
    t1 = PsychPortAudio('Start', pahandle, repetitions, 0, 1);
    WaitSecs(chipLoadTime);
end

%display outcome
configIm = imread([chipImDir, 'config6.jpg']);
configIm = imresize(configIm, [500 500]);
configTexture = Screen('MakeTexture', window, configIm); %make the image into a texture
Screen('DrawTexture', window, configTexture, [], configLoc, 0); %drawTexture
Screen('Flip', window);
t1 = PsychPortAudio('Start', pahandle2, repetitions, 0, 1);
WaitSecs(2); %wait 2 seconds

%% INSTRUCTIONS SET 2 %%
for i = 12:19
instructionPicName = ['Slide', int2str(i), '.jpeg'];
I1 = imread(instructionPicName);
Screen('PutImage', window, I1); % put image on screen

% Flip to the screen
Screen('Flip', window);
KbStrokeWait;
end



%% RUN PRACTICE TRIAL %%

for trial = 999
    trialType = 3; %3 node structure
    wireStrength = 1; %set wire strength as deterministic just for the practice trial
    
    %present practice images
    im1 = 'pracStructure1.jpg';
    im2 = 'pracStructure2.jpg';
    strategyDifference = 0;
    trueStruc = [schemImDir, im1]; %filename of true structure schematic
    foilStruc = [schemImDir, im2]; %filename of foil structure schematic
    connections = [0 0 0; 0 0 0; 0 1 0]; % connections that exist in true structure
    trueStrucSide = 1;
    struc1 = 0;
    struc2 = 0;
    trueStrucNum = 0;
    foilStrucNum = 0;
     run runTrial
end

%% INSTRUCTIONS SET 3 %%
for i = 20:22
instructionPicName = ['Slide', int2str(i), '.jpeg'];
I1 = imread(instructionPicName);
Screen('PutImage', window, I1); % put image on screen

% Flip to the screen
Screen('Flip', window);
KbStrokeWait;
end


%% PROBABILITY PRACTICE TRIALS %%
%create vector that determines whether the wire is on or off for the
%tutorial
wireOnVec = [1 1 0 1 1 1 0 1 1 1];

for ii = 1:numPracTrials
    wireOn = wireOnVec(ii);
    run pracTrials
end

Screen('Flip', window);



%% VERY IMPORTANT
wireStrength = wireStrengthProb; %RESET WIRE STRENGTH AS PROBABILISTIC


%% INSTRUCTIONS SET 4 %%
for i = 23:27
instructionPicName = ['Slide', int2str(i), '.jpeg'];
I1 = imread(instructionPicName);
Screen('PutImage', window, I1); % put image on screen

% Flip to the screen
Screen('Flip', window);
KbStrokeWait;
end


%% RUN TRIALS %%

%initialize accuracy sum
accSum = 0;

%initialize table indices (determines which info to pull on each trial)
threeNodeIndex = 1;
fourNodeIndex = 1;

% On each trial need to pull:
% true structure, foil structure, connection matrix for true structure,
% side of true structure

%for easy parsing of first and second half of trials
numtaskTrials = length(trialTypeVec);

for trial = 1:numtaskTrials
    
    %determine which table to use for the trial
    if trialTypeVec(trial) == 3 %if it is a 3-node trial
        trialType = 3; %label the trialType as a 3-node trial
        trialTable = strucTable; %use images from the 3-node, first-half table
        trialIndex = threeNodeIndex; %grab this row of the table
        threeNodeIndex = threeNodeIndex + 1; %move to the next row of this table for next time
    elseif trialTypeVec(trial) == 4 %if it is a 4-node trial
        trialType = 4;
        trialTable = strucTable_4; 
        trialIndex = fourNodeIndex;
        fourNodeIndex = fourNodeIndex + 1; %move to the next row of this table
    end
    
    %Get the relevant variables
    im1 = trialTable.im1{trialIndex}; %get the image name (1)
    im2 = trialTable.im2{trialIndex}; %get the image name (2)
    struc1 = trialTable.struc1(trialIndex); %get the image number (1)
    struc2 = trialTable.struc2(trialIndex); %get the image number (2) 
    
    %determine which is the foil and which is the true image
    if trueStrucVec(trial) == 1
       trueStruc = [schemImDir, im1]; %filename of true structure schematic
       foilStruc = [schemImDir, im2]; %filename of foil structure schematic
       trueStrucNum = struc1;
       foilStrucNum = struc2;
       connections = trialTable.connections1{trialIndex}; % connections that exist in true structure
       trueStrucSide = 1;
    elseif trueStrucVec(trial) == 2
       trueStruc = [schemImDir, im2];
       foilStruc = [schemImDir, im1];
       trueStrucNum = struc2;
       foilStrucNum = struc1;
       connections = trialTable.connections2{trialIndex};
       trueStrucSide = 2;
    end      
     run runTrial
     if strucClicked == trueStrucSide
         trialAcc = 1;
     else 
         trialAcc = 0;
     end
     accSum = accSum + trialAcc;
end
    
%% DISPLAY OVERALL ACCURACY %%

% End screen
line1 = 'Great job!';
line2 = '\n\n You correctly sorted ';
line3 = int2str(accSum);
line4 = ' computer chips! ';
line5 = '\n\n You have earned $2.50 of bonus payment!';

% Draw all the text in one go
Screen('TextSize', window, 30);
DrawFormattedText(window, [line1 line2 line3 line4 line5],...
    'center', screenYpixels * 0.33, white);

% Flip to the screen
HideCursor();
Screen('Flip', window);
KbStrokeWait;
% Clear the screen
sca;



