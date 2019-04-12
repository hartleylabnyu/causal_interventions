%%% Run One Trial %%%

%% Display schematics of both structures for set amount of time %%

trueStrucIm = imread(trueStruc);
foilStrucIm = imread(foilStruc);

%resize images
trueStrucIm = imresize(trueStrucIm, [s1 s2]); 
foilStrucIm = imresize(foilStrucIm, [s1 s2]); 

% Get the size of the image
[s1, s2, s3] = size(trueStrucIm);

% Make the image into a texture
trueStrucTexture = Screen('MakeTexture', window, trueStrucIm);
foilStrucTexture = Screen('MakeTexture', window, foilStrucIm);

if trueStrucSide == 1
    trueSchemLoc = schemLoc1; 
    foilSchemLoc = schemLoc2; 
elseif trueStrucSide == 2
    trueSchemLoc = schemLoc2; 
    foilSchemLoc = schemLoc1; 
end

% Draw the images on the screen, side by side
Screen('DrawTexture', window, trueStrucTexture,[],trueSchemLoc, 0);
Screen('DrawTexture', window, foilStrucTexture, [], foilSchemLoc, 0);

% Flip to the screen
Screen('Flip', window);
WaitSecs(schematicScreenTime);

%% Present chip to participant and record choice %%

 %initialize trial variables
    respToBeMade = true;
    nodeClicked = [];
    rt = [];
    strucSelected = [];
    selectRT = [];
    tStart = GetSecs; %initialize RT
    SetMouse(960, 540, window);
    ShowCursor(['hand']); %show the cursor

if trialType == 3
    % Present chip - always config 1
    Screen('DrawTexture', window, baselineConfigTexture,[],configLoc, 0);
    Screen('DrawTexture', window, trueStrucTexture,[],trueSchemLoc, 0);
    Screen('DrawTexture', window, foilStrucTexture,[],foilSchemLoc, 0);
    Screen('Flip', window, [], 1);

  
    while respToBeMade == true %while they have not clicked on a node
        [mouseX, mouseY, buttons] = GetMouse(window); 
        if sum(buttons) > 0 %once they click
            %determine what node they clicked on 
            nodeClicked = find(nodeCenters(:,1) + 45 > mouseX & nodeCenters(:,1) - 45 < mouseX  & nodeCenters(:,2) + 44 > mouseY & nodeCenters(:,2) - 44 < mouseY);
            if isempty(nodeClicked) %if they didn't click on one, continue
                continue
            end
            rt = GetSecs - tStart; %record RT
            HideCursor();
            respToBeMade = false; %response no longer needed 
        end    
    end




    %% Light up the clicked node and have the chip "load" %%

    if nodeClicked == 1
        Screen('DrawTexture', window, node1ConfigTexture, [], configLoc, 0);
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
    elseif nodeClicked == 2
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
    elseif nodeClicked == 3
        Screen('DrawTexture', window, node3ConfigTexture, [], configLoc, 0);
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
    
%% Determine which lights should turn on based on which button was pressed %
    % all nodes start as off
    node1 = 0;
    node2 = 0;
    node3 = 0;

    if nodeClicked == 1
        node1 = 1; % turn node 1 on
        node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
        node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
        if node2 == 1 && node3 == 0 %if node2 turned on but node3 didn't
            node3 = connections(2, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node2 
        elseif node3 == 1 && node2 == 0 %if node3 turned on but node2 didn't
            node2 = connections(3,2) * (rand(1) < wireStrength); %turn on node2 based on connection with node3
        end       
    elseif nodeClicked == 2
        node1 = connections(2, 1) * (rand(1) < wireStrength);
        node2 = 1;
        node3 = connections(2, 3) * (rand(1) < wireStrength); 
        if node1 == 1 && node3 == 0 %if node1 turned on but node3 didn't
            node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1 
        elseif node3 == 1 && node1 == 0 %if node3 turned on but node1 didn't
            node1 = connections(3,1) * (rand(1) < wireStrength); %turn on node1 based on connection with node3
        end 
    elseif nodeClicked == 3
        node1 = connections(3, 1) * (rand(1) < wireStrength);
        node2 = connections(3, 2) * (rand(1) < wireStrength);
        node3 = 1;
        if node1 == 1 && node2 == 0 %if node1 turned on but node2 didn't
            node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1 
        elseif node2 == 1 && node1 == 0 %if node2 turned on but node1 didn't
            node1 = connections(2, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node2
        end   
    end
    

%% Activate chip and display output %%

    %get correct image
    row = (configTable.nodeClicked == nodeClicked) & (configTable.node1 == node1) & (configTable.node2==node2) & (configTable.node3 == node3);
    configPic =  configTable.filename{row};
    configIm = imread([chipImDir configPic]);
    configTexture = Screen('MakeTexture', window, configIm); %make the image into a texture
    Screen('DrawTexture', window, configTexture, [], configLoc, 0); %drawTexture
    Screen('Flip', window, [], 1);
    t1 = PsychPortAudio('Start', pahandle2, repetitions, 0, 1);
    WaitSecs(1);
    ShowCursor();
    
    
    %%%% 
    
elseif trialType == 4
    % Present chip - always config 1
    Screen('DrawTexture', window, baselineConfigTexture4,[],configLoc, 0);
    Screen('DrawTexture', window, trueStrucTexture,[],trueSchemLoc, 0);
    Screen('DrawTexture', window, foilStrucTexture,[],foilSchemLoc, 0);
    Screen('Flip', window, [], 1);
    
    while respToBeMade == true %while they have not clicked on a node
        [mouseX, mouseY, buttons] = GetMouse(window); 
        if sum(buttons) > 0 %once they click
            %determine what node they clicked on 
            nodeClicked = find(nodeCenters4(:,1) + 45 > mouseX & nodeCenters4(:,1) - 45 < mouseX  & nodeCenters4(:,2) + 44 > mouseY & nodeCenters4(:,2) - 44 < mouseY);
            if isempty(nodeClicked) %if they didn't click on one, continue
                continue
            end
            rt = GetSecs - tStart; %record RT
            HideCursor();
            respToBeMade = false; %response no longer needed 
        end    
    end
    
     %% Determine which lights should turn on based on which button was pressed %
    % all nodes start as off
    node1 = 0;
    node2 = 0;
    node3 = 0;
    node4 = 0;

    %run function to determine which nodes should light up
  [node1 node2 node3 node4] = fourNodeOutput(nodeClicked, connections, wireStrength);

  %% Light up the clicked node and have the chip "load" %%

    if nodeClicked == 1
        Screen('DrawTexture', window, node1ConfigTexture4, [], configLoc, 0);
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
    elseif nodeClicked == 2
        Screen('DrawTexture', window, node2ConfigTexture4, [], configLoc, 0);
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
    elseif nodeClicked == 3
        Screen('DrawTexture', window, node3ConfigTexture4, [], configLoc, 0);
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
    elseif nodeClicked == 4
        Screen('DrawTexture', window, node4ConfigTexture4, [], configLoc, 0);
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
    
    
   
   
        %% Activate chip and display output %%

    %get correct image
    row = (configTable4.nodeClicked == nodeClicked) & (configTable4.node1 == node1) & (configTable4.node2==node2) & (configTable4.node3 == node3) & (configTable4.node4 == node4);
    configPic =  configTable4.filename{row};
    configIm = imread([chipImDir configPic]);
    configTexture = Screen('MakeTexture', window, configIm); %make the image into a texture
    Screen('DrawTexture', window, configTexture, [], configLoc, 0); %drawTexture
    Screen('Flip', window, [], 1);
    t1 = PsychPortAudio('Start', pahandle2, repetitions, 0, 1);
    WaitSecs(1);
    ShowCursor();
end
  
    



%% Select structure

respToBeMade = true;
tStart = GetSecs;

while respToBeMade == true %before the participant selects a structure, flash a border around them
Screen('FrameRect', window, [255 255 255], [schemLoc1(1) - 20 schemLoc1(2) - 20 schemLoc2(3) + 20 schemLoc2(4) + 20]);
Screen('Flip', window, [], 1);
 ShowCursor(['hand']); %show the cursor
    [mouseX, mouseY, buttons] = GetMouse(window); 
    if sum(buttons) > 0
        strucClicked = find(strucLocs(:,1) + 200 > mouseX & strucLocs(:,1) < mouseX  & strucLocs(:,2) + 200 > mouseY & strucLocs(:,2) < mouseY);
         if isempty(strucClicked) %if they didn't click on one, continue
            continue
         end
        strucRT = GetSecs - tStart; %record RT
        HideCursor();
        respToBeMade = false; %response no longer needed 
    end
end

if strucClicked == 1
    Screen('FrameRect', window, [57 255 20], [schemLoc1(1) - 20 schemLoc1(2) - 20 schemLoc1(3) + 20 schemLoc1(4) + 20], [10]);
    Screen('Flip', window, [], 1);
    WaitSecs(.5);
elseif strucClicked == 2
    Screen('FrameRect', window, [57 255 20], [schemLoc2(1) - 20 schemLoc2(2) - 20 schemLoc2(3) + 20 schemLoc2(4) + 20], [10]);
    Screen('Flip', window, [], 1);
    WaitSecs(.5);
end
    

%% Get confidence rating
[position, confRT, answer] = slideScale(window, 'How sure are you that you selected the correct chip?', [0 0 screenXpixels screenYpixels/2], {'not at all sure', 'completely sure'}, 'linelength', 10, 'width', 3, 'slidecolor', [255 0 0], 'scalacolor', [255 255 255], 'aborttime', 1000);
WaitSecs(1);

%% Add Data to Trial Array


trialData{trial, 1} = subjectNumber;
trialData{trial, 2} = cbNumber;
trialData{trial, 3} = trial;
trialData{trial, 4} = trueStruc;
trialData{trial, 5} = foilStruc;
trialData{trial, 6} = trueStrucNum;
trialData{trial, 7} = foilStrucNum;
trialData{trial, 8} = trueStrucSide;
trialData{trial, 9} = nodeClicked;
trialData{trial, 10} = rt;
trialData{trial, 11} = configPic;
trialData{trial, 12} = strucClicked;
trialData{trial, 13} = strucRT;
trialData{trial, 14} = position;
trialData{trial, 15} = confRT;
trialData{trial, 16} = trialType;



%% Save data
 fileID = fopen(filename, 'a'); %open the file in order to append data
 fprintf(fileID,'\n');
 fprintf(fileID,formatSpec,trialData{trial, :}); %add the trial data to the file
 fclose(fileID); %Close the file.
