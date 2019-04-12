%Run practice trials on the two-node configuration so participants can
%learn how the probabilistic wires work

% Draw the baseline configuration and the schematic texture
Screen('DrawTexture', window, pracConfigTexture,[],configLoc, 0);
Screen('DrawTexture', window, pracSchemTexture, [], pracSchemLoc, 0);

% Flip to the screen
Screen('Flip', window, [], 1);

%initialize trial variables
respToBeMade = true;
nodeClicked = [];
rt = [];
strucSelected = [];
selectRT = [];
tStart = GetSecs; %initialize RT
SetMouse(960, 540, window);
ShowCursor(['hand']); %show the cursor

while respToBeMade == true %while they have not clicked on a node
    [mouseX, mouseY, buttons] = GetMouse(window); 
    if sum(buttons) > 0 %once they click
        %determine if they clicked on the left node
        nodeClicked = find(pracNodeCenters(:,1) + 45 > mouseX & pracNodeCenters(:,1) - 45 < mouseX  & pracNodeCenters(:,2) + 44 > mouseY & pracNodeCenters(:,2) - 44 < mouseY);
        if isempty(nodeClicked) %if they didn't click on one, continue
            continue
        end
        rt = GetSecs - tStart; %record RT
        HideCursor();
        respToBeMade = false; %response no longer needed 
    end    
end

if nodeClicked == 1 %if they clicked the left node and the wire is "on"
    Screen('DrawTexture', window, prac2Texture, [], configLoc, 0); %display the correct processing textures
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
    
%then display the correct output image
if wireOn == 1
    Screen('DrawTexture', window, prac3Texture, [], configLoc, 0);
    Screen('Flip', window, [], 1); 
    t1 = PsychPortAudio('Start', pahandle2, repetitions, 0, 1);
    WaitSecs(2);
elseif wireOn == 0
    Screen('DrawTexture', window, prac2Texture, [], configLoc, 0);
    Screen('Flip', window); 
    t1 = PsychPortAudio('Start', pahandle2, repetitions, 0, 1);
    WaitSecs(2);
end


    


