function [ node1, node2, node3, node4 ] = fourNodeOutput( nodeClicked, connections, wireStrength )
%This function determines which lights should turn on after a light on a
%4-node structure is pressed

 if nodeClicked == 1
        node1 = 1; % turn node 1 on
        node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
        node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
        node4 = connections(1, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
        if node2 == 1 && node3 == 0 && node4 == 0 %if node2 turned on but node3 and node4 didn't
            node3 = connections(2, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node2
            node4 = connections(2, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node2
            if node3 == 1 && node4 == 0 %if node3 turned on but node4 didn't
                node4 = connections(3, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node3
            elseif node4 ==1 && node3 == 0 %if node4 turned on but node3 didn't
                node3 = connections(4, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node4
            end
        elseif node2 == 0 && node3 == 1 && node4 == 0
            node2 = connections(3, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node3
            node4 = connections(3, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node3
            if node2 == 1 && node4 == 0 %if node2 turned on but node4 didn't
                node4 = connections(2, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node2
            elseif node2 ==0 && node4 == 1 %if node4 turned on but node2 didn't
                node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node4 based on connection with node2
            end   
        elseif node2 == 0 && node3 == 0 && node4 == 1
            node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node4
            node3 = connections(4, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node4
            if node2 == 1 && node3 == 0 %if node2 turned on but node3 didn't
                node3 = connections(2, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node2
            elseif node2 ==0 && node3 == 1 %if node3 turned on but node2 didn't
                node2 = connections(3, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node3
            end
        elseif node2 == 1 && node3 == 1 && node4 == 0
            node4 = connections(2,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node2
            if node4 == 0 %if it's still not on
                node4 = connections(3,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node 3
            end
        elseif node2 == 1 && node3 == 0 && node4 == 1
            node3 = connections(2,3) * (rand(1) < wireStrength); 
            if node3 == 0 
                node3 = connections(4,3) * (rand(1) < wireStrength); 
            end
        elseif node2 == 0 && node3 == 1 && node4 == 1
            node2 = connections(3,2) * (rand(1) < wireStrength); 
            if node2 == 0 
                node2 = connections(4,2) * (rand(1) < wireStrength); 
            end
        end
    elseif nodeClicked == 2
        node1 = connections(2, 1) * (rand(1) < wireStrength);
        node2 = 1;
        node3 = connections(2, 3) * (rand(1) < wireStrength); 
        node4 = connections(2, 4) * (rand(1) < wireStrength);
        if node1 == 1 && node3 == 0 && node4 == 0 %if node2 turned on but node3 and node4 didn't
            node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
            node4 = connections(1, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            if node3 == 1 && node4 == 0 %if node3 turned on but node4 didn't
                node4 = connections(3, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node3
            elseif node4 ==1 && node3 == 0 %if node4 turned on but node3 didn't
                node3 = connections(4, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node4
            end
        elseif node1 == 0 && node3 == 1 && node4 == 0
            node1 = connections(3, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node3
            node4 = connections(3, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node3
            if node1 == 1 && node4 == 0 %if node1 turned on but node4 didn't
                node4 = connections(1, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            elseif node1 ==0 && node4 == 1 %if node4 turned on but node2 didn't
                node1 = connections(4, 1) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            end  
        elseif node1 == 0 && node3 == 0 && node4 == 1
            node1 = connections(4, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node4
            node3 = connections(4, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node4
            if node1 == 1 && node3 == 0 %if node1 turned on but node3 didn't
                node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
            elseif node1 ==0 && node3 == 1 %if node3 turned on but node1 didn't
                node1 = connections(3, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node3
            end
        elseif node1 == 1 && node3 == 1 && node4 == 0
            node4 = connections(1,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            if node4 == 0 %if it's still not on
                node4 = connections(3,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node 3
            end
        elseif node1 == 1 && node3 == 0 && node4 == 1
            node3 = connections(1,3) * (rand(1) < wireStrength); 
            if node3 == 0 
                node3 = connections(4,3) * (rand(1) < wireStrength); 
            end
        elseif node1 == 0 && node3 == 1 && node4 == 1
            node1 = connections(3,1) * (rand(1) < wireStrength); 
            if node1 == 0 
                node1 = connections(4,1) * (rand(1) < wireStrength); 
            end
        end       
    elseif nodeClicked == 3
        node1 = connections(3, 1) * (rand(1) < wireStrength);
        node2 = connections(3, 2) * (rand(1) < wireStrength); 
        node3 = 1; 
        node4 = connections(3, 4) * (rand(1) < wireStrength);
        if node1 == 1 && node2 == 0 && node4 == 0 %if node1 turned on but node2 and node4 didn't
            node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
            node4 = connections(1, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            if node2 == 1 && node4 == 0 %if node2 turned on but node4 didn't
                node4 = connections(2, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node3
            elseif node2 == 0 && node4 ==1 %if node4 turned on but node 2 didn't
                node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node4
            end  
        elseif node1 == 0 && node2 == 1 && node4 == 0
            node1 = connections(2, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node2
            node4 = connections(2, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node2
            if node1 == 1 && node4 == 0 %if node1 turned on but node4 didn't
                node4 = connections(1, 4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            elseif node1 ==0 && node4 == 1 %if node4 turned on but node2 didn't
                node1 = connections(4, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node4
            end    
        elseif node1 == 0 && node2 == 0 && node4 == 1 
            node1 = connections(4, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node4
            node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node4
            if node1 == 1 && node2 == 0 %if node1 turned on but node2 didn't
                node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
            elseif node1 ==0 && node2 == 1 %if node2 turned on but node1 didn't
                node1 = connections(2, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node2
            end
        elseif node1 == 1 && node2 == 1 && node4 == 0
            node4 = connections(1,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node1
            if node4 == 0 %if it's still not on
                node4 = connections(2,4) * (rand(1) < wireStrength); %turn on node4 based on connection with node 2
            end
        elseif node1 == 1 && node2 == 0 && node4 == 1
            node2 = connections(1,2) * (rand(1) < wireStrength); 
            if node2 == 0 
                node2 = connections(4,2) * (rand(1) < wireStrength); 
            end
        elseif node1 == 0 && node2 == 1 && node4 == 1
            node1 = connections(2,1) * (rand(1) < wireStrength); 
            if node1 == 0 
                node1 = connections(4,1) * (rand(1) < wireStrength); 
            end
        end
    elseif nodeClicked == 4
        node1 = connections(4, 1) * (rand(1) < wireStrength);
        node2 = connections(4, 2) * (rand(1) < wireStrength); 
        node3 = connections(4, 3) * (rand(1) < wireStrength);
        node4 = 1;
        if node1 == 1 && node2 == 0 && node3 == 0 %if node1 turned on but node2 and node3 didn't
            node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
            node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
            if node2 == 1 && node3 == 0 %if node2 turned on but node3 didn't
                node3 = connections(2, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node2
            elseif node2 == 0 && node4 ==1 %if node4 turned on but node 2 didn't
                node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node4
            end  
        elseif node1 == 0 && node2 == 1 && node3 == 0
            node1 = connections(2, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node2
            node3 = connections(2, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node2
            if node1 == 1 && node3 == 0 %if node1 turned on but node3 didn't
                node3 = connections(1, 3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
            elseif node1 ==0 && node3 == 1 %if node3 turned on but node1 didn't
                node1 = connections(3, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node1
            end    
        elseif node1 == 0 && node2 == 0 && node4 == 1 
            node1 = connections(4, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node4
            node2 = connections(4, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node4
            if node1 == 1 && node2 == 0 %if node1 turned on but node2 didn't
                node2 = connections(1, 2) * (rand(1) < wireStrength); %turn on node2 based on connection with node1
            elseif node1 ==0 && node2 == 1 %if node2 turned on but node1 didn't
                node1 = connections(2, 1) * (rand(1) < wireStrength); %turn on node1 based on connection with node2
            end
        elseif node1 == 1 && node2 == 1 && node3 == 0
            node3 = connections(1,3) * (rand(1) < wireStrength); %turn on node3 based on connection with node1
            if node3 == 0 %if it's still not on
                node3 = connections(2,3) * (rand(1) < wireStrength); %turn on node3 based on connection with node 2
            end
        elseif node1 == 1 && node2 == 0 && node3 == 1
            node2 = connections(1,2) * (rand(1) < wireStrength); 
            if node2 == 0 
                node2 = connections(3,2) * (rand(1) < wireStrength); 
            end
        elseif node1 == 0 && node2 == 1 && node3 == 1
            node1 = connections(2,1) * (rand(1) < wireStrength); 
            if node1 == 0 
                node1 = connections(3,1) * (rand(1) < wireStrength); 
            end
        end
    end

end

