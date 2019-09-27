
function Categorization(varargin)

% Pass in the parameters
if nargin < 1
    SubjectCode = 'nnn';
    stim_path = './Stimuli';
    results_path = './Results';
    round = 'a';
    continuum = '1';
else
    SubjectCode = varargin{1};
    stim_path = varargin{2};
    results_path = varargin{3};
    round  = varargin{4};
    continuum = varargin{5};
end


% Call some default settings
PsychDefaultSetup(2);
InitializePsychSound


% Disable keys except the arrows
RestrictKeysForKbCheck([115, 117, 66, 10])
% Get the screen numbers
screens = Screen('Screens');

% Select the external screen if it is present, else revert to native screen
screenNumber = max(screens);

 % Define colors 
background = [242,233,220] ./ 255;

% Open an on screen window and color it
[window, ~] = PsychImaging('OpenWindow', screenNumber, background);
HideCursor(window); % Hide the cursor

% Get the size of the onscreen window in pixels
[screenXpixels, screenYpixels] = Screen('WindowSize', window);




%% Load in sounds

% Some parameters for the audio
stim_list_dir = './Stim_List';
% Set interstumulus interval
ITI = 0.75; % set intertrial interval
flash_duration = 0.2; % when the participant responds, how long will a flash surrounding their response show?

% get endpoint labels
if strcmp(continuum, '1')
    end_pt_1_label = 'Ba';
    end_pt_2_label = 'Da';
else
    end_pt_1_label = 'Sa';
    end_pt_2_label = 'Sha';
end


shuffled_list = [];

if strcmp(round, 'a')
    type = 'Gaussian';
elseif strcmp(round, 'b')
    type = 'Uniform';
else
    error('Form not recognized! Did you mean to press a or b?')
end

   
% Read in the list
if strcmp(continuum, '1')
    stim = 'Ba_Da';
else
    stim = 'Sa_Sha';
end

stim_list_file_base = ['Stimlist_' type '_' stim '.txt'];
list_base = read_list([stim_list_dir '/' stim_list_file_base]);
shuffled_list_tmp = permute_list(list_base, 1);
shuffled_list = [shuffled_list, shuffled_list_tmp];


%% Load in images
imageLocation = './Images';


if strcmp(continuum, '1')
    end_pt_1_image_name = 'sheep_1.png';
    end_pt_2_image_name = 'sheep_2.png';
else
    end_pt_1_image_name = 'snake_1.png';
    end_pt_2_image_name = 'snake_2.png';
end


image_1 = imread([imageLocation '/' end_pt_1_image_name], 'BackgroundColor', background);
image_2 = imread([imageLocation '/' end_pt_2_image_name], 'BackgroundColor', background);


% Get size of images
[s11, s21, ~] = size(image_1);
[s12, s22, ~] = size(image_2);

aspect_ratio_1 = s21/s11;
aspect_ratio_2 = s22/s12;

imageHeights = 800;
imageWidth1 = imageHeights .* aspect_ratio_1;
imageWidth2 = imageHeights .* aspect_ratio_2;

imageTexture1 = Screen('MakeTexture', window, image_1);
imageTexture2 = Screen('MakeTexture', window, image_2);
% make the destination rectangles for our image

dstRects = zeros(4, 2);
theRect1 = [0 0 imageWidth1 imageHeights];
theRect2 = [0 0 imageWidth2 imageHeights];

dstRects(:,1) = CenterRectOnPointd(theRect1, screenXpixels/4, screenYpixels/2);
dstRects(:,2) = CenterRectOnPointd(theRect2, screenXpixels*(3/4), screenYpixels/2);

Screen('TextSize', window, 128)

%% Load the glowing images 
if strcmp(continuum, '1')
    resp_1_image = 'sheep_1_glow.png';
    resp_2_image = 'sheep_2_glow.png';
else
    resp_1_image = 'snake_1_glow.png';
    resp_2_image = 'snake_2_glow.png';
end
    


glow_1 = imread([imageLocation '/' resp_1_image], 'BackgroundColor', background);
glow_2 = imread([imageLocation '/' resp_2_image], 'BackgroundColor', background);

glowTexture1 = Screen('MakeTexture', window, glow_1);
glowTexture2 = Screen('MakeTexture', window, glow_2);

% Make a list of the reward/break images
break_imgs = {'mid_1_display.jpg';'Level_1_Success.jpg';'mid_2_display.jpg';...
    'Level_2_Success.jpg';'mid_3_display.jpg';'Level_3_Success.jpg'};


%% Set up where to save results
repeat_number = 1;
results_file_base = [SubjectCode '_' num2str(repeat_number) '.txt'];
results_file = [results_path '/' results_file_base];

% Check if this file already exists
while exist(results_file) == 2
    
    %update the repeat number and then the file name
    repeat_number = repeat_number + 1;
    results_file_base = [SubjectCode '_' num2str(repeat_number) '.txt'];
    results_file = [results_path '/' results_file_base];
end

output_pointer = fopen(results_file, 'w');

data_header_row = 'type,trial,stimulus,selection,RT';
timestamp = fix(clock);
fprintf(output_pointer, '%d-%d-%d,%d:%d:%d\n', timestamp(1),timestamp(2),timestamp(3),timestamp(4),timestamp(5),timestamp(6));
fprintf(output_pointer, '%s\n',data_header_row);
fclose(output_pointer);

%% Load up all the sounds in a buffer

for i = 1:length(shuffled_list)
    % Select at random a sound from the continuum
play_file = [stim_path '/' shuffled_list{i}];
[audio, freq] = audioread(play_file);
test_wavedata{i} = [audio'; audio'];



% Want to know the stimulus step, for plotting our psychometric at the end
tmp_str = strsplit(shuffled_list{i}, {'_','.'});
stimulus_step(i) = str2num(tmp_str{4});

end

%% Open the default audio device
PsychPortAudio('Close');
pahandle = PsychPortAudio('Open', [],[],0,freq,2);

%% Make a vector to store the percent classified as end_pt_1 for plotting at the end
% How many steps are in the continuum?
num_steps_in_continuum = max(stimulus_step);
psychometric = zeros(1, num_steps_in_continuum);

% Remind of the rules
instrIm = imread('lets_go.jpg');
theRect = [0 0 screenXpixels screenYpixels];
dstRect = CenterRectOnPointd(theRect, screenXpixels/2, screenYpixels/2);
goTexture = Screen('MakeTexture', window, instrIm);

Screen('DrawTexture', window, goTexture,[],dstRect);
Screen('Flip', window);   
WaitSecs(3)




%% ***********************FIRST BLOCK****************************
break_count = 1;
for j = 1:210
    
   % Draw two animals
    Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
    Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
    Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
    Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
    
    
    % Flip to the screen
    Screen('Flip', window);
    PsychPortAudio('FillBuffer', pahandle, test_wavedata{j});
    PsychPortAudio('Start', pahandle, 1,[]);
    PsychPortAudio('Stop', pahandle, 1);
    reps_start_time = GetSecs;
    keyPress = 0;
    % Wait for a keystroke to terminate
    while keyPress ==0
        [keyPress, secs, keyCode] = KbCheck();
    end
    
    Response_time = secs - reps_start_time;
    
    % Categorize as choice 1 or choice 2
    kbNameResult = KbName(keyCode);
    disp(kbNameResult)
    if strcmp(kbNameResult,'DownArrow')
        selection = end_pt_1_label;
        
        % Make the click response
        Screen('DrawTextures', window, glowTexture1, [], dstRects(:,1));
        Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
        Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
        Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
    elseif strcmp(kbNameResult,'RightArrow')
        selection = end_pt_2_label;
        psychometric(stimulus_step(j)) = psychometric(stimulus_step(j))+1;

        % Click response display
        Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
        Screen('DrawTextures', window, glowTexture2, [], dstRects(:,2));
        Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
        Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
    elseif strcmp(kbNameResult, 'ESCAPE')
        RestrictKeysForKbCheck([])
        sca
        ShowCursor;
    else
        selection = 'NA';
    end
    Screen('Flip', window);
    
    % Write to file
    output_pointer = fopen(results_file, 'a');
    fprintf(output_pointer, '%s,%d,%s,%s,%d\n',...
        type, ... %s
        j, ... %d
        shuffled_list{j}, ... %s
        selection, ... %s
        Response_time); %d
    fclose(output_pointer);
    
    % Flip back to the original screen
    Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
    Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
    Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
    Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
    
    
    WaitSecs(flash_duration)
    % Flip to the screen
    Screen('Flip', window);
     
    % Increment the psychometric function by
    WaitSecs(ITI-flash_duration);
    
    
    if rem(j, 35) == 0
         % Display progress 
        this_break = break_imgs{break_count};
        rewardIm = imread(this_break);
        break_count = break_count + 1;

        theRect = [0 0 screenXpixels screenYpixels];
        dstRect = CenterRectOnPointd(theRect, screenXpixels/2, screenYpixels/2);
        rewardTexture = Screen('MakeTexture', window, rewardIm);

        Screen('DrawTexture', window, rewardTexture,[],dstRect);
        Screen('Flip', window);
        WaitSecs(4)

        % Break?
        if break_count <= 6
            instrIm = imread('Break.jpg');
            instTexture = Screen('MakeTexture', window, instrIm);

            Screen('DrawTexture', window, instTexture,[],dstRect);
            Screen('Flip', window);
            % Wait for a key press
            wait4Space = 0;
            while ~wait4Space
                [keyIsDown, secs, keyCode, ~] = KbCheck(-1);
                if keyIsDown
                    wait4Space = 1;
                end
            end

            % Then display the go sign
            Screen('DrawTexture', window, goTexture,[],dstRect);
            Screen('Flip', window);
            WaitSecs(3)
        end
    end
    
end

%%%%%%%%%%%%%%%%% REWARD %%%%%%%%%%%%%%%%%5

% 
% %%%%%%%%%%%%%%%%%%% SECOND BLOCK ###########################
% for j = 71:140
%    % Starting screen
%     
%    % Draw two animals
%     Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%     Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%     Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%     Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     
%     
%     % Flip to the screen
%     Screen('Flip', window);
%     PsychPortAudio('FillBuffer', pahandle, test_wavedata{j});
%     PsychPortAudio('Start', pahandle, 1,[]);
%     PsychPortAudio('Stop', pahandle, 1);
%     reps_start_time = GetSecs;
%     keyPress = 0;
%     % Wait for a keystroke to terminate
%     while keyPress ==0
%         [keyPress, secs, keyCode] = KbCheck();
%     end
%     
%     
%     Response_time = secs - reps_start_time;
%     
%     % Categorize as choice 1 or choice 2
%     kbNameResult = KbName(keyCode);
%     disp(kbNameResult)
%     if strcmp(kbNameResult,'DownArrow')
%         selection = end_pt_1_label;
%         
%         % Make the click response
%         Screen('DrawTextures', window, glowTexture1, [], dstRects(:,1));
%         Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%         Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%         Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     elseif strcmp(kbNameResult,'RightArrow')
%         selection = end_pt_2_label;
%         psychometric(stimulus_step(j)) = psychometric(stimulus_step(j))+1;
%         
%         % Click response display
%         Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%         Screen('DrawTextures', window, glowTexture2, [], dstRects(:,2));
%         Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%         Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     elseif strcmp(kbNameResult, 'ESCAPE')
%         RestrictKeysForKbCheck([])
%         sca
%         ShowCursor;
%     else
%         selection = 'NA';
%     end
%     Screen('Flip', window);
%     
%     % Write to file
%     output_pointer = fopen(results_file, 'a');
%     fprintf(output_pointer, '%s,%d,%s,%s,%d\n',...
%         type, ... %s
%         j, ... %d
%         shuffled_list{j}, ... %s
%         selection, ... %s
%         Response_time); %d
%     fclose(output_pointer);
%     
%     % Flip back to the original screen
%     Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%     Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%     Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%     Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     
%     
%     WaitSecs(flash_duration)
%     % Flip to the screen
%     Screen('Flip', window);
%      
%     % Increment the psychometric function by
%     WaitSecs(ITI-flash_duration);
%     
% end
% 
% 
% %%%%%%%%%%%%%%%%% REWARD %%%%%%%%%%%%%%%%%5
%         % Display progress 
% rewardIm = imread('Level_2_Success.jpg');
%         
% theRect = [0 0 screenXpixels screenYpixels];
% dstRect = CenterRectOnPointd(theRect, screenXpixels/2, screenYpixels/2);
% rewardTexture = Screen('MakeTexture', window, rewardIm);
%         
% Screen('DrawTexture', window, rewardTexture,[],dstRect);
% Screen('Flip', window);
% WaitSecs(4)
% % Break?
% instrIm = imread('Break.jpg');
% instTexture = Screen('MakeTexture', window, instrIm);
% 
% Screen('DrawTexture', window, instTexture,[],dstRect);
% Screen('Flip', window);
% % Wait for a key press
% wait4Space = 0;
% while ~wait4Space
%     [keyIsDown, secs, keyCode, ~] = KbCheck(-1);
%     if keyIsDown 
%         wait4Space = 1;
%     end
% end
% 
% % Then display the go sign
% Screen('DrawTexture', window, goTexture,[],dstRect);
% Screen('Flip', window );
% WaitSecs(3)
% 
% %%%%%%%%%%%%%%%%%%% THIRD BLOCK ###########################
% for j = 141:210
%    % Starting screen
%     
%    % Draw two animals
%     Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%     Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%     Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%     Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     
%     
%     % Flip to the screen
%     Screen('Flip', window);
%     PsychPortAudio('FillBuffer', pahandle, test_wavedata{j});
%     PsychPortAudio('Start', pahandle, 1,[]);
%     PsychPortAudio('Stop', pahandle, 1);
%     reps_start_time = GetSecs;
%     keyPress = 0;
%     % Wait for a keystroke to terminate
%     while keyPress ==0
%         [keyPress, secs, keyCode] = KbCheck();
%     end
%     
%     
%     Response_time = secs - reps_start_time;
%     
%     % Categorize as choice 1 or choice 2
%     kbNameResult = KbName(keyCode);
%     disp(kbNameResult)
%     if strcmp(kbNameResult,'DownArrow')
%         selection = end_pt_1_label;
%         
%         % Make the click response
%         Screen('DrawTextures', window, glowTexture1, [], dstRects(:,1));
%         Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%         Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%         Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     elseif strcmp(kbNameResult,'RightArrow')
%         selection = end_pt_2_label;
%         psychometric(stimulus_step(j)) = psychometric(stimulus_step(j))+1;
%         
%         % Click response display
%         Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%         Screen('DrawTextures', window, glowTexture2, [], dstRects(:,2));
%         Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%         Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     elseif strcmp(kbNameResult, 'ESCAPE')
%         RestrictKeysForKbCheck([])
%         sca
%         ShowCursor;
%     else
%         selection = 'NA';
%     end
%     Screen('Flip', window);
%     
%     % Write to file
%     output_pointer = fopen(results_file, 'a');
%     fprintf(output_pointer, '%s,%d,%s,%s,%d\n',...
%         type, ... %s
%         j, ... %d
%          shuffled_list{j}, ... %s
%         selection, ... %s
%         Response_time); %d
%     fclose(output_pointer);
%     
%     % Flip back to the original screen
%     Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
%     Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));
%     Screen('DrawText', window, end_pt_1_label, screenXpixels/4, screenYpixels/7);
%     Screen('DrawText', window, end_pt_2_label, (3/4)*(screenXpixels), screenYpixels/7);
%     
%     
%     WaitSecs(flash_duration)
%     % Flip to the screen
%     Screen('Flip', window);
%      
%     % Increment the psychometric function by
%     WaitSecs(ITI-flash_duration);
%     
% end
% 
% %%%%%%%%%%%%%%%%% REWARD %%%%%%%%%%%%%%%%%5
        % Display progress
rewardIm2 = imread('You_Did_It.jpg');

theRect = [0 0 screenXpixels screenYpixels];
dstRect = CenterRectOnPointd(theRect, screenXpixels/2, screenYpixels/2);
rewardTexture2 = Screen('MakeTexture', window, rewardIm2);

Screen('DrawTexture', window, rewardTexture2,[],dstRect);
Screen('Flip', window);
WaitSecs(3)



% Clear the screen
sca   
PsychPortAudio('Close')
RestrictKeysForKbCheck([]);


end


                                                      
     