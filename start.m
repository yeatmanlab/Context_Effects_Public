PsychJavaTrouble

clear

clc

fprintf('Welcome!\n\n\n');

%subjectName = input('What is your name? ','s');
%fprintf('\n');
subjectIni = input('What is your subject ID? ','s');
fprintf('\n');
%ver = input('Experiment version (1 or 2)?  ');

%flag = input('Score board (0 = no, 1 = yes)? ');

fprintf('\n');
fprintf('Thank you!\n');
fprintf('\n');
fprintf('\n');

doPractice = 1;


% Get the directory we are getting results and stimuli from
basedir = fileparts(which('start'));
stimpath = fullfile(basedir,'Stimuli');
resultspath = fullfile(basedir,'Results');


trial = 1;
while doPractice
    if trial == 1
        aaa = input('Do you want to practice (y/n) ? ','s');
    else
        aaa = input('Do you want to practice more (y/n) ? ','s');
    end
    if strcmp(aaa, 'y')
        Categorization_Practice(subjectIni,'./Stimuli', './Results')
        trial = trial + 1;
        clc;
    else
        doPractice = 0;
        clc;
    end
end

doRunMain = 1;

while doRunMain == 1
    aaa = input('Do you want to run the game (y/n) ? ','s');
    
    
    
    
    
    if strcmp(aaa, 'y') == 1
        % Ask which continuum
        bbb = input('Which block? (a/b)', 's');
        
        if strcmp(bbb,'a') || strcmp(bbb, 'b')
           Categorization(subjectIni, stimpath, resultspath, bbb,'1');
         
        end

    end
    
    
    
    doRunMain = 0;
end

