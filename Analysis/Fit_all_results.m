% This scripts fits all the subjects to psychometric functions, based on
% the contents of a subject list.

close all
clear

% Get the subject list from the directory of results
file_list = dir('../Results/Psychometrics/Raw');

sid_list = {};

for i = 3:length(file_list)
    fname = file_list(i).name;
    s = strsplit(fname, {'_','.'});
    sid = s{3};
    
    
    sid_list{i-2} = sid;
end


% Just take the unique values of the sid list
sid_list = unique(sid_list);

% Find which subjects we've already computed fits for
% Find which subjects already have functions
% Get the subject list from the directory of results
file_list = dir('../Results/Psychometrics/Fit');

sid_list_done = {};

for i = 3:length(file_list)
    fname = file_list(i).name;
    s = strsplit(fname, {'_','.'});
    sid = s{3};
    
    
    sid_list_done{i-2} = sid;
end
sid_list_done = unique(sid_list_done);


use_list = setdiff(sid_list, sid_list_done);

% For each, compute the fit
for i = 1:length(use_list)
    
    % Inputs: subject id, continuum, width of rectangular prior
    read_psychometric_and_fit(use_list{i},'G',0.10)
    % Inputs: subject id, continuum, width of rectangular prior
    read_psychometric_and_fit(use_list{i},'U',0.10)
       
    
end

