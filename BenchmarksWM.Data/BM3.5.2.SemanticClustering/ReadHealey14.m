%%% Load Healey et al. 2014, JEP:General, data

clear variables

load('HealEtal14.mat');

fid = fopen('Healy14.dat', 'w');

fid = fopen('Healy14_10trials.dat', 'w');

for trial = 1:10 %length(data.subject)
    fprintf(fid, '%d %d  ', data.subject(trial), data.session(trial));
    for item = 1:data.listLength
        fprintf(fid, '%d ', data.pres_itemnos(trial,item)); % word identifier numbers
    end
    fprintf(fid, '  ');
    for item = 1:28 %longest recall-sequence (i.e, longest sequence of non-zero entries in rec_itemnos)
        fprintf(fid, '%d ', data.rec_itemnos(trial,item));  % word identifier numbers
    end
    fprintf(fid, '  ');
    for item = 1:28
        fprintf(fid, '%d ', data.recalls(trial, item)); % input positions; 
    end
    fprintf(fid, '  ');
    for item = 1:28
        fprintf(fid, '%d ', data.times(trial, item)); % cumulative recall times; 
    end
    fprintf(fid, '\n');
end
fclose(fid); 

