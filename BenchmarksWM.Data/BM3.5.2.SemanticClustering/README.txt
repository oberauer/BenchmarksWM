This archive contains a MATLAB-based data structure containing the behavioral data 
from the experiment described in the article:

Healey, M. K., Crutchley, P., and Kahana, M. J. (2014). Individual differences in memory search and their relation to intelligence. Journal of Experimental Psychology: General, 143(4), 1553–1569

Refer to this manuscript for the methods of the experiment, and description of the 
analyses that we carried out on these data.

%%%%%%%%%%%%%%%%%%%

This particular file written by Karl Healey (healeym@sas.upenn.edu)

(adapted from one by Sean Polyn sean.polyn@vanderbilt.edu)

Send word if you find anything weird or out of sorts with the data, or the explanation of the organization of the data!

If you are interested in the Context Maintenance and Retrieval model of human memory, go to this webpage:
http://memory.psy.vanderbilt.edu/groups/vcml/wiki/618f3/CMR_Documentation.html

Behavioral Toolbox (Release 1) analysis code available from:
http://memory.psych.upenn.edu/behavioral_toolbox

%%%%%%%%%%%%%%%%%%%

A quick tour of the data structure.

%%%%%%%%%%%%%%%%%%%

If you load the file HealEtal14.mat in MATLAB, you will find a structure called “data”.

There are a number of fields on the data structure.  Each row corresponds to a particular trial.  
If there is more than one column, then there are two possible organizations, refer below to see which one applies.  
(1) Yoked to the presentation order, each column corresponds to a study event.  
(2) Yoked to the recall order, each column corresponds to a recall event.

The most critical sub-fields:

data.subject		% Each row has a numerical index, a unique subject identifier.
data.listtype		% -1 = items studied with no encoding task (control), 0 = all items studied using the SIZE task, 1 = all items studied using ANIMACY task, 2 = task-shift list
data.recalls		% A numerical identifier for each response made by the participant during the free recall period.  Integers 1-24   
			    	% correspond to the serial position of the recalled item.  Yoked to the recall order.  -1 corresponds to an intrusion.  
				% -2 corresponds to a repetition.
data.list_length	% There were 16 items on each study list

The other sub-fields:

data.session		% A session label for each trial, either 1 or 2
data.pres_itemnos	% Each studied item has an index for the wordpool.  Yoked to presentation order.
data.intrusions		% -1 for extra-experimental intrusion, positive numbers correspond to how many lists back a prior-list intrusion 
		     	        % came from. 

Convenience fields (technically these are redundant with information in the other fields):

data.rec_itemnos  		% The wordpool index for each recalled item (can be constructed with pres_itemnos and recalls)

%%%%%%%%%%%%%%%%%%%

Other files that are included.

%%%%%%%%%%%%%%%%%%%

ltpFR_wp		% This is the wordpool for the experiment.  The index values in pres_itemnos and rec_itemnos can be used to 
			        % figure out which words are presented on each trial.  

ltpFR_lsa		% These are the LSA values used for the semantic analyses described 
subjects		% A list of subjects used in the paper analysis, should the data be reorganized in the future.
