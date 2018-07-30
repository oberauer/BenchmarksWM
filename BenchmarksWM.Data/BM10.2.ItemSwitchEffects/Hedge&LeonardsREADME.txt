
%%% File description %%%
The data are contained in "Hedge&Leonardsdata.csv". 
An R script to reproduce the reaction time analysis is given in "Hedge&Leonardsscript.r"

%%% Variable Explanation %%%
Column 1: Experiment (1 or 2). 
Column 2: Participant 
Column 3: Block number (block 1 was considered a practice and was removed for analyses)
Column 4: Trial sequence number (each block contained 9 trial sequences)
Column 5: Sequence_length (number of updates in the trial sequence)
Column 6: Update number
Column 7: Update_type (0= first update in sequence, 1 = repeat, 2=switch)
Column 8: RT (in seconds)
Column 9: Correct (1 = correct, 0 =incorrect. An update is counted as correct if the participant correctly identified the final location of both objects at the end of the trial sequence) 

%%% Task description %%%
The dataset contains behavioural data for two experiments examining object switch costs in spatial working memory. The basic effect is that reaction times are faster when participants 
perform two consecutive mental operations on the same object held in working memory (object repetitions) compared to when they perform consecutive operations on different objects 
(object switches). The difference in reaction times is referred to as a switch cost or repetition benefit.

Participants were initially presented with two objects (a red and a blue circle) in a 3x3 grid for 2000ms, and instructed to hold their locations in memory. The objects were then removed, 
and participants were presented with a sequence of arrow cues in the centre square of the gird. The task was for participants to update the location of the objects according to each arrow 
cue, and to press the space bar as quickly as possible when they had performed each update. For example, if they were presented with a rightwards pointing red arrow, participants should  
mentally shift the location of the red object one square to the right. Half of the updates were repetitions (e.g. updating the red object after updating the red object on the previous 
update) and half were switches (e.g. updating the blue object after updating the red object on the previous update), excluding the first update in the sequence which was neither a 
repetition nor a switch. At the end of each sequence, participants were required to indicate the final locations of both objects in the grid. 

Sequences were 9, 11 or 13 updates in length. Participants completed 8 blocks of 9 trial sequences. The first block (practice) and first update in a sequence are not included in the 
analysis. Experiment 2 was identical to Experiment 1 with the exception that participants were required to return their fixation to the centre grid square after each update in Experiment 2. 

Original Paper:
Hedge, C., & Leonards, U. (2013). Using eye movements to explore switch costs in working memory. Journal of Vision, 13(4):18, 1–19, https://doi.org/10.1167/13.4.18. 

Note that there is a minor error in the calculation of median RTs in the original paper:
https://jov.arvojournals.org/article.aspx?articleid=2686885


