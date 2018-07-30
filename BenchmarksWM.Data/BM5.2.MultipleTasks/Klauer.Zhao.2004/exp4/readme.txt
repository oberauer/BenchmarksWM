Each participant has one output file termed raus."participant_number".
First row of the file gives the participant ID.
The following 216 rows encode the performance for main and secondary tasks for the 108 experimental trials.
There are two rows for each trial.
The first row of each pair of rows gives the ID of the main task (0 = locations; 1 = chinese ideographs), the exposition duration in ms, the stimulus ID and the response ID (the response is correct if stimulus ID = response ID).
The second row gives the ID of the secondary tasks (0 = none, 1 = tapping, 2 = colors). If ID=2, i.e. for the color task, the next number gives the number of secondary tasks completed,followed by the stimulus ID and response ID for each secondary task completed. 

For the tapping task, the relevant data were stored in separate files that were lost as were the raw eye movement data.
The Pascal program auswert.pas takes these as input and produces the output files ergebnis and eyeout.

The file "ergebnis" is also included in spss format as the file "data1-18.sav". It contains percentages of correct responses in the
main tasks (i1-i18) as a function of exposure duration, main task, and secondary tasks with exposure duration varying most slowly,
as well as percentage correct responses in interference tasks (j1 to j12) as a function of exposure duration, main task and secondary task.
For the tapping task, the dependent variable (j1,j3,..,j11) shown in data1-18.sav is the average number of long tap pauses multiplied by 100.




