Each participant has one output file termed raus."participant_number".
The first row of the file gives the participant ID.
The following 216 rows encode the performance for main and secondary tasks for the 108 experimental trials.
There are two rows for each trial.
The first row of each pair of rows gives the ID of the main task (0 = locations; 1 = chinese ideographs), the exposition duration in ms, the stimulus ID and the response ID (the response is correct if stimulus ID = response ID).
The second row gives the ID of the secondary tasks (0 = none, 1 = movement, 2 = colors). If ID>0, the next number gives the number of secondary tasks completed,followed by the stimulus ID and response ID for each secondary task completed. 
For the movement task, the response is correct if stimulus ID = response ID. For the colors task, the response with ID 1 is correct if stimulus ID < 8; and the response with ID 2 is correct if stimulus ID>8.

The Pascal program auswert.pas takes these as input and produces the output files ergebnis and zweitno.

The file "ergebnis" is also included in SPSS format with variable labels as the file "data1-20.sav". It contains percentages of correct responses in the
main tasks (i1-i18) as a function of exposure duration, main task, and secondary tasks with exposure duration varying most slowly,
as well as percentage correct responses in interference tasks (j1 to j12) as a function of exposure duration, main task and secondary task.
The analyses in the body of the paper were based on these percentages.
