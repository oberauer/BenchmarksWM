# Read data of temporal-isolation experiments

# clear workspace
rm(list = ls());

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory


#Experiment 2 of Nimmo, L. M. & Lewandowsky, S. (2006) Distinctiveness revisited: Unpredictable temporal isolation does not benefit short-term serial recall of heard or seen events. Memory & Cognition, 34, 1368-1375

NimmoLsky <- read.table("AudiVisData.txt", 
                   col.names=c("subject", "session", "condition", 
                               "item1", "item2", "item3", "item4", "item5", "item6", "item7", 
                               "iti1", "iti2", "iti3", "iti4", "iti5", "iti6", 
                               "resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7", 
                               "rt1", "rt2", "rt3", "rt4", "rt5", "rt6", "rt7"))
# condition: v = visual, a = auditory
# item1 to item7: list items in their order of presentation
# iti1 to iti6: inter-item intervals 1 to 6 in seconds
# resp1 to resp7: recalled items (i.e., the key that was pressed)
# rt1 to rt7: response times in seconds


# Recognition data (Experiment 1) of Morin, Brown, & Lewandowsky (2010). 

MorinRecog <- read.table("data recognition random.txt", header=T)

# gap1 to gap8: iter-item intervals in order of presentation: number of digits to be shadowed in the gap (each digit = 550 ms)
# position: serial position tested (for positive probes; new probes are not included in the data set)
# CorrectR: letters that code a correct response (ur = u or r are correct)
# Response: p = new, r = remember, u = know (??)
# Type: Yes = probe was included in the list
# pre/ post: number of digits preceding / following the position of the item matching the probe
# TotalIsolation = sum of pre and post
# TotalTime = retention interval = total time between presentation of the item matching the probe and presentation of the probe
