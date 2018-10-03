# Read data of Kane et al (2004, JEP:General)

#remove all data and functions
rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

library("readxl")

# Read data
Kane2004 <- read_excel("Kane et al 2004, FINAL DATA for Benchmarks.xlsx", col_names=T, range="A4:AYS239")

# Explanation of column names:

# The initial columns are AGGREGATE SCORES FOR STM TASKS (Columns 2-7), WM TASKS (8-13), & REASONING TAKSKS (14-26)
# wor=word span; let=letter span; dig = digit span; bal = ball span; arr = arrow span; mat = matrix span; ope = operation span; cou = counting span; rea = reading span; nav = navigation span; sym = symmetry span; rot = rotation span; etsin = ets inferences; papfo = paper folding; datsr = dat space relations; remass = remote associates; afqrt = afq rotated blocks; etssy = ets nonsense syllogisms; etssd = ets surface development; afqrc = afq reading comprehension; etsfb = ets form board; wasim = wasi matrices; raven = ravens matrices; beta3 = beta 3 matrices

# The next columns are STM Task item level recall accuray (items a-h[max]), by trial number (01-21[max]) for the 6 STM tasks; the variable name codes the task, as listed above
# This is followed by WM Task item level recall accuracyy (items a-f[max]), by trial number (01-15[max]) for the 6 WM tasks; the variable name codes the task

# Next follows the WM Task trial level recall accy (UPC score; proportion correct), tasks coded as above;
# after that the STM Task trial level recall accy (UPC score; proportion correct), tasks coded as above

# The next columns are the WM task processing-component accuracy by item, by trial; variable names start with "p_" followed by the task code; with trial number (01-15) and items (a-f)

# After that, WM task processing-component accuracy by trial (proportion correct); variable names start with "p_" followed by the task code; with trial number (01-15) but no letter following

# Finally, the last 6 columns are the WM Task processing-component  Mean accuracy by task: variable names start with "p_" followed by the task code, no number or letter following









