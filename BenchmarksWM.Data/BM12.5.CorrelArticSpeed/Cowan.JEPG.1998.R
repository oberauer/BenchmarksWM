# Read data of Cowan et al (JEP:General, 1998, Exp. 2)

#remove all data and functions
rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

library("readxl")

# Read data
Cowan98 <- read_excel("Cowan.etal.JEPG.1998.expt2.xls", sheet=2, col_names=T)

# Explanation of column names:

# SubNo	subject number
# age	in years only
# sex	male or female
# ethnic	as shown, self-identified ethnicity:  caucasian, African-American (AfAmer), Asian-American (AsAmer), or unknown (UK)
# 
# DigSp	digit span
# LetSp	letter span
# 
# Let1	searching for 1 letter, number of letters found in time period allowed, 30 s
# Dig1	searching for 1 digit
# Let3	searching for 3 letters
# Dig3	searching for 3 digits
# Let5	searching for 5 letters
# Dig5	searching for 5 digits
# LETINTER	intercept of letter search function across set sizes 1, 3, 5
# LETSLOPE	slope of letter search function (ms per memory set item)
# DIGINTER	intercept of digit search function across set sizes 1, 3, 5
# DIGSLOPE	slope of digit search function (ms per memory set item)
#   
# Alpha	speed of covert recitation of alphabet, silent repetitions of A-Z in 60 s
# Count	speed of covert counting, silent repetitions of 1-10 in 30 s
