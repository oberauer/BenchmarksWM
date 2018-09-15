## Read data of Klauer & Zhao (2004)

rm(list=ls())
graphics.off()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

Data1 <- NULL
Data2 <- NULL
Data4 <- NULL
Nsubj <- c(20, 24, 18)

### Read data of Experiments 1 and 2

for (experiment in 1:2) {

  maxT2trials <- 16
  
  for (id in 1:Nsubj[experiment]) {
    
    filename = paste0("./Klauer.Zhao.2004/exp", experiment, "/RAUS.", id)
    ncol <- count.fields(filename, skip=1)
    data <- read.table(filename, header=F, skip=1, fill=T, col.names=1:max(ncol))
    
    D <- as.data.frame(matrix(NA, dim(data)[1]/2, 9 + 3*maxT2trials))
    varnames <- c("id", "trial", "T1", "T2", "T1prestime", "T1stim", "T1resp", "T1correct", "T2num")
    for (t2 in 1:maxT2trials) {
      varnames <- c( varnames, c(paste0("T2stim", t2), paste0("T2resp", t2), paste0("T2correct", t2)) )
    }
    names(D) <- varnames
    
    trial <- 1
    for (line in 1:dim(data)[1]) {
      task <- 2 - (line %% 2) # odd lines -> task 1, even lines -> task 2
      if (task == 1) {
        D[trial, "id"] <- id
        D[trial, "trial"] <- trial
        D[trial, c("T1", "T1prestime", "T1stim", "T1resp")] <- data[line, 1:4]  
        D[trial, "T1correct"] = D[trial,"T1stim"] == D[trial,"T1resp"] 
      }
      if (task == 2) {
        D[trial, "T2"] <- data[line, 1]
        if (data[line, 1] > 0) {
          D[trial, "T2num"] <- data[line, 2]
          a <- 3
          b <- 4
          for (t2 in 1:data[line, 2]) {
            D[trial, c(paste0("T2stim", t2), paste0("T2resp", t2))] <- data[line, a:b]
            if (D[trial, "T2"] == 1) D[trial, paste0("T2correct", t2)] = data[line,a] == data[line,b] 
            # T2 = 1: movement task: correct if stim == response
            if (D[trial, "T2"] == 2) D[trial, paste0("T2correct", t2)] = ceil(data[line,a]/8) == data[line,b]
            # T2 = 2: color task: correct for stim < 8 and response = 1, and for stim > 8 and response = 2
            a <- a+2
            b <- b+2
          }
        }
        trial <- trial + 1
      }
    }
    
    if (experiment == 1) Data1 <- rbind(Data1, D)
    if (experiment == 2) Data2 <- rbind(Data2, D)
  }
}

### Encode Experiment 4
# T1: task 1, 0 = locations, 1 = Chinese ideographs
# T2: task 2, 0 = none, 1 = movement (tapping), 2 = colors. Raw data from the movement task were lost -> aggregate data are read separately

maxT2trials <- 16

for (id in 1:Nsubj[3]) {
  
  filename = paste0("./Klauer.Zhao.2004/exp4/RAUS.", id)
  ncol <- count.fields(filename, skip=1)
  data <- read.table(filename, header=F, skip=1, fill=T, col.names=1:max(ncol))
  
  D <- as.data.frame(matrix(NA, dim(data)[1]/2, 9 + 3*maxT2trials))
  varnames <- c("id", "trial", "T1", "T2", "T1prestime", "T1stim", "T1resp", "T1correct", "T2num")
  for (t2 in 1:maxT2trials) {
    varnames <- c( varnames, c(paste0("T2stim", t2), paste0("T2resp", t2), paste0("T2correct", t2)) )
  }
  names(D) <- varnames
  
  trial <- 1
  for (line in 1:dim(data)[1]) {
    task <- 2 - (line %% 2) # odd lines -> task 1, even lines -> task 2
    if (task == 1) {
      D[trial, "id"] <- id
      D[trial, "trial"] <- trial
      D[trial, c("T1", "T1prestime", "T1stim", "T1resp")] <- data[line, 1:4]  
      D[trial, "T1correct"] = D[trial,"T1stim"] == D[trial,"T1resp"] 
    }
    if (task == 2) {
      D[trial, "T2"] <- data[line, 1]
      if (data[line, 1] == 2) {   # only for T2 = 2 (colors task) the T2 data are in this file
        D[trial, "T2num"] <- data[line, 2]
        a <- 3
        b <- 4
        for (t2 in 1:data[line, 2]) {
          D[trial, c(paste0("T2stim", t2), paste0("T2resp", t2))] <- data[line, a:b]
          D[trial, paste0("T2correct", t2)] = ceil(data[line,a]/8) == data[line,b]
          # T2 = 2: color task: correct for stim < 8 and response = 1, and for stim > 8 and response = 2
          a <- a+2
          b <- b+2
          }
      }
      trial <- trial + 1
    }
  }
  
  Data4 <- rbind(Data4, D)
}

# Read aggregate data of accuracy in the movement (tapping) task
Data4T2 <- read.table("./Klauer.Zhao.2004/exp4/data1-18.dat", header=T)
names(Data4T2) <- c("id", 
                    "T1corr.t1fast,t1loc.t2none", "T1corr.t1fast,t1loc.t2move", "T1corr.t1fast,t1loc.t2col", 
                    "T1corr.t1fast,t1ideo.t2none", "T1corr.t1fast,t1ideo.t2move", "T1corr.t1fast,t1ideo.t2col",  
                    "T1corr.t1medium,t1loc.t2none", "T1corr.t1medium,t1loc.t2move", "T1corr.t1medium,t1loc.t2col", 
                    "T1corr.t1medium,t1ideo.t2none", "T1corr.t1medium,t1ideo.t2move", "T1corr.t1medium,t1ideo.t2col",  
                    "T1corr.t1slow,t1loc.t2none", "T1corr.t1slow,t1loc.t2move", "T1corr.t1slow,t1loc.t2col", 
                    "T1corr.t1slow,t1ideo.t2none", "T1corr.t1slow,t1ideo.t2move", "T1corr.t1slow,t1ideo.t2col",  
                    "T2corr.t1fast.t1loc.t2move", "T2corr.t1fast.t1loc.t2col",
                    "T2corr.t1fast.t1ideo.t2move", "T2corr.t1fast.t1ideo.t2col",
                    "T2corr.t1medium.t1loc.t2move", "T2corr.t1medium.t1loc.t2col",
                    "T2corr.t1medium.t1ideo.t2move", "T2corr.t1medium.t1ideo.t2col",
                    "T2corr.t1slow.t1loc.t2move", "T2corr.t1slow.t1loc.t2col",
                    "T2corr.t1slow.t1ideo.t2move", "T2corr.t1slow.t1ideo.t2col")
# Mean accuracies of T1, for 3 levels of presentation duration of T1 (fast, medium, slow), 2 levels of kind of T1 (location, Chinese ideographs), and 3 levels of kind of T2 (none, movement, color), followed by
# Mean accuracies of T2, for 3 levels of presentation duration of T1 (fast, medium, slow), 2 levels of kind of T1 (location, Chinese ideographs), and 2 levels of T2 (movement, color).
# For the movement task, the accuracy is the average number of long tap pauses multiplied by 100

