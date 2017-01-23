# Effect of chunking in complex span (Portrat et al., 2016)


library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
library(plotrix)
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

bgk <- c("black","gray","black","black","white")

porchunk <- read.table("Portrat.Chunking.txt",header=TRUE)
names(porchunk) <- c("subject", "S0",	"S1",	"S3",	"S5",	"F0",	"F1",	"F3",	"F5") #for some reason, the headers are not read correctly

porchunkmslow <- porchunk %>% gather("acronym","slow",S0:S5)%>% 
  mutate(acronym=as.numeric(substr(acronym,2,10))) %>%
  select(-c(F0:F5))
porchunkmfast <- porchunk %>% gather("acronym","fast",F0:F5)%>% 
  mutate(acronym=as.numeric(substr(acronym,2,10))) %>%
  select(-c(S0:S5))
porchunkfin <- inner_join(porchunkmslow,porchunkmfast,by=c("subject","acronym")) %>%
  gather("Pace","pc",slow:fast) %>% mutate(acronym=as.factor(acronym)) %>%
  mutate(Pace=as.factor(Pace))
prchunkfin <- BakemanL(as.data.frame(porchunkfin), id="subject", dv="pc")

#plot barchart
p2f <- 0
if (p2f) {
  pdf(file="PortratChunking.pdf",height=8,width=10)
} else {x11(height=8,width=10)}

bp = bargraph.CI(x.factor=acronym, group = Pace, response=pc, data=porchunkfin,
                 cex.names=1.5, cex.lab = 1.5, ylim=c(2,6), cex.axis=1.5,
                 ylab="Number of Items Recalled", xlab="Acronym Condition",
                 legend = TRUE, cex.leg=1.5, yaxt="n",
                 ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))})

axis.break(2,2.5,style="slash") 
axis(2, at=c(3:6),lwd=1, lwd.ticks=1,cex.axis=1.5,las=1)
box()
if (p2f) {dev.off()}

