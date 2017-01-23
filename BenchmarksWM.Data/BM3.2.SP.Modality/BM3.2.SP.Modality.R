############## Figure 8: Modality Effect and its Interaction with Serial Position #####################################

library(readxl)
library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
library(plotrix)


#function that does rearranging
convert <- function(aw,mod,mat) {
  convert <- aw %>% gather("spos","rec",contains("-B")) %>% 
    mutate(spos=as.numeric(substr(spos,5,5))) %>%
    mutate(modality=(mod)) %>%
    mutate(material=(mat))
}

#read data

HB <- read_excel("Harvey.Beaman.2007.xls",sheet="SPSS Final", col_names=TRUE)
HB$Response <- as.numeric(HB$Response)
HB$S <- as.numeric(HB$S)
aw <- as.data.frame(HB[1:51, 1:which(letters=="k")]) #ridiculously, although aw is allegedly a data frame, it is in fact a tibble that has to be converted into a data frame!
an <- as.data.frame(HB[1:51, c(1:2, which(letters=="u"):(which(letters=="u")+8))])
vw <- as.data.frame(HB[54:104, 1:which(letters=="k")])
vn <- as.data.frame(HB[54:104, c(1:2, which(letters=="u"):(which(letters=="u")+8))])

#now gather columns in the usual way using function
aw2 <- convert(aw,"auditory","words")
an2 <- convert(an,"auditory","numbers")
vw2 <- convert(vw,"visual","words")
vn2 <- convert(vn,"visual","numbers")
#harvbeaman <- list(aw2,an2,vw2,vn2)
harvbeaman <- bind_rows(aw2,an2,vw2,vn2) %>% mutate(modality=as.factor(modality)) %>% 
  mutate(material=as.factor(material)) %>% mutate(rec=as.numeric(rec))

#now plot: 1=written response and 2=spoken response. 
colors <- c("black", "black", "grey", "grey")
pt <- c(21,22,21,22)
p2f <- 0
if (p2f) {
  pdf(file="harvbeaman.pdf",height=12,width=8)
} else {x11(height=12,width=8)}
par(mfrow=c(2,1),mar=c(4, 4, 2, 1) + 0.2)
#different responses in different panels
for (r in c(1:2)) {
  plot(0,0, xlim=c(0.5,9.5),ylim=c(0,100), type="n", las=1,
       xlab="Serial Position", ylab="Percent correct",cex.lab=1,xaxt="n",cex.axis=1)
  
  thisresponse <- filter(harvbeaman,Response==r) #pick data for the panel
  k <- 0  #index for plotting symbols
  for (mo in levels(thisresponse$modality)) {
    for (ma in levels(thisresponse$material)) {
      tbp <- thisresponse %>% filter(modality==mo & material==ma) %>% 
        group_by(S,spos) %>% summarize(rec=mean(rec)) %>% 
        group_by(spos) %>% summarize(rec=mean(rec))
      k <- k+1
      lines(tbp$spos,tbp$rec,lwd=1,lty=k)
      points(tbp$spos,tbp$rec,cex=1.3,pch=pt[k],bg=colors[k])
    }
  }
  axis(1, at=c(1:9),lwd=0, lwd.ticks=1,cex.axis=1)
  if (r==1) {text(2,95,"Written response",cex=1)} else {text(2,95,"Spoken response",cex=1)}
  
  if (r == 2) {
    legend(4.5,100,c("Auditory - numbers","Auditory - words",
                     "Visual - numbers", "Visual - words"),
           lty=c(1:4),pch=pt,pt.bg=colors,pt.cex=1, yjust=1)
  }
}
if (p2f) {dev.off()}


