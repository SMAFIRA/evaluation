############## Data handling
#X<-read.csv("Case_Studies_Auswertungen_Matthias_II.csv",sep=";")
X<-read.csv("data/Evaluation_PubMed_Rankings_Equivalence_Relevance.csv",sep=";")
reflist<-unique(X$Reference)
######################
tiff("results/FIg5.tiff",pointsize = 3,res=400)
par(mfrow=c(min(length(reflist),3),2),mex=0.15,mar=c(5,4,4,2)+0.1,mai=c(0.1,0.1,0.1,0.1))
PANEL1<-c("A","C","E")
PANEL2<-c("B","D","F")
for(i in 1: length(reflist)){
  D<-subset(X,Reference==reflist[i])
  Pindex<-which(D$Equivalence=="+")
  Rindex<-which(D$Relevance=="R")
  
  lD<-dim(D)[1]
  lP<-length(Pindex)
  lR<-length(Rindex)

  fP<-sapply(1:lD,function(xx){sum(sapply(0:lP,function(x)
    {x*choose(xx,x)*(choose(lD-xx,lP-x))/choose(lD,lP)}))})
  fR<-sapply(1:lD,function(xx){sum(sapply(0:lR,function(x)
  {x*choose(xx,x)*(choose(lD-xx,lR-x))/choose(lD,lR)}))})

  
  cumuP<-sapply(1:lD,function(xx){length(which(Pindex<=xx))})
  cumuR<-sapply(1:lD,function(xx){length(which(Rindex<=xx))})


  CEX<-0.9
  plot(1:lD,lwd=0.1,fP,type="l",col="black",xlab="Ranking",cex.axis=0.5*CEX,cex.lab=0.9*CEX,ylab="cumulative number",main="",cex.main=1.*CEX,axes=F)
  points(1:lD,lwd=0.1,cumuP,type="l",col="red")
  axis(side=2,pos=0,lwd=0.1,cex.axis=0.7,labels =seq(0,lP,2),at=seq(0,lP,2))
  axis(side=1,pos=0.2,lwd=0.1,cex.axis=0.7,labels =seq(0,100,50),at=seq(0,100,50))
  text(75,lP/4,PANEL1[i],cex=0.9)
  #text(50,1,"Ranking",cex=.9)
  plot(1:lD,fR,lwd=0.1,type="l",col="black",xlab="Ranking",cex.axis=0.5*CEX,cex.lab=0.9*CEX,ylab="cumulative number",main="",cex.main=1.*CEX,axes=FALSE)
  points(1:lD,cumuR,lwd=0.1,type="l",col="red")
 # axis(side=2,lwd=0.1,cex.axis=0.5 )
  axis(side=2,pos=0,lwd=0.1,cex.axis=0.7)#,labels =seq(0,lR,2),at=seq(0,lR,2))
  axis(side=1,pos=0.2,lwd=0.1,cex.axis=0.7,labels =seq(0,100,50),at=seq(0,100,50))
  text(75,lR/4,PANEL2[i],cex=0.9)
}
dev.off()

