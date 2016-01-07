#the purpose of this script is to simulate trajectory
#from group models and study the relationship between change of composition and label will impact the perfromance as measured by AUC
library(plyr)
library(reshape2)
library(ggplot2)

import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE, stringsAsFactors=FALSE))
}

import.csv.no.head <- function(filename) {
  return(read.csv(filename, sep = ",", header = FALSE, stringsAsFactors=FALSE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

write.csv.na <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE,na="")
}


#simulate from a selected trajectory

calcSigma <- function(X1,X2,global.sigma, l) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- global.sigma*exp(-(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}


sample_from_gp <- function(X, mu_matrix, grp.count, sigma) {
  
  numT <- nrow(mu_matrix)
  numGroup <- ncol(mu_matrix)
  
  lookup.df <- NULL
  sample.df <- NULL
  for (ii in 1:length(grp.count)){
    nsample <- grp.count[ii]
    out=matrix(rep(0, nsample*numT), ncol=nsample)
    for (kk in 1:nsample){
      out[,kk] <- mvrnorm(1,mu_matrix[,ii], sigma)
    }
    
    out.long <- melt(out)
    colnames(out.long) <- c('tvalue','id','ovalue')
    sample.df <- rbind(sample.df, data.frame(grp=ii, out.long))
  }
  
  return(sample.df)  
  
}


get_sample_with_label <- function(sample.df,  pos.pctg){
  num_grp <- length(unique(sample.df$grp))
  num_T <- length(unique(sample.df$tvalue))
  sample.df2 <- NULL
  
  for (kk in 1:num_grp){
    this.grp <- subset(sample.df, grp==kk)
    nsample <- length(unique(this.grp$id))
    label <- rep(0, nsample)
    pos.sample.id <- sample(1:nsample, round(pos.pctg[kk]*nsample,0))
    label[pos.sample.id] <- 1
    this.grp.with.label <- data.frame(this.grp, label)
    sample.df2 <- rbind(sample.df2, this.grp.with.label)
  }
  
  return(sample.df2)
  
}


get_auc <- function(sample.df2){
  auc.out <- NULL
  numT <- length(unique(sample.df2$tvalue))
  for (t in 1:numT){
    this.pred <- subset(sample.df2, tvalue==t)[, c('ovalue', 'label')]
    my.roc <- get_roc(this.pred)
    auc.out <- rbind(auc.out, data.frame(t, auc=my.roc$auc))
  }
  return(auc.out)  
}



#input: prediction data frame (prediction,true_output)
#output: a list with two component:auc and roc data frame with coordinates
get_roc<-function(pred.data){
  label<-pred.data[,2]
  if (length(unique(label))>=2){
    pred<-prediction(pred.data[,1],pred.data[,2])
    perf=performance(pred,"tpr","fpr")
    roc.x=perf@x.values[[1]]
    roc.y=perf@y.values[[1]]
    cutoff=perf@alpha.values[[1]]
    this.roc=data.frame(fpr=roc.x,tpr=roc.y,cutoff=cutoff)
    auc=performance(pred,"auc")@y.values[[1]]
    return(list(auc=auc,roc=this.roc))
  }
  else{
    return(list(auc=-1,roc=NULL))
  }
}

make_plot1 <- function(sample.df, sample.df.mean){
  
  #plot 1 : simulated trajectory 
  grp.id <- c(1,2,3,4)
  plot.data <- subset(sample.df, grp %in% grp.id)
  plot.data$grp <- as.factor(plot.data$grp)
  plot.data$id <- as.factor(plot.data$id)
  q <- ggplot() + geom_line(data=plot.data, aes(x=tvalue, y=ovalue, group=paste0(grp,'_',id), color=grp), alpha=0.5) + theme_bw()
  
  plot.data2 <- subset(sample.df.mean, grp %in% grp.id)
  plot.data2$grp <- as.factor(plot.data2$grp)
  q <- q + geom_line(data=plot.data2, aes(x=tvalue, y=mean, color=grp), size=2)
  q <- q + ylim(0,1) + xlab('timestamp') + ylab('values') +  theme(legend.position = "bottom")
  q <- q + scale_colour_manual(values = c("#ff5050","#cc66ff","#3399ff", "#009933"))
  q1 <- q
  plot(q1)
  
}

make_plot2 <- function(auc.df){
  
  #plot2 AUC score
  plot.data <- auc.df
  q <- ggplot(plot.data, aes(x=t, y=auc)) + geom_line(size=2)
  q <- q + theme_bw() + ylab('Area Under Curve') + xlab('timestamp') +  ylim(0.5,1)
  q2 <- q 
  plot(q2)
}



make_plot3 <- function(sample.df2){
  plot.data <- ddply(sample.df2, .(tvalue, label), summarize, 
                     mean=mean(ovalue),
                     sd=sd(ovalue),
                     nn=length(ovalue),
                     stderr=sd/sqrt(nn),
                     upperCI=mean+1.96*stderr,
                     lowerCI=max(mean-1.96*stderr,0))
  plot.data$label <- as.factor(plot.data$label)
  q <- ggplot(plot.data) 
  q <- q + geom_ribbon(aes(x=tvalue, ymin=lowerCI,ymax=upperCI, group=label), color='gray',alpha=0.3)
  q <- q + geom_line(aes(x=tvalue, y=mean, group=label, color=label), size=2)
  q <- q + theme_bw()+ theme(legend.position = "bottom")
  q <- q + scale_colour_manual(values = c("green","red")) + xlab('timestamp') + ylab('values') + ylim(0,1)
  plot(q)
  
}
