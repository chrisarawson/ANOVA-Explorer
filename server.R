library(shiny)

shinyServer(function(input, output) {
  
  values <- reactiveValues(i = 1212)
  
  observe({
    input$reRandom
    
    # We need to use isolate because otherwise whenever values$i changes
    # the observer will be run again and we'll get stuck in an infinite 
    # loop
    isolate({
      values$i <- values$i + 1
    })
  })
    
  output$DistSSwg<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    xPts<-seq(-200,500, length.out=10000)
    yPts1<-dnorm(x=xPts,mean=mean(group1),sd=sd(group1))
    yPts2<-dnorm(x=xPts,mean=mean(group2),sd=sd(group2))
    yPts3<-dnorm(x=xPts,mean=mean(group3),sd=sd(group3))
    gpsDF<-data.frame(rep(xPts,3),c(yPts1,yPts2,yPts3))
    colnames(gpsDF)<-c("X","Y")
    reqX<-gpsDF[which(gpsDF$Y>0.0001),"X"]
    group1dist<-data.frame(xPts,yPts1)
    group2dist<-data.frame(xPts,yPts2)
    group3dist<-data.frame(xPts,yPts3)
    yMax<-max(c(yPts1,yPts2,yPts3))
    gp1Jitter<-jitter(rep(max(yPts1)/2,length(group1)),25)
    gp2Jitter<-jitter(rep(max(yPts2)/2,length(group2)),25)
    gp3Jitter<-jitter(rep(max(yPts3)/2,length(group3)),25)
    plot(xPts,yPts1,type="n",ylim=c(0,yMax),xlab="X",ylab="Y Value",xlim=c(min(reqX),max(reqX)))
    polygon(xPts,yPts1,col=rgb(0,0,1,0.5),border=NA)
    polygon(xPts,yPts2,col=rgb(1,0,0,0.5),border=NA)
    polygon(xPts,yPts3,col=rgb(0,1,0,0.5),border=NA)
    lines(x=rep(mean(group1),2),y=c(0,max(yPts1)),col="blue")
    lines(x=rep(mean(group2),2),y=c(0,max(yPts2)),col="red")
    lines(x=rep(mean(group3),2),y=c(0,max(yPts3)),col="green")
    points(x=group1,y=gp1Jitter,col=rgb(0,0,1,0.5),pch=19)
    points(x=group2,y=gp2Jitter,col=rgb(1,0,0,0.5),pch=19)
    points(x=group3,y=gp3Jitter,col=rgb(0,1,0,0.5),pch=19)
    
    for(i in seq(1,length(group1))) {
      lines(x=c(group1[i],mean(group1)),y=rep(gp1Jitter[i],2))
    }
    for(j in seq(1,length(group2))) {
      lines(x=c(group2[j],mean(group2)),y=rep(gp2Jitter[j],2))
    }
    for(k in seq(1,length(group3))) {
      lines(x=c(group3[k],mean(group3)),y=rep(gp3Jitter[k],2))
    }
    
    reg.lm<-lm(yData~Group,data=Data)
    reg.anova<-anova(reg.lm)
    
    legend("topleft", legend = c("WITHIN GROUPS",
                                 paste("SS =",round(reg.anova$Sum[2],1)),
                                 paste("DF =",reg.anova$Df[2])),bty="n")
    
  })
  
  output$DistSSbg<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    xPts<-seq(-200,500, length.out=10000)
    yPts1<-dnorm(x=xPts,mean=mean(group1),sd=sd(group1))
    yPts2<-dnorm(x=xPts,mean=mean(group2),sd=sd(group2))
    yPts3<-dnorm(x=xPts,mean=mean(group3),sd=sd(group3))
    gpsDF<-data.frame(rep(xPts,3),c(yPts1,yPts2,yPts3))
    colnames(gpsDF)<-c("X","Y")
    reqX<-gpsDF[which(gpsDF$Y>0.0001),"X"]
    group1dist<-data.frame(xPts,yPts1)
    group2dist<-data.frame(xPts,yPts2)
    group3dist<-data.frame(xPts,yPts3)
    yMax<-max(c(yPts1,yPts2,yPts3))
    plot(xPts,yPts1,type="n",ylim=c(0,yMax),xlab="X",ylab="Y Value",xlim=c(min(reqX),max(reqX)))
    polygon(xPts,yPts1,col=rgb(0,0,1,0.5),border=NA)
    polygon(xPts,yPts2,col=rgb(1,0,0,0.5),border=NA)
    polygon(xPts,yPts3,col=rgb(0,1,0,0.5),border=NA)
    abline(v=mean(Data$yData),col="red")
    lines(x=rep(mean(group1),2),y=c(0,max(yPts1)),col="blue")
    lines(x=rep(mean(group2),2),y=c(0,max(yPts2)),col="red")
    lines(x=rep(mean(group3),2),y=c(0,max(yPts3)),col="green")
    lines(x=c(mean(group1),mean(Data$yData)),y=rep(max(yPts1),2))
    lines(x=c(mean(group2),mean(Data$yData)),y=rep(max(yPts2),2))
    lines(x=c(mean(group3),mean(Data$yData)),y=rep(max(yPts3),2))
    
    reg.lm<-lm(yData~Group,data=Data)
    reg.anova<-anova(reg.lm)
    
    legend("topleft", legend = c("BETWEEN GROUPS",
                                 paste("SS =",round(reg.anova$Sum[1],1)),
                                 paste("DF =",reg.anova$Df[1])),bty="n")
    
  })
  
  output$DotSSwg<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    groupDummy<-c(rep(1, length(group1)),rep(2,length(group2)),rep(3,length(group3)))
    plot(groupDummy,Data$yData,type="n",xlim=c(0,4),xaxt="n",xlab=NA,ylab="Y Value")
    axis(1,at=c(1,2,3),labels=levels(Data$Group))
    gp1Jitter2<-jitter(rep(1,length(group1)),15)
    gp2Jitter2<-jitter(rep(2,length(group2)),15/2)
    gp3Jitter2<-jitter(rep(3,length(group3)),15/3)
    points(gp1Jitter2,Data[which(Group=="A"),"yData"],pch=19,col=rgb(0,0,1,0.5))
    points(gp2Jitter2,Data[which(Group=="B"),"yData"],pch=19,col=rgb(1,0,0,0.5))
    points(gp3Jitter2,Data[which(Group=="C"),"yData"],pch=19,col=rgb(0,1,0,0.5))
    abline(h=mean(Data$yData))
    abline(h=mean(group1),col="blue",lty="dashed")
    abline(h=mean(group2),col="red",lty="dashed")
    abline(h=mean(group3),col="green",lty="dashed")
    
    for(i in seq(1,length(group1))) {
      lines(x=rep(gp1Jitter2[i],2),y=c(group1[i],mean(group1)))
    }
    for(j in seq(1,length(group2))) {
      lines(x=rep(gp2Jitter2[j],2),y=c(group2[j],mean(group2)))
    }
    for(k in seq(1,length(group3))) {
      lines(x=rep(gp3Jitter2[k],2),y=c(group3[k],mean(group3)))
    }
    
    reg.lm<-lm(yData~Group,data=Data)
    reg.anova<-anova(reg.lm)
    
    legend("topleft", legend = c("WITHIN GROUPS",
                                 paste("SS =",round(reg.anova$Sum[2],1)),
                                 paste("DF =",reg.anova$Df[2])),bty="n")
    
  })
  
  output$DotSSbg<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    groupDummy<-c(rep(1, length(group1)),rep(2,length(group2)),rep(3,length(group3)))
    plot(groupDummy,Data$yData,type="n",xlim=c(0.5,3.5),xaxt="n",xlab=NA,ylab="Y Value")
    axis(1,at=c(1,2,3),labels=levels(Data$Group))
    gp1Jitter2<-jitter(rep(1,length(group1)),15)
    gp2Jitter2<-jitter(rep(2,length(group2)),15/2)
    gp3Jitter2<-jitter(rep(3,length(group3)),15/3)
    points(gp1Jitter2,Data[which(Group=="A"),"yData"],pch=19,col=rgb(0,0,1,0.5))
    points(gp2Jitter2,Data[which(Group=="B"),"yData"],pch=19,col=rgb(1,0,0,0.5))
    points(gp3Jitter2,Data[which(Group=="C"),"yData"],pch=19,col=rgb(0,1,0,0.5))
    abline(h=mean(Data$yData))
    abline(h=mean(group1),col="blue",lty="dashed")
    abline(h=mean(group2),col="red",lty="dashed")
    abline(h=mean(group3),col="green",lty="dashed")
    lines(x=c(1,1),y=c(mean(group1),mean(yData)))
    lines(x=c(2,2),y=c(mean(group2),mean(yData)))
    lines(x=c(3,3),y=c(mean(group3),mean(yData)))
    
    reg.lm<-lm(yData~Group,data=Data)
    reg.anova<-anova(reg.lm)
    
    legend("topleft", legend = c("BETWEEN GROUPS",
                                 paste("SS =",round(reg.anova$Sum[1],1)),
                                 paste("DF =",reg.anova$Df[1])),bty="n")
    
  })
  
  output$aovSummary = renderPrint(function() {
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    reg.lm<-lm(yData~Group,data=Data)
    reg.anova<-anova(reg.lm)
    if(input$showAnova == 1) {
      print(reg.anova,signif.stars=FALSE)}
  })
  
  output$normPlot<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)

    reg.lm<-lm(yData~Group,data=Data)
    
    if(input$showQQnorm ==1) {
    qqnorm(reg.lm$res,main=NULL)
    qqline(reg.lm$res,col="red")}
  })
  
  output$resFitPlot<-renderPlot({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    reg.lm<-lm(yData~Group,data=Data)
    
    if(input$showFitRes ==1) {
    
    par(oma=c(0,0,0,0))
    plot(reg.lm$fit,reg.lm$res,col="blue",xlab="Fitted",ylab="Residuals")}
    
  })
  
  output$Shapiro<-renderText(function() {
    set.seed(values$i)
  group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
  group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
  group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
  Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
  yData<-c(group1,group2,group3)
  Data<-data.frame(yData,Group)
  
  reg.lm<-lm(yData~Group,data=Data)
  sh.test<-shapiro.test(reg.lm$res)
  
  if(input$showQQnorm ==1) {
  paste("Shapiro Test for Normality: p =", round(sh.test$p.value,5))}
  })
  
  output$Bartletts<-renderUI({
    set.seed(values$i)
    group1<-rnorm(input$sampSize1, input$mean1, input$sd1)
    group2<-rnorm(input$sampSize2, input$mean2, input$sd2)
    group3<-rnorm(input$sampSize3, input$mean3, input$sd3)
    Group<-c(rep("A", length(group1)),rep("B",length(group2)),rep("C",length(group3)))
    yData<-c(group1,group2,group3)
    Data<-data.frame(yData,Group)
    
    library(car)
    b.test<-bartlett.test(yData~Group,data=Data)
    l.test<-leveneTest(yData~Group,data=Data)
    text1<-paste("Bartlett's test for equal variance (normality assumed): p =", round(b.test$p.value,3))
    text2<-paste("Levene's test for equal variance (normality not assumed): p =", round(l.test$P[1],3))
   
    if(input$showFitRes ==1) {
    HTML(paste(text1,text2,sep = '<br/>'))}
  })
  
  output$mySeed<-renderText({
    print(values$i)
  })
  
})