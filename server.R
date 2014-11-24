library(shiny)
library(data.table)
library(googleVis)
library(ggplot2)
library(ggmap)
library(RCurl)
suppressPackageStartupMessages(library(googleVis));

########
load(file = "sparcs.rda");
data_summary <- function(x) {
  mu <- median(x)
  sigma1 <- mu-sd(x)
  sigma2 <- mu+sd(x)
  return(c(y=mu,ymin=sigma1,ymax=sigma2))
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))};
printMoney <- function(x){
  format(x, digits=10, nsmall=2, decimal.mark=".", big.mark=",")};
link<- function(var1) 
{ temp= paste("<a target=”_blank” href=”http://www.google.com/search?q=",
              paste(gsub("\\ ","+",gsub(",","%2C",var1)),"%2C+NY", sep=""), sep="");
  temp= paste(paste(paste(temp,"”>", sep=""),var1,sep=""), "</a>",sep="");
  return(temp)
}


paramterize<- function(Appen, LOSWeight, CostWeight, NumberWeight){
  hosp<- as.matrix(unique(Appen$Facility.Name)); 
  
  LOS<- as.data.frame(matrix(0,length(hosp),1));
  partialData<- cbind(hosp,LOS);
  partialData<- as.data.frame(partialData);
  
  Cost<- as.data.frame(matrix(0,length(hosp),1));
  partialData<- cbind(partialData,Cost);
  Number<- as.data.frame(matrix(0,length(hosp),1));
  partialData<- cbind(partialData,Number);
  Total<- as.data.frame(matrix(0,length(hosp),1))
  partialData<- cbind(partialData,Total);
  
  for(i in 1:length(hosp))  
  {
    dat<- Appen[which(Appen$Facility.Name== hosp[i]),];
    partialData[i,2] = round(mean(as.numeric(as.character(dat$Length.of.Stay))),0); 
    #print(hosp[i]);
    #print(dat[,'Length.of.Stay']); 
    partialData[i,3] = round(mean(as.numeric(as.character(dat$Total.Charges))),2);
    partialData[i,4] = nrow(dat);
  }
  partialData<- as.data.frame(partialData);
  
  temp<- cbind(LOSWeight*rank(-as.numeric(partialData[,2])),
               CostWeight*rank(-as.numeric(partialData[,3])),
               NumberWeight*rank(as.numeric(partialData[,4])));
  colnames(partialData)<- c('Hospital','LOS', 'Cost', 'Number', 'Total');
  
  for(i in 1:length(hosp))
  { partialData[i,'Total'] <- sum(temp[i,]);}
  partialData[,'Total'] <- round(range01(partialData[,'Total'])*100,0);
  return(partialData);
}
addDistance<- function(partialData, LOSWeight, CostWeight, NumberWeight,DistanceWeight, address){
  hosp<- as.matrix(partialData$Hospital);
  
  Distance<- as.data.frame(matrix(0,length(hosp),1)); colnames(Distance)<- 'Distance';
  partialData<- cbind(partialData,Distance);   partialData<- as.data.frame(partialData);
  colnames(partialData)<- c('Hospital','LOS', 'Cost', 'Number', 'Total','Distance' );  
  
  for(i in 1:length(hosp))  
  {
    partialData[i,'Distance'] = round(mapdist(paste(hosp[i], ", NY", sep = ""),
                                              address, mode = 'driving')$miles,2);
  }
  
  temp<- cbind(LOSWeight*rank(-as.numeric(partialData[,'LOS'])),
               CostWeight*rank(-as.numeric(partialData[,'Cost'])),
               NumberWeight*rank(as.numeric(partialData[,'Number'])),
               DistanceWeight*rank(-as.numeric(partialData[,'Distance'])))
  
  for(i in 1:length(hosp))
  { partialData[i,'Total'] <- sum(temp[i,]);}
  partialData[,'Total'] <- round(range01(partialData[,'Total'])*100,0);
  return(partialData);
}

##For Cost
addLOScolor<- function(Appen){
  hosp<- unique(Appen$Facility.Name)
  LOS<- matrix(mean(as.numeric(as.character(Appen$Length.of.Stay))),nrow(Appen),1)
  colnames(LOS)<- "Length.of.Stay"
  Appen<- cbind(Appen, LOS)
  print(3);
  for (i in 1:length(hosp))
  {    
    idx<- which(Appen$Facility.Name== hosp[i]);
    dat<- Appen[idx,];
    Appen[idx,]$Length.of.Stay = rep(mean(as.numeric(as.character(dat$Length.of.Stay))), length(idx));
  }  
  return(Appen);
}
options(scipen=5)


shinyServer(function(input, output) {  
 
  sparcs$Facility.Name<- paste(sparcs$Facility.Name, sparcs$Hospital.County, sep=", ") 
  #sparcs$Facility.Name<- paste(paste(sparcs$Facility.Name, sparcs$Hospital.County, sep=", "),sparcs$Hospital.Service.Area, sep=", ") 
  sparcs<- sparcs[, c("Facility.Name","Length.of.Stay","CCS.Diagnosis.Description","Total.Charges")] 
  sparcs$Total.Charges<- as.numeric(as.character((substring(sparcs$Total.Charges, 2))));
  sparcs$Length.of.Stay<- as.numeric(as.character((sparcs$Length.of.Stay)));
  load(file = "defaultCol.rda");

  compute <- reactive({
    if(as.character(input$variable)=="Search..."){
    #load(file = "defaultCol.rda"); partialData= defaultCol;
    #partialData[,"Hospital"]<- link(partialData[,"Hospital"]);
    partialData}
    else{
#     if(as.character(input$county)!="Search...")
#     { sparcs=sparcs[which(sparcs$Hospital.County== as.character(input$county)),];}  
      
    address<- as.character(input$caption);
    totalW<- c(input$LOSWeight, input$CostWeight, input$NumberWeight, input$DistanceWeight);
    LOSWeight= input$LOSWeight/sum(totalW); CostWeight= input$CostWeight/sum(totalW);
    NumberWeight= input$NumberWeight/sum(totalW); DistanceWeight= input$DistanceWeight/sum(totalW);
    
    partialData<- sparcs[which(sparcs$CCS.Diagnosis.Description== as.character(input$variable)),];
    partialData<- paramterize(partialData, LOSWeight, CostWeight, NumberWeight);
    partialData<- partialData[sort(partialData$Total, decreasing=TRUE, index.return=TRUE)$ix,];
    if(nrow(partialData) > (input$DistanceWeight)*10){numberOfHospitals<- (input$DistanceWeight)*10} 
    else {numberOfHospitals<-nrow(partialData)};
    
    partialData<- partialData[1:numberOfHospitals,];
    partialData<- addDistance(partialData, LOSWeight, CostWeight, NumberWeight,DistanceWeight, address);
    partialData<- partialData[,c("Hospital","LOS","Cost","Number","Distance","Total")];   
    #partialData[,"Hospital"]<- link(partialData[,"Hospital"]);    
    partialData
    }
  })  
  
  
  p2 <- function(dat){
    idx2<- which(dat$Total.Charges >= (median(dat$Total.Charges) + 3*sd(dat$Total.Charges)));    
    dat <- dat[-idx2,];
    
    dat<- addLOScolor(dat); 
    print(5);
    dat<- dat[rev(rownames(dat)),];
    dat$Facility.Name <- factor(dat$Facility.Name, levels=unique(as.character(dat$Facility.Name)) );

    return( ggplot(dat, aes(Facility.Name, Total.Charges)) + 
              geom_violin(trim = TRUE,scale = "width", aes(fill = Length.of.Stay)) +
              coord_flip()+ theme(legend.position = "bottom") +
              labs( y ="Total Charges ($)",  x = "Top Hospitals")+
              stat_summary(fun.data=data_summary) )
  }
  
  output$table <- renderDataTable({
    partialData<- compute(); #save(partialData, file = "defaultCol.rda");
    
      partialData[,'Cost']<-  printMoney(partialData[,'Cost']);
      data<- partialData[sort(partialData$Total, decreasing=TRUE, index.return=TRUE)$ix,];
       data<- cbind(1:nrow(data), data); colnames(data)<- c('Rank','Hospital','Length of Stay \n(days)',
                                            'Cost ($)','Number of \ncases seen','Distance \n(miles)','Total Score')
     data
    }, 
  options = list(bFilter=0, bSort=1, bProcessing=0, bPaginate=1, bInfo=0, display=0, iDisplayLength = 10,
  bAutoWidth=0, aoColumnDefs = list(list(sWidth="200px", aTargets=c(list(1))))))
  
  output$graphNumber <- renderGvis({   
    partialData<- compute();
    data<- partialData[sort(partialData$Total, decreasing=TRUE, index.return=TRUE)$ix,];
    data<- data[1:10,]; 
    colnames(data)<- c('Hospital','Length of Stay',
                       'Cost','Number of cases','Distance','Total Score')
    ranks<-paste(1:10,". ", sep="");
    data$Hospital<- paste(ranks, data$Hospital, sep = "");
    #print(data$Hospital)
    gvisBarChart(as.data.frame(data), xvar='Hospital', yvar='Number of cases',
                    options=list(isStacked=FALSE, width=850,height=500))
    
  })

  output$graphCost <- renderPlot({
    
    
    partialData<- compute();  
    data<- partialData[sort(partialData$Total, decreasing=TRUE, index.return=TRUE)$ix,];  
    data<- data[1:10,]; 
    colnames(data)<- c('Hospital','Length of Stay',
                     'Cost','Number of cases','Distance','Total Score');
    hospnames<-data[,'Hospital'];     
    idx<- list();
    for(i in 1:length(hospnames)){
#      idx[[i]]<- which(as.character(sparcs$Facility.Name)== strsplit(as.character(hospnames[i]), ",")[[1]][1] )
      idx[[i]]<- which(as.character(sparcs$Facility.Name)== as.character(hospnames[i]) )      
    }

    idx<- unlist(idx);   
    partialData<- sparcs[idx,];
    print(p2(partialData) );  
})

  output$mapped <- renderGvis({   
    partialData<- compute();
    data<- partialData[sort(partialData$Total, decreasing=TRUE, index.return=TRUE)$ix,];
    data<- data[1:10,]; 
    colnames(data)<- c('Hospital','Length of Stay',
                        'Cost','Number of cases','Distance','Total Score')   

    data$Hospital<- paste(unique(data$Hospital), ", New York, NY", sep = "");
    colnames(data)<- c('Hospital','Length of Stay',
                       'Cost','Number of cases','Distance','Total Score')
    ranks<-paste(1:10,". ", sep="");
    data$Hospital<- paste(ranks, data$Hospital, sep = "");
    #print(data$Hospital)
    gvisMap(data, "Hospital" , c("Hospital"), 
                 options=list(showTip=TRUE, 
                              showLine=TRUE, 
                              enableScrollWheel=TRUE,
                              mapType='normal', 
                              useMapTypeControl=TRUE))
    
  })
  
})













