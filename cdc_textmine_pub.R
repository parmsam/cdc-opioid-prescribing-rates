#Updated on 2019-03-11
RequestID <- "DR3554"
FolderName <- paste0(RequestID, "Pull and Store CDC Public Prescribing Rates Data", sep = "")
ProgramName <- FolderName  # If the DR has just one program, then that ProgramNam is usually the same as FolderName. IF YOU CHANGE IT, make sure to start it with RequestID.
DataSource <- "CDC Opioid Rx Rates"
ContactName <- "Sam Parmar"
DRdir <- paste("", FolderName, "/", sep = "")

# Created by Sam Parmar on 2018-03-09
# Validated by Sam Parmar on 2018-03-09
# Updated on 2019-03-11
# Purpose: Text mining from CDC pages to pull opioid prescription rates data for past few years 2009-2017 for county and state levels
#   -This program pulls from CDC Opioid Rx Rates data and stores the data in the specified directory as a dataframe 
#   -then plots the data based on the specified state or county filter 
# Production schedule: (if program is to be run on a regular basis)
# Limitations and Warnings: 
#   -the all county dataset lapply runtime is about one to two minutes to process pull the county level data
# CDC Data Source Info: 
"a. Source for all prescribing data: IQVIA Xponent 2006–2017. IQVIA Xponent is based on a sample of approximately 
50,000 retail (non-hospital) pharmacies, which dispense nearly 90% of all retail prescriptions in the United States. 
For this database, a prescription is an initial or refill prescription dispensed at a retail pharmacy in the sample 
and paid for by commercial insurance, Medicaid, Medicare, or cash or its equivalent. This database does not include 
mail order pharmacy data.
 b. For the calculation of prescribing rates, numerators are the total number of opioid prescriptions dispensed in a 
given year, state, or county, as appropriate.  Annual resident population denominator estimates were obtained 
from the U.S. Census Bureau.
 c. Opioid prescriptions, including buprenorphine, codeine, fentanyl, hydrocodone, hydromorphone, methadone, 
morphine, oxycodone, oxymorphone, propoxyphene, tapentadol, and tramadol, were identified using the 
National Drug Code. Cough and cold formulations containing opioids and buprenorphine products typically 
used to treat opioid use disorder were not included. In addition, methadone dispensed through methadone maintenance 
treatment programs is not included in the IQVIA Xponent data.
 d. Table 2 ndisplays the percentage of counties in the United States that have opioid prescribing rates 
available for a given year. A lack of available data may indicate that the county had no retail pharmacies, 
the county had no retail pharmacies sampled, or the prescription volume was erroneously attributed to an adjacent, 
more populous county according to the sampling rules used."

# Program derived from "other_program_name.R"
# Program Flow Description (high level review of the steps of the program)
#  1) Define key parameters, etc.
#  2) Read and parse HTML file
#  3) Filter by State name and County name
#  4) Plot graph for County and/or State Prescription Rate Trends
#  X) Clean up. 

# Message string for clients to know who to contact about program output.
ContactMe <- paste(RequestID, ContactName, "EPI", Sys.Date(), "Data Source:", DataSource)

# Add the following to all plots created by ggplot2: +labs(caption = ContactMe)

# Leave mark at beginning of program log see when the run started. 
print("###############################################")
paste("Run started at", Sys.time())
print("###############################################")
#1) Define key constants, formats, etc. ; ----
library(rvest)
library(tidyverse)
library(reshape2)
library(stringr)
library(data.table)
library(rowr)
library(ggrepel)

#More information on CDC posted rates data: 
infoURL="https://www.cdc.gov/drugoverdose/maps/rxrate-maps.html"
#What State? Use full state name (example="Indiana")
StateName="Indiana"
#What county? Format as county comma then State 2 letter abbreviation here (example="Marion, IN")
#also fill out the County Only and State only fields
CountyOnly="Marion"
StateOnly="IN"
CountyName=paste(CountyOnly,StateOnly,sep=", ")

#Would you like to output a plot for the selected county(ChooseOut='county') 
#or for the selected state(ChooseOut='state')?
ChooseOut='county'
#What is your year range of interest?
year1=2006
year2=2017
#Would you like to save the ALL Data as a CSV? Declare Save=1 and declare destination path 
#(with forward slashes) in filePath
Save=1
filePath="/Users/Sam/Documents/Programming/R_Prog/cdc_textmine/fullDat/"
destPath<-paste(filePath, ChooseOut, '_OpioidRxData_',year1,"-",year2,'.csv', sep='')
#Would you like to save the State or County-specfici Data as a CSV? Declare Save2=1 and declare destination path 
#(with forward slashes) in dpath
Save2=1
dPath<-paste("/Users/Sam/Documents/Programming/R_Prog/cdc_textmine/",
             ChooseOut,year1,"-",year2,"_data",sep='')
#Would you like to save a graph with the State or County-specific data and declare the path in gPath? 
Save3=1
gPath<-paste("/Users/Sam/Documents/Programming/R_Prog/cdc_textmine/",
             ChooseOut,year1,"-",year2,"_graph",sep='')
if(ChooseOut=='state'){
  dPath <- paste(dPath,"_",StateOnly,sep="")
  gPath <- paste(gPath,"_",StateOnly,sep="") 
}
if(ChooseOut=='county'){
  dPath <- paste(dPath,"_",CountyOnly,"_",StateOnly,sep="")
  gPath <- paste(gPath,"_",CountyOnly,"_",StateOnly,sep="") 
}
#2) Read and parse HTML file ----

#from: http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
# webpage1 <- read_html("https://www.cdc.gov/drugoverdose/maps/rxcounty2017.html")
# tbls_ls1 <-webpage1 %>%
#   html_nodes("table") %>%
#   html_table(fill = TRUE, header=TRUE)
# tbls_ls1<-data.frame(tbls_ls1) 
# colnames(tbls_ls1)<-tbls_ls1[1,]
# tbls_ls1 <- tbls_ls1[-1,]

#created function that performs above actions
my_webPull <- function(url) {
  webpage <- read_html(url)
  tbls_ls2 <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE,header=TRUE) 
  tbls_ls2<-data.frame(tbls_ls2) 
  if (tbls_ls2[1,1]=="County" | tbls_ls2[1,1]=="State"){
    colnames(tbls_ls2)<-tbls_ls2[1,]
    tbls_ls2 <- tbls_ls2[-1,]
  }
  tbls_ls2 <- tbls_ls2%>% arrange(.[[2]],.[[1]]) 
  if (ncol(tbls_ls2)>3){
     tbls_ls2 <- tbls_ls2 %>% mutate(Year=colnames(tbls_ls2[4]))
   }  
  return(tbls_ls2)
}

#tested function and it works
 # t2<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxstate2016.html")
 # View(t2)
# t3<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxcounty2016.html")
# View(t3)
# t4<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxstate2017.html")
# View(t4)
# t5<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxcounty2017.html")
# View(t5)
 # t3<-my_webPull("https://www.cdc.gov/drugoverdose/maps/rxcounty2007.html")
 # View(t3)

# urls have commmon patterns .../rxstate2016, /rxstate2015, etc
# so get a table of the urls using a the base and generating the pattern for both county and state

state_urls<-c()
for (i in year1:year2){
  url_text<-paste("https://www.cdc.gov/drugoverdose/maps/rxstate", i,".html", sep="")
  state_urls<-append(state_urls, url_text)
}
state_urls<-data.frame(state_urls)
#make sure that the elements are characters using a paste since i guess it works
state_urls$state_urls <- paste0("", state_urls$state_urls, "")

#change the data frame to just a list using a for loop
for (state_url in state_urls){
}

county_urls<-c()
for (i in year1:year2){
  url_text<-paste("https://www.cdc.gov/drugoverdose/maps/rxcounty", i,".html", sep="")
  county_urls<-append(county_urls, url_text)
}
county_urls<-data.frame(county_urls)
#make sure that the elements are characters using a paste since it works
#as.character conversion might work instead too
county_urls$county_urls <- paste0("", county_urls$county_urls, "")

#change the data frame to just a list using a for loop
for (county_url in county_urls){
}

if (ChooseOut=='county'){
  #use lapply to pipe list of arguments into the function that was created in earlier steps
  All_CountyData_ls<-lapply(county_url, my_webPull)
  #missing number of counties on 2017 so using cbind.fill to successfully create data.frame
  All_CountyData<-do.call(function(...) data.frame(rowr::cbind.fill(...,fill=NA)), All_CountyData_ls)
}
if(ChooseOut=='state'){
  #use lapply to pipe list of arguments into the function that was created in earlier steps
  All_StateData<-lapply(state_url, my_webPull)
  All_StateData<-data.frame(All_StateData)
}
#All_CountyData %>% View()

#now have datasets for state data (All_StateData) OR county data (All_CountyData). 

#3) Save ALL state or county data as a data frame in specified directory ----
SaveDf<-function(Save,ChooseOut,destPath) {
  if (Save==1){
    if(ChooseOut=='county'){
      All_CountyData<- All_CountyData %>% mutate_all(funs(type.convert(as.character(replace(., .=='–', NA)))))
      write_excel_csv(All_CountyData,path=destPath,na = '') 
    }
    else if (ChooseOut=='state'){
      All_StateData<- All_StateData %>% mutate_all(funs(type.convert(as.character(replace(., .=='–', NA)))))
      write_csv(All_StateData,path=destPath,na = "NA")   
    }
  }
}

SaveDf(Save, ChooseOut, destPath)
#4)  Filter by State name and County name ----
if (ChooseOut=='state'){
  StateData<-All_StateData %>% filter(State==StateName)
  StateData <- melt(StateData, id=c("State"))
  StateData<- StateData %>% filter(!str_detect(variable, 'State|County|Abbr|ABBR')) %>% 
    mutate(variable=substr(variable, 2, 5))
}
if (ChooseOut=='county'){
  CountyData<-rbind(All_CountyData[1:5])
  n=5
  for (i in 1:ncol(All_CountyData)){
    if (i %% n==0 & i>(n)) {
      print(i)
      print(i-4)
      a<-All_CountyData[,(i-4):i]
      colnames(a)<-colnames(CountyData)
      print(a)
      CountyData<-rbind(CountyData,a) %>% filter(!is.na(.[1]))
    }}
  CountyData<-CountyData %>% rename("Rate"="X2006.Prescribing.Rate") %>%
    filter(str_detect(County,regex(CountyOnly,ignore_case=T)) & 
             str_detect(State,regex(StateOnly,ignore_case=T))) %>%
    mutate(Year=str_extract(Year,"\\d..."))
}
#5) Save County or State Specific data ----
if (Save2==1){
  if (ChooseOut=='state'){
    write_csv(StateData %>% rename(Year=variable, Rate=value),path=paste(dPath,".csv",sep="")) 
  }
  else if (ChooseOut=='county'){
    write_csv(CountyData,path=paste(dPath,".csv",sep="")) 
  }
}
#6) Use ggplot to plot graph for County or State Prescription Rate Trends ----
if (ChooseOut=='state'){
  
  StateData$variable <- factor(StateData$variable, levels =StateData$variable)
  StateData$value <- as.numeric(StateData$value)
  
  titleState <- paste("Estimated Rate of Opioid Prescriptions per 100 U.S. Residents by Year,", 
                      StateData[1,1], "(State)")
}
if (ChooseOut=='county'){
  CountyData$Year <- factor(CountyData$Year, levels =CountyData$Year)
  CountyData$Rate <- as.numeric(as.character(CountyData$Rate))
  titleCounty <- paste("Estimated Rate of Opioid Prescriptions per 100 U.S. Residents by Year,", 
                       CountyData[1,1], "(County, State)")
}
plotFunc<-function(pickGeo) {
  if (pickGeo=='state'){
    dataset<-StateData %>% mutate(value=as.numeric(value))
    gg_IN<-ggplot(data = dataset,aes(x=variable,y=value))+
      geom_point()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_y_continuous(breaks=seq(round(min(dataset$value))-15,round(max(dataset$value))+15,5))+
      geom_line(alpha=1,color='black', group=1) +
      labs(x = "Year",y="Estimated Rate", 
           title=titleState,
           subtitle = paste(RequestID,"; ", DataSource,sep=""),
           caption = infoURL) + ggrepel::geom_text_repel(aes(label=value),show.legend=F);
    gg_IN
  }
  else if (pickGeo=='county'){
    dataset<-CountyData %>% mutate(Rate=as.numeric(as.character(Rate)))
    gg_MC<-ggplot(data = dataset,aes(x=Year,y=Rate))+
      geom_point()+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      #scale_y_continuous(breaks=seq(round(min(dataset$Rate))-15,round(max(dataset$Rate))+15,5))+
      geom_line(alpha=1,color='black', group=1) +
      labs(x = "Year",y="Estimated Rate", 
           title=titleCounty,
           subtitle = paste(RequestID,"; ", DataSource, sep=""),
           caption = infoURL) + ggrepel::geom_text_repel(aes(label=Rate),show.legend=F)
    gg_MC
  }}
a<-plotFunc(ChooseOut);a

if (Save3==1) {
ggsave(a,filename=paste(gPath,".png",sep=""), width = 9, height = 4, dpi = 500, units = "in", device='png')
}

#  X) Clean up.
# Delete all objects that were created.
# rm(list=ls())

# Leave mark at end of program to see when the run ended.
print("###############################################")
paste("Run ended at", Sys.time())
print("###############################################")
