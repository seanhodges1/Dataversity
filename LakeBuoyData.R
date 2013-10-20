#LAKE-BEGIN===================================================================================================

# HASH SYMBOLS = COMMENTS

#===================================================================================================

# Add required libraries

# These libraries will need to be installed within R first, otherwise
# the script will error and stop. The first couple of lines do the install
# in a clumsy way if they are not detected.

necessary = c('XML', 'ggplot2')

if(!all(necessary %in% installed.packages()[, 'Package']))
  install.packages(necessary, dep = T)

library(XML)
library(ggplot2)

#===================================================================================================
# Set Enquiry Time Range for data off the lake buoy

timeRange <- "P7D"

#===================================================================================================
# Downloading Atmospheric Pressure Data

DataSourceName <- "Atmospheric%20Pressure"
url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=SOS&request=GetObservation&featureOfInterest=Lake%20Horowhenua%20at%20Buoy&observedProperty=",DataSourceName,"&temporalFilter=om:phenomenom,",timeRange,sep="")
cat(url,'\n')

getData.xml <- xmlInternalTreeParse(url)

DateTime <- sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:time"), xmlValue)
value <- as.numeric(sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:value"), xmlValue))

tmp<-data.frame(DateTime,value,stringsAsFactors=FALSE)

df_atm<- tmp
colnames(df_atm) <- c("dateTime","val")
df_atm$dateTime <- as.POSIXct(strptime(df_atm[,1],format="%Y-%m-%dT%H:%M:%S"))

#-------------------------------------------------------------------------------------
# Create a simple timeseries plot of atmospheric pressure for given time period
h <- ggplot(df_atm, aes(dateTime,val))
h + geom_line() + geom_point()

#===================================================================================================
# Downloading Temperature Profile Data
# having to fake the datasource name with the datasourceIDs because

# haven't updated the sql database in the dmz yet.

#DataSourceID <- c("%3CID10647%3E","%3CID10648%3E","%3CID10649%3E","%3CID10650%3E")

DataSourceName <- c("Water%20Temperature%20(-500%20mm)","Water%20Temperature%20(-1000%20mm)","Water%20Temperature%20(-1500%20mm)","Water%20Temperature%20(-2000%20mm)")

Depth <- c(-500,-1000,-1500,-2000)

# for each datasource ...

for(i in 1:length(x=DataSourceName)){
  
  url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=SOS&request=GetObservation&featureOfInterest=Lake%20Horowhenua%20at%20Buoy&observedProperty=",DataSourceName[i],"&temporalFilter=om:phenomenom,",timeRange,sep="")
  cat(url,'\n')
  
  # ...load the xml...
  
  getData.xml <- xmlInternalTreeParse(url)
  
  # ...parse the xml...
  
  DateTime <- sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:time"), xmlValue)
  
  value <- as.numeric(sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:value"), xmlValue))
  
  depth <- rep(Depth[i],length(value))
  
  cat(i,'\n')
  
  #... Load data to a data frame
  
  if(i==1){
    
    tmp<-data.frame(DateTime,depth,value,stringsAsFactors=FALSE)
    
  } else{
    
    tmp1<-data.frame(DateTime,depth,value,stringsAsFactors=FALSE)
    
    tmp <- rbind(tmp,tmp1)
    
  }
  
}



# Take a copy of the dataframe as a working copy

df<- tmp



# Rename columns vars

colnames(df) <- c("dateTime","lakedepth","val")



# Create real dates

df$dateTime <- as.POSIXct(strptime(df[,1],format="%Y-%m-%dT%H:%M:%S"))



#===================================================================================================

# CREATE SOME MORE NICE PLOTS



# 1. Lake Profile temperature time series - default colours and labels

g <- ggplot(df, aes(dateTime,lakedepth,fill=val))

g + geom_tile()





# 2. Lake Profile temperature time series - labels and title

g + geom_tile() + xlab("Time") +
  ylab("Lake depth (mm)") +
  labs(title = "Water temperature profile")



# 3. Lake Profile temperature time series - labels, title and greyscale

g + geom_tile() + xlab("Time") +
  ylab("Lake depth (mm)") +
  labs(title = "Water temperature profile") +
  scale_fill_gradient(limits=c(min(df$val),max(df$val)),low="black",high="white")


# 4. Typical timeseries plot for the four datasources

# just need to treat lake depth as a category, rather than as a numeric, first

df$lakedepth <- as.factor(df$lakedepth)

k <- ggplot(df, aes(dateTime,val, group=lakedepth, colour=lakedepth))

k + annotate("text", x=as.POSIXct(max(df$dateTime)) ,y=min(df$val) ,label = "PROOF ONLY",
             hjust=1.1, vjust=-1.1, col="white", cex=12,
             fontface = "bold", alpha = 0.8)  + geom_line()

#LAKE-END==============================================================================================

#MCI-BEGIN==============================================================================================
# Downloading Atmospheric Pressure Data

DataSourceName <- "MCI"
SERVER <- "hilltopserver.horizons.govt.nz"
#SERVER <- "hydro.marlborough.govt.nz"

Site <- "Manawatu at Hopelands"
#Site <- "Kaituna River at Higgins Bridge"

timeRange <- "P5Y"
url <- paste("http://",SERVER,"/macro.hts?service=SOS&request=GetObservation&featureOfInterest=",Site,"&observedProperty=",DataSourceName,"&temporalFilter=om:phenomenom,",timeRange,sep="")
cat(url,'\n')

getData.xml <- xmlInternalTreeParse(url)

DateTime <- sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:time"), xmlValue)
value <- as.numeric(sapply(getNodeSet(doc=getData.xml, path="//wml2:point/wml2:MeasurementTVP/wml2:value"), xmlValue))

tmp<-data.frame(DateTime,value,stringsAsFactors=FALSE)

df_atm<- tmp
colnames(df_atm) <- c("dateTime","val")
df_atm$dateTime <- as.POSIXct(strptime(df_atm[,1],format="%Y-%m-%dT%H:%M:%S"))

#-------------------------------------------------------------------------------------
# Create a simple timeseries plot of atmospheric pressure for given time period
h <- ggplot(df_atm, aes(dateTime,val))
h + geom_line() + geom_point()

h + geom_bar(stat="identity")
summary(df_atm)
#MCI-END==============================================================================================
