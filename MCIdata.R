
#=MCI-BEGIN==============================================================================================
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
# Downloading MCI Data

DataSourceName <- "MCI"
SERVER <- "hilltopserver.horizons.govt.nz"
SERVER <- "hydro.marlborough.govt.nz"

Site <- "Manawatu at Hopelands"
Site <- "Kaituna River at Higgins Bridge"

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
