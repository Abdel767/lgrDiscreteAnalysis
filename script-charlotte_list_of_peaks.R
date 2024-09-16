
#Before running your peaks run the function below ("area") to activate it (just once per session)

  #This reads file in .csv format EXACTLY in the form you downloaded from the Los Gatos
	
#getwd()
ggh<- read.csv(file.choose(), header=F) #load .csv data, choose .txt file from the los gatos
head(ggh) #check what you loaded, it is displayed in the R console


	#Run this function once
	int <- function(x,y) #function for integration of the peak
	{ 
area <- 0
c <- length(x[x>0])
	for (i in 2:c) {
	if (y[i]-y[i-1]>=0 & x[i]>0)
area <- (x[i]-x[i-1])*y[i-1] + ((x[i]-x[i-1])*(y[i]-y[i-1]))/2+area
	if (y[i]-y[i-1]<0 & x[i]>0)
area <- (x[i]-x[i-1])*y[i] + ((x[i]-x[i-1])*(y[i-1]-y[i]))/2+area
}
if (y[c]>=y[1])
area <- area-y[1]*(x[c]-x[1])-(y[c]-y[1])*(x[c]-x[1])/2

if (y[c]<y[1])
area <- area-y[c]*(x[c]-x[1])-(y[1]-y[c])*(x[c]-x[1])/2

return(paste(area))
}
	
#Run next lines to transform the file loaded
CO2=as.numeric(as.character(ggh[,10]))
len <- length(na.omit(CO2))+2
CO2 <- rnorm(0)
CO2 <- as.numeric(as.character(ggh[3:len,10]))
all <- ggh[1:len,] #extract time data only
k <- as.character(all[,1]) #transform them to character
t <- unlist(strsplit(k, " "))
p <- rnorm(0)
l <- 32
o <- 1
for (i in 1:len) {
	p[o]=t[l]
	l=l+4
	o=o+1
}
j <- unlist(strsplit(p, ":"))
j <- as.numeric(j)
o=1
time=rnorm(0)
for (i in 1:(length(j)/3)) {
time[i]=(j[o]-j[1])*60+j[o+1]+j[o+2]/60
o=o+3 }
ifelse (length(time)==length(CO2),"can continue","something went wrong, try again")


CH4=as.numeric(as.character(ggh[,2]))
len <- length(na.omit(CH4))+2
CH4 <- rnorm(0)
CH4 <- as.numeric(as.character(ggh[3:len,2]))

CO2dry=as.numeric(as.character(ggh[,14]))
len <- length(na.omit(CH4))+2
CO2dry <- rnorm(0)
CO2dry <- as.numeric(as.character(ggh[3:len,14]))

CH4dry=as.numeric(as.character(ggh[,12]))
len <- length(na.omit(CH4))+2
CH4dry <- rnorm(0)
CH4dry <- as.numeric(as.character(ggh[3:len,12]))

##############for a list of peaks###########################################
tp<- read.table(file.choose(), header=T) #choose .txt file with the time of the peaks (3 columns, one column name of sample, another column "start", corresponds to start of peak, and a column end peak)
head(tp) #

a<-tp$start
b<-tp$end
par(mfrow=c(2,2))
result <- matrix(data=NA, nrow=length(a+1), ncol=5) 
colnames(result)<-c("name_sample", "areaCO2", "areaCO2dry", "areaCH4", "areaCH4dry")
tp$name<-as.character(tp$name)

for (s in 1:length(a)){ 
print(plot(time[(a[s]-2):(b[s]-2)],CO2dry [(a[s]-2):(b[s]-2)],type="o", xlab="time, min", ylab="CO2dry, ppm", main=tp$name[s]))
print(plot(time[(a[s]-2):(b[s]-2)],CH4dry [(a[s]-2):(b[s]-2)],type="o", xlab="time, min", ylab="CH4dry, ppm", main=tp$name[s]))
result[s,1]<-tp$name[s]
result[s,2]<-int(time[(a[s]-2):(b[s]-2)]*60*10, CO2[(a[s]-2):(b[s]-2)])
result[s,3]<-int(time[(a[s]-2):(b[s]-2)]*60*10, CO2dry[(a[s]-2):(b[s]-2)])
result[s,4]<-int(time[(a[s]-2):(b[s]-2)]*60*10, CH4[(a[s]-2):(b[s]-2)])
result[s,5]<-int(time[(a[s]-2):(b[s]-2)]*60*10, CH4dry[(a[s]-2):(b[s]-2)])
}

write.csv(result, file = "areas_peaks.csv")#creates a file with the name of the sample and the areas of the peaks

