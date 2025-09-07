##SHP
setwd("/Users/thiemo/Dropbox/Research/Benefit Sanctions/")
setwd("/Users/eleala/Dropbox/Benefit Sanctions/Raw Data/")


readFiles<-function(folder, ftype = "csv", collate = "rbind", Encoding = "latin1", 
    fname = TRUE) 
{

    ftype=tolower(ftype)
    ffs <- list.files(folder)
    ffs <- grep(paste(ftype, "$", sep = ""), ffs, value = TRUE,ignore.case=TRUE)
    if (ftype == "dta") {
        library(haven)
        DAT <- lapply(ffs, function(x) data.table(read_dta(file = paste(folder, 
            x, sep = ""))))
    }
    else if (ftype == "csv") {
        if (fname == TRUE) {
            DAT <- lapply(ffs, function(x) data.table(data.frame(fname = x, 
                read.csv(file = paste(folder, x, sep = ""), fileEncoding = Encoding))))
        }
        else {
            DAT <- lapply(ffs, function(x) data.table(data.frame(read.csv(file = paste(folder, 
                x, sep = ""), fileEncoding = Encoding))))
        }
    }
    if (collate == "rbind") {
        DAT <- rbindlist(DAT, fill = TRUE)
    }
    DAT
}

library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){

  distance = levenshteinSim(string, stringVector);
  
  data.table(stringVector[distance == max(distance)],max(distance))	  
}
library(zoo)
library(lubridate)
library(rgeos)

options(stringsAsFactors=FALSE)

WARD17.shp<-readOGR(dsn="shapefiles/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain", layer="Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain")
WARD17<-data.table(read.dbf(file="shapefiles/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain/Wards_December_2017_Generalised_Clipped_Boundaries_in_Great_Britain.dbf"))
LOOKUP<-data.table(read.csv(file="shapefiles/Ward_to_Local_Authority_District_December_2017_Lookup_in_the_United_Kingdom.csv"))
WARD17 <-join(WARD17, LOOKUP[, list(wd17cd=WD17CD, LAD17CD, LAD17NM)])

EUREF<-data.table(read.csv(file="Ward EURef/EU-referendum-result-data.csv"))

setnames(EUREF, "Area_Code","LAD17CD")
EUREF[, Area := NULL]
WARD17 <-join(WARD17,EUREF)

REF<-data.table(read.csv(file="Ward EURef/ward-results.csv"))
REF <-REF[WardCode!=""]

WARD17<-join(WARD17, REF[, list(wd17cd=WardCode, WardCode,  Ward_Remain=Remain, Ward_Leave=Leave, Ward_RemainPct=Remain., Ward_LeavePct=Leave.)])
WARD17[, WardCode :=NULL]



#WARD LEVEL CENSUS STATS

#SHP11 <- readOGR(dsn="/Users/thiemo/Dropbox/Research/Austerity and Brexit/NEW DATA REVISION/Ward Level Local Election Results/shp/infuse_ward_lyr_2011_clipped", layer="infuse_ward_lyr_2011_clipped")
SHP11 <- readOGR(dsn="Ward Level Local Election Results/shp/infuse_ward_lyr_2011_clipped", layer="infuse_ward_lyr_2011_clipped")

CENT11<-lapply(1:length(SHP11), function(x) data.table(wd11cd = SHP11[x,]@data$geo_code , gCentroid(SHP11[x,])@coords))
CENT11 <-rbindlist(CENT11)
CENT11.ov<-over(SpatialPoints(CENT11[, list(x, y)], proj4string=CRS(proj4string(WARD17.shp))), WARD17.shp)
CENT11<-cbind(CENT11,CENT11.ov)
MAPPER<-CENT11

ffs<-list.files("Ward Characteristics/Census 2011")

LABEL<-NULL
for(ff in ffs) {

FILE<-readFiles(paste("Ward Characteristics/Census 2011/",ff,"/",sep=""), collate="list", fname=FALSE)

FILE[[2]][, label := paste("label variable ", ColumnVariableCode, " \"",ff, " ", ColumnVariableMeasurementUnit, " ", ColumnVariableDescription,"\"",sep="" )]
LABEL<-c(LABEL, FILE[[2]]$label)

FILE[[1]]<-join(FILE[[1]], MAPPER[, list(GeographyCode=wd11cd, wd17cd)])[!is.na(wd17cd)]
FILE[[1]]<- data.table(data.frame(FILE[[1]][,c(1,ncol(FILE[[1]])),with=F], apply(FILE[[1]][, 2:(ncol(FILE[[1]])-1),with=F], 2, function(x) as.numeric(x))))

WARD17 <- join(WARD17, DTUniqueBy(FILE[[1]],"wd17cd"))

}

writeLines(LABEL, path="do files/label.ward.census.var.do")

MSOA<-readOGR(dsn="shapefiles/Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales/",layer="Middle_Layer_Super_Output_Areas_December_2011_Full_Clipped_Boundaries_in_England_and_Wales") 
MSOA.data<-data.table(MSOA@data)


WARD17.CENT<-mclapply(1:length(WARD17.shp), function(x) data.table(wd17cd = WARD17.shp[x,]@data$wd17cd , gCentroid(WARD17.shp[x,])@coords), mc.cores=4)
WARD17.CENT <-rbindlist(WARD17.CENT)
WARD17.CENT.ov<-over(SpatialPoints(WARD17.CENT[, list(x, y)], proj4string=CRS(proj4string(MSOA))), MSOA)
WARD17.CENT<-cbind(WARD17.CENT,WARD17.CENT.ov)


#MSOA DATA WITH LESS MISSINGS

OUT<-NULL
i =1
for(type in c("WP","JSA","ESA","UC","BTAX","PIP")) {


ff<-grep(type, list.files("SANCTIONS"), value=TRUE)

JSA<-data.table(read.csv(file=paste("SANCTIONS/",ff,sep="")))
JSA[OA=="", OA:=NA ]
JSA[, OA := na.locf(OA)]
JSA.LONG<-melt(JSA, id.vars = c("OA","Type") )
JSA.LONG[, value := as.numeric(value)]

setnames(JSA.LONG, "variable", "date")
if(type %in% c("WP","UC")) {
JSA.LONG[, date := dmy(paste("01 ",gsub("\\."," ",date),sep=""))]
JSA.LONG[, year := year(date)]
JSA.LONG[, month := year(date)]
JSA.LONG[, quarter := quarter(date)]
}  else {
JSA.LONG[, year := as.numeric(substr(date,2,5))]
JSA.LONG[, month := as.numeric(substr(date,6,7))]
JSA.LONG[, timevec := dmy(paste("01",month, year,sep="-"))]
JSA.LONG[, quarter := quarter(timevec)]
JSA.LONG[, timevec := NULL]
}
JSA.LONG<-JSA.LONG[!is.na(year)]
JSA.LONG[, date :=NULL]
JSA.LONG<-JSA.LONG[, list(value=sum(value,na.rm=TRUE)), by=c("OA","Type","year","quarter")]

JSA.LONG<-join(JSA.LONG, WARD17.CENT[, list(wd17cd, OA= msoa11nm, msoa11cd)])
JSA.LONG<-JSA.LONG[, list(value=mean(value,na.rm=TRUE)), by=c("wd17cd","Type","year","quarter")]
JSA.LONG[, measure := type]
JSA.LONG<-JSA.LONG[!is.na(wd17cd)]
OUT[[i]]<-JSA.LONG

i = i+1
}



for(i in 1:length(OUT)) {


tts<-OUT[[i]][, .N, by=Type]$Type
measure<-OUT[[i]][, .N, by=measure]$measure[1]

for(tt in tts) {

TEMP<-OUT[[i]][Type==tt]
TEMP[, time := paste(year,"q",quarter,sep="")]

TEMP.WIDE<-reshape(TEMP[, list(wd17cd,time, value)], idvar="wd17cd", timevar="time", direction="wide")

tt<-str_trim(gsub("\\(.*\\)","",tt))
tt<-gsub("Decision not to apply a sanction","No Sanction", tt)
tt<-gsub("Decision to apply a sanction","Sanction", tt)

varname<-paste(measure,gsub("[^A-z0-9]","",tt),sep="_")
names<-grep("value",names(TEMP.WIDE), value=TRUE)
setnames(TEMP.WIDE, names, gsub("value\\.",varname,names))
 
WARD17<-join(WARD17, TEMP.WIDE)
}


}


ffs<-list.files("Claimants", full.names=TRUE)

CLAIM<-NULL
for(ff in ffs) {

TMP<-readLines(con=ff)
ITEMS<-grep("Item Name  :", TMP)
ITEMS.LABEL<-TMP[grep("Item Name  :", TMP)]

for(it in 1:length(ITEMS)) {

if(it == length(ITEMS)) {
rows = -1
} else {
rows = ITEMS[it+1]-ITEMS[it]
}
TT<-read.csv(file=ff, skip=ITEMS[it]+1,  nrows = rows)
TT.LONG<-data.table(melt(TT, id.vars = c("X2003.CAS.ward","mnemonic") ))
TT.LONG[, variable := paste("01.",variable,sep=" ")]
TT.LONG[, date := dmy(variable) ]
TT.LONG[, list := ITEMS.LABEL[it]]
CLAIM<-rbind(CLAIM, TT.LONG)
}
}

CLAIM[, list := gsub("\"Item Name  :\",\"|\"", "",list)]

CLAIM<-DTUniqueBy(CLAIM, names(CLAIM))

##CSAS WARD

CASWARD<-readOGR(dsn="shapefiles/CASW", layer="UK_caswa_2001NAD1936")
CASWARD.data <- data.table(CASWARD@data)
CASWARD.data[is.na(ons_label), ons_label := label]
CASWARD@data<-CASWARD.data

CASWARD.CENT<-mclapply(1:length(CASWARD), function(x) data.table(ons_label = CASWARD[x,]@data$ons_label , gCentroid(CASWARD[x,])@coords), mc.cores=4)
CASWARD.CENT <-rbindlist(CASWARD.CENT)
CASWARD.CENT.ov<-over(SpatialPoints(CASWARD.CENT[, list(x, y)], proj4string=CRS(proj4string(WARD17.shp))), WARD17.shp)
CASWARD.CENT<-cbind(CASWARD.CENT,CASWARD.CENT.ov)
#CASWARD.CENT <-DTUniqueBy(CASWARD.CENT, "ons_label")

MAPPER<-DTUniqueBy(join(CLAIM[, .N, by= mnemonic], CASWARD.CENT[, list(mnemonic =ons_label, wd17cd)])[!is.na(wd17cd)], "mnemonic")


CLAIM.MERGE<-join(CLAIM, MAPPER)
CLAIM.MERGE<-CLAIM.MERGE[!is.na(wd17cd)]

CLAIM.MERGE[, quarter := quarter(date)]
CLAIM.MERGE[, year := year(date)]

CLAIM.MERGE<-CLAIM.MERGE[, list(value=mean(as.numeric(as.character(value)),na.rm=TRUE)), by=c("wd17cd","year","quarter","list")]

lists<-CLAIM.MERGE[, .N, by=list]$list

for(i in 1:length(lists)) {

TEMP<-CLAIM.MERGE[list==lists[i]]
TEMP<-TEMP[year>=2007]
TEMP<-TEMP[order(year,quarter)]
TEMP[, time := paste(year,"q",quarter,sep="")]

TEMP.WIDE<-reshape(TEMP[, list(wd17cd,time, value)], idvar="wd17cd", timevar="time", direction="wide")

tt<-str_trim(gsub("statistical group - ","",lists[i]))

varname<-paste("CLAIM",gsub("[^A-z0-9]","",tt),sep="_")
names<-grep("value",names(TEMP.WIDE), value=TRUE)
setnames(TEMP.WIDE, names, gsub("value\\.",varname,names))
 
WARD17<-join(WARD17, TEMP.WIDE)
}


POP<-data.table(read.csv(file="Ward Characteristics/WARD_POP_2015.csv"))

POP[, All.Ages := as.numeric(gsub(",","",All.Ages))]

POP<-data.table(data.frame(POP[, c(1,4 ), with=F], apply(POP[, c(5:95), with=F], 2, function(x) as.numeric(gsub(",","",x)))) )

 

POP.WARD<-POP[, list(wd15cd=Ward, POP2015 = as.numeric(gsub(",","",All.Ages)))][wd15cd!=""]

WARD17 <-join(WARD17, DTUniqueBy(POP.WARD[, list(wd17cd, POP2015)],"wd17cd"))

WARD17 <-join(WARD17, POP[, list(wd17cd=Ward, POP2015 = as.numeric(gsub(",","",All.Ages)))][wd17cd!=""])

POP<-data.table(data.frame(POP[, c(1), with=F], POP[, 3:93, with=F]/POP$All.Ages))
setnames(POP, names(POP)[2:ncol(POP)], paste("AGESH_",gsub("X","",names(POP)[2:ncol(POP)]),sep=""))
setnames(POP, "Ward", "wd17cd")

WARD17<-join(WARD17, POP)

write.dta(WARD17, file="EUREF.WARDLEVEL.dta")


JSA.LONG <-join(JSA.LONG,WARD17[, list(wd17cd, LAD17CD)])




#WARD17 MAPPED TO LSOA2001 
MSOA.data<-data.table(MSOA@data)

LSOA.CENT<-mclapply(1:length(LSOA), function(x) data.table(lsoa01nm = LSOA[x,]@data$lsoa01nm , gCentroid(LSOA[x,])@coords), mc.cores=4)
LSOA.CENT <-rbindlist(LSOA.CENT)
LSOA.CENT.ov<-over(SpatialPoints(LSOA.CENT[, list(x, y)], proj4string=CRS(proj4string(WARD17.shp))), WARD17.shp)
LSOA.CENT<-cbind(LSOA.CENT,LSOA.CENT.ov)



#LSOA2001 MAPPED TO WARD17
LSOA<-readOGR(dsn="shapefiles/Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales",layer="Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales") 
LSOA.data<-data.table(LSOA@data)

LSOA.CENT<-mclapply(1:length(LSOA), function(x) data.table(lsoa01nm = LSOA[x,]@data$lsoa01nm , gCentroid(LSOA[x,])@coords), mc.cores=4)
LSOA.CENT <-rbindlist(LSOA.CENT)
LSOA.CENT.ov<-over(SpatialPoints(LSOA.CENT[, list(x, y)], proj4string=CRS(proj4string(WARD17.shp))), WARD17.shp)
LSOA.CENT<-cbind(LSOA.CENT,LSOA.CENT.ov)



##HOUSING BENEFIT CUTS
HBC<-data.table(read.csv(file="Ward Level/Ward Level Austerity/HB_COUNT.csv"))
HBC.LONG<-melt(HBC, id.vars = "OA" )
HBC.LONG <-HBC.LONG[!is.na(value)]
HBC.LONG <-HBC.LONG[variable!="Month"]


HBC.LONG <-HBC.LONG[value!=".."]

HBC.LONG[, date := str_extract(variable, "[0-9]{6}")]
HBC.LONG<-HBC.LONG[-grep("Not avail|Total",OA)]
HBC.LONG[, variable := NULL]

setnames(HBC.LONG, "value", "housing_ben_rec_count")
HBC.LONG[, year := as.numeric(substr(date,1,4))]
HBC.LONG[, month := as.numeric(substr(date,5,6))]
 
HBC.LONG <-join(HBC.LONG, LSOA.CENT[, list(OA= lsoa01nm, wd17cd)])
HBC.LONG<-HBC.LONG[!is.na(wd17cd)]
HBC.LONG[, housing_ben_rec_count := as.numeric(as.character(housing_ben_rec_count))]

HBC.WARD<-HBC.LONG[, list(housing_ben_rec_count =sum(housing_ben_rec_count)), by=c("wd17cd","year","month")]

BTX<-data.table(read.csv(file="Ward Level/Ward Level Austerity/BEDROOM_TAX_CASES.csv"))
BTX[Type=="", Type:=NA ]
BTX[, Type := na.locf(Type)]
BTX<-BTX[Type=="Reduction applied"]
BTX<-BTX[, Type :=NULL]
BTX.LONG<-melt(BTX, id.vars = "OA" )

BTX.LONG <-BTX.LONG[variable %!in% c("X","Month","X.1")]
BTX.LONG <-BTX.LONG[!is.na(value)]
BTX.LONG[, value := as.numeric(value)]
setnames(BTX.LONG, "variable", "date")
BTX.LONG[, year := as.numeric(substr(date,2,5))]
BTX.LONG[, month := as.numeric(substr(date,6,7))]
BTX.LONG <-BTX.LONG[!is.na(value)]
BTX.LONG[, date := NULL]
BTX.LONG <-join(BTX.LONG, LSOA.CENT[, list(OA= lsoa01nm, wd17cd)])
BTX.LONG <-BTX.LONG[!is.na(wd17cd)]

BTX.WARD<-BTX.LONG[, list(btx_cases = sum(value)), by=c("wd17cd","year","month")]


PIP<-data.table(read.csv(file="Ward Level/Ward Level Austerity/PIP Registrations.csv"))
PIP[OA=="", OA:=NA ]
PIP[, OA := na.locf(OA)]

PIP.LONG<-melt(PIP, id.vars = c("OA","Type") )
PIP.LONG[, value := as.numeric(value)]
setnames(PIP.LONG, "variable", "date")
PIP.LONG[, year := as.numeric(substr(date,2,5))]
PIP.LONG[, month := as.numeric(substr(date,6,7))]
PIP.LONG<-PIP.LONG[date!="X"]
PIP.LONG<-PIP.LONG[!is.na(year)]
PIP.LONG[, date :=NULL]
PIP.LONG <-PIP.LONG[!is.na(value)]

PIP.LONG <-join(PIP.LONG, LSOA.CENT[, list(OA= lsoa01nm, wd17cd)])
PIP.LONG <-PIP.LONG[!is.na(wd17cd)]

PIP.WARD<-PIP.LONG[, list(pipcases = sum(value)), by=c("wd17cd","Type","year","month")]


JSA<-data.table(read.csv(file="Ward Level/Ward Level Austerity/JSA Sanctions high frequency around ref.csv"))
JSA[OA=="", OA:=NA ]
JSA[, OA := na.locf(OA)]
JSA.LONG<-melt(JSA, id.vars = c("OA","Type") )
JSA.LONG[, value := as.numeric(value)]
setnames(JSA.LONG, "variable", "date")
JSA.LONG[, year := as.numeric(substr(date,2,5))]
JSA.LONG[, month := as.numeric(substr(date,6,7))]
JSA.LONG<-JSA.LONG[date!="X"]
JSA.LONG<-JSA.LONG[!is.na(year)]
JSA.LONG[, date :=NULL]
JSA.LONG <-JSA.LONG[!is.na(value)]
JSA.LONG <-join(JSA.LONG, LSOA.CENT[, list(OA= lsoa01nm, wd17cd)])
JSA.LONG <-JSA.LONG[!is.na(wd17cd)]

JSA.WARD<-JSA.LONG[, list(jsasanctioncases = sum(value)), by=c("wd17cd","Type","year","month")]


##WARD PANEL

WARD.PAN<-rbindlist(lapply(WARD17$wd17cd, function(x) data.table(wd17cd=x, timevec=timeVector("2007-01-12","2017-12-31"))))
WARD.PAN[, year := year(timevec)]
WARD.PAN[, month := month(timevec)]

WARD.PAN <-join(WARD.PAN, HBC.WARD)
WARD.PAN <-join(WARD.PAN, BTX.WARD)
WARD.PAN <-join(WARD.PAN, PIP.WARD[Type=="Total", list(wd17cd, year, month, pip_total_registrations=pipcases)])
WARD.PAN <-join(WARD.PAN, PIP.WARD[Type=="Reassessment", list(wd17cd, year, month, pip_reassessment_registrations=pipcases)])
WARD.PAN <-join(WARD.PAN, JSA.WARD[Type=="Decision to apply a sanction (adverse) (IX)", list(wd17cd, year, month, jsa_sanctions_adverse=jsasanctioncases)])
WARD.PAN <-join(WARD.PAN, JSA.WARD[Type=="Decision not to apply a sanction (non-adverse) (VIII)", list(wd17cd, year, month, jsa_sanctions_nonadverse=jsasanctioncases)])
WARD.PAN <-join(WARD.PAN, JSA.WARD[Type=="Total", list(wd17cd, year, month, jsa_sanctions_total=jsasanctioncases)])


##WARD15 to WARD17 CROSS WALK
WARD15.shp<-readOGR(dsn="Ward Level/shapefiles/Wards_December_2015_Full_Clipped_Boundaries_in_Great_Britain", layer="Wards_December_2015_Full_Clipped_Boundaries_in_Great_Britain")
WARD15.data<-data.table(WARD15.shp@data)

WARD15.CENT<-mclapply(1:length(WARD15.shp), function(x) data.table(wd15cd = WARD15.shp[x,]@data$wd15cd , gCentroid(WARD15.shp[x,])@coords), mc.cores=4)
WARD15.CENT <-rbindlist(WARD15.CENT)
WARD15.CENT.ov<-over(SpatialPoints(WARD15.CENT[, list(x, y)], proj4string=CRS(proj4string(WARD17.shp))), WARD17.shp)

WARD15.CENT<-cbind(WARD15.CENT,WARD15.CENT.ov)

POP<-data.table(read.csv(file="Ward Characteristics/WARD_POP_2015.csv"))

POP[, All.Ages := as.numeric(gsub(",","",All.Ages))]

POP<-data.table(data.frame(POP[, c(1,4 ), with=F], apply(POP[, c(5:95), with=F], 2, function(x) as.numeric(gsub(",","",x)))) )

 

POP.WARD<-POP[, list(wd15cd=Ward, POP2015 = as.numeric(gsub(",","",All.Ages)))][wd15cd!=""]

WARD17 <-join(WARD17, DTUniqueBy(POP.WARD[, list(wd17cd, POP2015)],"wd17cd"))

WARD17 <-join(WARD17, POP[, list(wd17cd=Ward, POP2015 = as.numeric(gsub(",","",All.Ages)))][wd17cd!=""])

POP<-data.table(data.frame(POP[, c(1), with=F], POP[, 3:93, with=F]/POP$All.Ages))
setnames(POP, names(POP)[2:ncol(POP)], paste("AGESH_",gsub("X","",names(POP)[2:ncol(POP)]),sep=""))
setnames(POP, "Ward", "wd17cd")

WARD17<-join(WARD17, POP)

WARD.PAN[is.na(WARD.PAN)]<-0

AROUNDREF<-WARD.PAN[year>=2012 & year<=2017]

AROUNDREF[month<10,time := as.numeric(paste(year,"0",month,sep=""))]

AROUNDREF[month>=10,time := as.numeric(paste(year,"",month,sep=""))]

vars<-names(AROUNDREF)[5:(ncol(AROUNDREF)-1)]

for(vv in vars) {
setnames(AROUNDREF, vv, "temp")

WIDE<-dcast(AROUNDREF[,list(wd17cd,time, temp)], wd17cd ~ time)
setnames(WIDE, names(WIDE)[2:ncol(WIDE)], paste(vv,"_",names(WIDE)[2:ncol(WIDE)],sep=""))

WARD17<-join(WARD17, WIDE)

setnames(AROUNDREF, "temp", vv)
}



write.dta(WARD17, file="WARD_LEVEL_REF.dta")


