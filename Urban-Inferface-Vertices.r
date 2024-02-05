###################################################
# Manuel Campagnolo (mlc@isa.ulisboa.pt)
# September 10, 2019
###################################################

# Loading necessary libraries
library(sf)
library(maptools)
library(RANN)
library(data.table)
library(mapview)
library(leaflet)
library(tidyverse)
library(htmlwidgets)

rm(list=ls()) # clear all objects

FOLDER<-"D:\\interface-yannick"
if (Sys.info()["nodename"]=="DMMLC1") FOLDER<-"Z:\\interface-yannick"
setwd(FOLDER)

opcao<-"altorisco" # "todos" # "altorisco"
if (opcao=="altorisco") inputFlamm<-"Combustivel_AltoRisco_Yannick_GLisboa.gpkg"
if (opcao=="todos") inputFlamm<-"Combustiveis_Todos_Yannick_GLisboa_fx.gpkg"#  "CombustivelMinusUrban_TestInterface.gpkg" # "Flam2015v4.shp" #   "Flam2015v4.shp" # dez 2018
# densidade de residentes ("DensResC") e o Core area index ("CorePC")
# inputUrb<-"Urban_TesteInterface2_corrigido.gpkg" com erros topológicos
# inputUrb<-"Urban_TesteInterface3_Fixed.gpkg"  # "Urban_TesteInterface.gpkg" #urb2015_final.shp"   #"urb2015_final.shp"  ## supor que tem atributo "freg" [Dicofre]
inputUrb<-"Areas_Urbanas_Yannick_GLisboa.gpkg"  # "Urban_TesteInterface.gpkg" #urb2015_final.shp"   #"urb2015_final.shp"  ## supor que tem atributo "freg" [Dicofre]

if (opcao=="altorisco") extraname<-"8set19GLisboaAltoRisco" # pode ser qualquer coisa a ser incluido no nome do ficheiro
if (opcao=="todos") extraname<-"8set19GLisboaTodos" # pode ser qualquer coisa a ser incluido no nome do ficheiro


###################################################################################################
CREATE.INTERFACE<-TRUE #TRUE # to create even if file already exists
TESTIDX<-FALSE
DRAWSEGMENTS<-FALSE
DRAWPOINTS<-FALSE #TRUE
####################################################################################################

# maximum distance
D<-500 # maximum distance
K<-60 # number of flammable neighbors to explore
KF<-40 # number of urban neighbors of the flammable neighbors to explore OR number of urban neighbors to explore
limiar<-1.05 # threshold for triangular inequality
limiartheta <- 60 # largest angle to be elegible to protect
KS<-1:K 
KFS<-1:KF
MAXDIST<-0 # if 0 do not densify #to densify: maximum distance in meters between urban vertices
# classes of distance
tolerance<-3
bigN<-10^6
smallN<-10^-6
ftype<-function(d,D) (d==0)+(d>0 & d<=100) *2 + (d>100 & d<=250) * 3 + (d>250 & d<=D)*4 + (d>D)* 5
#ftype<-function(d,tolerance) (d<=tolerance)+(d>tolerance & d<=100) *2 + (d>100 & d<=250) * 3 + (d>250)*4 ####
POSVALUE<-9999
NEGVALUE<- -1 # NA value for variables that can only be positice (distance, azimuth, feature index)


# important: to group interface in segments
ADDVAR<-TRUE #TRUE # para usar um atributo adicional de urbano na construção nos elementos da interface (e.g. freguesia)
NEWVAR<-"fid_1" # "DensResC"  ##### <<<< substituir por e.g. "freguesia" (ou o que for o nome do atributo de urbano a considerar)
ADDVAR2<-FALSE #TRUE 
NEWVAR2<-"CorePC"  #

# just to add to the output
ADDFLAMVAR<-FALSE #TRUE # 
NEWFLAMVAR<-"idflam" #  if fact, idflam is created by the algorithm, just enumerating features of flam. #"RCOS15_ic"  
ADDFLAMVAR2<-FALSE #TRUE #TRUE 
NEWFLAMVAR2<-"FuelRisk" 

###########################################################################################
# to test a given particular location
d<-4000
if (TESTIDX) {K<-20;KS<-1:K;KF<-40;KFS<-1:KF; extraname<-paste0("test-",extraname)}
#- Vertice de interface direta classificado como não-interface: [fid=367860, lat:-82628, lon:-98681]; [fid:19737 lat:-97401, lon:-111052]
#- Vertice de interface direta classificado com interface 400-500m: [fid=8998, lat:-130407, lon:-86460]
# convert  lon/lat to pttm06
# pttm06<-"+proj=tmerc +lat_0=39.6682583333333 +lon_0=-8.133108333333331 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
x0y0<-st_as_sf(data.frame(x=-9.27950,y=38.74991),coords=c("x","y"),crs=4326) # 
#x0y0<-st_as_sf(data.frame(x=-9.29135,y=38.75310),coords=c("x","y"),crs=4326) # bom caso para testar segmentos em F
#x0y0<-st_as_sf(data.frame(x=-9.28668,y=38.75072),coords=c("x","y"),crs=4326) # bom caso para testar segmentos em F
#x0y0<-st_as_sf(data.frame(x=-9.32964,y=38.73253),coords=c("x","y"),crs=4326) # bom caso para testar segmentos em F
x0y0<-st_as_sf(data.frame(x=-9.27794,y=38.74570),coords=c("x","y"),crs=4326) # bom caso para testar segmentos em F
#x0y0<-st_as_sf(data.frame(x= -98681, y = -82628),coords=c("x","y"),crs=3763) # Y1
#x0y0<-st_as_sf(data.frame(x=  -86460, y = -130407),coords=c("x","y"),crs=3763) # Y2
#x0y0<-st_as_sf(data.frame(x=-9.34158,y=38.72661),coords=c("x","y"),crs=4326) # bom caso para testar segmentos em F
#x0y0<-st_coordinates(st_transform(x0y0,crs=4326))
x0y0<-st_coordinates(st_transform(x0y0,crs=3763))
x0y0<-data.frame(X= -98913 , Y=-103194)
#############################################################################################
x0<-x0y0[,"X"]
y0<-x0y0[,"Y"]
xyll<-c(NA,NA, x0-d,y0-d) # 
xyur<-c(NA,NA, x0+d,y0+d)
BOX<-ext<-c(xmin=xyll[3], xmax=xyur[3], ymin=xyll[4], ymax=xyur[4])

# dot prof between (P1-P2) and (P3-P4)
dotprod<-function(x1,y1,x2,y2,x3,y3,x4,y4) (x1-x2)*(x3-x4)+(y1-y2)*(y3-y4)
crossprod<-function(x1,y1,x2,y2,x3,y3,x4,y4) (x2-x1)*(y4-y3)-(y2-y1)*(x4-x3)
# definir crossproduct e substituit <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TODO

# 28 ago2019
azimuthVF<-function(xV,yV,xF,yF) 
{
  d <- complex(real = xF-xV, imaginary = yF-yV); 
  az<-((2*pi+ pi/2 - Arg(d)) %% (2*pi)) * 180/pi; 
  return(360*(xF==xV & yF==yV)+(xF!=xV | yF!=yV) *az)
}

# DEC 2018: add rule for using border, from coordinates ,xU,yU,xF,yF,xW,yW,xWW,yWW
# is U protected from F by (WWW,W,WW)?
decision<-function(D,limiar,limiartheta,xV,yV,xF,yF,xW,yW,xWW,yWW,xWWW,yWWW) # idxF,idxW,
{
  d2VF<-(xV-xF)^2+(yV-yF)^2
  d2VW<-(xV-xW)^2+(yV-yW)^2
  d2WF<-(xW-xF)^2+(yW-yF)^2
  dot<-(xW-xV)*(xF-xV)+(yW-yV)*(yF-yV)
  thetaV<-dot # provisório
  cond<- !is.na(xV) & !is.na(xF) & !is.na(xW) & d2VW>0 & d2VF>0 & dot^2 <= d2VW*d2VF # otherwise it is not protected
  thetaV[cond]<-acos(dot[cond]/sqrt(d2VW[cond]*d2VF[cond]))* 180/pi
  thetaV[is.na(thetaV)]<-limiartheta # in case there are NA

  # dec 2018: protected by the urban edge
  protedge.next<- (crossprod(xW,yW,xWW,yWW,xW,yW,xF,yF)*crossprod(xW,yW,xWW,yWW,xW,yW,xV,yV)< -smallN |
                     abs(crossprod(xW,yW,xWW,yWW,xW,yW,xF,yF))<=smallN ) & # julho 2019 : se F está entre W e WW ou entre W e WWW
    crossprod(xV,yV,xF,yF,xV,yV,xW,yW)*crossprod(xV,yV,xF,yF,xV,yV,xWW,yWW)< -smallN 
  protedge.next[is.na(protedge.next)]<-FALSE
  
  # julho 2019: protected by the urban edge (previous)
  # protedge.prev<- crossprod(xWWW,yWWW,xW,yW,xV,yV,xW,yW)*crossprod(xWWW,yWWW,xW,yW,xF,yF,xW,yW)<smallN &
  #   crossprod(xV,yV,xF,yF,xW,yW,xF,yF)*crossprod(xV,yV,xF,yF,xWWW,yWWW,xF,yF)<smallN
  protedge.prev<- (crossprod(xW,yW,xWWW,yWWW,xW,yW,xF,yF)*crossprod(xW,yW,xWWW,yWWW,xW,yW,xV,yV)< -smallN |
                     abs(crossprod(xW,yW,xWWW,yWWW,xW,yW,xF,yF))<=smallN ) & # julho 2019 : se F está entre W e WW ou entre W e WWW
    crossprod(xV,yV,xF,yF,xV,yV,xW,yW)*crossprod(xV,yV,xF,yF,xV,yV,xWWW,yWWW)< -smallN 
  protedge.prev[is.na(protedge.prev)]<-FALSE
  
  return( #!is.na(idxF) & idxF!=0 & !is.na(idxW) & idxW!=0 &
    (xF==bigN & yF==bigN) | # se xF ne yF são NA, xF<-yF<-bigN e nesse caso V está "protegido" de F
       (
            !is.na(d2VF) & !is.na(d2VW) & !is.na(d2WF) &
            d2VW>0 & # V cannot be protected by itself
            d2VF>0 &  # if d2VF==0 then V has to be in the interface
            #d2WF>0 &  # just having W coindident with F is not sufficient to protect V from F
             (
            ((sqrt(d2VW)+sqrt(d2WF))< limiar * sqrt(d2VF) & # triangular inequality
            thetaV<limiartheta   &  d2VF>= d2VW & d2VF>= d2WF )# limit angle: nov 2018
         | protedge.next | protedge.prev | 
           d2VF > 2* D^2))) # se xF ne yF são NA, xF=yF=bigN e nesse caso V está "protegido" de F
}

# mat<-mat.urb1
# idxviz<-kvw$nn.idx[,2]
# IN<-"u"
# idxneigh(mat=mat.urb1,idxviz=kvw$nn.idx[,2],IN="u")
# retruns idx of prev neigh and of next neigh (if of the sam part); otherwise return idxviz it self
idxneigh<-function(mat,idxviz,IN) # idxviz can be NA
{
  colpart<-paste0("idx.part.",IN) # " name of part column"
  idxnext<-(idxviz<nrow(mat))*(idxviz+1)+(idxviz==nrow(mat))*nrow(mat) # can be NA
  idxprev<-(idxviz>1)*(idxviz-1)+(idxviz==1)*1
  
  # test if cor/next and cor/prev are in the same part: if not prev<-current, and next<-current
  idxprev<-idxprev*(mat[idxprev,colpart]==mat[idxviz,colpart]) + idxviz * (mat[idxprev,colpart]!=mat[idxviz,colpart]) # can be NA
  idxnext<-idxnext*(mat[idxnext,colpart]==mat[idxviz,colpart]) + idxviz * (mat[idxnext,colpart]!=mat[idxviz,colpart]) 
  
  # idxprev[is.na(idxprev)]<-idxviz[is.na(idxprev)]
  # idxnext[is.na(idxnext)]<-idxviz[is.na(idxnext)]
  
  return(list(idxprev=idxprev,idxnext=idxnext))
}

################################################################### read data
# read data and create idx features
if (CREATE.INTERFACE | TESTIDX | !exists("mat.Flamm") | !exists("mat.urb1"))
{
  # Reading Flammable shapefile
  flam<-st_read(inputFlamm, promote_to_multi=TRUE)
  if (TESTIDX) 
  {
    #table(st_geometry_type(flam))
    flam<-st_crop(flam,BOX)
    #flam<-flam[st_geometry_type(flam) %in% c("POLYGON","MULTIPOLYGON"),]
    auxflam<-flam[st_geometry_type(flam) == "MULTIPOLYGON",] # as a result of st_crop
    polyflam<-st_cast(auxflam,"POLYGON")
    # to be able to rbind urb and polyurb
    newgeomcolname<-colnames(polyflam)[which(grepl(colnames(polyflam),pattern="^geom"))] # special column, usually named 'geom' or 'geometry'.
    geomcolname<-colnames(flam)[which(grepl(colnames(flam),pattern="^geom"))] # special column, usually named 'geom' or 'geometry'. 
    attributes(polyflam)$names[which(attributes(polyflam)$names==newgeomcolname)]<-geomcolname
    attributes(polyflam)$sf_column<-geomcolname
    flam<-rbind(flam[st_geometry_type(flam) == "POLYGON",],polyflam)
    flam<-st_cast(flam,"MULTIPOLYGON")
  }
  flam$idflam<-1:nrow(flam)
  
  xyflam<-st_coordinates(flam)
  if (!("L3" %in% colnames(xyflam))) stop("xyflam missing L3") # 3 levels
  if (max(xyflam[,"L3"])!=nrow(flam)) stop("L3 is not the index of features")
  idx.L1<-xyflam[,"L1"] #level 1
  idx.L2<-xyflam[,"L2"] #level 2
  # create part number with format e.g. idx.feat*100+idx.part.feat
  M<-10^(1+ceiling(log10(max(idx.L2)))) # e.g. 100
  Q<-10^(1+ceiling(log10(max(idx.L1)))) # e.g. 100
  idx.feat.f<-xyflam[,"L3"] # feat
  idx.part.f<-as.double(M*Q*idx.feat.f + M* idx.L1+idx.L2)  # double has 53 bits in R
  mat.Flamm<-round(cbind(xyflam[,c("X"  ,"Y" ,"L3")],idx.part.f=idx.part.f)) #,idx.vert.u=1:nrow(xyurb)) <<<<<<<<<<<<<<<<<<<  round coordinates
  colnames(mat.Flamm)<-c("x","y", "idx.feat.f","idx.part.f")#,"idx.vert.u") # tail(mat.urb1)
  
  # create auxiliary table indexed by idx.feat.f to be merged later
  if (ADDFLAMVAR) flamtable<-data.frame(idx.feat.f=1:nrow(flam),newflamvar=flam[[NEWFLAMVAR]]) # not elegant: would be better to have VARSFLAM and use those
  if (ADDFLAMVAR2) flamtable<-data.frame(idx.feat.f=1:nrow(flam),newflamvar=flam[[NEWFLAMVAR]],newflamvar2=flam[[NEWFLAMVAR2]])
  
  # remove repeated vertices except the last vertice of the feature  (so the feature closes) 
  dups<-duplicated(as.data.table(mat.Flamm)) # TRUE if the row of mat.Flam already occurs
  step <- as.integer(c(diff(mat.Flamm[,"idx.part.f"])!=0,FALSE)) # TRUE if idx.part.f changes
  mat.Flamm<-mat.Flamm[!dups | !step,]  # 
  mat.Flamm<-cbind(mat.Flamm,data.frame(idx.vert.f=1:nrow(mat.Flamm)))
  
  # Reading urban shapefile
  urb<-st_read(inputUrb,promote_to_multi=TRUE) # special column, usually named 'geom' or 'geometry'. 
  urb<-st_transform(urb,st_crs(flam)) # to be sure that the CRS are the same
  if (TESTIDX) 
  {
    urb<-st_crop(urb,BOX)
    #urb<-urb[st_geometry_type(urb) == "POLYGON",]
    auxurb<-urb[st_geometry_type(urb) == "MULTIPOLYGON",] # as a result of st_crop
    polyurb<-st_cast(auxurb,"POLYGON")
    # to be able to rbind urb and polyurb
    newgeomcolname<-colnames(polyurb)[which(grepl(colnames(polyurb),pattern="^geom"))] # special column, usually named 'geom' or 'geometry'.
    geomcolname<-colnames(urb)[which(grepl(colnames(urb),pattern="^geom"))] # special column, usually named 'geom' or 'geometry'. 
    attributes(polyurb)$names[which(attributes(polyurb)$names==newgeomcolname)]<-geomcolname
    attributes(polyurb)$sf_column<-geomcolname
    urb<-rbind(urb[st_geometry_type(urb) == "POLYGON",],polyurb)
    urb<-st_cast(urb,"MULTIPOLYGON")
  }
  urb$idurb<-1:nrow(urb)
  ## indicates the index at various levels 
  ## this is 
  ## L3 1st multipolygon (there is only one) -> feature
  ## L2 27th island -> parte
  ## L1 14 hole in L2 -> polígono exterior ou buraco
  xyurb<-st_coordinates(urb)
  
  if (!("L3" %in% colnames(xyurb))) stop("xyurb missing L3") # 3 levels
  if (max(xyurb[,"L3"])!=nrow(urb)) stop("L3 is not the index of features")
  idx.L1<-xyurb[,"L1"] #level 1
  idx.L2<-xyurb[,"L2"] #level 2
  # create part number with format e.g. idx.feat*100+idx.part.feat
  M<-10^(1+ceiling(log10(max(idx.L2)))) # e.g. 100
  Q<-10^(1+ceiling(log10(max(idx.L1)))) # e.g. 100
  idx.feat.u<-xyurb[,"L3"] # feat
  idx.part.u<-as.double(M*Q*idx.feat.u + M* idx.L1+idx.L2)  # double has 53 bits in R
  mat.urb1<-round(cbind(xyurb[,c("X"  ,"Y" ,"L3")],idx.part.u=idx.part.u)) #,idx.vert.u=1:nrow(xyurb)) <<<<<<<<<<<<<<<<<<<  round coordinates
  colnames(mat.urb1)<-c("x","y", "idx.feat.u","idx.part.u")#,"idx.vert.u") # tail(mat.urb1)
  
  # create auxiliary table indexed by idx.feat.u to be merged later
  if (ADDVAR) newtable<-data.frame(idx.feat.u=1:nrow(urb),newvar=urb[[NEWVAR]]) # not elegant: would be better to have VARSURB and use those
  if (ADDVAR2) newtable<-data.frame(idx.feat.u=1:nrow(urb),newvar=urb[[NEWVAR]],newvar2=urb[[NEWVAR2]])
  
  
   # remove repeated vertices except the last vertice of the feature  (so the feature closes) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TODO
  dups<-duplicated(as.data.table(mat.urb1))
  step <- as.integer(c(diff(mat.urb1[,"idx.part.u"])!=0,FALSE))
  mat.urb1<-mat.urb1[!dups | !step,]  
  mat.urb1<-cbind(mat.urb1,data.frame(idx.vert.u=1:nrow(mat.urb1)))
  
  
  # add to mat.Flam index of coincident Urb, or NA if there is none
  idxUF<-nn2(query=mat.Flamm[,c("x","y")],data=mat.urb1[,c("x","y")],k=1,searchtype = "radius", radius=0)$nn.idx
  idxUF[idxUF==0]<-NA
  mat.Flamm<-cbind(mat.Flamm,data.frame(idx.vert.u=idxUF))  # urban vertices of flam vertices
  
  # add to mat.urb1 index of coincident F, or NA if there is none
  idxFU<-nn2(data=mat.Flamm[,c("x","y")],query=mat.urb1[,c("x","y")],k=1,searchtype = "radius", radius=0)$nn.idx
  idxFU[idxFU==0]<-NA
  mat.urb1<-cbind(mat.urb1,data.frame(idx.vert.f=idxFU))  # Flammable neighbors of urban vertices
  
}


id0<-which.min((mat.urb1[,"x"]-x0y0[1])^2+(mat.urb1[,"y"]-x0y0[2])^2) 

# determining the K Flam neighbors up to distance D meters from each urban neighbor
# Calculating the distance from each vertice of the urban polygons to each vertice within D meters  of the flammable polygons
# knn: neighbors urban X Flam 
knn<-nn2(data=mat.Flamm[,c("x","y")],query=mat.urb1[,c("x","y")],k=K,searchtype = "radius", radius=D)  # Flammable neighbors of urban vertices
#str(knn) # When nn.idx is zero, it means that there is no flammable vertices within D=500 m
# replace 0s by NAs
#knn$nn.dists[knn$nn.idx==0]<-NA
knn$nn.idx[knn$nn.idx==0]<-NA

FICHNAME<-paste0("interface_K",K,"_KF",KF,"_limiar",round(limiar*100),"_maxdist",MAXDIST,"-","maxtheta",limiartheta,"-",extraname)
fichs<-list.files(FOLDER,pattern=paste0(FICHNAME,".RData"))

################################################## main algorithm
if (CREATE.INTERFACE | TESTIDX | length(fichs)==0)
{
  # initialize: all vertices are not in the interface
  notinterface<-rep(TRUE,nrow(mat.urb1))
  dF<- rep(POSVALUE,nrow(mat.urb1)) # distance to closect non protected F
  dFplus<- rep(NEGVALUE,nrow(mat.urb1)) # distance to farest non protected F
  azFplus<- azF <- rep(NEGVALUE,nrow(mat.urb1)) # 28ago2019: azimuth of the closest not protected Flam (in degrees)
  iF<- rep(NEGVALUE,nrow(mat.urb1)) # 30ago2019: index of closest not protected Flam
  # NEW: determine KF urban neighbors W of urban V
  kvw<-nn2(mat.urb1[,c("x","y")], k=KF, searchtype = "radius", radius=D)  # (GROUP 1 of potential protectors) KF Urban neighbors of urban vertices
  kvw$nn.idx[kvw$nn.idx==0]<-NA # NEW
  
  xV<-mat.urb1[,"x"]
  yV<-mat.urb1[,"y"]
  
  
  if (DRAWSEGMENTS | DRAWPOINTS) #!protected[id0]) # &  lambda[id0] >0 & lambda[id0] <1) 
  {
    cond <- (xV-x0y0[1])^2+(yV-x0y0[2])^2 < d^2 # & mat.urb1[,"idx.part.u"]==311
    plot(xV[cond],yV[cond],pch="o",asp=1)
    #text(xV[cond],yV[cond],mat.urb1[cond,"idx.vert.u"], pos=3, cex=0.7)
  }
  
  
  k<-1
  for (k in KS) # cycle through K FLAM neighbors of urban vertice 
  {
    threetimesprotected<-rep(TRUE,nrow(mat.urb1)) #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    
    # the goal is to try to show that it is protected from its k-th flammable neighbor
    
    # xyd gets the index of the k-th F-neighbor, and the distance to it
    idxF=knn$nn.idx[,k] # can be NA
    #d2VF=knn$nn.dists[,k]^2 
    
    # xF,yF are NA if there is not k-th F-neighbor of the urban vertex within distance D
    xF<-mat.Flamm[idxF,"x"]; #xF[is.na(xF)]<- 10^6 # maior do que Portugal
    yF<-mat.Flamm[idxF,"y"]; #yF[is.na(yF)]<- 10^6 # maior do que Portugal

    # <<<<<<<<<<<<<<<<<<< define FF, FFF, and do cycle tthrough F, FF, FFF: each turn test threetimesprotected <- threetimesprotected & protected 
    # at the end threetimesprotected iff it is protected from F, FF, and FFF
    res<-idxneigh(mat=mat.Flamm,idxviz=idxF,IN="f") # returns Flamm idx for the previous and next (w.r.t. idxF) Flam vertex
    xFF<-mat.Flamm[res$idxnext,"x"]; yFF<-mat.Flamm[res$idxnext,"y"]; 
    xFFF<-mat.Flamm[res$idxprev,"x"]; yFFF<-mat.Flamm[res$idxprev,"y"]; 

    # 30ago2019: to register the closest non protected Flam feature
    idxfeatF<-mat.Flamm[idxF,"idx.feat.f"]; 
    #idxfeatFF<-mat.Flamm[res$idxnext,"idx.feat.f"]; 
    #idxfeatFFF<-mat.Flamm[res$idxprev,"idx.feat.f"]; 
    
    # plot current, connected to previous and to next
    if (DRAWSEGMENTS) 
      for (t in 1:length(xF)) lines(c(xFF[t],xF[t],xFFF[t]), c(yFF[t],yF[t],yFFF[t]), col="green")
    
    # Flamm point closest to xF,yF over the edge (F,FF) - next
    # edgeurb<- (mat.urb1[mat.Flamm[res$idxnext,"idx.vert.u"],"idx.part.u"] == mat.urb1[mat.Flamm[idxF,"idx.vert.u"],"idx.part.u"]) &
    #   abs(mat.Flamm[res$idxnext,"idx.vert.u"]-mat.Flamm[idxF,"idx.vert.u"])==1
    # edgeurb[is.na(edgeurb)]<-FALSE # if F does not coincide with any U
    Deltax<- xF-xFF; Deltay<- yF-yFF; Delta<-Deltax^2+Deltay^2
    Delta[is.na(Delta)]<-0
    lambda<- (Delta>0) * ((xV-xFF)*Deltax + (yV-yFF) * Deltay)/ Delta +  (Delta==0)*1 # when lambda=1 xFF=xF
    lambda[ lambda <=0 | lambda >=1 ] <- 1 # cancels point | edgeurb
    xxFF<-xFF; yyFF<-yFF
    xFF<-lambda*xF + (1-lambda)*xFF; 
    yFF<-lambda*yF + (1-lambda)*yFF
    
    # Flamm point closest to xF,yF over the edge (F,FFF) -- prev
    # edgeurb<- (mat.urb1[mat.Flamm[res$idxprev,"idx.vert.u"],"idx.part.u"] == mat.urb1[mat.Flamm[idxF,"idx.vert.u"],"idx.part.u"]) &
    #   abs(mat.Flamm[res$idxprev,"idx.vert.u"]-mat.Flamm[idxF,"idx.vert.u"])==1
    # edgeurb[is.na(edgeurb)]<-FALSE # if F does not coincide with any U
    Deltax<- xF-xFFF; Deltay<- yF-yFFF; Delta<-Deltax^2+Deltay^2
    Delta[is.na(Delta)]<-0 # remove NAs
    lambda<- (Delta>0) * ((xV-xFFF)*Deltax + (yV-yFFF) * Deltay)/ Delta +  (Delta==0)*1 # when lambda=1 xFF=xF
    lambda[ lambda <=0 | lambda >=1 ] <- 1 # cancels point | edgeurb
    #xxFFF<-xFFF; yyFFF<-yFFF
    xFFF<-lambda*xF + (1-lambda)*xFFF; 
    yFFF<-lambda*yF + (1-lambda)*yFFF
    
    xFback<-xF; yFback<-yF
    
    if (DRAWPOINTS) #!protected[id0]) # &  lambda[id0] >0 & lambda[id0] <1) 
    {
      text(xFback[id0],yFback[id0],"F",pos=4,cex=0.8)
      text(xFF[id0],yFF[id0],"FF",pos=3,cex=0.8)
      text(xFFF[id0],yFFF[id0],"FFF",pos=2,cex=0.8)
      
    }
    
    #if (DRAWSEGMENTS) for (t in 1:length(xF)) lines(c(xxFF,xFF,xF,xFFF,xxFFF), c(yyFF,yFF,yF,yFFF,yyFFF), col="blue")
    if (DRAWSEGMENTS) 
      for (t in 1:length(xF)) lines(c(xFF[t],xF[t],xFFF[t]), c(yFF[t],yF[t],yFFF[t]), col="blue")
    
    
    idxFviz<-3
    for (idxFviz in 1:3) # V está protegido se está protegido de F e FF e FFF
    {
      
      # decide if V is protected from its k-th flammable neighbor by some urban vertice W
      protected<-rep(FALSE,nrow(mat.urb1)) # initialization: it is not protected
      
      if (idxFviz==2) {xF<-xFF;yF<-yFF} #; idxfeatF<-idxfeatFF}
      if (idxFviz==3) {xF<-xFFF;yF<-yFFF} #; idxfeatF<-idxfeatFFF}
      
      if (!TESTIDX) print(paste("iteration",k,"among F-neighbors and idxFviz=",idxFviz, "in 3"))
      
      # if idxfeatF is not defined:
      xF[is.na(xF)]<-bigN; yF[is.na(yF)]<-bigN 
      idxfeatF[is.na(idxfeatF)]<-NEGVALUE
      
      if (DRAWSEGMENTS) #!protected[id0]) # &  lambda[id0] >0 & lambda[id0] <1) 
      {
        # cond <- (xV-x0y0[1])^2+(yV-x0y0[2])^2 < 20000
        # plot(xV[cond],yV[cond],pch=".",cex=2)
        #cond <- (xF-x0y0[1])^2+(yF-x0y0[2])^2 < d^2
        points(xF,yF,pch=".",col="green", cex=2+idxFviz)
        #text(xF,yF,idxF,pos=4,cex=0.8)

      }
      
      
      # GROUP 1 of potential protectors:  KF Urban neighbors of urban vertices
      # Dec 2018: moved outside  cycle GROUP 2: it should be k and not j, since kvw does not depend on the index of the flammable vertices
      j<-1
      for (j in KFS) # cycle through URB neighbors of selected Flam vertices GROUP 1
      {
        if (!TESTIDX & j%%10==0) print(paste("GROUP1: iteration",j,"among urban neighbors of V"))
        
        d2VW<-kvw$nn.dists[,j]^2 # distance between urban vertex V and its urban neighbor W
        # GROUP 1: coordinates of W, k-th urban neighbor of V
        xW<-mat.urb1[kvw$nn.idx[,j],"x"]
        yW<-mat.urb1[kvw$nn.idx[,j],"y"]
        # d2WF<-(mat.urb1[kvw$nn.idx[,k],"x"]-xF)^2+(mat.urb1[kvw$nn.idx[,k],"y"]-yF)^2 # distance from W to the k-th flammable neighbor F of V
        d2WF<-(xW-xF)^2+(yW-yF)^2 # distance from W to the k-th flammable neighbor F of V
        
        res<-idxneigh(mat=mat.urb1,idxviz=kvw$nn.idx[,j],IN="u")
        xWW<-mat.urb1[res$idxnext,"x"]; yWW<-mat.urb1[res$idxnext,"y"]; 
        xWWW<-mat.urb1[res$idxprev,"x"]; yWWW<-mat.urb1[res$idxprev,"y"]; 
        
        # # Dec 2018: determine WW=vertice next to W (if any)
        # #xWW<-yWW<-rep(NA,nrow(mat.urb1))
        # xWW<-mat.urb1[(kvw$nn.idx[,j] < nrow(mat.urb1)) * (kvw$nn.idx[,j]+1) + (kvw$nn.idx[,j] == nrow(mat.urb1)) ,"x"] 
        # yWW<-mat.urb1[(kvw$nn.idx[,j] < nrow(mat.urb1)) * (kvw$nn.idx[,j]+1) + (kvw$nn.idx[,j] == nrow(mat.urb1)) ,"y"] 
        # currentpart<-mat.urb1[kvw$nn.idx[,j],"idx.part.u"]
        # # assign 1 to when (kvw$nn.idx[,j]==nrow(mat.urb1)); 
        # # then existnext==FALSE for those vertices
        # nextpart<- mat.urb1[(kvw$nn.idx[,j] < nrow(mat.urb1)) * (kvw$nn.idx[,j]+1) + (kvw$nn.idx[,j] == nrow(mat.urb1)) ,"idx.part.u"] 
        # existnext<- (currentpart==nextpart)
        # existnext[length(existnext)]<-FALSE # to avoid error with mat.urb1[which(existnext)+1,c("x","y")]
        # existnext[is.na(existnext)]<-FALSE
        # xWW[!existnext]<-yWW[!existnext]<-NA
        #  
        # # July 2019: determine WWW=vertice previous to W (if any)
        # xWWW<-mat.urb1[(kvw$nn.idx[,j] > 1) * (kvw$nn.idx[,j]-1) + (kvw$nn.idx[,j] == 1) ,"x"] 
        # yWWW<-mat.urb1[(kvw$nn.idx[,j] > 1) * (kvw$nn.idx[,j]-1) + (kvw$nn.idx[,j] == 1) ,"y"] 
        # currentpart<-mat.urb1[kvw$nn.idx[,j],"idx.part.u"]
        # # assign 1 to when (kvw$nn.idx[,j]==nrow(mat.urb1)); 
        # # then existnext==FALSE for those vertices
        # previouspart<- mat.urb1[(kvw$nn.idx[,j] >1) * (kvw$nn.idx[,j]-1) + (kvw$nn.idx[,j] == 1) ,"idx.part.u"] 
        # existprevious<- (currentpart==previouspart)
        # existprevious[length(existprevious)]<-FALSE # to avoid error with mat.urb1[which(existnext)+1,c("x","y")]
        # existprevious[is.na(existprevious)]<-FALSE
        # xWWW[!existprevious]<-yWWW[!existprevious]<-NA
        
        # update protected
        #isprotected <- decision(idxF,idxW=kfw$nn.idx[,j],d2VF,d2VW,d2WF,limiar,limiartheta)  # TRUE if V is "protected by W" wrt to F
        isprotected <- decision(D,limiar,limiartheta,xV,yV,xF,yF,xW,yW,xWW,yWW,xWWW,yWWW)  # TRUE if V is "protected by W" wrt to F
        # xV=xV[id0];yV=yV[id0];xF=xF[id0];yF=yF[id0];xW=xW[id0];yW=yW[id0];xWW=xWW[id0];yWW=yWW[id0];xWWW=xWWW[id0];yWWW=yWWW[id0]
        
            # & 
            # xW[id0]>xV[id0] & xWW[id0]>xV[id0] & xWWW[id0]>xV[id0] &
            # yW[id0]< -100960 & yWW[id0]< -100960 & yWWW[id0]< -100960)# & j==3 & idxFviz ==3) #!protected[id0]) # &  lambda[id0] >0 & lambda[id0] <1) 
        if ( DRAWPOINTS) # !isprotected[id0] &
        {
          text(xW[id0],yW[id0],"W",pos=1,cex=0.8, col="red")
          text(xWW[id0],yWW[id0],"WW",pos=2,cex=0.8, col="red")
          text(xWWW[id0],yWWW[id0],"WWW",pos=3,cex=0.8, col="red")
          points(xF[id0],yF[id0],pch="O",cex=3,col="green")
          points(xV[id0],yV[id0],pch="O",cex=3,col="gray")
          
        }
        
        protected<-protected | isprotected
        
        #if (!decision(limiar,limiartheta,xV,yV,xF,yF,xW,yW,xWW,yWW,xWWW,yWWW)[id0]) {KK<-k; IDXFVIZ<-idxFviz; J<-j;XV<-xV; YV<-yV; XF<-xF; YF<-yF; XW<-xW; YW<-yW; XWW<-xWW; YWW<-yWW;XWWW<-xWWW; YWWW<-yWWW}
        
      }
      
      ########################################################################################
      # GROUP 2 of potential protectors: KF Urban neighbors of selected flammable vertices
      # urban neighbors of selected flammable vertices (xF,yF)
      # nn2 does not accept NAs
      
      kfw<-nn2(data=mat.urb1[,c("x","y")],      # (GROUP 2 of potential protectors) KF Urban neighbors of selected flammable vertices
               query=cbind(xF,yF),
               k=KF,
               searchtype = "radius", radius=D)
      kfw$nn.idx[kfw$nn.idx==0]<-NA
      
      # Dec 2018: moved up
      # # decide if V is protected by some W, where W is the j-th Urban neighbor of idxF
      # protected<-rep(FALSE,nrow(mat.urb1))
      for (j in KFS) # cycle through URB neighbors of selected Flam vertices GROUP 2
      {
        if (!TESTIDX & j%%10==0) print(paste("GROUP2: iteration",j,"among urban neighbors of Flam neighbors of V"))
        # determine d2VW and d2WF, where W is the j-th Urban neighbor of V
        d2WF<-kfw$nn.dists[,j]^2
        xW<-mat.urb1[kfw$nn.idx[,j],"x"]
        yW<-mat.urb1[kfw$nn.idx[,j],"y"]
        
        
        #d2VW<-(mat.urb1[kfw$nn.idx[,j],"x"]-mat.urb1[,"x"])^2+(mat.urb1[kfw$nn.idx[,j],"y"]-mat.urb1[,"y"])^2
        d2VW<-(xW-xV)^2+(yW-yV)^2 # Dec 2018
        
        # julho 2019
        res<-idxneigh(mat=mat.urb1,idxviz=kfw$nn.idx[,j],IN="u")
        xWW<-mat.urb1[res$idxnext,"x"]; yWW<-mat.urb1[res$idxnext,"y"]; 
        xWWW<-mat.urb1[res$idxprev,"x"]; yWWW<-mat.urb1[res$idxprev,"y"]; 
        
      
        # update protected
        #isprotected <- decision(idxF,idxW=kfw$nn.idx[,j],d2VF,d2VW,d2WF,limiar,limiartheta)  # TRUE if V is "protected by W" wrt to F
        isprotected <- decision(D,limiar,limiartheta,xV,yV,xF,yF,xW,yW,xWW,yWW,xWWW,yWWW)  # TRUE if V is "protected by W" wrt to F
        
        if ( DRAWPOINTS ) #!isprotected[id0] &
        {
          text(xW[id0],yW[id0],"W",pos=1,cex=0.8, col="red")
          text(xWW[id0],yWW[id0],"WW",pos=2,cex=0.8, col="red")
          text(xWWW[id0],yWWW[id0],"WWW",pos=3,cex=0.8, col="red")
          points(xF[id0],yF[id0],pch="O",cex=3,col="green")
          points(xV[id0],yV[id0],pch="O",cex=3,col="gray")
          
        }
          
        protected<-protected | isprotected
        
        #if (!decision(limiar,limiartheta,xV,yV,xF,yF,xW,yW,xWW,yWW,xWWW,yWWW)[id0]) {KK<-k; IDXFVIZ<-idxFviz; J<-j;XV<-xV; YV<-yV; XF<-xF; YF<-yF; XW<-xW; YW<-yW; XWW<-xWW; YWW<-yWW;XWWW<-xWWW; YWWW<-yWWW; stop()}
        
        
      }
      
      if (DRAWSEGMENTS) #!protected[id0]) # &  lambda[id0] >0 & lambda[id0] <1) 
      {
        if (protected[id0]) lines(c(xV[id0],xF[id0]), c(yV[id0],yF[id0]), col="black", lty=idxFviz)
        if (!protected[id0]) lines(c(xV[id0],xF[id0]), c(yV[id0],yF[id0]), col="red", lty=idxFviz)
        
        # cond <- (xV-x0y0[1])^2+(yV-x0y0[2])^2 < 20000
        # plot(xV[cond],yV[cond],pch=".",cex=2)
        # cond <- (xF-x0y0[1])^2+(yF-x0y0[2])^2 < 50000
        # points(xF[cond],yF[cond],pch=".",col="green", cex=3)
        # points(XV[id0],YV[id0], pch="+")
        # points(XF[id0],YF[id0], pch="o", col="green")
        # points(XW[id0],YW[id0], pch="o", col="blue")
        # points(XWW[id0],YWW[id0], pch="o", col="red")
        # points(XWWW[id0],YWWW[id0], pch="o", col="red")
        # print(paste(KK, IDXFVIZ, J))
        # print(cbind(xV,yV,xFback,yFback,xFF,yFF,xFFF,yFFF,xW,yW,xWW,yWW,xWWW,yWWW)[id0,])
        # decision(limiar,limiartheta,XV,YV,XF,YF,XW,YW,XWW,YWW,XWWW,YWWW)[id0]
        # stop()
      }
      
      # set2019: define new variables d2VF, azVF and idxVF that are updated to depend on the closest neighbor among F,FF,FFF
      d2VFcurrent<-((xV-xF)^2+(yV-yF)^2); #if ( !protected[id0] & d2VF[id0]<dF[id0]^2 ) 
      azVFcurrent<-azimuthVF(xV=xV,yV=yV,xF=xF,yF=yF) #,protected)
      idxVFcurrent<-idxfeatF
      
      if (idxFviz==1) 
      {
        d2VF<-d2VFcurrent
        azVF<-azVFcurrent
        idxVF<-idxVFcurrent
      }
      if (idxFviz>1) 
      {
        idxVF<- (d2VFcurrent < d2VF) * idxVFcurrent + (d2VFcurrent >= d2VF) * idxVF
        azVF <- (d2VFcurrent < d2VF) * azVFcurrent  + (d2VFcurrent >= d2VF) * azVF
        d2VF <- (d2VFcurrent < d2VF) * d2VFcurrent  + (d2VFcurrent >= d2VF) * d2VF
      }
      #print(cbind(xV,yV,xF,yF,xFback,yFback,xFF,yFF,xFFF,yFFF,xW,yW,xWW,yWW,xWWW,yWWW)[id0,])
      #print(paste(k,j, idxFviz, d2VF[id0], dF[id0]^2))

      threetimesprotected <- threetimesprotected & protected
    } # end for (idxFviz in 1:3) 
    
    # notinterface will be FALSE if V is not protected from its k-th F-neighbor
    
    #28ago2019: determinar azimute V-F
    # azVF<-azimuthVF(xV=xV,yV=yV,xF=xF,yF=yF)
    # d2VF<-((xV-xF)^2+(yV-yF)^2) # new: before d2VF was given just by nn.dists from the original xF,yF
    
    # 28ago2019: do like dF to set indF from current idxfeatF, and azF from current azVF 
    iF<- (threetimesprotected) * iF +  (!threetimesprotected) * ((d2VF<dF^2) * idxVF     + (d2VF>=dF^2) * iF  )  # NEW
    # does not always work:
    # aZFplus will store the az of the farest non protected F (to determine side of F for interface)
    # condplus<- (d2VF>0 & d2VF > dFplus^2) 
    # azFplus<- (threetimesprotected) * azFplus +  (!threetimesprotected) * (condplus * azVF + (!condplus) * azFplus  )  # NEW
    azF<- (threetimesprotected) * azF + (!threetimesprotected) * ((d2VF<dF^2) * azVF      + (d2VF>=dF^2) * azF  )  # NEW
    # dF <- (protected) * dF +  (!protected) * ((d2VF<dF^2) * sqrt(d2VF) + (d2VF>=dF^2) * dF  )  # NEW
    # dFplus<- (threetimesprotected) * dFplus +  (!threetimesprotected) * ((d2VF>dFplus^2) * sqrt(d2VF) + (d2VF<=dFplus^2) * dFplus  )  # NEW
    dF<- (threetimesprotected) *  dF +  (!threetimesprotected) * ((d2VF<dF^2) *sqrt(d2VF) + (d2VF>=dF^2) * dF  )  # NEW
    # e.g. if in direct interface, d2VF~0 and protected=FALSE, notinterface becomes FALSE 
  
    notinterface<-notinterface & threetimesprotected
  } # end for (k in KS) -- cycle through K FLAM neighbors of urban vertice 
  
  interface<- !notinterface
  
  # additional restriction
  # if there are no F-neighbors then V is excluded from the interface
  interface[is.na(knn$nn.idx[,1])]<-FALSE
  
  # the vertices identified earlier as being part of the direct interface must be preserved.
  #if (exists("tobeinterface")) {interface[tobeinterface]<-TRUE; knn$nn.dists[tobeinterface,1]<-0}
  
  #if (!TESTIDX) save(interface,dF,file=paste0("interface_",extraname,"K",K,"_KF",KF,"_limiar",round(limiar*100),"_maxdist",MAXDIST,".RData"))  
  if (!TESTIDX) save(interface,dF,azF,iF,azFplus, dFplus ,file=file.path(FOLDER,paste0(FICHNAME,".RData")))  
  
}

if (!CREATE.INTERFACE & !TESTIDX & length(fichs)>0) load(fichs[1])

################################################# select interface and add features
# if densify: (but this is not done anymore)
if (MAXDIST>0) {interface[tobeinterface]<-TRUE; dF[tobeinterface]<-0} #NEW

# add Flamm feature idx (closest neighbor)
xyd<-data.table(x=mat.urb1[,"x"], y=mat.urb1[,"y"],
                idx.part.u=mat.urb1[,"idx.part.u"],
                idx.feat.u=mat.urb1[,"idx.feat.u"],
                idx.vert.u=mat.urb1[,"idx.vert.u"],
                vert.type=ftype(dF,D), # dF<-rep(POSVALUE,nrow(mat.urb1)) by default, dF==9999
                idx.feat.f=mat.Flamm[knn$nn.idx[,1],"idx.feat.f"], # closest flam feature; index of the features from flam
                dist.feat.f=knn$nn.dists[,1], # distance to closest flam feature
                #d=knn$nn.dists[,1],
                d=dF, # NEW
                az=azF, # 28ago2019
                # dplus=dFplus,
                # azFplus=azFplus,
                iF=iF, # 28ago2019, index of closest non protected Flam feature, can be NEGVALUE
                #areacos.f=flam$AreaCOS[mat.Flamm[knn$nn.idx[,1],"idx.feat.f"]], # other attributes from flam
                #fidcos.f=flam[[FIDFLAM]][mat.Flamm[knn$nn.idx[,1],"idx.feat.f"]],
                #idflam=flam$idflam[mat.Flamm[knn$nn.idx[,1],"idx.feat.f"]],
                interface=as.integer(interface))
                #protected=protected)

# replace POSVALUE por NEGVALUE for distances d=dF
xyd[d==POSVALUE,d:=NEGVALUE]
xyd[is.na(iF),iF:=NEGVALUE] # since iF can be NA

# 28ago2019: add distances of segments
xydL<-xyd[3:nrow(xyd)]
colnames(xydL)<-paste0(colnames(xyd),".L")
xydR<-xyd[1:(nrow(xyd)-2)]
colnames(xydR)<-paste0(colnames(xyd),".R")
xydDT<-cbind(xyd[2:(nrow(xyd)-1)],xydL,xydR)
xydDT<-xydDT[order(idx.vert.u)]
# determine length of edges
xydDT[,lengthL:=sqrt((x.L-x)^2+(y.L-y)^2)]
xydDT[,lengthR:=sqrt((x.R-x)^2+(y.R-y)^2)]
xydDT[idx.part.u!=idx.part.u.L,lengthL:=NEGVALUE]
xydDT[idx.part.u!=idx.part.u.R,lengthR:=NEGVALUE]
# azimuth of segments 
xydDT[,azimuthL:=azimuthVF(x,y,x.L,y.L)]
xydDT[,azimuthR:=azimuthVF(x,y,x.R,y.R)]
xydDT[idx.part.u!=idx.part.u.L,azimuthL:=NEGVALUE]
xydDT[idx.part.u!=idx.part.u.R,azimuthR:=NEGVALUE]
# determine when segments start/end
xydDT[,linkR:= (interface | interface.R) & (idx.part.u==idx.part.u.R) & (abs(idx.vert.u-idx.vert.u.R)<=1)] # same part and successive vertex
xydDT[,linkL:= (interface | interface.L) & (idx.part.u==idx.part.u.L) & (abs(idx.vert.u-idx.vert.u.L)<=1)] # same part and successive vertex
# sequences 0/1 and segment numbering
xydDT[,steplinkL:=c(diff(as.integer(linkL)),0)] # same part and successive vertex
xydDT[,segmentL:=cumsum((steplinkL>=0) * steplinkL)]
xydDT[,steplinkR:=c(0,diff(as.integer(linkR)))] # same part and successive vertex
xydDT[,segmentR:=1+cumsum((steplinkR<=0) * abs(steplinkR))]
# remove segment numbers when not interface
xydDT[(!interface & !interface.R),segmentR:=NEGVALUE]
xydDT[(!interface & !interface.L),segmentL:=NEGVALUE]
xydDT[,azsegmentL:=NEGVALUE]; 


paste0(colnames(xydDT),collapse="','")
# variables to keep
VARS<-c('idx.feat.u','x','y','idx.part.u','idx.vert.u','vert.type','idx.feat.f','dist.feat.f','d','az','iF','interface',
        'linkL','linkR','lengthL','lengthR','segmentL','segmentR','azimuthL','azimuthR') # 'dplus','azFplus',
xydDT<-xydDT[,VARS,with=FALSE]

if (ADDVAR) xydDT<-merge(xydDT,newtable,by="idx.feat.u", all.x=TRUE) # also brings in newvar2 if any
if (ADDFLAMVAR)  xydDT<-merge(xydDT,flamtable,by="idx.feat.f",all.x=TRUE) # also brings in newflamvar2 if any

# 8 set 2019:errado: iF é o índice da feature de F mais próxima if not protected
# merge(xydDT,as.data.table(mat.Flamm[,c("x", "y", "idx.vert.f")]),
#       by.x="iF",by.y="idx.vert.f",suffixes=c("","iF"),all.x=TRUE)

# create point sf for outut with all relevant information on each urban vertex
interface.pts<-xydDT[order(idx.vert.u)]#[interface==1]
interface.pts$idx.part.u<-NULL
interface.pts$idx.vert.u<-NULL
if (ADDVAR) {interface.pts[[NEWVAR]]<-interface.pts$newvar; interface.pts$newvar<-NULL}
if (ADDVAR2) {interface.pts[[NEWVAR2]]<-interface.pts$newvar2; interface.pts$newvar2<-NULL}
if (ADDFLAMVAR) {interface.pts[[NEWFLAMVAR]]<-interface.pts$newflamvar; interface.pts$newflamvar<-NULL}
if (ADDFLAMVAR2) {interface.pts[[NEWFLAMVAR2]]<-interface.pts$newflamvar2; interface.pts$newflamvar2<-NULL}

#OUTNAME<-paste0(FICHNAME,".gpkg")
OUTNAME<-paste0(FICHNAME,".txt")
# interface.pts.sf<-st_as_sf(interface.pts, coords=c("x","y"),crs = st_crs(urb)$proj4string) #<--------------------- point sf with attributes for points (no segment, though)
# if (!TESTIDX) st_write(interface.pts.sf,file.path(FOLDER,paste0("points-",OUTNAME)),delete_layer=TRUE)
if (!TESTIDX) fwrite(interface.pts,file.path(FOLDER,paste0("points-",OUTNAME)))



##############################################################################  mapview
cores<-c("red","yellow","blue","black")
# mapa
if (TESTIDX)
{
  interface.pts.sf<-st_as_sf(interface.pts, coords=c("x","y"),crs = st_crs(urb)$proj4string) #<--------------------- point sf with attributes for points (no segment, though)
  
  cores<-c("red","yellow","blue","black")
  m<-mapview(st_transform(urb,4326),col.regions="white")
  m<-m+mapview(st_transform(flam,4326),col.regions="light green")
  m+st_transform(interface.pts.sf,4326)
  #addFeatures(m,st_transform(fastint,4326),zcol="vert.type",color=cores[fastint$vert.type])
}

