library(ncdf4)
library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tidyverse)

## GET DATA

### Getting global reconstructed catch data - these are from Watson & Tidd Marine Policy.

# read in total catch data
total_catch <- read.csv("catch_histsoc_1869_2017.csv")
# summarise total catch data
tc_sum_C6 <- total_catch %>%
  # filter(LME != "No LME") %>%
  group_by(Year)%>%
  summarise(tc = sum(Reported+IUU +Discards)) %>% filter(Year > "1949")  %>% filter(Year < "2015")
tc_sum_C6


### FUNCTION TO READ IN MODEL DATA

getGlobalCatchPerYear<-function(filename="boats_gfdl-esm4_nobasd_historical_histsoc_default_tc_global_monthly_1950_2014.nc",shape=NULL,year_index=Years,modelname="boats_gfdl_cmip6"){
  #get gridded monthly time series
  gridded_ts <- brick(file.path(filename))
  #replace missing values with 0
  gridded_ts[gridded_ts > 1e20]<-0
  if (!is.null(shape)) { 
    crs(gridded_ts) = crs(shape)
    temp <- crop(gridded_ts, extent(shape))
    gridded_ts <- mask(gridded_ts, shape)
  }
  #multiply each grid cell value by cell area and sum over all grid cells.
  #to multiply by area,  need to convert area from km2 to m2 (*1e6), and sum over all cells each month. 
  monthly<-cellStats(gridded_ts*(area(gridded_ts)*1e6), 'sum')  
  # convert form grams to tonnes
  monthly<-monthly/1e6 
  # convert to data.frame and combine with year index
  df_yr<-data.frame(Catch=monthly, Year= as.factor(Years)) 
  # sum over all months in each year to get units of tonnes per year, for each year    
  total<-df_yr%>%group_by(Year) %>% summarise(CatchPerYr=sum(Catch))
  # add a column to contain modelname for easier data plotting
  total$modelname<-modelname
  return(total)
}

####### FILENAMES FOR 3a

file1<-"/rd/gem/private/fishmip_outputs/ISIMIP3a/UploadArea/marine-fishery_global/FEISTY/gfdl-mom6_cobalt2_none_ctrlclim_histsoc_onedeg_bd90cm_global_monthly_1961_2010.nc"
file2<-"/rd/gem/private/fishmip_outputs/ISIMIP3a/UploadArea/marine-fishery_global/BOATS/gfdl-mom6_cobalt2_none_ctrlclim_histsoc_onedeg_bd90cm_global_monthly_1961_2010.nc"
file2<-"/rd/gem/private/fishmip_outputs/ISIMIP3a/obsclim/netcdf/dbpm_obsclim_nobasd_1deg_nat_default_tcb_global_monthly_1961_2010.nc"
gridded<-brick(file2)

gridded<- nc_open(file2,return_on_error=TRUE)

boats_obsclim_tc_yr<-getGlobalCatchPerYear(filename=file1,shape=NULL,year_index=Years,modelname ="boats_gfdl_cmip6")

boats_ctrlclim_tc_yr<-getGlobalCatchPerYear(filename="boats_ipsl-cm6a-lr_nobasd_historical_histsoc_default_tc_global_monthly_1950_2014.nc",shape=NULL,year_index=Years,modelname ="boats_ipsl_cmip6")

# depending on how you want to plot could edit the above function make more specific to have separate columns for MEM (boats or ecoocean) and ESM (gfdl or ipsl) and CMIP_round  (cmip5 or cmip6)


#### PLOTS

modeldata<-rbind(boats_gfdl_c6_tc_yr,boats_ipsl_c6_tc_yr)

boats_plot<-ggplot() +
  geom_line(data = modeldata, aes(x = Year, y = CatchPerYr/1e6, group = modelname, colour = modelname)) +
  geom_point(data = tc_sum_C6, aes(x = as.factor(Year), y = tc/1e6)) +
  theme_classic() + theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=.5, vjust=.5),
                          axis.text.y = element_text(colour="grey20", size=12),
                          text=element_text(size=16)) + 
  labs(x = 'Year',
       y = 'Million tonnes per year') 


ggsave("boats_globalcatch_timeseries.png", boats_plot, width=15, height=10)

