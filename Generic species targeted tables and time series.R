# Replicating the travel distance code with updated files. 

#Need to create efficient codes for:
#	-Top 12 species targeted tables: overall, in county, out of county, out of state
#	-Time series of proportion targeting for top X species (matrix plot & individual plots)
#	-Tables of top origins of out-of county trips (instate, so counties), and out of state, states
#	-Maps of overall trips



rm(list=ls())
library(sjPlot)
library(Hmisc)
library(dplyr)


##############################################################################
### DATA ###
##############################################################################
	setwd("U:/UF/NMFS MRIP/MRIP data/sas output/trips only")
	#setwd("C:/Users/edvcamp/Desktop/temp mrip files sept 2018/trips")
	dat <- read.csv("FL_2017_trips_corrected.csv", header=T)


##############################################################################
### CONTROL PANEL ###
##############################################################################
	cntname <- "Charlotte"					#County name
	cntcode <- "15"							#Couunty fips code
	cntreg <- c("81", "115", "15", "71")  	#Charlotte Harbor counties fips codes
	cntreg_name <- "Charlotte Harbor"		#Name for plotting
	stcnty <- "12_15"						#state_county codes
	coast <- "7"  							#this corresponds to "sub_reg" in the data file, 7= gulf, 6= atl

	#Potential Regions for subsetting by broader region
		bigbend <- c("37", "129", "65", "123", "29", "75", "17", "53", "101")
		pan <- c("33", "113", "91", "31", "13", "45")
		westcen <- c("103", "57", "81", "115", "15", "71")
		NW <- c("37", "129", "65", "123", "29", "75", "17", "53", "101", "33", "113", "91", "31", "13", "45", "103", "57", "81", "115", "15", "71")

		#	years <- ... 						# !! May want to filter by years eventually !!


##############################################################################
### DATA MANIPULATIONS ###
##############################################################################
	names(dat) <- tolower(names(dat))
	dat1 <- dat[,c("prim2_common", "prim1_common", "strat_id", "psu_id", "year", "reg_res", "st_res", "cnty_res", "st" , "cnty", "intsite" , "mode_f", "mode_fx", "area", "area_x",
	        "hrsf", "id_code", "sub_reg", "wave", "catch", "boat_hrs", "month", "kod", "county", "wp_int", "var_id")]

	dat1$cntyf <- as.factor(as.character(dat1$cnty)) 
	dat1$st_cnty <- as.factor(as.character(paste(dat1$st, dat1$cntyf, sep="_")))
	dat1$st_cnty_res <- as.factor(as.character(paste(dat1$st_res, dat1$cnty_res, sep="_")))
	dat1$prim1_common <- capitalize(tolower(dat1$prim1_common))
	levels(dat1$prim1_common) <- c(levels(dat1$prim1_common), "No Target")
	dat1$prim1_common[which(dat1$prim1_common =="")] <- "No Target"


##############################################################################
### FUNCTIONS / MODELS ###
##############################################################################
	fun <- function(df)
		{
		dat1 <- df
		### All trips (including no target)
			yr <- aggregate(x=list(all_trips=dat1$id_code), by=list(year=dat1$year), FUN=length) #total trips sampled per year
			tot_trips <- sum(yr$all_trips) #total trips sampled in all years
			spec <- aggregate(x=list(trips=dat1$id_code), by=list(target=dat1$prim1_common), FUN=length) #total trips with prim1 target 
			spec$prop <- round(spec$trips/tot_trips, 3) #proportions
			spec2 <- spec[order(-spec$prop),]	#order by top targeted

		### Targeted trips (excluding no targets)
			notarg <- filter(spec, target=="No Target")
			tot_trips_notarg <- tot_trips - notarg$trips
			spec1 <- aggregate(x=list(trips=dat1$id_code), by=list(target=dat1$prim1_common), FUN=length)
			spec_notarg <- filter(spec1, !target=="No Target")
			spec_notarg$prop_targ <- round(spec_notarg$trips/tot_trips_notarg, 3)
			spec_notarg2 <- spec_notarg[order(-spec_notarg$prop_targ),]	

		### Bringing together
			prop_targ <- cbind(spec2[1:15,c(1,3)], spec_notarg2[1:15,c(1,3)])
			prop_targ
		}


	fun_ts <- function(species, region, reg_name)
		{
		# Filter by spatial region
			df <- filter(dat1, dat1$cntyf %in% region)

		# Aggregate by YEAR and SPECIES
			yr <- aggregate(x=list(all_trips=df$id_code), by=list(year=df$year), FUN=length)
			spec_yr <- aggregate(x=list(trips=df$id_code), by=list(target=df$prim1_common, year=df$year), FUN=length)
			notarg <- filter(spec_yr, target=="No Target")
				colnames(notarg) <- c("target", "year", "notarg_trips")
			yr_targ <- left_join(x=yr, y=notarg[,2:3], by=c("year"))
			yr_targ$targ_trips <- yr_targ$all_trips-yr_targ$notarg_trips

			spec_yr_prop <- left_join(x=spec_yr, y=yr_targ, by=c("year"))
			spec_yr_prop$prop_all <- spec_yr_prop$trips/spec_yr_prop$all_trips  #proportion of all trips
			spec_yr_prop$prop <- spec_yr_prop$trips/spec_yr_prop$targ_trips		#proportion of targeted trips (i.e. excluding "No Target" trips)
			
		# Plot 
			spec_df <- filter(spec_yr_prop, target==species)
			plot(spec_df$year, spec_df$prop_all, type="l", ylab="Proportion trips targeting", xlab="Year", cex.lab=1.5,
				main=paste(reg_name, " ", species, " trips", sep=""), col="dodgerblue3", lwd=2, cex.main=2)
			#text(quantile(spec_df$year, .15), quantile(spec_df$prop, .97), expression(paste(species, " (", italic("Big Bend"), ")", sep="")), cex=1.5)
			#text(quantile(spec_df$year, .15), quantile(spec_df$prop_all, .97), paste(species, " (reg_name)", sep=""), cex=1.5)
			#text(quantile(spec_df$year, .10), quantile(spec_df$prop_all, .99), species, cex=1.5)
			mtext(expression(italic("Source: NMFS MRIP Trips data")), side=1, line=3, adj=1, cex=1, outer=FALSE)
		}


	fun_ts1 <- function(species, region, reg_name, species_prop)
		{
		# Filter by spatial region
			df <- filter(dat1, dat1$cntyf %in% region)

		# Aggregate by YEAR and SPECIES
			yr <- aggregate(x=list(all_trips=df$id_code), by=list(year=df$year), FUN=length)
			spec_yr <- aggregate(x=list(trips=df$id_code), by=list(target=df$prim1_common, year=df$year), FUN=length)
			notarg <- filter(spec_yr, target=="No Target")
				colnames(notarg) <- c("target", "year", "notarg_trips")
			yr_targ <- left_join(x=yr, y=notarg[,2:3], by=c("year"))
			yr_targ$targ_trips <- yr_targ$all_trips-yr_targ$notarg_trips

			spec_yr_prop <- left_join(x=spec_yr, y=yr_targ, by=c("year"))
			spec_yr_prop$prop_all <- spec_yr_prop$trips/spec_yr_prop$all_trips  #proportion of all trips
			spec_yr_prop$prop <- spec_yr_prop$trips/spec_yr_prop$targ_trips		#proportion of targeted trips (i.e. excluding "No Target" trips)
			
		# Plot 
			spec_df <- filter(spec_yr_prop, target==species)
			plot(spec_df$year, spec_df$prop_all, type="l", ylab="", xlab="", 
				main="", col="dodgerblue3", lwd=2, cex.main=2)
			#text(quantile(spec_df$year, .20), quantile(spec_df$prop_all, .99), species, cex=1.5)
			mtext(paste(species_prop, sep=""), side=3, line=0, adj=0, cex=1, outer=FALSE)
		}


##############################################################################
### REPORT ###
##############################################################################
	setwd("U:/UF/Extension specific/FL Sea Grant/fish data program/Charlotte")


### Tables ###################################################################
	###These are just general tables that are not county specific
		#All FL trips all years
			all_fl_trips <- fun(dat1)
			all_fl_trips
			# setwd("U:/UF/Extension specific/FL Sea Grant/fish data program/Lee & Charlotte")
			# tab_df(all_fl_trips, title="Proportion species targeted, All FL trips", use.viewer=TRUE, show.rownames=FALSE, file="all_fl_trips.doc")

		#All Gulf Trips
			dat1_gulf <- filter(dat1, sub_reg=coast)
			all_gulf_trips <- fun(dat1_gulf)
			all_gulf_trips
			# tab_df(all_gulf_trips, title="Proportion species targeted, All FL Gulf trips", use.viewer=TRUE, show.rownames=FALSE, file="all_gulf_trips.doc")

		#Out of State Trips
			oos <- filter(dat1, !dat1$st_res==12)
			oos_trips <- fun(oos)
			oos_trips

	### Area-specific Trips
		#Region, all time
			reg <- filter(dat1, dat1$cntyf %in% cntreg) #c("81", "115", "15", "71"))
			reg_trips <- fun(reg)
			reg_trips
				tab_df(CH_trips, title="Proportion species targeted, All Charlotte Harbor trips", use.viewer=TRUE, show.rownames=FALSE, file="pan_trips.doc")

		#County, all origins, all years
			county <- filter(dat1, dat1$cntyf %in% cntcode)
			county_trips <- fun(county)
			county_trips
				tab_df(county_trips, title="Proportion species targeted, Charlotte county trips", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_county_trips.doc")

		#County, in-county origin, all years
			st_cnty_list <- c(stcnty) #c("12_15")   #These are the st county combos for big bend
			incounty <- filter(dat1, dat1$st_cnty %in% st_cnty_list & dat1$st_cnty_res %in% st_cnty_list)     		#which trips were intercepted here where residences were not from here
			incounty_trips <- fun(incounty)
			incounty_trips
				tab_df(incounty_trips, title="Proportion species targeted, Charlotte in-county trips", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_incounty_trips.doc")

		#County, out-of-county origin, all years
			st_cnty_list <- c(stcnty) #c("12_15")   #These are the st county combos for big bend
			oocounty <- filter(dat1, dat1$st_cnty %in% st_cnty_list & !dat1$st_cnty_res %in% st_cnty_list)     		#which trips were intercepted here where residences were not from here
			oocounty_trips <- fun(oocounty)
			oocounty_trips
				tab_df(oocounty_trips, title="Proportion species targeted, Charlotte out-of-county trips", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_oocounty_trips.doc")

		#County, out-of-state origin, all years
			oostcounty <- filter(dat1, dat1$cntyf %in% c(cntcode) & !dat1$st_res =="12")     		#which trips were intercepted here where residences were not from here
			oostcounty_trips <- fun(oostcounty)
			oostcounty_trips
				tab_df(oostcounty_trips, title="Proportion species targeted, Charlotte out-of-state trips", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_oostcounty_trips.doc")

	### Summary tables
		#Comparison to rest of state, gulf, region,
			st_comp <- cbind (all_fl_trips[,1:2], all_gulf_trips[,1:2], reg_trips[,1:2], county_trips[,1:2])
			colnames(st_comp) <- c("FL Trips Target", "Proportion", "FL Gulf trips", "Proportion", "Char. Har. trips", "Proportion", "Charlotte county trips", "proportion")
			st_comp
				tab_df(st_comp, title="Proportion species targeted, Charlotte state comparison", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_st_comp.doc")

			cnty_comp <- cbind (county_trips[,1:2], incounty_trips[,1:2], oocounty_trips[,1:2], oostcounty_trips[,1:2]) 
			colnames(cnty_comp) <- c("Charlotte county trips", "Proportion", "In-county trips", "Proportion", "Out-of-county trips", "Proportion", "Out-of-state trips", "proportion")
			cnty_comp
				tab_df(cnty_comp, title="Proportion species targeted, Charlotte county comparison", use.viewer=TRUE, show.rownames=FALSE, file="Charlotte_cnty_comp.doc")


### Figures ######################################################################
	#County-region trends in targeting
		#Getting species names from regional table to run the time series
		speclist <- reg_trips[1:9,1] 

		#Making list of species name and proportion for labeling
		specprop_list=vector(length=9) 
			for(i in 1:length(specprop_list)){
			specprop_list[i] = paste(reg_trips[i,1], " (", (100*reg_trips[i,2]), "%)", sep="")
			}
			
	#Single Panel Plots
		par(mfrow=c(1,1))
		for(i in 1:length(speclist))
		{
			pdf(paste("U:/UF/Extension specific/FL Sea Grant/fish data program/Charlotte/", speclist[i], "_TS.pdf", sep=""), width=10, height=7)
				fun_ts(species=speclist[i], region=cntreg, reg_name=cntreg_name)
			dev.off()
		}

	#Multi-Panel Plot 
	pdf("U:/UF/Extension specific/FL Sea Grant/fish data program/Charlotte/prop_targeting_TS_top9.pdf", width=10, height=7)
		par(mfrow=c(3,3), 
		    mgp = c(2.5, .5, 0), 		#mex lines for axis title [1], axis labels and lines [2:3], default is  c(3,1,0). use lower values to move axis titles (e.g. "x") and labels (eg "40") close to tick marks
		     mar=c(3,2,1,1), 			#margin lines (bot, left, top, right). Default (5,4,4,2).  Use (5,4,2,2) or similar to bring chart title (e.g., redfish) towards top of chart.
		     oma=c(2,3,3,1))  			#outside margins in lines, try (2,2,1,1) or (1,2,1,1) if there's too much white space at bottom of multipanel fig.

		for(i in 1:length(speclist)) {
			fun_ts1(species=speclist[i], region=cntreg, reg_name=cntreg_name, species_prop=specprop_list[i])
		}
			mtext(text="Year", side=1, line=0, outer=TRUE, cex=1.5)
			mtext(text="Proportion intercepted trips targeting",side=2,line=1,outer=TRUE, cex=1.5)
			mtext(text=cntreg_name,side=3,line=1,outer=TRUE, cex=1.75)
			mtext(expression(italic("Source: NMFS MRIP Trips data")), side=1, line=0.5, adj=1, cex=1, outer=TRUE)
	dev.off()

