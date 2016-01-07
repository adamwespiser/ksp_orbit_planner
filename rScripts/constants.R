

#https://docs.google.com/spreadsheets/d/1HwrFq6r2Wfzvghq8VYMNx0F2rG6nYMAEl_cyLj2EXjA/edit#gid=0
constants_file <<- "./data/ksp_params.csv"

readInParams <- function(param_file=constants_file){
		col_read <- read.csv(file = param_file, nrows=2,stringsAsFactors=FALSE,header=FALSE)
		col_titles <- tolower(paste(as.character(unlist(col_read[1,])),c("-in-"),as.character(unlist(col_read[2,])),sep=""))
		
		df <- read.csv(file = param_file, stringsAsFactors=FALSE,header=FALSE,skip=2,comment.char="#")
		colnames(df) <- col_titles
		# TODO
		# convert char cols to numeric
		# make first two cols into one, add designation of planet or moon
		return df
}


#helper function-> Current Planet SOI -> planet data
#define orbits from a,v,sigma



Kerbin_GM = 3.5316000*(10^12) #m^3/s^2
Kerbin_SOI_radius = 84159286 #meters
Kerbin_atm_scale_height = 5600 # meters
Kerbin_sync_orbit =     2868.75  * pow(10,3)
Kerbin_radius = 600 * pow(10,3)
KR = Kerbin_radius

#local
Moho <- list(GM = 1.6860938*(10^11),
						 SOI = 9646663.0)
Eve <- list(GM = 8.1717302*(10^12),
						SOI = 85109365,
						atm_height = 90000)
Kerbin <- list(GM  = 3.5316000*(10^12),
							 SOI = 84159286,
							 atm_height = 70000)
Duna <- list(GM = 3.0136321*(10^11),
						 SOI = 47921949,
						 atm_height = 50000)

