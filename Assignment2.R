#! /opt/local/bin/Rscript

#Block to load R packages 
library("data.table")
library("R.utils")


#Block to set working directory
CWD <- "~/Coursera/DataScience/Part5/RepData_PeerAssessment2"
setwd(CWD)

cacheData <- function(x, ...) {
  DT <- x$get_dt()
  if(!is.null(DT)) {
    return(DT)
  }
  data <- x$get()
  DT <- get_data_helper(File = data,  ...)
  x$set_dt(DT)
  return(DT)
}

read_dataCache <- function(file = character()) {
  DT <- NULL
  set <- function(y) {
    file <<- y
    DT <<- NULL
  }
  get <- function() {file}
  set_dt <- function(dt) {DT <<- dt }
  get_dt <- function() { DT }
  return (list (set = set, get = get, 
                set_dt = set_dt, get_dt = get_dt))
}

read_data <- function(File=character()) {
    DT <- fread(input = File)
    DT$EVTYPE <- tolower(DT$EVTYPE)
    return (DT)
}

# Read working file returns data.table. If file is absent in directory downloads it from web
get_data_helper <- function(File = character()) {
    if (file.exists(File) ){
        DT <- read_data(File) 
        return ( DT )
    }
    URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" # url to data
    DestFile <- paste( File, "bz2", sep = '.')
    download.file(url = URL, destfile = DestFile, quiet = T, method = "curl")
    dataDate <- date()
    cat( sprintf("Data Set was downloaded on %s\n", dataDate))
    bunzip2(DestFile)
    DT <- read_data(File) 
    return ( DT )
}

#Function assigns types of event try to fix spelling errors 
#Events of unknown types marked as other
#Some discretions are ambiguous such event could be marked as two events
#ALL EVENTS ARE CONVERTED IN UPPERCASE
assign_event_type <- function (DT) {
    # ASTRONOMICAL LOW TIDE
    set(DT, i = grep("astronomical low tide", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "ASTRONOMICAL LOW TIDE") 

    # AVALANCHE
    set(DT, i = intersect(grep("avalanche|avalance", DT$EVTYPE, ignore.case = F),
                          grep("snow", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "AVALANCHE") 
    set(DT, i = intersect(grep("avalanche|avalance", DT$EVTYPE, ignore.case = F),
                          grep("snow", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "AVALANCHE|BLIZZARD|HEAVY SNOW") 

    # BLIZZARD
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("snow|wind|storm", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "BLIZZARD") 
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("winter storm", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "BLIZZARD|WINTER STORM") 
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("storm", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "BLIZZARD|ICE STORM") 
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("freezing rain", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "BLIZZARD|WINTER WEATHER") 
    set(DT, i = grep("high wind/wind chill/blizzard", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "BLIZZARD|HIGH WIND|COLD WIND/CHILL") 
    set(DT, i = grep("high wind/blizzard/freezing ra", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "BLIZZARD|HIGH WIND|WINTER WEATHER") 
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("heavy snow", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "BLIZZARD|HEAVY SNOW") 
    set(DT, i = intersect(grep("blizzard", DT$EVTYPE, ignore.case = F),
                          grep("high wind", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "BLIZZARD|HIGH WIND") 
    set(DT, i = grep("blizzard and extreme wind chil", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "BLIZZARD|EXTREME COLD/WIND CHILL")

    # COASTAL FLOOD
    set(DT, i = grep("heavy surf coastal flooding", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "COASTAL FLOOD|HIGH SURF") 
    set(DT, i = grep("high winds/coastal flood", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "COASTAL FLOOD|HIGH WIND") 
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("coast|cstl|beach", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "COASTAL FLOOD") 
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("tidal", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "COASTAL FLOOD") 

    # COLD/WIND CHILL 
    set(DT, i = intersect(intersect(grep("chill", DT$EVTYPE, ignore.case = F),
                                    grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
                          grep("extre|snow|high", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "COLD/WIND CHILL") 
    set(DT, i = intersect(intersect(grep("chill", DT$EVTYPE, ignore.case = F),
                                    grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
                          grep("extre", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "EXTREME COLD/WIND CHILL") 
    set(DT, i = intersect(grep("chill", DT$EVTYPE, ignore.case = F),
                          grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "COLD/WIND CHILL|HIGH WIND") 
    set(DT, i = grep("cold/winds|unusually cold|cold wave|prolong cold|cold weather", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "COLD/WIND CHILL")
    set(DT, i = grep("high winds/cold", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "COLD/WIND CHILL|HIGH WIND")
    set(DT, i = grep("unusually cold|cold temperat|unseasonable cold|unseasonably cold", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "COLD/WIND CHILL")

    #DEBRIS FLOW
    set(DT, i = intersect(grep("slide", DT$EVTYPE, ignore.case = F),
                          grep("flood", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "DEBRIS FLOW") 
    set(DT, i = grep("heavy rain/mudslides/flood", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "DEBRIS FLOW|FLOOD|HEAVY RAIN") 
    set(DT, i = intersect(grep("slide", DT$EVTYPE, ignore.case = F),
                          grep("flood", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "DEBRIS FLOW|FLASH FLOOD") 

    #DENSE FOG
    set(DT, i = intersect(grep("fog|dense fog|vog", DT$EVTYPE, ignore.case = F),
                          grep("cold|ice|freez", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "DENSE FOG") 
    set(DT, i = intersect(grep("fog", DT$EVTYPE, ignore.case = F),
                          grep("cold|ice|freez", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FREEZING FOG") 

    #DENSE SMOKE
    set(DT, i = grep("smoke", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "DENSE SMOKE")

    #DROUGHT
    set(DT, i = intersect(grep("drought|driest month", DT$EVTYPE, ignore.case = F),
                          grep("heat", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "DROUGHT") 
    set(DT, i = intersect(grep("drought", DT$EVTYPE, ignore.case = F),
                          grep("excessive heat", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "DROUGHT|EXCESSIVE HEAT") 
    set(DT, i = grep("drought", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "DROUGHT|HEAT")
    set(DT, i = grep("drynes|dry weatheri|lack of snow|abnormally dry|below normal precipitation", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "DROUGHT")

    #DUST DEVIL
    set(DT,i = intersect(grep("dust devil|landspout|whirlwind", DT$EVTYPE, ignore.case = F),
                         grep("waterspout", DT$EVTYPE, ignore.case = F, invert = T)) ,
        j = 8L, value = "DUST DEVIL") 
    set(DT,i = intersect(grep("dust devil", DT$EVTYPE, ignore.case = F),
                         grep("waterspout", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "DUST DEVIL|WATERSPOUT") 

    #DUST STORM 
    set(DT,i = grep("dust", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "DUST STORM") 

    #EXCESSIVE HEAT
    set(DT,i = intersect(grep("heat", DT$EVTYPE, ignore.case = F),
                         grep("excessive|extre|record", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "EXCESSIVE HEAT") 

    #EXTREME COLD/WIND CHILL
    set(DT, i = grep("record cold and high wind", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "EXTREME COLD/WIND CHILL|HIGH WIND")
    set(DT, i = grep("record snow/cold", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "EXTREME COLD/WIND CHILL|HEAVY SNOW")
    set(DT, i = intersect(grep("cold", DT$EVTYPE, ignore.case = F),
                          grep("extreme|record|severe|extended|excessive", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "EXTREME COLD/WIND CHILL") 

    #FLASH FLOOD AND FLOOD
    set(DT, i = intersect(intersect(grep("flood|floood", DT$EVTYPE, ignore.case = F),
                                    grep("flash|minor|ice jam|breakup|local", DT$EVTYPE, ignore.case = F, invert = F)),
                          grep("rain|wind|storm", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "FLASH FLOOD") 
    set(DT, i = intersect(intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                                    grep("flash|urban", DT$EVTYPE, ignore.case = F, invert = F)),
                          grep("thunder", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLASH FLOOD|THUNDERSTORM WIND")
    set(DT, i = grep("thunderstorm winds/ flood|thunderstorm winds/flooding", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "FLOOD|THUNDERSTORM WIND")
    set(DT, i = intersect(grep("flash flood", DT$EVTYPE, ignore.case = F),
                          grep("heavy rain", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLASH FLOOD|HEAVY RAIN") 
    set(DT, i = intersect(grep("flood|small stream urban", DT$EVTYPE, ignore.case = F),
                          grep("heavy rain", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLOOD|HEAVY RAIN") 
    set(DT, i = grep("flash flood winds|small stream flood|ice jam|high water|rapidly rising water", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "FLASH FLOOD")
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("river|urban|watch|major|snowmelt|street|highway|rural|stream", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLOOD") 
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("lake", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "LAKESHORE FLOOD") 
    set(DT, i = grep("heavy snow/high winds & flood", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "FLOOD|HEAVY SNOW|HIGH WIND")
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLOOO|HIGH WIND") 
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("hail", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLASH FLOOO|HAIL") 
    set(DT, i = intersect(grep("flood", DT$EVTYPE, ignore.case = F),
                          grep("ice", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FLASH FLOOO|ICE STORM") 
    set(DT, i = grep("flood", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "FLOOD")
    set(DT, i = grep("urban/sml stream fld", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "FLASH FLOOD")

    #FROST/FREEZE 
    set(DT,  i = intersect(grep("frost|freez", DT$EVTYPE, ignore.case = F),
                           grep("rain|snow|driz|spray", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "FROST/FREEZE") 
    set(DT,  i = intersect(grep("frost|freez", DT$EVTYPE, ignore.case = F),
                           grep("snow|sleet", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "FROST/FREEZE|ICE STORM") 
    set(DT, i = intersect(grep("frost|freez", DT$EVTYPE, ignore.case = F),
                          grep("heavy|sleet", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "ICE STORM") 

    #FUNNEL CLOUD
    set(DT, i = intersect(grep("funnel|turbulence", DT$EVTYPE, ignore.case = F),
                          grep("hail|thunder|water", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "FUNNEL CLOUD") 
    set(DT, i = intersect(grep("funnel", DT$EVTYPE, ignore.case = F),
                          grep("thunder", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FUNNEL CLOUD|THUNDERSTORM WINDS") 
    set(DT, i = intersect(grep("funnel", DT$EVTYPE, ignore.case = F),
                          grep("hail", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FUNNEL CLOUD|HAIL") 
    set(DT, i = intersect(grep("funnel", DT$EVTYPE, ignore.case = F),
                          grep("waterspout", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "FUNNEL CLOUD|WATERSPOUT") 

    #FREEZING FOG

    #HAIL
    set(DT, i = intersect(grep("hail", DT$EVTYPE, ignore.case = F),
                          grep("marine|thunder|wind", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "HAIL") 
    set(DT, i = grep("tornadoes, tstm wind, hail", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "HAIL|THUNDERSTORM WIND|TORNADO")
    set(DT, i = grep("marine hail", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "MARINE HAIL")
    set(DT, i = intersect(grep("hail", DT$EVTYPE, ignore.case = F),
                          grep("thunder|tstm", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HAIL|THUNDERSTORM WIND") 
    set(DT, i = intersect(grep("hail", DT$EVTYPE, ignore.case = F),
                          grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HAIL|STRONG WIND") 

    #HEAT
    set(DT, i = grep("heat", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "HEAT")

    #HEAVY RAIN
    set(DT,i = intersect(grep("heavy rain|heavy shower", DT$EVTYPE, ignore.case = F),
                         grep("wind|snow|ligh|tstm|surf|stream", DT$EVTYPE, ignore.case = F, invert = T)) ,
        j = 8L, value = "HEAVY RAIN") 
    set(DT,i = intersect(grep("heavy rain", DT$EVTYPE, ignore.case = F),
                         grep("ligh", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY RAIN|LIGHTNING") 
    set(DT,i = intersect(grep("heavy rain", DT$EVTYPE, ignore.case = F),
                         grep("tstm|thunde", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY RAIN|THUNDERSTORM WIND") 
    set(DT,i = intersect(grep("heavy rain", DT$EVTYPE, ignore.case = F),
                         grep("wind", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY RAIN|HIGH WIND") 
    set(DT, i = grep("heavy rain/high surf", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "HEAVY RAIN|HIGH SURF")
    set(DT, i = grep("heavy rain/snow", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "HEAVY RAIN|HEAVY SNOW")

    #HEAVY SNOW
    set(DT,i = intersect(grep("heavy snow", DT$EVTYPE, ignore.case = F),
                         grep("high wind", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW|HIGH WIND") 
    set(DT,i = intersect(grep("heavy snow", DT$EVTYPE, ignore.case = F),
                         grep("wind", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW|STRONG WIND") 
    set(DT,i = intersect(grep("heavy snow", DT$EVTYPE, ignore.case = F),
                         grep("ice|freez", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW|ICE STORM") 
    set(DT,i = intersect(grep("heavy snow", DT$EVTYPE, ignore.case = F),
                         grep("winter", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW|WINTER STORM") 
    set(DT,i = intersect(grep("heavy snow", DT$EVTYPE, ignore.case = F),
                         grep("sleet", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW|SLEET") 
    set(DT, i = grep("heavy lake snow", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "LAKE-EFFECT SNOW")
    set(DT,i = intersect(grep("heavy|winter|snowfall", DT$EVTYPE, ignore.case = F),
                         grep("snow", DT$EVTYPE, ignore.case = F, invert = F)) ,
        j = 8L, value = "HEAVY SNOW") 

    #HIGH SURF
    set(DT,i = intersect(grep("surf|high tide|heavy seas|high seas|high waves|rogue", DT$EVTYPE, ignore.case = F),
                         grep("rip", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "HIGH SURF") 
    set(DT,i = intersect(grep("surf", DT$EVTYPE, ignore.case = F),
                         grep("rip", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HIGH SURF|RIP CURRENT") 

    #HIGH WIND 
    set(DT,i = intersect(grep("high wind", DT$EVTYPE, ignore.case = F),
                         grep("winter|snow", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HIGH WIND|WINTER STORM") 
    set(DT,i = intersect(grep("high wind", DT$EVTYPE, ignore.case = F),
                         grep("marine", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "MARINE HIGH WIND") 
    set(DT,i = intersect(grep("high wind", DT$EVTYPE, ignore.case = F),
                         grep("tides|seas", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HIGH WIND|STORM SURGE/TIDE") 
    set(DT,i = intersect(grep("high wind", DT$EVTYPE, ignore.case = F),
                         grep("hurricane", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HURRICANE/TYPHOON") 
    set(DT,i = intersect(grep("high", DT$EVTYPE, ignore.case = F),
                         grep("wind", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HIGH WIND") 

    #HURRICANE/TYPHOON 
    set(DT, i = grep("hurr|typ", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "HURRICANE/TYPHOON")

    #ICE STORM
    set(DT, i = grep("ice/strong winds", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "ICE STORM|STRONG WIND")
    set(DT, i = grep("ice|sleet storm", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "ICE STORM")

    #LAKE-EFFECT SNOW
    set(DT,i = intersect(grep("lake", DT$EVTYPE, ignore.case = F),
                         grep("snow", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "LAKE-EFFECT SNOW") 

    #LAKESHORE FLOOD

    #LIGHTNING
    set(DT,i = intersect(grep("lightn|lighti|ligntning", DT$EVTYPE, ignore.case = F),
                         grep("thun|tstm", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "LIGHTNING") 
    set(DT,i = intersect(grep("lightn|lighti", DT$EVTYPE, ignore.case = F),
                         grep("thun|tstm", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "LIGHTNING|THUNDERSTORM WIND") 

    #MARINE HAIL

    #MARINE HIGH WIND 
    set(DT, i = grep("marine accident", DT$EVTYPE, ignore.case = F), 
        j = 8L, value = "STORM SURGE/TIDE")

    #MARINE STRONG WIND
    set(DT,i = intersect(grep("marine", DT$EVTYPE, ignore.case = F),
                         grep("strong|mishap", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "MARINE STRONG WIND") 

    #MARINE THUNDERSTORM WIND
    set(DT,i = intersect(grep("marine", DT$EVTYPE, ignore.case = F),
                         grep("tstm|thund", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "MARINE THUNDERSTORM WIND") 

    #RIP CURRENT
    set(DT,i = grep("rip", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "RIP CURRENT") 

    #SEICHE
    set(DT,i = grep("seiche", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "SEICHE") 

    #SLEET
    set(DT,i = grep("sleet", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "SLEET") 

    #STORM SURGE/TIDE
    set(DT,i = grep("surge|coastalstorm|coastal storm|rough seas|wind and wave", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "STORM SURGE/TIDE") 

    #STRONG WIND
    set(DT,i = intersect(grep("strong|gusty", DT$EVTYPE, ignore.case = F),
                         grep("rain|thund", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "STRONG WIND") 
    set(DT,i = intersect(grep("strong|gusty", DT$EVTYPE, ignore.case = F),
                         grep("rain", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HEAVY RAIN|STRONG WIND") 
    set(DT,i = intersect(grep("strong|gusty", DT$EVTYPE, ignore.case = F),
                         grep("thund", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "STRONG WIND|THUNDERSTORM WIND")

    #THUNDERSTORM WIND
    set(DT,i = intersect(grep("thund|tst|thunerstorm|thuderstorm|tunderstorm", DT$EVTYPE, ignore.case = F),
                         grep("non", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "THUNDERSTORM WIND")
    set(DT,i = intersect(grep("thund|tst", DT$EVTYPE, ignore.case = F),
                         grep("non", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "HIGH WIND")

    #TORNADO
    set(DT,i = intersect(grep("tornado|torndao", DT$EVTYPE, ignore.case = F),
                         grep("water", DT$EVTYPE, ignore.case = F, invert = T)),
        j = 8L, value = "TORNADO")
    set(DT,i = intersect(grep("tornado", DT$EVTYPE, ignore.case = F),
                         grep("water", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "TORNADO|WATERSPOUT")

    #TROPICAL DEPRESSION
    set(DT,i = grep("tropical depression", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "TROPICAL DEPRESSION") 

    #TROPICAL STORM
    set(DT,i = grep("tropical storm", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "TROPICAL STORM") 

    #TSUNAMI
    set(DT,i = grep("tsunami", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "TSUNAMI")

    #VOLCANIC ASH
    set(DT,i = grep("ash|volcanic", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "VOLCANIC ASH")

    #WATERSPOUT
    set(DT,i = grep("spout", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "WATERSPOUT")

    #WILDFIRE
    set(DT,i = grep("fire", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "WILDFIRE")

    #WINTER STORM
    set(DT,i = intersect(grep("winter", DT$EVTYPE, ignore.case = F),
                         grep("storm", DT$EVTYPE, ignore.case = F, invert = F)),
        j = 8L, value = "WINTER STORM")

    #WINTER WEATHER
    set(DT,i = grep("winter|wint|light snow|accumulated snowfal|snow", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "WINTER WEATHER")

    #RANDOM WIND MARKED AS STRONG WIND
    set(DT,i = grep("wind", DT$EVTYPE, ignore.case = F),
        j = 8L, value = "STRONG WIND")

    #MARK THE REST AS OTHER
    set(DT,i = grep("[a-z]", DT$EVTYPE, ignore.case = F, perl = T),
        j = 8L, value = "OTHER")

    return (DT)
}

# Replace exponent from letter to numeric number
# allowed exponents H, K, M, B, rest are replaced by 1
replace_exp_by_number <- function (DT) {
    DT[!(DT$PROPDMGEXP%in%c('k', 'K', 'm', 'M', 'b', 'B', 'h', 'H'))]$PROPDMGEXP <- '1'
    DT[ (DT$PROPDMGEXP%in%c('h', 'H'))]$PROPDMGEXP <- "1E2"
    DT[ (DT$PROPDMGEXP%in%c('k', 'K'))]$PROPDMGEXP <- "1E3"
    DT[ (DT$PROPDMGEXP%in%c('m', 'M'))]$PROPDMGEXP <- "1E6"
    DT[ (DT$PROPDMGEXP%in%c('b', 'B'))]$PROPDMGEXP <- "1E9"
    DT$PROPDMGEXP <- as.numeric(DT$PROPDMGEXP)
    DT[!(DT$CROPDMGEXP%in%c('k', 'K', 'm', 'M', 'b', 'B', 'h', 'H'))]$CROPDMGEXP <- "1"
    DT[ (DT$CROPDMGEXP%in%c('h', 'H'))]$CROPDMGEXP <- "1E2"
    DT[ (DT$CROPDMGEXP%in%c('k', 'K'))]$CROPDMGEXP <- "1E3"
    DT[ (DT$CROPDMGEXP%in%c('m', 'M'))]$CROPDMGEXP <- "1E6"
    DT[ (DT$CROPDMGEXP%in%c('b', 'B'))]$CROPDMGEXP <- "1E9"
    DT$CROPDMGEXP <- as.numeric(DT$CROPDMGEXP)
    return (DT)
}

DataFile <- "./StormData.csv"
CacheData <- read_dataCache( DataFile )
DT <- cacheData(CacheData)
DT <- assign_event_type(DT)

ListOfEvents <- c(
                  "Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", 
                  "Cold/Wind Chill", "Debris Flow","Dense Fog", "Dense Smoke", 
                  "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Hail",
                  "Extreme Cold Wind/Chill", "Flash Flood", "Flood", "Frost Freeze",
                  "Funnel Cloud", "Freezing Fog", "Heat", "Heavy Rain", "Heavy Snow", 
                  "High Surf", "High Wind", "Hurricane/Typhoon","Ice Storm", "Lake-Effect Snow", 
                  "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind",
                  "Sleet", "Tornado","Marine Strong Wind", "Marine Thunderstorm Wind", 
                  "Rip Current", "Seiche", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
                  "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
                  "Waterspout", "Wildfire", "Winter Storm", "Winter Weather" )

ListOfEvents <- toupper(sort(ListOfEvents, decreasing = F))

#Remove ambiguous events and mark them out as OTHER

setkey(DT, EVTYPE)
DT[!(EVTYPE %in% ListOfEvents)]$EVTYPE <- "OTHER"

#sum injuries and fatalities 
FATALITIES.TOTAL <- aggregate(cbind(FATALITIES, INJURIES)~EVTYPE, data=DT, FUN=sum)
Plot.Matrix <-as.matrix(FATALITIES.TOTAL[,2:3])
rownames(Plot.Matrix) <- FATALITIES.TOTAL$EVTYPE
#sort in decreasing order
Plot.Matrix <- Plot.Matrix[order(Plot.Matrix[,1]+ Plot.Matrix[,2], decreasing = T),]
#drop events with no casualties
Plot.Matrix <- Plot.Matrix[(Plot.Matrix[,2] + Plot.Matrix[,1] > 0),]

#Plot casualties data
par(mar=c(10, 4, 5, 1))
barplot(t(Plot.Matrix), main="Total number of injured people because of weather event",
        xlab="", col=c("darkblue","red"), ylab = "Number of Injured and Killed",
         las=3,  ylim=c(1, 100000))
legend("top", bty="n",legend = colnames(Plot.Matrix),  
       fill = c("darkblue","red"), y.intersp=2 )

#Calculate total damage
DT <- replace_exp_by_number(DT)
DT$PROPDMG <- DT$PROPDMG * DT$PROPDMGEXP
DT$CROPDMG <- DT$CROPDMG * DT$CROPDMGEXP
#sum proprty damage
DAMAGE.TOTAL <- aggregate(cbind(PROPDMG, CROPDMG)~EVTYPE, data=DT, FUN=sum)
Damage.Matrix <-as.matrix(DAMAGE.TOTAL[,2:3])
rownames(Damage.Matrix) <- DAMAGE.TOTAL$EVTYPE
# convert to millions
Damage.Matrix <- Damage.Matrix/1E6
#drop events with damage less then million $
Damage.Matrix <- Damage.Matrix[(Damage.Matrix[,2] + Damage.Matrix[,1] > 1),]
#sort in decreasing order
Damage.Matrix <- Damage.Matrix[order(Damage.Matrix[,1]+Damage.Matrix[,2], decreasing = T),]

#Plot casualties data
par(mar=c(10, 4, 5, 1))
barplot(t(Damage.Matrix), main="Total damage from weather event",
        xlab="", col=c("darkblue","red"), ylab = "Total damage [million dollars]",
         las=3)
legend("top", bty="n",legend = c("PROPERTY DAMAGE", "CROP DAMAGE"),  
       fill = c("darkblue","red"), y.intersp=2)

