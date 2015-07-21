# required packages
require(httr)

# Mashape API KEY
key <- 'Mashape X-key'

# geographic bounding box for search query
nelat <- 'replace with NE corner latitude'
swlat <- 'replace with SW corner latitude'
nelon <- 'replace with NE corner longitude'
swlon <- 'replace with SW corner longitude'
bbox <- paste0('nelatitude=',nelat,"&nelongitude=",nelon,"&swlatitude=",swlat,"&swlongitude=",swlon)

# Available providers 

# tested
# airbnb
# homeaway - reviews/ratings unavailable
#
# not tested yet:
# 
# "alwaysonvacation","apartmentsapart","bedycasa","bookingpal","citiesreference",
# "edomizil","geronimo","gloveler","holidayvelvet","homestay",
# "hostelworld","housetrip","interhome","nflats","roomorama","stopsleepgo","theotherhome",
# "travelmob","vacationrentalpeople","vaycayhero","waytostay","webchalet","zaranga"

provs <- 'enter provider from above list'

# set options
resultsperpage <- 50 # 50 is the max number of results per page
options <- paste0("&provider=",provs,"&resultsperpage=",resultsperpage)
page <- 0 # initialize 
pageText <- paste0("&page=",page)

# init df - provide one variable, the rest will generate dynamically
df <- data.frame(id = character(resultsperpage),stringsAsFactors = FALSE)

# grabber function
# query is generated from the globally declared options, provider, and geographic bounding box
getPage <- function(x) {
  page <- x
  pageText <- paste0("&page=",page)
  query <- paste0("https://zilyo.p.mashape.com/search?",bbox,options,pageText)
  zilyo <- GET(query, add_headers("X-Mashape-Key" = key, "Accept" = "application/json"))
  zilyo <- content(zilyo, type = 'application/json')
  for (i in 1:length(zilyo$ids)) {
    # id
    df$id[[i]] <- zilyo$result[[i]]$id
    df$provider[[i]] <- zilyo$result[[1]]$provider$cid
    # page
    df$page <- zilyo$page
    # geo
    df$lat[[i]] <- zilyo$result[[i]]$latLng[[1]]
    df$lon[[i]] <- zilyo$result[[i]]$latLng[[2]]
    # location
    df$neighbourhood[[i]] <- zilyo$result[[i]]$location$neighbourhood
    df$city[[i]] <- zilyo$result[[i]]$location$city
    df$streetName[[i]] <- zilyo$result[[i]]$location$streetName
    df$postalCode[[i]] <- zilyo$result[[i]]$location$postalCode
    # prices
    df$monthlyPrice[[i]] <- zilyo$result[[i]]$price$monthly
    df$weeklyPrice[[i]] <- zilyo$result[[i]]$price$weekly
    df$nightlyPrice[[i]] <- zilyo$result[[i]]$price$nightly
    df$minNight[[i]] <- zilyo$result[[i]]$price$minNight
    # attributes
    df$roomType[[i]] <- zilyo$result[[i]]$attr$roomType$text
    df$propType[[i]] <- zilyo$result[[i]]$attr$propType$text
    df$bed[[i]] <- zilyo$result[[i]]$attr$bedrooms
    df$beds[[i]] <- zilyo$result[[i]]$attr$beds
    df$bath[[i]] <- zilyo$result[[i]]$attr$bathrooms
    df$occupancy[[i]] <- zilyo$result[[i]]$attr$occupancy
    df$heading[[i]] <- zilyo$result[[i]]$attr$heading
    df$desc[[i]] <- zilyo$result[[i]]$attr$description
    # reviews
    df$reviewCount[[i]] <- zilyo$result[[i]]$reviews$count
    df$reviewScore[[i]] <- zilyo$result[[i]]$reviews$rating
    # url
    df$url[[i]] <- zilyo$result[[i]]$provider$url
  } 
  return(df)
}

# iterate across pages
repeat {
  if (page < 2){
    data <- getPage(page)
    page <- page + 1
  }
  if (page > 1) {
    df <- getPage(page)
    page <- page + 1
    data <- rbind(data,df)
  }
}

# reminder to fix the error when the last page is less than the stated returnPerPage count - entirely functional otherwise

# add exchange rate at the time of data collection
# Zilyo API returns listings in USD base. We want both local and USD.
# available exchange rates through the Currency Exchange Mashape API:
#  'USD' 'CAD' 'EUR' 'SGD' 'MYR' 'AUD' 'JPY' 'CNH' 'HKD' 'INR' 'DKK' 'GBP' 'RUB' 'NZD' 'MXN'
baseCurrency <- 'USD' 
localCurrency <- #'enter a local currency from above list'
exchangeRateQuery <- paste0("https://currency-exchange.p.mashape.com/exchange?from=",baseCurrency,"&q=1.0&to=",localCurrency)
exchangeRates <- GET(exchangeRateQuery, add_headers("X-Mashape-Key" = key, "Accept" = "text/plain"))
exchange <- as.numeric(content(exchangeRates, type = 'text/plain'))
exchangeDesc <- paste0(baseCurrency,"to",localCurrency,"_",Sys.Date())
data$exchange <- exchange
data$exchangeDesc <- exchangeDesc

# Save results in a dated file
date <- Sys.Date()
provider <- provs
format <- ".csv"
destination <- paste0(provider,date,format)
write.csv(data,destination)

#####
# Additional Attributes
# additional attributes can be collected by inspecting the 'zilyo' object returned from the REST call
# e.g. for zilyo$result[[i]]$attr:
# $extraGuests($fee,$after), $instantBookable, $cancellation($text,$link), $isCalAvailable. $responseTime, $fees($fee,$description)
# $lastUpdateAt, $securityDeposit, $checkOut, $checkIn, $size
# for zilyo$result[[i]]$price: $maxNight, $maxNight
# for zilyo$result[[i]]$photos[[j]]: $large, $small, $medium, $xlarge, $caption
# for zilyo$result[[i]]$location: $all, $state
# for zilyo$result[[i]]$amenities[[j]]: $text, $id - each [[j]] corresponds to an entry in the amenities list
# for zilyo$result[[i]]$availability[[j]]: $start, $end look like unix values for seconds - not sure how this works yet
# zilyo$result[[i]]$priceRange: $nightly, $start, $end, $maxNight, $weekend, $monthly, $minNight, $weekly - appears to duplicate $price
# zilyo$result[[i]]$provider: $domain, $full, $nid, $micro
# zilyo$result[[i]]$reviews$entries[[j]]: $date, $text, $picture, $name, $rating
