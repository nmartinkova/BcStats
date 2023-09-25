#' Time from Sunrise
#'
#' Calculates how many hours after sunrise the animal was caught. Sunrise at the site
#' is calculated from the geographic coordinates of the site description.
#' @param hour numeric. Hour of capture.
#' @param minute numeric. Minute of capture.
#' @param date Date. Date of capture.
#' @param site character. Site name with latitude and longitude in decimal degrees.
#' @export
#' @import suncalc
#' @return Number of hours since sunrise when the animal was captured.
#' @details At this time, only geographic coordinates on the northern and eastern
#'   hemispheres are considered.
#' @import suncalc
#' @examples
#'   timeFromSunrise(hour = 10, minute = 30, date = as.Date("2023-05-30"), site = "Hustopece, CZ, 48.93N, 16.72E")





timeFromSunrise = function(hour, minute, date, site){

   latitude = unlist(strsplit(sub("[A-Z]", "", site), ", "))
   latitude = as.numeric(sub("N", "", latitude[grepl("N", latitude)]))
   
   longitude = unlist(strsplit(sub("[A-Z]", "", site), ", "))
   longitude = as.numeric(sub("E", "", longitude[grepl("E", longitude)]))
	
   sunrise = suncalc::getSunlightTimes(data = data.frame(date = date, lat = latitude, lon = longitude), tz = "CET")$sunrise
   
   sunrise = as.numeric(format(sunrise, "%H")) + as.numeric(format(sunrise, "%M")) / 60
   
   
   return(hour + minute / 60 - sunrise)
   
}