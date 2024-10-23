#' Clean and Process Animal Data
#'
#' This function reads a tab-separated file selected by the user, processes the data, 
#' and calculates additional fields such as animal identifier, date, time from sunrise, 
#' Julian day, age, and corrects age based on the first recorded juvenile data from a specific location.
#'
#' @return A data frame with cleaned and processed animal data, including calculated fields.
#' 
#' @details 
#' - The input file should have columns for cohort, toe.clip.tatoo, year, month, day, hour, minute, site, age, and sex.
#' - The function adds an 'animal' identifier by concatenating the cohort and toe.clip.tatoo.
#' - The 'date' column is constructed from the year, month, and day columns.
#' - The 'timeFromSunrise' is calculated based on the provided date and time, using a user-defined `timeFromSunrise` function.
#' - Egg entries in the 'age' column are set to NA.
#' - The 'sex' column resolves entries marked as "maleV" to "male."
#' - The Julian day is calculated as the 'season' column.
#' - The age of the animals is calculated, adjusted based on the first juvenile records from specific years, and converted into months.
#'
#' @importFrom utils read.table
#' 
#' @examples 
#' \dontrun{
#'   # Run the function to clean data
#'   cleanData()
#' }

cleanData <- function(){

dat = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dat$animal = paste(dat$cohort, dat$toe.clip.tatoo, sep = "-")
dat$date = as.Date(paste(dat$year, dat$month, dat$day, sep="-"))

dat$timeFromSunrise = timeFromSunrise(hour = dat$hour, minute = dat$minute, date = dat$date, site = dat$site)

# remove eggs
dat$age = sub("egg", NA, dat$age)
# resolve sex estimated from ventralia count
dat$sex = sub("maleV", "male", dat$sex)
# Julian day
dat$season = as.numeric(format(dat$date, "%j"))
# calculate age of animals
dat$aged = dat$season + 365 * (dat$year - dat$cohort)

# first juveniles recorded in Hustopece
mladata = data.frame(prve.mlada = as.numeric(format(as.Date(c("28.7.2018", "14.8.2020", "29.7.2021", "28.7.2022", "18.7.2023", "23.7.2024"), "%d.%m.%Y"), "%j")), 
					rok = c(2018, 2020, 2021, 2022, 2023, 2024))
korekcia = rep(round(mean(mladata$prve.mlada), 0), nrow(dat))
for(i in 1:nrow(mladata)){
	korekcia[sapply(dat$cohort == mladata$rok[i], isTRUE)] = mladata$prve.mlada[i]
}

dat$aged = dat$aged - korekcia

dat$aged = round(dat$aged / 30.41, 0)


return(dat)


}