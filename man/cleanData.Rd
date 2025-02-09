% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanData.r
\name{cleanData}
\alias{cleanData}
\title{Clean and Process Animal Data}
\usage{
cleanData(filename = NULL)
}
\arguments{
\item{filename}{A character vector with path to a file.}
}
\value{
A data frame with cleaned and processed animal data, including calculated fields.
}
\description{
This function reads a tab-separated file selected by the user, processes the data,
and calculates additional fields such as animal identifier, date, time from sunrise,
Julian day, age, and corrects age based on the first recorded juvenile data from a specific location.
}
\details{
\itemize{
\item The input file should have columns for cohort, toe.clip.tatoo, year, month, day, hour, minute, site, age, and sex.
\item The function adds an 'animal' identifier by concatenating the cohort and toe.clip.tatoo.
\item The 'date' column is constructed from the year, month, and day columns.
\item The 'timeFromSunrise' is calculated based on the provided date and time, using a user-defined \code{timeFromSunrise} function.
\item Egg entries in the 'age' column are set to NA.
\item The 'sex' column resolves entries marked as "maleV" to "male."
\item The Julian day is calculated as the 'season' column.
\item The age of the animals is calculated, adjusted based on the first juvenile records from specific years, and converted into months.
}
}
\examples{
\dontrun{
  # Run the function to clean data
  cleanData()
}
}
