% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateSpeed.r
\name{calculateSpeed}
\alias{calculateSpeed}
\title{Estimates running speed from Arduino Lizards log}
\usage{
calculateSpeed(inputFolder, outputFile = "runSpeeds.txt", ...)
}
\arguments{
\item{inputFolder}{character path to a folder with running log files.}

\item{outputFile}{character specifying filename where to save the results.}
}
\value{
Invisibly returns data.frame with animal ID, animal temperature during
measurement, maximum speed between consecutive sensors in meters per second,
day, month, year of the measurement and the air humidity and temperature of the
racetrack.
}
\description{
Reads running speed logs generated by Tomas Fryza's Arduino Lizards software
developed for a custom racetrack for sand lizards. Returns top speed in meters per
second.
}
\details{
The function calculates maximum speed the animal achieved between consecutive
sensors on the racetrack in meters per second. Saves the results into a specified
file.

If any experiments did not have two consecutive measurements on neighbouring
sensors, the experiments are considered to have failed. The function issues a warning
and all failed experiments are returned in a file "neuspesneExperimenty.txt" in the
working directory.
}
\note{
TO DO: incorporate attempt to correct the results, i.e. if speed is negative,
the animal likely ran twice on one measurement.
}
