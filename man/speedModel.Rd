% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/speedModel.r
\name{speedModel}
\alias{speedModel}
\title{Calculates mixed regression model from continuous data}
\usage{
speedModel(farebna.paleta = "Accent", vymaz.odlahle = TRUE, vymaz.NA = TRUE)
}
\arguments{
\item{farebna.paleta}{character. Color palette in \code{hcl.pals()} or
\code{palette.pals()}.}

\item{vymaz.odlahle}{logical, indicating whether to remove outliers in the dependent
variable using Tukey's fences.}

\item{vymaz.NA}{logical, indicating whether to remove rows in data, where information is
missing.}
}
\value{
Called for side effects. Stores summary of the model fit in a text file and
plots predicted values in pdf files.
}
\description{
Fits a linear mixed model for
analysing continuous data, where more than one measurement was taken per animal on
each day, such as running speed. Allows the user to choose
working directory and the data file, choose the dependent and explanatory variables,
fits the model and plots variable predictions as specified by the user.
}
\details{
Function communicates with the user in Slovak to distinguish what dialogs
represent intended functionality. Pay attention to dialog boxes (3) at the beginning
and prompts on the R console afterwards.

Dialogs:
\enumerate{
\item Select a folder where the results will be saved.
\item Select a tab-delimited table with data from multiple measurements per animal
per day. This is likely the result from the \code{caclulateSpeed()} function.
\item Select a tab-delimited table with data describing the animals. This was likely
provided by the supervisor.
}

Once the data tables are selected, the function asks to select variables that would
be used to construct the model. The predicted values from the model can be plotted,
displaying one partial fit at the time. All results can be found in the folder
selected in dialog 1.

Argument \code{vymaz.NA} checks for missing data in columns \code{amimal.temp}, \code{air.temp},
\code{humidity}, \code{site}, \code{L}, \code{Lc.L}, \code{sex}, \code{age}, and \code{weight} and then removes the
rows is any missing information.
}
\examples{
\dontrun{
# Open R console and type
speedModel()
# Hit Enter and follow instructions in dialogs and on the console
}
}
