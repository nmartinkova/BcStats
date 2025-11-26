#' Estimate running speed from Arduino Lizards logs
#'
#' Reads running logs produced by Tomas Fryza’s Arduino Lizards software for the
#' sand lizard racetrack and calculates maximum speed between consecutive sensors.
#'
#' @param inputFolder Path to the folder containing corrected log files. If not
#'    provided, an interactive dialog opens.
#' @param outputFile Name of the file where calculated speeds will be saved. If
#'    not provided, a filename is created from the folder name and the current date.
#'
#' @details The function extracts the measurement date from each log file, reads
#'    sensor timestamps, and converts the shortest time difference between two
#'    neighbouring sensors into speed (meters per second). Negative or zero speeds
#'    indicate a failed run and are separated into a file named
#'    *neuspesneExperimenty.txt*.
#'
#'    The output file contains, for each valid experiment, the animal ID,
#'    temperature of the animal during the measurement, maximum speed, measurement
#'    date, humidity, air temperature, and experiment ID.
#'
#' @note Students should always check the saved output file rather than rely only
#'    on the returned data frame. Confirm that the number of experiments matches
#'    expectations and that failed runs are handled correctly.
#'
#' @return Invisibly returns a data.frame with the calculated values, while also
#'    saving the results to `outputFile`.
#'
#' @export
#' @examples
#' \dontrun{
#' # To run the function in an interactive mode, open the R console and type
#' calculateSpeed()
#' # Hit Enter and select a folder with running logs in the dialog box
#' }
calculateSpeed <- function(inputFolder = NULL, outputFile = NULL) {
  if (is.null(inputFolder)) {
    choose_directory <- function(caption = "Vyber adresar, kde su vsetky opravene log subory z behatka") {
      if (exists("utils::choose.dir")) {
        utils::choose.dir(caption = caption)
      } else {
        tcltk::tk_choose.dir(caption = caption)
      }
    }
    inputFolder <- choose_directory()
  }

  if (is.null(outputFile)) {
    outputFile <- paste0(basename(inputFolder), format(Sys.time(), "%y%m%d"), ".txt")
    message("Subor s rychlostami ", outputFile, " bude ulozeny v ", getwd())
  }


  subory <- dir(inputFolder, recursive = TRUE, full.names = TRUE)

  # extract date from log file header
  datum <- as.numeric(unlist(strsplit(unlist(strsplit(readLines(subory[1], n = 1), " "))[4], "\\.")))

  # calculate top speed in meters per second
  dat <- read.table(subory[1], skip = 5, header = TRUE, sep = ";", na.strings = c("NA", "-"))

  rychlost <- 100 / suppressWarnings(apply(dat[, 2:12], 1, FUN = \(x) min(diff(x), na.rm = T)))

  res <- data.frame(
    id.animal = dat$idAnimal,
    animal.temp = dat$tempAnimal,
    speed = rychlost,
    day = datum[3],
    month = datum[2],
    year = datum[1],
    humidity = dat$humid,
    air.temp = dat$temp,
    id.experiment = dat$id
  )
  neuspesne <- data.frame(matrix(NA,
    ncol = ncol(res), nrow = 1,
    dimnames = list(NULL, colnames(res))
  ))
  if (sum(rychlost <= 0) > 0) {
    neuspesne <- rbind(neuspesne, res[rychlost <= 0, ])
    res <- res[rychlost > 0, ]
  }

  if (length(subory) > 1) {
    for (i in 2:length(subory)) {
      # extract date from log file header
      datum <- as.numeric(unlist(strsplit(unlist(strsplit(readLines(subory[i], n = 1), " "))[4], "\\.")))

      # calculate top speed in meters per second
      dat <- try(read.table(subory[i], skip = 5, header = TRUE, sep = ";", na.strings = c("NA", "-")))
      if (class(dat) == "try-error") {
        stop("Subor ", subory[i], " ma problem s nacitanim dat")
      }

      rychlost <- try(100 / apply(dat[, 2:12], 1, FUN = \(x) min(diff(x), na.rm = T)))
      if (class(rychlost) == "try-error") {
        stop("Subor ", subory[i], " ma problem s vypoctom rychlosti")
      }

      res2 <- data.frame(
        id.animal = dat$idAnimal,
        animal.temp = dat$tempAnimal,
        speed = rychlost,
        day = datum[3],
        month = datum[2],
        year = datum[1],
        humidity = dat$humid,
        air.temp = dat$temp,
        id.experiment = dat$id
      )
      if (sum(rychlost <= 0) > 0) {
        neuspesne <- rbind(neuspesne, res2[rychlost == 0, ])
        res2 <- res2[rychlost > 0, ]
      }
      res <- rbind(res, res2)
    }
  }

  write.table(res, file = outputFile, sep = "\t", row.names = FALSE, quote = FALSE)
  if (nrow(neuspesne) > 1) {
    warning("Najdenych ", nrow(neuspesne) - 1, " neuspesnych experimentov. Ich zoznam ukladam do suboru 'neuspesneExperimenty.txt'.")
    write.table(neuspesne[-1, ],
      file = "neuspesneExperimenty.txt", sep = "\t",
      row.names = FALSE, quote = FALSE
    )
  }
  return(invisible(res))
}
