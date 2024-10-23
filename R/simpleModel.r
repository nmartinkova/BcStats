#' Calculates linear regression model from continuous data
#'
#' Fits linear model for 
#' analysing continuous data, where one measurement was taken per animal on 
#' each day, such as weight or body temperature. Allows the user to choose
#' working directory and the data file, choose the dependent and explanatory variables, 
#' fits the model and plots variable predictions as specified by the user.
#' @param farebna.paleta character. Color palette in \code{hcl.pals()} or 
#'   \code{palette.pals()}.
#' @param vymaz.odlahle logical, indicating whether to remove outliers in the dependent
#'   variable using Tukey's fences.
#' @details Function communicates with the user in Slovak to distinguish what dialogs
#'   represent intended functionality. Pay attention to dialog boxes (2) at the beginning 
#'   and prompts on the R console afterwards. 
#' 
#'   Dialogs:
#'   1. Select a folder where the results will be saved.
#'   2. Select a tab-delimited table with data describing the animals. This was likely
#'      provided by the supervisor.
#'
#'   Once the data tables are selected, the function asks to select variables that will
#'   be used to construct the model. The predicted values from the model can be plotted,
#'   displaying one partial fit at the time. All results can be found in the folder 
#'   selected in dialog 1.
#' @returns Called for side effects. Stores summary of the model fit in a text file and
#'   plots predicted values in pdf files.
#' @import vioplot utils tcltk
#' @export
#' @examples
#' \dontrun{
#' # Open R console and type
#' simpleModel()
#' # Hit Enter and follow instructions in dialogs and on the console
#' }


simpleModel <- function(farebna.paleta = "Accent", vymaz.odlahle = FALSE){

for(i in c("utils", "tcltk", "vioplot", "MASS")){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}


paleta = farebna.paleta
farby = { if(paleta %in% palette.pals())
			palette.colors(11, palette = paleta, recycle = TRUE)
		 else 
		 	hcl.colors(11, paleta) }


### vybrat adresar pre pracu

choose_directory = function(caption = 'Vyber adresar, kde sa budu ukladat vysledky') {
  if (exists('utils::choose.dir')) {
    utils::choose.dir(caption = caption) 
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}




setwd(doma <- choose_directory())

if(!dir.exists("Vysledky")) dir.create("Vysledky")



message("Vyber tabulku s udajmi Lagilis - musi byt tab-delimited format")

dat = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dat$animal = paste(dat$cohort, dat$toe.clip.tatoo, sep = "-")
dat$date = as.Date(paste(dat$year, dat$month, dat$day, sep="-"))

dat$timeFromSunrise = timeFromSunrise(hour = dat$hour, minute = dat$minute, date = dat$date, site = dat$site)

dat$age = sub("egg", NA, dat$age)
dat$age[grepl("juv|sub", dat$age)] = "young"
dat$age[grepl("adult", dat$age)] = "adult"

dat$sex = sub("maleV", "male", dat$sex)

dat[] = lapply(dat, FUN = function(x) if(is.character(x)) as.factor(x) else {x})

dat$season = as.numeric(format(as.Date(paste(dat$year, dat$month, dat$day, sep="-")), "%j")) 

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


zavisla = "temperature"
stlpce = c("season", "age", "sex")

### kontrola stlpcov pre analyzu

suhlas = "a"

while(suhlas == "a"){
	cat("Pre vypocet budem predikovat premennu:", zavisla, "a vysvetlovat ju budu tieto stlpce:", stlpce,
		sep = "\n")
	suhlas = readline("Chces zmenit niektore premenne [a/n]? ")
	if(suhlas == "a"){
		print(colnames(dat))
		zavisla = colnames(dat)[as.numeric(readline("Napis cislo stlpca premennej, ktoru chces modelovat: "))]
		ktore = as.numeric(unlist(strsplit(readline("Napis cisla stlpcov oddelene ciarkou pre premenne, ktore ju maju vysvetlovat: "), ",")))
		stlpce = colnames(dat)[ktore]
	}
}

dat2 = droplevels(dat[complete.cases(dat[,c(stlpce, zavisla, "animal")]),])

# odstranint outliery 
if(vymaz.odlahle){
	k = 1.5
	lims = c(quantile(dat2[, zavisla], .25) - k * IQR(dat2[, zavisla]), 
	         quantile(dat2[, zavisla], .75) + k * IQR(dat2[, zavisla]))

	dat2 = dat2[dat2[, zavisla] <= lims[2] & dat2[, zavisla] >= lims[1], ]
}

# finalne data 

message("Datova sada pre analyzu ma ", nrow(dat2), " riadkov. Pocitam regresny model.")


# model
fit = lm(as.formula(paste(zavisla, paste(stlpce, collapse = "+"), sep = "~")), 
  data = dat2)


k = sum(grepl("LM", dir(doma, recursive = TRUE))) +1
cat(paste(zavisla, paste(stlpce, collapse = " + "), sep = " ~ "), "\n\n", file = paste0("Vysledky/LM",k,".txt"), append =FALSE)
capture.output(summary(fit), file = paste0("Vysledky/LM",k,".txt"), append = TRUE)

print(summary(fit))

suhlas = "a"

while(suhlas == "a"){
  cat("\n",paste(1:length(stlpce), stlpce), sep = "\n")
  ktore = as.numeric(readline("Ktoru premennu zobrazit do grafu? Poradie v zozname: "))
  
  pred = predict(fit, type = "response", se.fit = TRUE)

  pdf(paste0("Vysledky/", stlpce[ktore], k, ".pdf"), width = ifelse(is.factor(dat2[,stlpce[ktore]]) | length(stlpce) > 1, 8, 5.5), height = 4.5)
  par(mar = c(4.1, 4.1, .5, .5))
  popisok = switch(stlpce[ktore], temperature = expression("Temperature ("^"o" * "C)"),
  	   temp = expression("Temperature ("^"o" * "C)"),
  	   animal.temp = expression("Temperature ("^"o" * "C)"),
       sex = "Sex",
       humidity = "Humidity (%)",
       air.temp = expression("Air temperature ("^"o" * "C)"),
       season = "Calendar date",
       weight = "Weight (g)",
       site = "Site",
       age = "Age",
       timeFromSunrise = "Time from sunrise (h)")
  popisok.y = switch(zavisla, speed = "Predicted running speed (m/s)",
  	weight = "Predicted weight (g)",
  	temperature = expression("Predicted temperature ("^"o" * "C)"),
  	temp = expression("Predicted temperature ("^"o" * "C)"),
  	animal.temp = expression("Predicted temperature ("^"o" * "C)"))
  if(is.factor(dat2[,stlpce[ktore]])){   
    layout(matrix(c(1,1,2), ncol = 3))
    par(mar = c(4.1,4.1,.5,.5))
      vioplot::vioplot(pred$fit ~ dat2[, stlpce[ktore]], col = farby, wex = .6, las = 1, xlab = popisok, ylab = popisok.y, axes = FALSE)
      box()
      axis(1, at = 1:nlevels(dat2[, stlpce[ktore]]), labels = levels(dat2[, stlpce[ktore]]))
      axis(2, las = 1)
    plot.new()
    L = legend("topleft", legend = levels(as.factor(dat2[, stlpce[ktore]])), fill = farby[1:nlevels(as.factor(dat2[, stlpce[ktore]]))])
	if(length(stlpce) > 1){
		legend(x = L$rect$left, y = L$rect$top - L$rect$h, title = "Covariates:", 
		  legend = sapply(seq_along(stlpce)[-ktore], \(x) switch(stlpce[x], temperature = expression("Temperature ("^"o" * "C)"),
  	   temp = expression("Temperature ("^"o" * "C)"),
  	   animal.temp = expression("Temperature ("^"o" * "C)"),
       sex = "Sex",
       humidity = "Humidity (%)",
       air.temp = expression("Air temperature ("^"o" * "C)"),
       season = "Calendar date",
       weight = "Weight (g)",
       site = "Site",
       age = "Age",
       timeFromSunrise = "Time from sunrise (h)")),
       bty = "n")
	}
  } else {
	newdat = data.frame(x1 = seq(min(dat2[, stlpce[ktore]]), max(dat2[, stlpce[ktore]]), length.out = 100))
	if(length(stlpce) > 1){
		layout(matrix(c(1,1,2), ncol = 3))
		for(i in seq_along(stlpce)[-ktore]){
			newdat[, stlpce[i]] = ifelse(is.factor(dat2[,stlpce[i]]), 
							names(summary(dat2[,stlpce[i]]))[1], 
							mean(dat2[,stlpce[i]]))
		}
	}
	colnames(newdat) = c(stlpce[ktore], stlpce[-ktore])[1:length(stlpce)]
	pred = predict(fit, newdata = newdat, interval = "confidence")
	par(mar = c(4.1,4.1,.5,.5))
	plot(dat2[, stlpce[ktore]], dat2[, zavisla], pch = 20, xlab = popisok, ylab = popisok.y, las = 1)
	lines(newdat[,1], pred[,"fit"], lwd = 3, col = farby[1])
	lines(newdat[,1], pred[,"lwr"], lty = 2, col = farby[1])
	lines(newdat[,1], pred[,"upr"], lty = 2, col = farby[1])
	if(length(stlpce) > 1){
		plot.new()
		legend("topleft", title = "Covariates:", 
		  legend = sapply(seq_along(stlpce)[-ktore], \(x) switch(stlpce[x], temperature = expression("Temperature ("^"o" * "C)"),
  	   temp = expression("Temperature ("^"o" * "C)"),
  	   Temp = expression("Temperature ("^"o" * "C)"),
       sex = "Sex",
       humidity = "Humidity (%)",
       air.temp = expression("Air temperature ("^"o" * "C)"),
       season = "Calendar date",
       weight = "Weight (g)",
       site = "Site",
       timeFromSunrise = "Time from sunrise (h)",
       age = "Age")),
       bty = "n")
	}
  }
  dev.off()
  suhlas = readline("Chces nakreslit predikovane hodnoty pre dalsiu premennu z tohto modelu? [a/n] ")
}

}