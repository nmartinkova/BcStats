#' Calculates mixed regression model from continuous data
#'
#' Fits a linear mixed model for 
#' analysing continuous data, where more than one measurement was taken per animal on 
#' each day, such as running speed. Allows the user to choose
#' working directory and the data file, choose the dependent and explanatory variables, 
#' fits the model and plots variable predictions as specified by the user.
#' @param farebna.paleta character. Color palette in \code{hcl.pals()} or 
#'   \code{palette.pals()}.
#' @param vymaz.odlahle logical, indicating whether to remove outliers in the dependent
#'   variable using Tukey's fences.
#' @details Function communicates with the user in Slovak to distinguish what dialogs
#'   represent intended functionality. Pay attention to dialog boxes (3) at the beginning 
#'   and prompts on the R console afterwards. 
#' 
#'   Dialogs:
#'   1. Select a folder where the results will be saved.
#'   2. Select a tab-delimited table with data from multiple measurements per animal
#'      per day. This is likely the result from the \code{caclulateSpeed()} function.
#'   3. Select a tab-delimited table with data describing the animals. This was likely
#'      provided by the supervisor.
#'
#'   Once the data tables are selected, the function asks to select variables that would
#'   be used to construct the model. The predicted values from the model can be plotted,
#'   displaying one partial fit at the time. All results can be found in the folder 
#'   selected in dialog 1.
#' @returns Called for side effects. Stores summary of the model fit in a text file and
#'   plots predicted values in pdf files.
#' @import nlme utils tcltk vioplot MASS
#' @export
#' @examples
#' \dontrun{
#' # Open R console and type
#' speedModel()
#' # Hit Enter and follow instructions in dialogs and on the console
#' }


speedModel <- function(farebna.paleta = "Accent", vymaz.odlahle = TRUE){


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


# nakreslit LMM model

nakresli_lmm = function(model, dat, nezavisla, zavisla, ...){
	plot(dat[,nezavisla], pred, pch = 1, col = farby[1], las=1, ...)
	# https://cosanlab.com/static/papers/AdvPlot_HO.pdf
	fixed.p = fixef(model)
	random.p = ranef(model)
	coefs = switch(ncol(random.p), 
	               cbind(fixed.p[1] + random.p[,1], fixed.p[nezavisla]), 
	               cbind(fixed.p[1] + random.p[,1], fixed.p[nezavisla] + random.p[,2]))
	for(i in 1:nrow(random.p)){
		abline(a = coefs[i,1], b = coefs[i,2], lty=2, col="grey")
	}
	abline(fixed.p[c("(Intercept)", nezavisla)], lwd=2)
}


setwd(doma <- choose_directory())

if(!dir.exists("Vysledky")) dir.create("Vysledky")

message("Vyber tabulku s rychlostou behu - idealne vysledok z funkcie calculateSpeed()")

beh = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = TRUE)
beh$date = as.Date(paste(beh$year, beh$month, beh$day, sep="-"))




message("Vyber tabulku s udajmi Lagilis - musi byt tab-delimited format")

dat = read.table(file.choose(), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
dat$animal = paste(dat$cohort, dat$toe.clip.tatoo, sep = "-")
dat$date = as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$season = as.numeric(format(dat$date, "%j"))   # dni od zaciatku roka
dat$timeFromSunrise = timeFromSunrise(hour = dat$hour, minute = dat$minute, date = dat$date, site = dat$site)

dat$age = sub("egg", NA, dat$age)
dat$age[grepl("juv|sub", dat$age)] = "young"
dat$age[grepl("adult", dat$age)] = "adult"

dat$sex = sub("maleV", "male", dat$sex)


dat[] = lapply(dat, FUN = function(x) if(is.character(x)) as.factor(x) else {x})

dat$season = as.numeric(format(as.Date(paste(dat$year, dat$month, dat$day, sep="-")), "%j")) 



dat = merge(beh, dat, by.y = c("toe.clip.tatoo", "date"), by.x = c("id.animal", "date"), all = TRUE)

zavisla = "speed"
stlpce = c("animal.temp", "humidity", "air.temp")

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

dat2 = droplevels(dat[complete.cases(dat[,c(stlpce, zavisla, "id.animal")]),])

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
fit = nlme::lme(as.formula(paste(zavisla, paste(stlpce, collapse = "+"), sep = "~")), 
  random = ~ 1|id.animal, data = dat2)


k = sum(grepl("LMM", dir(doma, recursive = TRUE))) +1
cat(paste(zavisla, paste(stlpce, collapse = " + "), sep = " ~ "), "\n\n", file = paste0("Vysledky/LMM",k,".txt"), append =FALSE)
capture.output(summary(fit), file = paste0("Vysledky/LMM",k,".txt"), append = TRUE)

print(summary(fit))

suhlas = "a"

while(suhlas == "a"){
  cat("\n",paste(1:length(stlpce), stlpce), sep = "\n")
  ktore = as.numeric(readline("Ktoru premennu zobrazit do grafu? Poradie v zozname: "))
  
  pred = predict(fit, type = "response", se.fit = TRUE)

  pdf(paste0("Vysledky/", stlpce[ktore], k, ".pdf"), width = ifelse(is.factor(dat2[,stlpce[ktore]]) | length(stlpce) > 1, 8, 5.5), height = 4.5)
  par(mar = c(4.1, 4.1, .5, .5))
  popisok = switch(stlpce[ktore], 
  	   temperature = expression("Temperature ("^"o" * "C)"),
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
  popisok.y = switch(zavisla, speed = "Predicted running speed (m/s)")
  if(is.factor(dat2[,stlpce[ktore]])){   
    layout(matrix(c(1,1,2), ncol = 3))
    par(mar = c(4.1,4.1,.5,.5))
      vioplot::vioplot(pred ~ dat2[, stlpce[ktore]], col = farby, wex = .6, las = 1, xlab = popisok, ylab = popisok.y)
      box()
      axis(1, at = 1:nlevels(dat2[, stlpce[ktore]]), labels = levels(dat2[, stlpce[ktore]]))
      axis(2, las = 1)
    plot.new()
    legend("topleft", legend = levels(as.factor(dat2[, stlpce[ktore]])), fill = farby[1:nlevels(as.factor(dat2[, stlpce[ktore]]))])
  } else {
  	if(length(stlpce) > 1){ layout(matrix(c(1,1,2), ncol = 3)) }
	nakresli_lmm(model = fit, dat = dat2, zavisla = zavisla, nezavisla = stlpce[ktore],
		xlab = popisok, ylab = popisok.y)
		if(length(stlpce) > 1){
		plot.new()
		legend("topleft", title = "Covariates:", 
		  legend = sapply(seq_along(stlpce)[-ktore], \(x) switch(stlpce[x], temperature = expression("Temperature ("^"o" * "C)"),
  	   temp = expression("Temperature ("^"o" * "C)"),
  	   animal.temp = expression("Temperature ("^"o" * "C)"),
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