###Week 2 Assignment

#Explore 'airquality' dataset in RStudio

View(airquality)

#Explore relationship between solar radiation (in lang) and ozone (ppb).
ozone=as.numeric(airquality$Ozone, na.rm=TRUE)
solar=as.numeric(airquality$Solar.R, na.rm=TRUE)

solar.ozone=plot(solar, ozone, xlab="Solar Radiation (lang)", ylab="Ozone (ppb)", col="red", abline(lm(ozone~solar)))

#How frequently did ozone exceed 70 ppb (NAAQS)?
aboveNAAQS=table(ozone>70)
#Ozone exceeded NAAQS (70 ppb) on 25 days; did not exceed on 91 days.


average.ozone=mean(ozone, na.rm=TRUE)
#42.1 ppb



