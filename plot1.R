power <- read.table("household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
power2 <- power[power$Date %in% c("1/2/2007","2/2/2007") ,]
power2$Date <- as.Date(power2$Date, format = "%d/%m/%Y")
power2$Date <- with(power2, paste(Date, "T", Time, sep = " "))
power2$Date <- strptime(power2$Date, format = c("%Y-%m-%d T %H:%M:%S")) 
power2[,3:9] <- apply(power2[,3:9], 2, function(x) as.numeric(x))
gap<-power2$Global_active_power
png(file="plot1.png", width=480, height=480)
hist(gap,col="red",xlab="Global Active Power (kilowatts)", main="Global Active Power", cex.axis=0.6)
dev.off()