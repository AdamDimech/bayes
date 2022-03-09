
# Clear workspace
rm(list=ls())

# Set path
setwd("C:\\Data\\projects\\R-projects\\B2021004-Glasshouse-Temps")

# Get data
cold.data <- fread("PPVGH1_temperatures.csv", data.table=FALSE)

# Set variables
temp <- cold.data$`Control Temp`
set <- cold.data$Setpoint
date <- substr(cold.data$Timestamp, 0, 10)
date <- as.Date(date, "%Y-%m-%d")
time <- substr(cold.data$Timestamp, 12, 19)
timestamp <- as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")
cold.temps <- data.frame(timestamp, set, temp)

# Plot
ggplot(cold.temps, aes(timestamp)) + geom_line(aes(y = temp, colour = "temp")) + geom_line(aes(y = set, colour = "set")) + labs(title="PPV Bundoora: Cold Temperature Measurements (Glasshouse 1)", x="Date and Time", y="Temperature (°C)", colour = "Measurement") + theme(axis.text.x = element_text(angle = 270, vjust = 0.2, hjust = 0.0, size=10), axis.text.y = element_text(size=12), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), title = element_text(size=12)) + scale_fill_manual(name = "Measuremets", labels = c("Set Temperature", "Actual Temperature")) + scale_color_manual(labels = c("Target", "Observed"), values = c("red3", "cyan4")) + scale_x_datetime(expand=c(0,0), date_breaks= "1 days", date_minor_breaks = "1 days", date_labels = "%d %b %Y")

# Save file
ggsave("temperatures_plot_ppv_GH1.svg", plot = last_plot(), device = png(), path = NULL, scale = 0.7, width = 40, height = 25, units = "cm", dpi = 300, limitsize = TRUE)
dev.off()


# Subset to 18-degrees
cold.data.18 <- cold.data[cold.data$Setpoint==18,]

# Fix format
temp.18 <- cold.data.18$`Control Temp`
set.18 <- cold.data.18$Setpoint
date.18 <- substr(cold.data.18$Timestamp, 0, 10)
date.18 <- as.Date(date.18, "%Y-%m-%d")
time.18 <- substr(cold.data.18$Timestamp, 18, 19)
timestamp.18 <- as.POSIXct(paste(date.18, time.18), format="%Y-%m-%d %H:%M:%S")
cold.temps.18 <- data.frame(timestamp.18, set.18, temp.18)

# Plot
ggplot(cold.temps, aes(timestamp)) + geom_line(aes(y = temp, colour = "temp")) + geom_line(aes(y = set, colour = "set")) + labs(title="PPV Bundoora: Cold Temperature Measurements (Glasshouse 1)", x="Date and Time", y="Temperature (°C)", colour = "Measurement") + theme(axis.text.x = element_text(angle = 270, vjust = 0.2, hjust = 0.0, size=10), axis.text.y = element_text(size=12), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), title = element_text(size=12)) + scale_fill_manual(name = "Measuremets", labels = c("Set Temperature", "Actual Temperature")) + scale_color_manual(labels = c("Target", "Observed"), values = c("red3", "cyan4")) + scale_x_datetime(expand=c(0,0), date_breaks= "1 days", date_minor_breaks = "1 days", date_labels = "%d %b %Y")

# Save
ggsave("temperatures_plot_ppv_GH1.svg", plot = last_plot(), device = png(), path = NULL, scale = 0.7, width = 40, height = 25, units = "cm", dpi = 300, limitsize = TRUE)
dev.off()

# Summary
summary(temp.18, quantile.type=8)

# Quantiles
quantile(temp.18, probs = seq(0, 1, 0.1), na.rm = T, names = TRUE, type = 7)

# Remove ramping temperatures from data frame (to get proper distribution of values)
cold.data.18.clean <- cold.data.18[!(cold.data.18$`Control Temp`<=16),]
cold.data.18.clean <- cold.data.18.clean[!(cold.data.18.clean$`Control Temp`>=20),]

# Fix format
temp.18.c <- cold.data.18.clean$`Control Temp`
set.18.c <- cold.data.18.clean$Setpoint
date.18.c <- substr(cold.data.18.clean$Timestamp, 0, 10)
date.18.c <- as.Date(date.18.c, "%Y-%m-%d")
time.18.c <- substr(cold.data.18.clean$Timestamp, 18, 19)
timestamp.18.c <- as.POSIXct(paste(date.18.c, time.18.c), format="%Y-%m-%d %H:%M:%S")
cold.temps.18.c <- data.frame(timestamp.18.c, set.18.c, temp.18.c)

# Plot histogram
ggplot(cold.temps.18.c, x=temp.18.c) + geom_histogram(aes(x=temp.18.c, y=..count..), size=1.1, color="cadetblue4", fill="cadetblue3") + labs(title="Distribution of PPVB Glasshouse 1 Temperature Measurements (Target 18°C)", x="Temperature Bins", y="Count") + theme(axis.text.x = element_text(angle = 270, vjust = 0.2, hjust = 0.0, size=12), axis.text.y = element_text(size=12), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), title = element_text(size=12)) + scale_x_continuous(breaks = seq(round(min(temp.18, na.rm=T), 0)-1, round(max(temp.18, na.rm=T), 0), by = 0.5))

ggsave("temperatures_histogram_18degrees_ppv_GH1.svg", plot = last_plot(), device = png(), path = NULL, scale = 0.7, width = 40, height = 25, units = "cm", dpi = 300, limitsize = TRUE)
dev.off()



# Subset to 18-degrees
cold.data.12 <- cold.data[cold.data$Setpoint==12,]

# Fix format
temp.12 <- cold.data.12$`Control Temp`
set.12 <- cold.data.12$Setpoint
date.12 <- substr(cold.data.12$Timestamp, 0, 10)
date.12 <- as.Date(date.12, "%Y-%m-%d")
time.12 <- substr(cold.data.12$Timestamp, 18, 19)
timestamp.12 <- as.POSIXct(paste(date.12, time.12), format="%Y-%m-%d %H:%M:%S")
cold.temps.12 <- data.frame(timestamp.12, set.12, temp.12)

# Summary
summary(temp.12, quantile.type=8)

# Quantiles
quantile(temp.12, probs = seq(0, 1, 0.1), na.rm = T, names = TRUE, type = 7)