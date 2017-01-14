#' Combined Graphs
#' 
#' Creates several graphs in a single .png file:
#'     - global active power (Y axis) at given time instant (X axis);
#'     - voltage (Y axis) at given time instant (X axis);
#'     - energy sub meters values (Y axis) at given time instant (X axis);
#'     - global reactive power (Y axis) at given time instant (X axis).
#' 
#' The graph created is saved in the working directory as 'plot4.png' file.
#' 
#' Function dependencies note. The function requires 'dplyr' and 'lubridate'
#' packages to be installed.
#' 
#' @param download_data Logical. Indicates whether to download data set to
#' process automatically.
#'     - TRUE: If no 'household_power_consumption.txt' file is found in
#'       the working directory, the file will be downloaded automatically.
#'     - FALSE: If no 'household_power_consumption.txt' file is found in
#'       the working directory, an error is produced.
plot4 <- function(download_data = TRUE)
{
	suppressMessages(
		{
			library(dplyr)
			library(lubridate)
		}
	)
	
	data_set_file_name <- "household_power_consumption.txt"
	
	## If there is no required data set file to process in the working directory
	if (!file.exists(data_set_file_name))
	{
		if (download_data)
		{
			message("There is no '",
				data_set_file_name,
				"' data set file to process. Downloading required data...")
			
			## Coursera's link
			data_set_url <-
				"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
			## UCI's link. Used in case Coursera's link fails
			data_set_url_alternative <-
				"https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
			
			## Temporary zip file to download required data to
			temp_file <- tempfile(
				pattern = "household_power_consumption_temp",
				fileext = ".zip")
			
			## If failed to download data from the Coursera's link
			if (download.file(url = data_set_url,
					method = "curl",
					quiet = 2,
					destfile = temp_file)
			)
			{
				message("Failed to download data set from the main source.",
					" Attempting to download data from the reserve one...")
				
				## If failed to download data from the reserve link
				if (download.file(url = data_set_url_alternative,
						method = "curl",
						quiet = 2,
						destfile = temp_file)
				)
				{
					## If failed to delete temporary file
					if (unlink(temp_file))
					{
						warning("Failed to remove temporary file ", temp_file)
					}
					
					stop("Failed to download the data set")
				}
			}
			
			unzip(zipfile = temp_file, files = data_set_file_name)
			
			## If failed to delete temporary file
			if (unlink(temp_file))
			{
				warning("Failed to remove temporary file ",
					temp_file,
					" after extracting required data set from it")
			}
		}
		else
		{
			stop("There is no ",
				 data_set_file_name,
				 " data set file to process.")
		}
	}
	
	data_set <-
		read.table(
			file = data_set_file_name,
			header = TRUE,
			sep = ";",
			na.strings = "?"
		)
	
	data_set$Date <- dmy(data_set$Date)
	
	required_dates_subset <-
		filter(
			.data = data_set,
			Date == dmy("1.02.2007") | Date == dmy("2.02.2007")
		) %>%
		mutate(date_time = ymd_hms(paste(Date, Time)))
	
	## To show exact X-axis tick labels (in English) some machines need to
	## switch 'LC_TIME' locale for current session. Currently set value is saved
	## with a view set to set it back when the process is done
	previous_LC_TIME_locale <-
		strsplit(Sys.getlocale(), split = ";")[[1]] %>%
		grep(pattern = "^LC_TIME", ., value = TRUE) %>%
		sub(pattern = "LC_TIME=", replacement = "", .)
	
	Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
	
	saved_file_name <- "plot4.png"
	
	png(saved_file_name)
	par(mfrow = c(2, 2))
	
	
	##### Making plots ----------------------------------------------------#####
	#####-1# Top left -----------------------------------------------------#####
	plot(x = required_dates_subset$date_time,
		y = required_dates_subset$Global_active_power,
		type = "l",
		xlab = "",
		ylab = "Global Active Power")
	#####-1# --------------------------------------------------------------#####
	
	
	#####-2# Top right ----------------------------------------------------#####
	plot(x = required_dates_subset$date_time,
		y = required_dates_subset$Voltage,
		type = "l",
		xlab = "datetime",
		ylab = "Voltage")
	#####-2# --------------------------------------------------------------#####
	
	
	#####-3# Bottom left --------------------------------------------------#####
	plot(x = required_dates_subset$date_time,
		y = required_dates_subset$Sub_metering_1,
		type = "l",
		xlab = "",
		ylab = "Energy sub metering")
	
	lines(x = required_dates_subset$date_time,
		y = required_dates_subset$Sub_metering_2,
		col = "red")
	lines(x = required_dates_subset$date_time,
		y = required_dates_subset$Sub_metering_3,
		col = "blue")
	
	legend("topright",
		## Explicit names are used instead of variable names subseting
		## with a view to prevent risk of incorrect names if variables in
		## the data set are scattered
		legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		col = c("black", "red", "blue"),
		lty = 1,
		bty = "n")
	#####-3# --------------------------------------------------------------#####
	
	
	#####-4# Bottom right -------------------------------------------------#####
	with(
		data = required_dates_subset,
		expr = plot(
			x = date_time,
			y = Global_reactive_power,
			type = "l",
			xlab = "datetime"
		)
	)
	#####-4# --------------------------------------------------------------#####
	##### -----------------------------------------------------------------#####
	
	
	dev.off()
	Sys.setlocale(category = "LC_TIME", locale = previous_LC_TIME_locale)
	
	message("Result saved to '", saved_file_name, "' file")
}
