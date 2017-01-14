#' Global Active Power Histogram
#' 
#' Creates histogram representing global active power values frequency.
#' The histogram created is saved in the working directory as 'plot1.png'.
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
plot1 <- function(download_data = TRUE)
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
		)
	
	saved_file_name <- "plot1.png"
	png(saved_file_name)
	
	hist(
		required_dates_subset$Global_active_power,
		main = "Global Active Power",
		xlab = "Global Active Power (kilowatts)",
		col = "red"
	)
	
	dev.off()
	
	message("Result saved to '", saved_file_name, "' file")
}
