
# Loads the necessary libraries
library(dplyr) # Package used for data frame manipulation (http://www.r-bloggers.com/hands-on-dplyr-tutorial-for-faster-data-manipulation-in-r/)
library(lubridate) # Package used for date manipulation (https://www.google.com/?gws_rd=ssl#q=rvest+tutorial)
library(rvest) # Package used to for webscraping (https://www.google.com/?gws_rd=ssl)
library(stringr) # Package used for string manipulation (https://www.google.com/?gws_rd=ssl#q=stringr+functions)


# This function is used to create the "date table". The only arguments you pass in are the begin and end date
Create.Date.Table <- function(start.date, end.date) {

    #Base date table
    Dates <- seq(ymd(start.date), ymd(end.date), by="days") # Creates a vector of dates starting with the start.date and ending with the end.date incrementing by a day
    FiscalYearEndMonth = 6
    
    # Creates a one column data frame based on the Dates vector created above
    DateTable <- data.frame(Dates)
    
    DateTable <- DateTable %>%
      # Uses the mutate dplyr verb and a few functions from the lubridate and base package to create date attribute fields for the "DateTable" data frame  
      mutate("DateKey" = format(Dates, "%Y%m%d") # Uses the format function to format the date in YYYYMMDD format so that it can be used as a key
             ,"Month Name" = format(Dates, "%b") # Uses the format function to return the proper abbreviated month name for the given date.
             ,"Weekday Name" = wday(Dates, label = TRUE) # Uses the wday function to return the proper abbreviated weekday name for the given date. When you set the label property equal to "TRUE" it returns the name of the weekday. When it is set to false or not specified it return a key that represents the position in the week of the day.
             ,"Weekday Key" = wday(Dates) # Uses the wday function to return the integer representation of the weekday for the given date.
             ,"Year" = year(Dates) # Uses the year function to return the year for the given date.
             ,"Fiscal Year" = Year + ifelse(month(Dates) > FiscalYearEndMonth,1,0) # Calculates the fiscal year. It does so by first using the FiscalYearEndMonth variable to determine the end of the fiscal year. Any date that occurs after that month is considered to be part of the new fiscal year.
             ,"Month Key" = month(Dates) # Uses the month function to return the integer representation of the month for the given date.
             ,"Month Day" = mday(Dates) # Uses the mday function to return the day of the month for the given date
             ,"Iso Year" = isoyear(Dates) # Uses the isoyear function to return the iso year for the given date. To learn more about "iso dates" go to this wiki: https://en.wikipedia.org/wiki/ISO_week_date
             ,"Week" = week(Dates) # Uses the week function to return the week of the year for the given date
             ,"Iso Week" = isoweek(Dates) # Uses the isoweek function to return the iso week of the year for the given date
             ,"Quarter" = paste("Q",quarter(Dates),sep="") # Uses the quarter function to return the quarter of the year for the given date
             ,"Quarter Day" = qday(Dates) # (looks like it is returning seconds) USes the qday function to return the day of the quarter for the given date
             ,"Year Day" = yday(Dates) # Uses the yday function to return the day of the year for the given date
             ,"Weekend" = ifelse(wday(Dates) %in% c(1,7),TRUE,FALSE) # Determines if the given date occurs in the weekend based on the wkday function
      ) %>%
      # The select verb below is used to reorder the fields in the data frame in a more logical order    
      select(`DateKey`,`Dates`,`Year`,`Fiscal Year`,`Iso Year`,`Year Day`,`Quarter`,`Quarter Day`,`Month Name`,`Month Key`,
             `Month Day`,`Week`,`Iso Week`,`Weekday Name`,`Weekday Key`,`Weekend`
      )
    
    
    #Add Federal holidays
    cal_years <- unique(DateTable$Year) # Creates a unique list of years based on the "Year" field in the "DateTable" data frame created above
    federal_holidays <- character() # Creates an empty character vector that will be used to store a vector of federal holidays that will get scrapped from www.holidayscalendar.com below
    
    # The loop below scraps federal holiday data from www.holidayscalendar.com for each year in the "Date Table" created above 
    for (cal_year in cal_years) { 
      
      # Builds a url string based on the concatenation of the static part of the url and the cal_year
      url <- paste("http://www.holidayscalendar.com/holidays/federal/?year=",cal_year,sep="")
      
      # Uses the rvest package to extract the federal holidays from the website for each year passed to it. It does so by returning the html of the site defined
      # by the url string by using the read_html function. It uses the html_nodes function to identify all of the nodes that contains the federal holidays and
      # pass them back to us using the html_text() function in the form of a character vector. You can use the "SelectorGadget" tool to identify the nodes you
      # want. You can find more about the "SelectorGadget" tool here: http://selectorgadget.com/.
      holiday_data <- 
        read_html(url) %>%
        html_nodes(".calendarevent") %>%
        html_text()
      
      # The dates are only in every other member in the vector. The vector below is used to slice out the memebers that contain dates.
      date_elements <- seq(from = 1, to = length(holiday_data)-1,by=2)
      
      # Creates a new vector that just has the dates
      holiday_data <- holiday_data[date_elements]
      
      # The first part of the value that contains the date is prefaced with the weekday name. The str_loacte finds the start and end
      # position of the part of the string that has the day information for each member of the vector using a regex. The information is
      # returned in the form of a matrix.
      WeekdayPart <- str_locate(holiday_data,"^.*,")
      
      # The code below uses the str_sub function to extract the part of the string that contains the date. It uses the second column
      # in the weekdayPart matrix to determine what the start position is for the date
      holiday_data <- str_trim(str_sub(holiday_data, WeekdayPart[,2]+1))
      
      # Concatenates the given year to each element in the vector. The result will be a string with the following format ("MMMM dd yyyy")
      holiday_data <- paste(holiday_data, cal_year, sep=" ")
      
      # Converts the character vector to a date vector using the mdy function from lubridate
      holiday_data <- mdy(holiday_data)
      
      # Appends the federal holidays for the given year to the federal_holidays vector
      federal_holidays <- append(federal_holidays, holiday_data)
    
    }
    
    # Adds a boolean field called "Federal Holiday" to the DateTable data frame. For each date in the DateTable dataframe it test whether
    # the date is one of the dates in the federal_holidays vector and if it is it returns TRUE otherwise it returns FALSE
    DateTable$`Federal Holiday` <- DateTable$Dates %in% federal_holidays 

    return(DateTable) # Returns a data frame of a date table based on the date parameters passed in.
}

My.R.Date.Table <- Create.Date.Table(start.date = "2010-01-01", end.date = "2016-12-31")

