# How to Create a Date Table using R

In this repository you will find a R script that generates a "date" table based on a start date and end date. I created this script to generate a date table in Microsoft's Power BI. If you have questions please contact me at rwade@dieselanalytics.com and I will do my best to answer them.

FYI, I am having trouble with the "Quarter Day" field. for some reason it returns seconds and not the day. I am investigating this problem. If you know how to correct it then please submit a pull request with the solution. Thanks!

## Instructions on how to use a R script as a data source in Power BI
1. Make sure you have R installed on your machine
2. Open up your IDE of choice (I prefer R Studio)
2. Create a R script that returns the data set that you want to import into Power BI as a data.frame
2. Test the script and if it works copy it into memory
3. Open up Power BI Desktop
4. Click "Get Data" > "Other" > "R Script"
5. Paste the script into the editor and click "Ok"
6. After you execute the script the data.frame will be available to you as a data set that you can import into Power BI
