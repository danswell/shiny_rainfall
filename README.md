# shiny_rainfall

## A shiny app for exploring the ACT Government rainfall data that is published by data.act.gov.au

8 January 2020


This basic app. The intent is to demonstrate a possible solution to visualising data on the data.act.gov.au portal.

It does the following:

Using R package RSocrata, pulls rainfall data from the ACT Government Open data portal, from 31 December 2019 to the current day.

It loads a .csv file of historical rainfall (from start of records to 31 December 2019), identical to the entire rainfall dataset on data.act.gov.au. This is done soley to speed up the loading of the app. It takes ~2 minutes to pull the entire dataset via API.

It merges the two seperate datasets together, does some minor data prepping (eg. set datetime as date object.)

It pulls the metadata file from data.act.gov.au - strips it back to match the rainfall records in the dataset.

Creates the UI with the site selector, datetime slider, month and year aggregator check boxes, map and plot output, and data statement at the bottom.

Finally, the Server function does the heavy lifting - filters the dataset to selected site and date time. Automatically updates the datetime slider based on data availability for the selected site.

Creates the plot using ggplot, based on the site, datetime and montly/yearly aggregator.

Danswell

