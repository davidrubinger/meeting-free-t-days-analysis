# Meeting-Free T-Days Analysis

This repo gathers the calendar data, runs the statistical analyses, and produces the plots that comprised the Meeting-Free T-Days Analysis blog post.

To run the entire analysis, open and run the `scripts-runner.R` script.

Note: `scripts/1-load-data.R` loads a credentials file that contains a Google API key and secret--to access the Google Calendar API--as well as a Google Sheets ID--to access spreadsheets containing employee, holiday, and summit information specific to the company. You'll need to create these yourself and save as `scripts/credentials.R`.