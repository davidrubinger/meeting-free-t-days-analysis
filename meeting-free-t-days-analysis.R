setwd('~/Documents/meeting-free-t-days-analysis')
library(httr)
library(dplyr)

#### Reading Google Calendar Data ####
#source('credentials.R')
my_app <- oauth_app('GOOGLE_APIS', key = my_app_key, secret = my_app_secret)
google_token <- oauth2.0_token(
    oauth_endpoints('google'), my_app,
    scope = 'https://www.googleapis.com/auth/calendar.readonly')

# Getting calendars
req_cals <- GET("https://www.googleapis.com/calendar/v3/users/me/calendarList",
                config(token = google_token))
cals <- content(req_cals)$items %>%
    lapply(., function (x) x$id) %>%
    unlist() %>%
    data_frame() %>%
    filter(grepl('polar.me', `.`)) %>%
    .$`.` %>%
    sort()

# Reading events from calendars
events_list_all <- NULL
for (i in 1:length(cals)) {
    print(paste('Reading calendar', cals[i], '...'))
    req <- GET(paste0('https://www.googleapis.com/calendar/v3/calendars/',
                      cals[i], '/events?maxResults=1e6'),
               config(token = google_token))
    events_list <- lapply(content(req)$items,
                          function (x) {
                              x[['calendar']] <- cals[i]
                              x})
    events_list_all <- append(events_list_all, events_list)
}
