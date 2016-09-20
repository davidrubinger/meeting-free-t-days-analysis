setwd('~/Documents/meeting-free-t-days-analysis')
library(httr)
library(dplyr)
library(zoo)

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

#### Tidying Data ####
is_ext_attendee <- function (attendee) {
    int_domains <- c('polar.me', 'polarmobile.com')
    !(grepl(paste(int_domains, collapse = '|'), attendee) | is.na(attendee))
}
is_vacation_attendee <- function (attendee) {
    grepl('vacations@polar.me', attendee)
}

events <- bind_rows(lapply(events_list_all, data.frame,
                           stringsAsFactors = FALSE)) %>%
    rename(organizer = organizer.email, creator = creator.email,
           start_time = dateTime, end_time = dateTime.1) %>%
    select(start_time, organizer, summary, calendar, description, creator,
           location, end_time, contains('attendees.email'), id, iCalUID, etag) %>%
    mutate_each(funs(is_ext_attendee = is_ext_attendee(.),
                     is_vacation_attendee = is_vacation_attendee(.)),
                contains('attendees.email')) %>%
    mutate(start_time = as.POSIXct(substr(start_time, 1, 19),
                                   format = "%Y-%m-%dT%H:%M:%S",
                                   tz = 'America/Toronto'),
           yr_mo = as.yearmon(start_time),
           day_of_week = weekdays(start_time),
           is_ext_mtg = rowSums(
               select(., contains('is_ext_attendee'))) > 0,
           is_vacation = rowSums(
               select(., contains('is_vacation_attendee'))) > 0)

# Internal meetings
int_mtgs <- events %>%
    filter(!(is.na(attendees.email) | is_ext_mtg | is_vacation)) %>%
    distinct(id, .keep_all = TRUE)
