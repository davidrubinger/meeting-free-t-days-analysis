#### Setting Environment ####
# Using R version 3.3.2
setwd('~/Documents/meeting-free-t-days-analysis')
#source('credentials.R')
library(httr)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

#### Reading Google Calendar Data ####
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
                      cals[i], '/events?maxResults=2500&orderBy=startTime&',
                      'singleEvents=true'),
               config(token = google_token))
    events_list <- lapply(content(req)$items,
                          function (x) {
                              x[['calendar']] <- cals[i]
                              x})
    events_list_all <- append(events_list_all, events_list)
    
    # If there's more than one page of results
    while (!is.null(content(req)$nextPageToken)) {
        req <- GET(paste0('https://www.googleapis.com/calendar/v3/calendars/',
                          cals[i], '/events?maxResults=2500&orderBy=startTime&',
                          'singleEvents=true&pageToken=',
                          content(req)$nextPageToken),
                   config(token = google_token))
        events_list <- lapply(content(req)$items,
                              function (x) {
                                  x[['calendar']] <- cals[i]
                                  x})
        events_list_all <- append(events_list_all, events_list)
    }
}

#### Tidying Data ####
list_element_to_df <- function (list_elements, element) {
    if (element == 'attendees') {
        list_elements <- lapply(list_elements, function (x) {
            if (is.null(x[['attendees']]))
                x[['attendees']][[1]] <- list(email = NA)
            x
        })
        list_element <- lapply(list_elements, function (x) {
            lapply(x[['attendees']], function (y) y[['email']])
        })
        for (i in 1:length(list_element)) {
            names(list_element[[i]]) <- paste(
                'attendee', seq(length(list_element[[i]])), sep = '_')
        }
    } else if (element %in% c('organizer', 'creator')) {
        list_elements <- lapply(list_elements, function (x) {
            if (is.null(x[[element]]))  x[[element]] <- NA
            x
        })
        list_element <- lapply(list_elements, function (x) x[[element]])
        list_element <- lapply(list_element, function (x) {
            if (is.na(x[[1]]))  x <- list(email = NA)
            x
        })
    } else {
        list_elements <- lapply(list_elements, function (x) {
            if (is.null(x[[element]]))  x[[element]] <- NA
            x
        })
        list_element <- lapply(list_elements, function (x) x[[element]])
    }
    df_element <- bind_rows(lapply(list_element, as_data_frame))
    names(df_element) <- gsub('_value', '',
                              paste(element, names(df_element), sep = '_'))
    df_element
}

# Binding list together into data frame
features <- c('start', 'end', 'summary', 'description', 'location',
              'organizer', 'creator', 'calendar', 'id', 'recurringEventId',
              'attendees')
events_df <- NULL
for (i in 1:length(features)) {
    events_df <- bind_cols(
        events_df, list_element_to_df(events_list_all, features[i]))
}

is_ext_attendee <- function (attendee) {
    int_domains <- c('polar.me', 'polarmobile.com')
    !(grepl(paste(int_domains, collapse = '|'), attendee) | is.na(attendee))
}
is_vacation_attendee <- function (attendee) {
    grepl('vacations@polar.me', attendee)
}

# Ad hoc non-internal meeting events to remove
blacklisted_recurring_ids <- c(
    '_89142ca36sq3eba174rk8b9k84s4cb9o60s38ba684o48d1n84qjccpk70')

policy_start_date <- as.Date('2016-01-04')

# Tidying
events <- events_df %>%
    rename(start_dt = start_dateTime, end_dt = end_dateTime,
           organizer_name = organizer_displayName,
           creator_name = creator_displayName) %>%
    mutate_each(funs(is_ext_attendee = is_ext_attendee(.),
                     is_vacation_attendee = is_vacation_attendee(.)),
                contains('attendee')) %>%
    mutate(start_dt = as.POSIXct(substr(start_dt, 1, 19),
                                 format = "%Y-%m-%dT%H:%M:%S",
                                 tz = 'America/Toronto'),
           start_date = as.Date(ifelse(
               is.na(start_date), as.character(as.Date(start_dt)), start_date)),
           week = as.Date(cut(start_date, breaks = 'week')),
           day_of_week = weekdays(start_date),
           is_t_day = ifelse(day_of_week %in% c('Tuesday', 'Thursday'),
                             TRUE, FALSE),
           is_weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'),
                               TRUE, FALSE),
           has_ext_organizer = is_ext_attendee(organizer_email),
           is_ext_mtg = rowSums(
               select(., contains('is_ext_attendee'))) > 0,
           has_vacation_attendee = rowSums(
               select(., contains('is_vacation_attendee'))) > 0,
           has_vacation_organizer = ifelse(
               is.na(organizer_name) | organizer_name != 'Vacations',
               FALSE, TRUE),
           has_vacation_creator = ifelse(
               is.na(creator_name) | creator_email != 'tempus.nova@polar.me',
               FALSE, TRUE),
           has_vacation_location = ifelse(
               is.na(location) | location != 'Vacations', FALSE, TRUE),
           is_vacation = ifelse(
               has_vacation_attendee | has_vacation_location |
                   has_vacation_organizer | has_vacation_creator, TRUE, FALSE),
           blacklisted = grepl(blacklisted_recurring_ids, recurringEventId),
           policy_ind = ifelse(start_date >= policy_start_date, 1, 0))

# Internal meetings used in analysis
company_info <- gs_key(gs_id)
scope_start <- as.Date('2015-07-27')
scope_end <- as.Date('2016-07-17')
scope_employees <- gs_read(company_info, 'directory') %>%
    mutate(end_date = as.Date(ifelse(is.na(end_date), Sys.Date(), end_date),
                              origin = '1970-01-01')) %>%
    filter(start_date <= scope_start & end_date >= scope_end &
               (dept != 'finance' | is.na(dept)))

mtgs <- events %>%
    filter(!(is.na(attendees_attendee_1) | is.na(start_dt) | is_ext_mtg |
                 has_ext_organizer | is_vacation | blacklisted) &
               start_date >= scope_start & start_date <= scope_end &
               organizer_email %in% scope_employees$email) %>%
    distinct(id, .keep_all = TRUE)

hq_holidays <- gs_read(company_info, 'holidays') %>%
    filter(location %in% c('global', 'toronto')) %>%
    mutate(is_holiday = TRUE)

summits <- gs_read(company_info, 'summits') %>%
    mutate(is_summit = TRUE)

#### Aggregating ####
# By day
mtgs_day <- mtgs %>%
    group_by(date = start_date) %>%
    summarise(n_mtgs = n()) %>%
    full_join(data_frame(date = seq(scope_start, scope_end, by = 'day')),
              by = 'date') %>%
    left_join(select(hq_holidays, date, is_holiday), by = 'date') %>%
    left_join(select(summits, date, is_summit), by = 'date') %>%
    replace_na(list(n_mtgs = 0, is_holiday = FALSE, is_summit = FALSE)) %>%
    mutate(week = as.Date(cut(date, breaks = 'week')),
           day_of_week = weekdays(date),
           is_t_day = ifelse(day_of_week %in% c('Tuesday', 'Thursday'),
                             TRUE, FALSE),
           is_weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'),
                               TRUE, FALSE),
           is_reg_biz_day = ifelse(is_weekend == TRUE | is_holiday == TRUE |
                                       is_summit == TRUE, FALSE, TRUE))
