#### Setting Environment ####
# Using R version 3.3.2
setwd('~/Documents/meeting-free-t-days-analysis')
#source('credentials.R')
library(httr)
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
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
           blacklisted = grepl(blacklisted_recurring_ids, recurringEventId))

# Internal meetings used in analysis
company_info <- gs_key(gs_id)
scope_start <- as.Date('2015-07-27')
scope_end <- as.Date('2016-07-17')
employees <- gs_read(company_info, 'directory') %>%
    mutate(end_date = as.Date(ifelse(is.na(end_date), Sys.Date(), end_date)))
scope_employees <- employees %>%
    filter(start_date <= scope_start & end_date >= scope_end &
               (dept != 'finance' | is.na(dept)))

# Filtering
mtgs <- events %>%
    filter(!(is.na(attendees_attendee_1) | is.na(start_dt) | is_ext_mtg |
                 has_ext_organizer | is_vacation | blacklisted) &
               start_date >= scope_start & start_date <= scope_end &
               organizer_email %in% scope_employees$email) %>%
    distinct(id, .keep_all = TRUE)

#### Aggregating ####
# Adding features
hq_holidays <- gs_read(company_info, 'holidays') %>%
    filter(location %in% c('global', 'toronto')) %>%
    mutate(is_holiday = TRUE)
summits <- gs_read(company_info, 'summits') %>%
    mutate(is_summit = TRUE)
employee_tenures <- NULL
for (i in 1:nrow(employees)) {
    employee_tenure <- data_frame(
        employee = employees$email[i],
        date = with(employees, seq(start_date[i], end_date[i], by = 'day')))
    employee_tenures <- bind_rows(employee_tenures, employee_tenure)
}
n_employees_day <- employee_tenures %>%
    group_by(date) %>%
    summarise(n_employees = n())
policy_start_date <- as.Date('2016-01-04')

# By day
mtgs_day <- mtgs %>%
    group_by(date = start_date) %>%
    summarise(n_mtgs = n()) %>%
    full_join(data_frame(date = seq(scope_start, scope_end, by = 'day')),
              by = 'date') %>%
    left_join(select(hq_holidays, date, is_holiday), by = 'date') %>%
    left_join(select(summits, date, is_summit), by = 'date') %>%
    left_join(n_employees_day, by = 'date') %>%
    replace_na(list(n_mtgs = 0, is_holiday = FALSE, is_summit = FALSE)) %>%
    mutate(week = as.Date(cut(date, breaks = 'week')),
           day_of_week = weekdays(date),
           is_t_day = ifelse(day_of_week %in% c('Tuesday', 'Thursday'),
                             TRUE, FALSE),
           is_weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'),
                               TRUE, FALSE),
           is_biz_day = ifelse(is_weekend == TRUE | is_holiday == TRUE,
                               FALSE, TRUE))

# By week, incl. only business days
mtgs_wk_biz_days <- mtgs_day %>%
    filter(is_biz_day) %>%
    group_by(week, is_t_day) %>%
    summarise(n_mtgs = sum(n_mtgs),
              n_biz_days = sum(is_biz_day),
              is_summit_week = ifelse(sum(is_summit) > 0, TRUE, FALSE),
              avg_n_employees = mean(n_employees)) %>%
    mutate(avg_n_mtgs_biz_day = n_mtgs / n_biz_days,
           policy_ind = ifelse(is_summit_week, NA,
                               ifelse(week >= policy_start_date, 1, 0))) %>%
    ungroup()

# Plot by T-day/non-T-day and week
mtgs_wk_biz_days %>%
    ggplot(aes(week, avg_n_mtgs_biz_day, color = is_t_day)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    lims(y = c(0, 25)) +
    labs(title = 'Avg number of internal meetings for T-days and non-T-days by week')
ggsave('mtgs-t-day-week.png')

#### Time Series Analysis ####
# Select day group
mtgs_wk_biz_days_group <- filter(mtgs_wk_biz_days, is_t_day)

# Box-Jenkins methodology
acf(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day, na.action = na.pass)
pacf(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day, na.action = na.pass)
mod_arma <- Arima(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day, order = c(1, 0, 0))
summary(mod_arma)
acf(mod_arma$residuals, na.action = na.pass)
pacf(mod_arma$residuals, na.action = na.pass)
Box.test(mod_arma$residuals, lag = 25, type = 'Ljung-Box')

# Intervention analysis
mod_int <- Arima(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day,
                 order = c(1, 0, 0), xreg = mtgs_wk_biz_days_group$policy_ind)
summary(mod_int)
fitted_mtgs <- data_frame(
    week = mtgs_wk_biz_days_group$week,
    avg_n_mtgs_biz_day = fitted(mod_int))

# Plotting model fit for T-days
t_day_fit_plot <- mtgs_wk_biz_days_group %>%
    ggplot(aes(week, avg_n_mtgs_biz_day)) +
    geom_line() +
    geom_line(data = fitted_mtgs) +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    lims(y = c(0, 25)) +
    labs(title = 'Avg number of internal meetings for T-days by week')

# Non-T-days
mtgs_wk_biz_days_group <- filter(mtgs_wk_biz_days, !is_t_day)

mod_int <- Arima(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day,
                 order = c(0, 0, 0), xreg = mtgs_wk_biz_days_group$policy_ind)
summary(mod_int)
fitted_mtgs <- data_frame(
    week = mtgs_wk_biz_days_group$week,
    avg_n_mtgs_biz_day = fitted(mod_int))

# Plotting model fit for T-days
non_t_day_fit_plot <- mtgs_wk_biz_days_group %>%
    ggplot(aes(week, avg_n_mtgs_biz_day)) +
    geom_line() +
    geom_line(data = fitted_mtgs) +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    lims(y = c(0, 25)) +
    labs(title = 'Avg number of internal meetings for non-T-days by week')

library(gridExtra)
fit_plots <- grid.arrange(t_day_fit_plot, non_t_day_fit_plot)
ggsave('fit_plots.png', fit_plots)
