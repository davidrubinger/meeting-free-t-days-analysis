#### Setting Environment ####
library(googlesheets)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forecast)
theme_set(theme_bw() +
              theme(panel.border = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = rel(1.5)),
                    axis.text = element_text(size = rel(1.1)),
                    axis.title = element_text(size = rel(1.2)),
                    legend.text = element_text(size = rel(1.2)),
                    strip.background = element_rect(fill = 'gray95',
                                                    color = 'gray95'),
                    strip.text = element_text(size = rel(1.2))))

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
company_info <- gs_key(gs_id)
blacklisted_terms <- gs_read(company_info, 'meeting_summary_blacklist') %>%
    .$term

# Tidying and creating features
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
           start_hr = hour(start_dt),
           is_personal_event = is.na(attendees_attendee_1) |
               (is.na(attendees_attendee_2) &
                    attendees_attendee_1 == organizer_email),
           has_ext_organizer = is_ext_attendee(organizer_email),
           is_ext_mtg = rowSums(
               select(., contains('is_ext_attendee'))) > 0,
           is_after_hrs = ifelse((start_hr >= 9 & start_hr < 17) |
                                     is.na(start_hr), FALSE, TRUE),
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
           blacklisted = grepl(paste(blacklisted_terms, collapse = '|'),
                               summary, ignore.case = TRUE))

# Internal meetings used in analysis
scope_start <- as.Date('2015-07-27')
scope_end <- as.Date('2016-07-17')
employees <- gs_read(company_info, 'directory') %>%
    mutate(end_date = as.Date(ifelse(is.na(end_date), Sys.Date(), end_date)))
scope_employees <- employees %>%
    filter(start_date <= scope_start & end_date >= scope_end &
               (dept != 'finance' | is.na(dept)))

# Filtering
mtgs <- events %>%
    filter(!(is.na(start_dt) | is_personal_event | is_ext_mtg |
                 has_ext_organizer | is_after_hrs | is_vacation |
                 blacklisted) &
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
col_palette <- c('deepskyblue4', 'lightskyblue')
mtgs_wk_biz_days %>%
    mutate(is_t_day = factor(is_t_day, levels = c(TRUE, FALSE),
                             labels = c('T-Day', 'Non-T-Day'))) %>%
    ggplot(aes(week, avg_n_mtgs_biz_day, color = is_t_day)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    annotate('text', x = as.Date(policy_start_date) + 40, y = 17.5,
             label = 'Policy Start Date', size = 5) +
    lims(y = c(0, 20)) +
    labs(x = '', y = 'Meetings per Day', color = '',
         title = 'Average Number of Meetings on T-Days and Non-T-Days by Week') +
    scale_color_manual(values = col_palette)
ggsave('plots/mtgs-day-group-week.png', units = 'mm', width = 265,
       height = 140)

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

# Intervention analysis - T-days
mod_int <- Arima(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day,
                 order = c(1, 0, 0), xreg = mtgs_wk_biz_days_group$policy_ind)
summary(mod_int)
fitted_mtgs <- data_frame(
    week = mtgs_wk_biz_days_group$week,
    avg_n_mtgs_biz_day = fitted(mod_int))

# Plotting model fit for T-days
col_palette <- c('deepskyblue4', 'red')
mtgs_wk_biz_days_group %>%
    left_join(
        rename(fitted_mtgs, avg_n_mtgs_biz_day_fitted = avg_n_mtgs_biz_day),
        by = 'week') %>%
    select(week, avg_n_mtgs_biz_day, avg_n_mtgs_biz_day_fitted) %>%
    gather(type, value, -week) %>%
    mutate(type = factor(type, levels = c('avg_n_mtgs_biz_day',
                                          'avg_n_mtgs_biz_day_fitted'),
                         labels = c('Actual Data', 'Model Fit'))) %>%
    ggplot(aes(week, value, color = type)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    annotate('text', x = as.Date(policy_start_date) + 40, y = 17.5,
             label = 'Policy Start Date', size = 5) +
    lims(y = c(0, 20)) +
    labs(x = '', y = 'Meetings per Day', color = '',
         title = 'Average Number of Meetings on T-Days and Model Fit by Week') +
    scale_color_manual(values = col_palette)
ggsave('plots/t-day-fit-plot.png', units = 'mm', width = 265,
       height = 140)

# Intervention analysis - non-T-days
mtgs_wk_biz_days_group <- filter(mtgs_wk_biz_days, !is_t_day)

mod_int <- Arima(mtgs_wk_biz_days_group$avg_n_mtgs_biz_day,
                 order = c(0, 0, 0), xreg = mtgs_wk_biz_days_group$policy_ind)
summary(mod_int)
fitted_mtgs <- data_frame(
    week = mtgs_wk_biz_days_group$week,
    avg_n_mtgs_biz_day = fitted(mod_int))

# Plotting model fit for non-T-days
col_palette <- c('lightskyblue', 'red')
mtgs_wk_biz_days_group %>%
    left_join(
        rename(fitted_mtgs, avg_n_mtgs_biz_day_fitted = avg_n_mtgs_biz_day),
        by = 'week') %>%
    select(week, avg_n_mtgs_biz_day, avg_n_mtgs_biz_day_fitted) %>%
    gather(type, value, -week) %>%
    mutate(type = factor(type, levels = c('avg_n_mtgs_biz_day',
                                          'avg_n_mtgs_biz_day_fitted'),
                         labels = c('Actual Data', 'Model Fit'))) %>%
    ggplot(aes(week, value, color = type)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(policy_start_date), color = 'gray',
               linetype = 'dashed') +
    annotate('text', x = as.Date(policy_start_date) + 40, y = 17.5,
             label = 'Policy Start Date', size = 5) +
    lims(y = c(0, 20)) +
    labs(x = '', y = 'Meetings per Day', color = '',
         title = 'Average Number of Meetings on Non-T-Days and Model Fit by Week') +
    scale_color_manual(values = col_palette)
ggsave('plots/non-t-day-fit-plot.png', units = 'mm', width = 265,
       height = 140)

#### Individual-Level Analysis ####
# By organizer and day
mtgs_organizer_day <- mtgs %>%
    group_by(date = start_date,
             organizer = organizer_email) %>%
    summarise(n_mtgs = n()) %>%
    right_join(employee_tenures %>% filter(
        date >= scope_start & date <= scope_end &
            employee %in% scope_employees$email),
        by = c('date' = 'date', 'organizer' = 'employee')) %>%
    left_join(select(hq_holidays, date, is_holiday), by = 'date') %>%
    left_join(select(summits, date, is_summit), by = 'date') %>%
    replace_na(list(n_mtgs = 0, is_holiday = FALSE, is_summit = FALSE)) %>%
    mutate(week = as.Date(cut(date, breaks = 'week')),
           day_of_week = weekdays(date),
           is_t_day = ifelse(day_of_week %in% c('Tuesday', 'Thursday'),
                             TRUE, FALSE),
           is_weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'),
                               TRUE, FALSE),
           is_biz_day = ifelse(is_weekend == TRUE | is_holiday == TRUE,
                               FALSE, TRUE),
           policy_ind = ifelse(is_summit, NA,
                               ifelse(date >= policy_start_date, 1, 0)))

# By organizer, day group and policy indicator, incl. only business days
mtgs_organizer_group_ind <- mtgs_organizer_day %>%
    filter(is_biz_day & !is_summit) %>%
    group_by(organizer, is_t_day, policy_ind) %>%
    summarise(n_mtgs = sum(n_mtgs),
              n_biz_days = sum(is_biz_day)) %>%
    group_by(is_t_day, policy_ind) %>%
    mutate(avg_n_mtgs_biz_day = n_mtgs / n_biz_days,
           mtgs_share = n_mtgs / sum(n_mtgs)) %>%
    ungroup()

# Visualizing
top_n_employees <- 10
plot_employees <- mtgs_organizer_group_ind %>%
    group_by(organizer) %>%
    summarise(n_mtgs = sum(n_mtgs)) %>%
    top_n(top_n_employees, n_mtgs) %>%
    select(organizer)

# Plot
mtgs_organizer_group_ind %>%
    inner_join(plot_employees, by = 'organizer') %>%
    mutate(is_t_day = factor(is_t_day, levels = c(TRUE, FALSE),
                             labels = c('T-Day', 'Non-T-Day')),
           policy_ind = factor(policy_ind, levels = c(0, 1),
                               labels = c('Before', 'After'))) %>%
    ggplot(aes(policy_ind, avg_n_mtgs_biz_day, group = organizer,
               color = organizer)) +
    facet_wrap(~ is_t_day) +
    geom_point() +
    geom_line() +
    labs(x = 'Policy Implementation Status', y = 'Meetings per Day',
         title = paste('Average Number of Meetings per Day by Organizer\n(Top',
                       top_n_employees, 'Meeting Organizers Only)')) +
    guides(color = FALSE) +
    scale_color_hue(h = c(100, 360))
ggsave('plots/individuals-plot.png', units = 'mm', width = 265,
       height = 140)

# Individual changes in meeting organizing behaviour on T-days
mtgs_organizer_group_ind %>%
    filter(is_t_day) %>%
    select(organizer, policy_ind, avg_n_mtgs_biz_day) %>%
    spread(policy_ind, avg_n_mtgs_biz_day) %>%
    mutate(change_avg_n_mtgs_biz_day = `1` - `0`)

# Individual changes in meeting organizing behaviour on non-T-days
mtgs_organizer_group_ind %>%
    filter(!is_t_day) %>%
    select(organizer, policy_ind, avg_n_mtgs_biz_day) %>%
    spread(policy_ind, avg_n_mtgs_biz_day) %>%
    mutate(change_avg_n_mtgs_biz_day = `1` - `0`)
