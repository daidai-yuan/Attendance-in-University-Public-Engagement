#The following script contains the core analytical code used in the MDS project.
#1 read data

library(tidyverse)
library(lubridate)

output_dir <- "figures"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dur_raw <- read_csv("Science Engagement Tracker.csv")
hesa_raw <- read_csv("table-5.csv", skip = 10)

names(dur_raw) <- str_squish(names(dur_raw))
names(hesa_raw) <- str_squish(names(hesa_raw))

# 2. Clean the Durham data
# Remove only fully identical records.
dur_unique <- dur_raw %>%
  distinct()
dur_clean <- dur_unique %>%
  rename(
    activity_name = `Activity Name`,
    department = Department,
    other_departments = `Other Department(s) / Internal Group(s) involved`,
    staff_time_hours = `Academic Staff Time`,
    venue = `Venue / Location`,
    start_datetime_raw = `Activity Date Start`,
    end_datetime_raw = `Activity Date End`,
    activity_type_raw = `Type of Activity`,
    attendee_category_raw = `Category of Attendees`,
    attendees_raw = `Number of Attendees`,
    chargeable_status_raw = `Chargeable Activity?`,
    public_status_raw = `Was this activity or event open to the public?`,
    activity_description = `Activity Description`
  ) %>%
  mutate(
    across(
      c(
        activity_name,
        department,
        other_departments,
        venue,
        activity_type_raw,
        attendee_category_raw,
        chargeable_status_raw,
        public_status_raw,
        activity_description
      ),
      ~ str_squish(as.character(.x))
    ),
    
    activity_name = na_if(activity_name, ""),
    department = na_if(department, ""),
    venue = na_if(venue, ""),
    
    attendees = parse_number(
      as.character(attendees_raw),
      locale = locale(grouping_mark = ",")
    ),
    
    staff_time_hours = as.numeric(staff_time_hours),
    
    start_datetime = dmy_hm(start_datetime_raw, quiet = TRUE),
    end_datetime = dmy_hm(end_datetime_raw, quiet = TRUE),
    
    invalid_date_order =
      !is.na(start_datetime) &
      !is.na(end_datetime) &
      end_datetime < start_datetime,
    
    activity_duration_hours = if_else(
      !is.na(start_datetime) &
        !is.na(end_datetime) &
        end_datetime >= start_datetime,
      as.numeric(difftime(end_datetime, start_datetime, units = "hours")),
      NA_real_
    ),
    
    academic_year_start = if_else(
      !is.na(start_datetime),
      if_else(
        month(start_datetime) >= 8,
        year(start_datetime),
        year(start_datetime) - 1L
      ),
      NA_integer_
    ),
    
    academic_year = if_else(
      !is.na(academic_year_start),
      sprintf(
        "%d/%02d",
        academic_year_start,
        (academic_year_start + 1L) %% 100L
      ),
      NA_character_
    ),
    
    chargeable_status = case_when(
      str_to_lower(chargeable_status_raw) == "free" ~ "Free",
      str_to_lower(chargeable_status_raw) == "chargeable" ~ "Chargeable",
      TRUE ~ NA_character_
    ),
    
    public_status = case_when(
      str_to_lower(public_status_raw) == "yes" ~ "Yes",
      str_to_lower(public_status_raw) == "no" ~ "No",
      TRUE ~ NA_character_
    )
  )

# 3. Construct Durham activity categories

dur_clean <- dur_clean %>%
  mutate(
    activity_name_lower = str_to_lower(coalesce(activity_name, "")),
    description_lower = str_to_lower(coalesce(activity_description, "")),
    type_lower = str_to_lower(coalesce(activity_type_raw, "")),
    attendee_category_lower = str_to_lower(
      coalesce(attendee_category_raw, "")
    ),
    venue_lower = str_to_lower(coalesce(venue, "")),
    
    name_description_text = str_c(
      activity_name_lower,
      description_lower,
      sep = " "
    ),
    
    all_search_text = str_c(
      activity_name_lower,
      description_lower,
      venue_lower,
      sep = " "
    ),
    
    # 1 media / communication outputs
    is_media_output =
      str_detect(
        type_lower,
        "podcast|blog|visual media|other media"
      ) |
      str_detect(
        activity_name_lower,
        paste0(
          "interview|article|podcast|blog|radio|television|",
          "\\btv\\b|newspaper|magazine|video|broadcast|the conversation"
        )
      ),
    
    # 2 festival / public event
    # "Open day" is no longer kept as a separate analytical category.
    is_festival_public_event = str_detect(
      name_description_text,
      paste0(
        "festival|open day|celebrate science|big bang|lumiere|",
        "public event|miners.? gala"
      )
    ),
    
    # 3: school visit / activity
    is_school_activity =
      str_detect(attendee_category_lower, "school group") |
      str_detect(
        name_description_text,
        paste0(
          "school visit|primary school|secondary school|school group|",
          "work experience|",
          "\\byear\\s*(7|8|9|10|11|12|13)\\b|",
          "\\by\\s*(7|8|9|10|11|12|13)\\b"
        )
      ),
    
    # 4: exhibition
    is_exhibition =
      str_detect(type_lower, "exhibition") |
      str_detect(name_description_text, "\\bexhibition\\b"),
    
    # 5: online event
    is_online_event = str_detect(
      all_search_text,
      "\\bonline\\b|\\bzoom\\b|\\bteams\\b|webinar|\\bvirtual\\b"
    ),
    
    # 6: lecture / talk
    is_lecture = str_detect(
      type_lower,
      "public lecture|lecture|talk|presentation"
    ),
    
    # 7: workshop / hands-on activity
    is_workshop = str_detect(
      type_lower,
      "workshop|lab / practical|laboratory|practical|hands-on|co-creation|citizen science"
    ),
    
    activity_category = case_when(
      is_media_output ~ "Media / communication output",
      is_festival_public_event ~ "Festival / public event",
      is_school_activity ~ "School visit / activity",
      is_exhibition ~ "Exhibition",
      is_online_event ~ "Online event",
      is_lecture ~ "Lecture / talk",
      is_workshop ~ "Workshop / hands-on activity",
      TRUE ~ "Other"
    ),
    
    activity_category = factor(
      activity_category,
      levels = c(
        "School visit / activity",
        "Lecture / talk",
        "Online event",
        "Festival / public event",
        "Other",
        "Workshop / hands-on activity",
        "Exhibition",
        "Media / communication output"
      )
    )
  )

# 4 Standardise lead departments (10)

dur_clean <- dur_clean %>%
  mutate(
    department_lower = str_to_lower(coalesce(department, "")),
    
    lead_department = case_when(
      str_detect(department_lower, "physics") ~ "Physics",
      str_detect(department_lower, "chemistry") ~ "Chemistry",
      str_detect(
        department_lower,
        "faculty of science|science faculty"
      ) ~ "Science Faculty",
      str_detect(department_lower, "psychology") ~ "Psychology",
      str_detect(
        department_lower,
        "durham energy institute|\\bdei\\b"
      ) ~ "Durham Energy Institute",
      str_detect(
        department_lower,
        "bioscience|biosciences|biology"
      ) ~ "Biosciences",
      str_detect(
        department_lower,
        "computer science"
      ) ~ "Computer Science",
      str_detect(
        department_lower,
        "mathematical sciences|mathematics"
      ) ~ "Mathematical Sciences",
      str_detect(
        department_lower,
        "earth sciences|earth science"
      ) ~ "Earth Sciences",
      str_detect(department_lower, "engineering") ~ "Engineering",
      TRUE ~ NA_character_
    ),
    lead_department = factor(
      lead_department,
      levels = c(
        "Physics",
        "Chemistry",
        "Science Faculty",
        "Psychology",
        "Durham Energy Institute",
        "Biosciences",
        "Computer Science",
        "Mathematical Sciences",
        "Earth Sciences",
        "Engineering"
      )
    )
  )

# Check for any missing departments
unmatched_departments <- dur_clean %>%
  filter(
    !is.na(department),
    is.na(lead_department)
  ) %>%
  count(department, sort = TRUE)

if (nrow(unmatched_departments) > 0) {
  message("Check these unmatched Department values before final analysis:")
  print(unmatched_departments)
}

# 5. Main analysis for Sub-question 1 and Sub-question 2
dur_analysis <- dur_clean %>%
  filter(activity_category != "Media / communication output") %>%
  droplevels()

# Basic sample check
durham_analysis_check <- tibble(
  activity_count = nrow(dur_analysis),
  total_attendance = sum(dur_analysis$attendees, na.rm = TRUE),
  missing_attendance = sum(is.na(dur_analysis$attendees)),
  missing_lead_department = sum(is.na(dur_analysis$lead_department))
)

print(dur_analysis_check)

# 6. Descriptive statistics

dur_quality_summary <- tibble(
  raw_records = nrow(dur_raw),
  exact_duplicate_records = nrow(dur_raw) - nrow(dur_unique),
  cleaned_records = nrow(dur_clean),
  media_or_communication_outputs = sum(
    dur_clean$activity_category == "Media / communication output",
    na.rm = TRUE
  ),
  main_analytical_sample = nrow(dur_analysis),
  missing_attendance_values = sum(is.na(dur_clean$attendees)),
  invalid_date_order_records = sum(
    dur_clean$invalid_date_order,
    na.rm = TRUE
  )
)

# Table 5.1
dur_overall_summary <- dur_analysis %>%
  summarise(
    activity_count = n(),
    total_reported_attendance = sum(attendees, na.rm = TRUE),
    mean_attendance = mean(attendees, na.rm = TRUE),
    attendance_sd = sd(attendees, na.rm = TRUE),
    median_attendance = median(attendees, na.rm = TRUE),
    first_quartile_attendance = quantile(
      attendees, 0.25, na.rm = TRUE, names = FALSE
    ),
    third_quartile_attendance = quantile(
      attendees, 0.75, na.rm = TRUE, names = FALSE
    ),
    minimum_attendance = min(attendees, na.rm = TRUE),
    maximum_attendance = max(attendees, na.rm = TRUE)
  )

# Table 5.2: activity-category composition
dur_category_composition <- dur_analysis %>%
  count(activity_category, name = "activity_count", .drop = FALSE) %>%
  mutate(
    activity_percentage = 100 * activity_count / sum(activity_count)
  ) %>%
  arrange(desc(activity_count))

# Table 5.3: departmental composition
dur_department_composition <- dur_analysis %>%
  filter(!is.na(lead_department)) %>%
  count(lead_department, name = "activity_count", .drop = FALSE) %>%
  mutate(
    activity_percentage = 100 * activity_count / sum(activity_count)
  ) %>%
  arrange(desc(activity_count))

# Table 5.4
sub1_category_attendance <- dur_analysis %>%
  group_by(activity_category) %>%
  summarise(
    n = n(),
    total_attendance = sum(attendees, na.rm = TRUE),
    mean_attendance = mean(attendees, na.rm = TRUE),
    median_attendance = median(attendees, na.rm = TRUE),
    Q1 = quantile(attendees, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = quantile(attendees, 0.75, na.rm = TRUE, names = FALSE),
    minimum = min(attendees, na.rm = TRUE),
    maximum = max(attendees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_attendance))

# Overall Kruskal-Wallis test
sub1_kw <- kruskal.test(
  attendees ~ activity_category,
  data = dur_analysis
)

# Epsilon-squared effect size
sub1_H <- unname(sub1_kw$statistic)
sub1_k <- nlevels(droplevels(dur_analysis$activity_category))
sub1_n <- sum(
  complete.cases(
    dur_analysis[, c("attendees", "activity_category")]
  )
)
sub1_epsilon_squared <- (
  sub1_H - sub1_k + 1
) / (
  sub1_n - sub1_k
)
sub1_test_summary <- tibble(
  H = sub1_H,
  df = unname(sub1_kw$parameter),
  p_value = sub1_kw$p.value,
  epsilon_squared = sub1_epsilon_squared
)

# Holm-adjusted pairwise Wilcoxon tests
sub1_pairwise <- pairwise.wilcox.test(
  x = dur_analysis$attendees,
  g = dur_analysis$activity_category,
  p.adjust.method = "holm",
  exact = FALSE
)
pairwise_matrix_to_long <- function(pairwise_object) {
  as.data.frame(as.table(pairwise_object$p.value)) %>%
    filter(!is.na(Freq)) %>%
    transmute(
      group_1 = as.character(Var1),
      group_2 = as.character(Var2),
      adjusted_p_value = as.numeric(Freq),
      significant_0_05 = adjusted_p_value < 0.05
    ) %>%
    arrange(adjusted_p_value)
}
sub1_pairwise_long <- pairwise_matrix_to_long(sub1_pairwise)

# Figure 5.1
sub1_plot_data <- dur_analysis %>%
  filter(
    !is.na(attendees),
    !is.na(activity_category)
  ) %>%
  mutate(
    activity_category_plot = fct_reorder(
      activity_category,
      attendees,
      .fun = median,
      na.rm = TRUE
    )
  )
figure_sub1_boxplot <- ggplot(
  sub1_plot_data,
  aes(
    x = activity_category_plot,
    y = attendees
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.55,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::label_number(
      big.mark = ",",
      accuracy = 1
    ),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  coord_flip() +
  labs(
    x = "Activity category",
    y = "Number of attendees (pseudo-log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(
      size = 10,
      margin = margin(t = 6)
    ),
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(size = 11),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  filename = file.path(
    output_dir,
    "figure_5_1_attendance_by_activity_category.png"
  ),
  plot = figure_sub1_boxplot,
  width = 10,
  height = 6.5,
  dpi = 300
)

# Table 5.5
sub2_department_attendance <- dur_analysis %>%
  filter(!is.na(lead_department)) %>%
  group_by(lead_department) %>%
  summarise(
    n = n(),
    total_attendance = sum(attendees, na.rm = TRUE),
    mean_attendance = mean(attendees, na.rm = TRUE),
    median_attendance = median(attendees, na.rm = TRUE),
    Q1 = quantile(attendees, 0.25, na.rm = TRUE, names = FALSE),
    Q3 = quantile(attendees, 0.75, na.rm = TRUE, names = FALSE),
    minimum = min(attendees, na.rm = TRUE),
    maximum = max(attendees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_attendance))

# Overall Kruskal-Wallis test
sub2_kw <- kruskal.test(
  attendees ~ lead_department,
  data = dur_analysis %>%
    filter(!is.na(lead_department))
)

# Epsilon-squared effect size
sub2_H <- unname(sub2_kw$statistic)
sub2_k <- nlevels(
  droplevels(
    dur_analysis$lead_department[
      !is.na(dur_analysis$lead_department)
    ]
  )
)
sub2_n <- sum(
  complete.cases(
    dur_analysis[, c("attendees", "lead_department")]
  )
)

sub2_epsilon_squared <- (
  sub2_H - sub2_k + 1
) / (
  sub2_n - sub2_k
)

sub2_test_summary <- tibble(
  H = sub2_H,
  df = unname(sub2_kw$parameter),
  p_value = sub2_kw$p.value,
  epsilon_squared = sub2_epsilon_squared
)

# Holm-adjusted pairwise Wilcoxon tests
sub2_pairwise <- pairwise.wilcox.test(
  x = dur_analysis$attendees,
  g = dur_analysis$lead_department,
  p.adjust.method = "holm",
  exact = FALSE
)

sub2_pairwise_long <- pairwise_matrix_to_long(sub2_pairwise)

#  Figure 5.2 

sub2_plot_data <- dur_analysis %>%
  filter(
    !is.na(attendees),
    !is.na(lead_department)
  ) %>%
  mutate(
    lead_department_plot = fct_reorder(
      lead_department,
      attendees,
      .fun = median,
      na.rm = TRUE
    )
  )
figure_sub2_boxplot <- ggplot(
  sub2_plot_data,
  aes(
    x = lead_department_plot,
    y = attendees
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.55,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = scales::label_number(
      big.mark = ",",
      accuracy = 1
    ),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  coord_flip() +
  labs(
    x = "Lead department",
    y = "Number of attendees (pseudo-log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(
      size = 10,
      margin = margin(t = 6)
    ),
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(size = 11),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  filename = file.path(
    output_dir,
    "figure_5_2_attendance_by_lead_department.png"
  ),
  plot = figure_sub2_boxplot,
  width = 10,
  height = 7.5,
  dpi = 300
)

# figure 5.3
department_activity_mix <- dur_analysis %>%
  filter(!is.na(lead_department)) %>%
  count(
    lead_department,
    activity_category,
    name = "activity_count",
    .drop = FALSE
  ) %>%
  group_by(lead_department) %>%
  mutate(
    activity_percentage =
      100 * activity_count / sum(activity_count)
  ) %>%
  ungroup()

department_order <- dur_analysis %>%
  filter(!is.na(lead_department)) %>%
  count(lead_department, sort = TRUE) %>%
  pull(lead_department) %>%
  as.character()

department_activity_mix <- department_activity_mix %>%
  mutate(
    lead_department_plot = factor(
      as.character(lead_department),
      levels = rev(department_order)
    )
  )
figure_department_activity_mix <- ggplot(
  department_activity_mix,
  aes(
    x = lead_department_plot,
    y = activity_percentage,
    fill = activity_category
  )
) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Lead department",
    y = "Percentage of activities",
    fill = "Activity category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )
ggsave(
  filename = file.path(
    output_dir,
    "figure_5_3_activity_category_composition_by_department.png"
  ),
  plot = figure_department_activity_mix,
  width = 10,
  height = 6.5,
  dpi = 300
)

# 7. Prepare for HESA data
hesa_clean <- hesa_raw %>%
  rename(
    ukprn = UKPRN,
    provider = `HE Provider`,
    country = `Country of HE provider`,
    region = `Region of HE provider`,
    academic_year = `Academic Year`,
    nature_of_event = `Nature of Event`,
    metric = Metric,
    event_type = `Type of event`,
    value_raw = Value
  ) %>%
  mutate(
    across(
      c(
        provider,
        country,
        region,
        academic_year,
        nature_of_event,
        metric,
        event_type
      ),
      ~ str_squish(as.character(.x))
    ),
    
    value = parse_number(
      as.character(value_raw),
      locale = locale(grouping_mark = ",")
    )
  )

hesa_years <- c(
  "2014/15",
  "2015/16",
  "2016/17",
  "2017/18",
  "2018/19",
  "2019/20",
  "2020/21",
  "2021/22",
  "2022/23",
  "2023/24",
  "2024/25"
)

hesa_analysis <- hesa_clean %>%
  filter(
    country == "All",
    region == "All",
    academic_year %in% hesa_years,
    metric == "Attendees",
    !is.na(ukprn)
  ) %>%
  mutate(
    provider_group = if_else(
      provider == "University of Durham",
      "University of Durham",
      "Other UK HE providers"
    ),
    
    provider_group = factor(
      provider_group,
      levels = c(
        "University of Durham",
        "Other UK HE providers"
      )
    )
  )

hesa_sample_summary <- hesa_analysis %>%
  summarise(
    attendance_rows = n(),
    unique_providers = n_distinct(provider),
    provider_year_records = n_distinct(
      str_c(provider, academic_year, sep = "___")
    ),
    total_reported_attendance = sum(value, na.rm = TRUE)
  )

hesa_provider_group_summary <- hesa_analysis %>%
  group_by(provider_group) %>%
  summarise(
    attendance_rows = n(),
    unique_providers = n_distinct(provider),
    provider_year_records = n_distinct(
      str_c(provider, academic_year, sep = "___")
    ),
    total_reported_attendance = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Table 5.6
#In Durham
dur_event_type <- hesa_analysis %>%
  filter(provider_group == "University of Durham") %>%
  group_by(event_type) %>%
  summarise(
    dur_attendance = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dur_percentage =
      100 * dur_attendance / sum(dur_attendance)
  )

# Others
other_event_type <- hesa_analysis %>%
  filter(provider_group == "Other UK HE providers") %>%
  group_by(event_type) %>%
  summarise(
    other_attendance = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    other_percentage =
      100 * other_attendance / sum(other_attendance)
  )

# Table 5.6.
table_5_6 <- dur_event_type %>%
  left_join(
    other_event_type,
    by = "event_type"
  ) %>%
  mutate(
    difference_pp =
      dur_percentage - other_percentage
  ) %>%
  select(
    event_type,
    dur_attendance,
    dur_percentage,
    other_percentage,
    difference_pp
  )

# Table 5.7
# Charging-status percentages in each provider group
hesa_charge_by_group <- hesa_analysis %>%
  group_by(provider_group, nature_of_event) %>%
  summarise(
    total_reported_attendance = sum(value, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    attendance_percentage =
      100 * total_reported_attendance /
      sum(total_reported_attendance)
  ) %>%
  ungroup()

# Distribution of HESA-reported attendance by charging status
table_5_7 <- hesa_charge_by_group %>%
  select(
    provider_group,
    nature_of_event,
    attendance_percentage
  ) %>%
  pivot_wider(
    names_from = provider_group,
    values_from = attendance_percentage
  ) %>%
  rename(
    `Charging status` = nature_of_event,
    `University of Durham` = `University of Durham`,
    `Other UK HE providers` = `Other UK HE providers`
  ) %>%
  mutate(
    `Difference (pp)` =
      `University of Durham` -
      `Other UK HE providers`
  )

# Figutr 5.4
# Durham chargeable activity attendance
dur_chargeable_by_type <- hesa_analysis %>%
  filter(
    provider_group == "University of Durham",
    str_detect(
      str_to_lower(nature_of_event),
      "chargeable"
    )
  ) %>%
  group_by(event_type) %>%
  summarise(
    chargeable_attendance = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percentage_of_dur_chargeable_attendance =
      100 * chargeable_attendance /
      sum(chargeable_attendance)
  ) %>%
  arrange(desc(percentage_of_dur_chargeable_attendance))
figure_5_4 <- ggplot(
  dur_chargeable_by_type,
  aes(
    x = fct_reorder(
      event_type,
      percentage_of_dur_chargeable_attendance
    ),
    y = percentage_of_dur_chargeable_attendance
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "HESA event type",
    y = "Percentage of Durham chargeable-event attendance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = file.path(
    output_dir,
    "figure_5_4_durham_chargeable_attendance_by_hesa_event_type.png"
  ),
  plot = figure_5_4,
  width = 9,
  height = 5.5,
  dpi = 300
)

# Figure 5.5
dur_charge_within_type <- hesa_analysis %>%
  filter(provider_group == "University of Durham") %>%
  group_by(event_type, nature_of_event) %>%
  summarise(
    attendance = sum(value, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    percentage_within_event_type =
      100 * attendance / sum(attendance)
  ) %>%
  ungroup() %>%
  mutate(
    nature_of_event = case_when(
      str_detect(str_to_lower(nature_of_event), "chargeable") ~
        "Chargeable events",
      str_detect(str_to_lower(nature_of_event), "free") ~
        "Free events",
      TRUE ~ nature_of_event
    )
  )
figure_5_5 <- ggplot(
  dur_charge_within_type,
  aes(
    x = event_type,
    y = percentage_within_event_type,
    fill = nature_of_event
  )
) +
  geom_col(
    width = 0.75,
    position = "stack"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "HESA event type",
    y = "Percentage of attendance within event type",
    fill = "Charging status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    output_dir,
    "figure_5_5_free_and_chargeable_attendance_within_durham_hesa_event_type.png"
  ),
  plot = figure_5_5,
  width = 9,
  height = 5.8,
  dpi = 300
)

# 8. Sensitivity analyses
run_sensitivity <- function(data, analysis_label) {
  sub1_data <- data %>%
    filter(!is.na(attendees), !is.na(activity_category)) %>%
    droplevels()
  sub2_data <- data %>%
    filter(!is.na(attendees), !is.na(lead_department)) %>%
    droplevels()

  sub1_result <- kruskal.test(
    attendees ~ activity_category,
    data = sub1_data
  )
  sub2_result <- kruskal.test(
    attendees ~ lead_department,
    data = sub2_data
  )

  sub1_H <- as.numeric(sub1_result$statistic)
  sub2_H <- as.numeric(sub2_result$statistic)
  sub1_k <- nlevels(sub1_data$activity_category)
  sub2_k <- nlevels(sub2_data$lead_department)
  sub1_n <- nrow(sub1_data)
  sub2_n <- nrow(sub2_data)

  tibble(
    Analysis = analysis_label,
    n = nrow(data),
    Sub1_H = sub1_H,
    Sub1_p = sub1_result$p.value,
    Sub1_epsilon2 = (sub1_H - sub1_k + 1) / (sub1_n - sub1_k),
    Sub2_H = sub2_H,
    Sub2_p = sub2_result$p.value,
    Sub2_epsilon2 = (sub2_H - sub2_k + 1) / (sub2_n - sub2_k)
  )
}

maximum_attendance <- max(dur_analysis$attendees, na.rm = TRUE)
attendance_99 <- quantile(
  dur_analysis$attendees,
  0.99,
  na.rm = TRUE,
  names = FALSE
)

sensitivity_summary <- bind_rows(
  run_sensitivity(dur_analysis, "Main analysis"),
  run_sensitivity(
    filter(dur_analysis, attendees < maximum_attendance),
    "Exclude maximum attendance"
  ),
  run_sensitivity(
    filter(dur_analysis, attendees <= attendance_99),
    "Exclude observations above 99th percentile"
  ),
  run_sensitivity(
    filter(dur_analysis, attendees > 0),
    "Exclude zero attendance"
  )
)

# 9. some outputs
print(dur_quality_summary)
print(sub1_test_summary)
print(sub1_pairwise_long)
print(sub2_test_summary)
print(sub2_pairwise_long)
print(hesa_sample_summary)
print(hesa_provider_group_summary)
print(table_5_6)
print(table_5_7)
print(dur_chargeable_by_type)
print(dur_charge_within_type)
print(sensitivity_summary)
#tables in disserssion
print(dur_overall_summary)
print(dur_category_composition)
print(dur_department_composition)
print(sub1_category_attendance)
print(sub2_department_attendance)
print(table_5_6)
print(table_5_7)
