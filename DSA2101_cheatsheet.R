# %/% integer division
# %% modulo - remainder

apply() #apply a function across rows or columns of a matrix or a data frame.
sapply() #apply a function across elements of a list, and return a vector or a matrix.
lapply() # apply a function across elements of a list, and return a list.
tapply() # apply a function on a subset of data frame broken down by factor levels.
tapply(df$Temp, df$season, summary)

library(tidyverse)

##### Reading Files #####

# reading excel sheets
library(readxl)
# Read the sheets, one by one
pop_1 <- read_excel("urbanpop.xlsx", sheet = 1) # Can put sheet="<sheetname>"
pop_2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop_3 = read_excel("urbanpop.xlsx", sheet = 3)
# Skipping lines
qn2_1 = read_excel("../data/tourist.xlsx", skip=9)
# Provide range
read_excel("../data/read_excel_01.xlsx",
           range = "C6:E8", col_names = c("var1", "var2", "var3"))
# Normal csv
library("readr")
qn1_1 = read_csv("../data/Shanghai.csv")

##### Column naming and selection #####
# rename the column "name" as "character_name"
starwars %>%
  rename(character_name = name) %>%
  head(3)
# <new_name> = col_num
rename(region = 1, country = 2, iso = 3,
       least_developed = 4, land_locked = 5, small_island = 6) 

select()
pull() # get one column from the dataframe
filter()


##### Mutate #####
mutate() 

# Use of reorder()
mutate(media_activity = reorder(media_activity, pct))
# Make region to an ordered factor based on median hdi by region
mutate(region = reorder(region, hdi, FUN = median))

# Editing entry values
case_when() # (if else for multiple conditions - switch statements
mutate(country = recode("S.K." = "South Korea")) # Used to relabel strings.

# Compute values from values from different rows
lag() # NOTE: lag() can specify number of rows to lag: lag(arrivals, 13). Specify the order_by to arrange before doing lag computation
mutate(rate = (arrivals-lag(arrivals, order_by = k))/lag(arrivals, order_by = k) * 100)

# Renaming numerical month values to Abbreviated months
mutate(month_abb = month(Month, abbr = TRUE, label = TRUE))


##### Slicing dataframe #####
arrange() arrange(desc(density))
# arrange ignores grouping, need set .by_group = TRUE
slice_max() count(artist, sort = TRUE) %>% slice_max(n, n = 10)
head()


##### group by #####
group_by()
ungroup()


##### Pivoting Dataframe #####
pivot_longer(`2022 Dec`:`2022 Jul`, names_to = "year_month", values_to = "arrivals") # Reduce columns
pivot_wider(names_from = new_admission_type, values_from = count) # Reduce rows

# Additional: Splitting the columns into 3 columns: sp_m_1524 => (method - sp, sex - m, age - 1524) 
# Wk 7 slides for reference of how dataframe looks like
pivot_longer(cols = !(country:year), # All columns exluding those stated
             names_to = c("method", "sex", "age"), names_sep = "_",
             values_to = "count", values_drop_na = TRUE)


##### summary and counts, NAs #####
summarize()
mean("colname", na.rm=TRUE)
distinct() # same as unique() base R
na_if # Set to NA if certain value na_if(column, <value>)
na.omit()
across # do same transformation on multiple columns (repeat 'mutate' action for diff columns)
n
# ▶ Measures of center: mean(), median()
# ▶ Measures of spread: sd(), var(), IQR()
# ▶ Measures of range: min(), quantile(), max()
# ▶ Measures of positions: first(x), nth(x, 2), last(x)
# ▶ Measures of count: n(), n_distinct().

##### Misc #####
block_length = 6
num_blocks = 3
k=rep(seq(1, num_blocks), each = block_length)
k # [1] 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3

##### Joins #####
# Mutating Joins
outer_join() # All observations from both
## An inner_join() only keeps observations from x that have a matching key in y. The most important property of an inner join is that unmatched rows in either input are not included in the result. 
inner_join(table_to_join, join_by(sales_date > promo_date)) # Inequality join, use join_by parameter
inner_join(table_to_join, join_by(closest(sales_date > promo_date)))
left_join() # Only keeps observations from left table that matches with right table.

# Filtering joins
anti_join() # Shows Present current table, absent in the table to join
semi_join() # Shows present in both
# Inequality Join - See week 6 slides

# Concats
bind_rows() # concat - if u alr have same column types, concat vertically
bind_cols() # concat horizontally, if all have same number of rows as existing columns.

### NOTE: For inner_join, the by arg, left and right of equality follows the order in which table are presented. Original table is left side.
### NOTE: Left join is used when you want to keep the NAs from the right table after joining. For example, which questions are unanswered?
### Left join questions tables with answers table, rows with NA from resultant table means unanswered.


##### Handling Duplicates #####
qn1_1 = qn1_1[which(duplicated(qn1_1)==FALSE),] # Base R way of Keeping distinct values

# Count the number of full duplicates
sum(duplicated(bike_share_rides))
# Remove duplicates
bike_share_rides_unique <- distinct(bike_share_rides)
# Count the full duplicates in bike_share_rides_unique
sum(duplicated(bike_share_rides_unique))
# Find duplicated ride_ids
bike_share_rides %>% 
  count(ride_id) %>% 
  filter(n > 1)
# Remove full and partial duplicates
bike_share_rides_unique <- bike_share_rides %>%
# Only based on ride_id instead of all cols
  distinct(ride_id, .keep_all = TRUE) 
# on whole dataframe, not just checking one column
distinct(bike_share_rides)
### NOTE: Can pass multiple columns to distict function. distinct(ride_id, date, .keep_all=TRUE)


##### Handling Dates #####
library(lubridate)  # NOTE: Dates can be used with inequalities directly, or used as axis for plot directly
ym() 
ymd() ... 
quarter(date) # returns integer value 1,2,3,4
month.abb # Use this to get vector of months strings (useful for creating levels for factor e.g.)
df$Month = factor(df$Month, levels=month.abb, ordered=TRUE)
# Create factor for quarters
qn1_3 = qn1_1 %>% filter(year(date) > 2018) %>%
  mutate(year = year(date)) %>%
  mutate(quarter = case_when(month(date)==1 ~ "Q1",
                             month(date)==4 ~ "Q2",
                             month(date)==7 ~ "Q3",
                             month(date)==10 ~ "Q4")) %>%
  mutate(quarter = factor(quarter, levels=c("Q1","Q2","Q3", "Q4")))


##### stringr methods #####
str_split_fixed(file_names, "_", 4)
mutate(month = str_remove(unlist(str_extract_all(year_month, "\\D+")), " "))

# Getting values from str_split
name <- "John Doe Smith"
result <- str_split(name, " ", simplify = TRUE)[, 1] # "John"

# Filter for rows with "(" or ")" in the phone column
sfo_survey %>%
  filter(str_detect(phone, fixed("(")) | str_detect(phone, fixed(")")))

str_replace_all()
str_remove_all()

## Check string length for a column
# Check out the invalid numbers
sfo_survey %>%
  filter(str_length(phone)!=12)
# Remove rows with invalid numbers
sfo_survey %>%
  filter(str_length(phone)==12)



##### Base R Plots #####
# lwd - thickness; lty - line type (dotted or not); las - vertical or horizontal axis values
# plot type="o" (line going through points) type="l" (continuous line plot)
# par mar - margin adjust
df1 <- df %>% count(artist, sort = TRUE) %>% slice_max(n, n = 10)
par(mar = c(5, 6, 2, 2))
barplot(df1$n, names.arg = df1$artist,
        horiz = TRUE, las = 1,
        cex.names = 0.6, cex.axis = 0.6, border = NA,
        main = "The most popular artists in the year 2000")

# Plot and additional lines() or points() on same plot, legend() on plot
plot(qn2_1$Hospitalised ~ qn2_1$date, main="Covid-19 Weekly Statistics in Singapore", ylab="Number of Cases", xlab="", type="l", col="steelblue", lwd=2)
lines(qn2_1$ICU ~ qn2_1$date, lwd=2)
#legend(ym("2023-3"),1000,legend=c("Hospitalisations", "ICU Admissions"),lty = c(1,1),col=c("steelblue", "black"))
legend(x="topleft",legend=c("Hospitalisations", "ICU Admissions"),lty = c(1,1),col=c("steelblue", "black")) 
# lty param makes sure legend displays line

# ab line, a is intercept, b is gradient
abline(a=average, b=0, lty=3) # this code draws a horizontal line at certain avg value

# remove exponential in axis of plots
options(scipen = 999)


## Population Pyramid
# Population pyramid: NOTE: use of -population/100 in the aes(y=) to get this plot, and scale_y_continuous(labels=abs) to ensure positive on both ends of axis. 
ggplot(data = df_total, aes(x = age_group, y = population/1000, fill = Gender)) +
  geom_col(data = df_total %>% filter(Gender == "Male"),
           aes(y = -population/1000)) +
  geom_col(data = df_total %>% filter(Gender == "Female")) +
  coord_flip() +
  labs(x = "", y = "Population (in thousands)", 
       title = "Population Age Structure of Singapore (2015)",
       subtitle = paste("Total Resident Population:", sum(df_total$population)),
       caption = "Source: Singapore Department of Statistics") +
  scale_y_continuous(labels = abs) +
  theme_bw()



# Mutating Different Data types, Handling Factors ####

### Converting factor to numeric (convert to char first, then to numeric)
### Categorical data inconsistency: (string problems), (fct_collapse to reduce types)

# Count categories of dest_region
sfo_survey %>%
  count(dest_region)
# Categories to map to Europe
europe_categories <- c("EU", "eur", "Europ")
# Add a new col dest_region_collapsed
sfo_survey %>%
  # Map all categories in europe_categories to Europe
  mutate(dest_region_collapsed = fct_collapse(dest_region, 
                                              Europe = europe_categories)) %>%
  # Count categories of dest_region_collapsed
  count(dest_region_collapsed)

### Use of as.integer() to ensure only integers returned (truncate)
questions %>%
  # Inner join questions and answers with proper suffixes
  inner_join(answers, by=c("id"="question_id"), suffix=c("_question", "_answer")) %>%
  # Subtract creation_date_question from creation_date_answer to create gap
  mutate(gap = as.integer(creation_date_answer-creation_date_question))


### Factors: To plot in specific order, specify levels and ordered = TRUE
factor(qn2_2$`Data Series`, levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8-10", "11-14", "15-29", "30-59", "60+"), ordered=TRUE)




# Formatting Dates ####
# Define the date formats
formats <- c("%Y-%m-%d", "%B %d, %Y")
# Convert dates to the same format
accounts %>%
  mutate(date_opened_clean = parse_date_time(date_opened, orders = formats))


# Viz Missing ####
# Visualize the missing values by column
vis_miss(accounts) # Visualise all columns missing 
accounts %>%
  # missing_inv: Is inv_amount missing?
  mutate(missing_inv = is.na(inv_amount)) %>%
  # Group by missing_inv
  group_by(missing_inv) %>%
  # Calculate mean age for each missing_inv group
  summarize(avg_age = mean(age))
# Sort by age and visualize missing vals
accounts %>%
  arrange(age) %>%
  vis_miss() ## vis_miss Can also be used in pipeline

# Other Examples #####

county_data = counties %>%
  # Keep state, county and add proportion_men
  mutate(state, county, population, proportion_men = men/population, .keep="none") %>%
  # Filter for population of at least 10,000
  filter(population >= 10000) %>%
  # Arrange proportion of men in descending order 
  arrange(desc(proportion_men))

class(county_data) # [1] "tbl_df"     "tbl"        "data.frame"


# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(state, wt=citizens, sort =TRUE)

### NOTE: weighted arg wt: if not specified, it is the first column that count for is returned.


counties_selected %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>%
  # Add a density column
  mutate(density = total_population/total_area) %>%
  # Sort by density in descending order
  arrange(desc(density))


### When you group by multiple columns and then summarize, 
### it's important to remember that the summarize "peels off" one of the groups,
### but leaves the rest on. For example, if you group_by(X, Y) then summarize, 
### the result will still be grouped by X.

counties_selected %>%
  # Group and summarize to find the total population
  group_by(region, state) %>%
  summarize(total_pop = sum(population)) %>%
  # Calculate the average_pop and median_pop columns 
  summarize(average_pop = mean(total_pop),
            median_pop = median(total_pop))

counties_selected %>%
  group_by(region, state) %>%
  # Calculate average income
  summarize(average_income=mean(income)) %>%
  # Find the lowest income state in each region
  slice_min(average_income, n=1)


counties_selected %>%
  # Find the total population for each combination of state and metro
  group_by(state, metro) %>%
  summarize(total_pop = sum(population)) %>%
  # Extract the most populated row for each state
  slice_max(total_pop, n = 1) %>%
  # Count the states with more people in Metro or Nonmetro areas
  ungroup() %>%
  count(metro)

### Ungroup after computations to reduce load, mutate to persist intermediate columns

# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  slice_max(fraction, n = 1)

# Once you add new columns, the result will still be grouped by name. This splits it into 48,000 groups, which actually makes later steps like mutates slower.

library(stringr)
str_split()

relevant_names = sheet_names[3:5]
df_female = NULL # create an empty object
for(names in relevant_names) {
  # Read data from relevant sheet
  temp_data <- read_excel(file_name, sheet = names)
  # split a string
  age_grp <- str_split(names, ",", simplify = TRUE)[3]
  temp_data$age <- str_trim(age_grp)
  df_female <- rbind(df_female, temp_data) # bind data frames by rows
}

## Use of replace_na from the tidyr package
# Replace the NAs in the tag_name column
library(tidyverse)
questions %>%
  left_join(question_tags, by = c("id" = "question_id")) %>%
  left_join(tags, by = c("tag_id" = "id")) %>%
  replace_na(list(tag_name= "only-r"))

## Use of n() funciton to give number of items per group from aggregated table
questions_with_tags %>% 
  # Group by tag_name
  group_by(tag_name) %>%
  # Get mean score and num_questions
  summarize(score = mean(score),
            num_questions = n()) %>%
  # Sort num_questions in descending order
  arrange(desc(num_questions))