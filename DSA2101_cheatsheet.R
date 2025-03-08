#### random syntax ####

# For loop with if else condition
for (year in 2023:2025) {
  message = ifelse(year==2025, 
                    paste(year, "- the current year!"),
                    paste(year, "- not the current year!"))
}

# sampling random number
sample(1:6, size=2, replace=TRUE)

# matrix, generates a 2x5 matrix with values 1 to 10
mat = matrix(1:10, nrow=2, byrow=FALSE)

# apply a function to the columns (2), (1 for row) of matrix 
apply(mat, margin=2, mean)


####### factor() function ########
x1 = c("Junior", "seniors", "Freshmen", "sophomores")
x1_levels = c("Freshmen", "sophomores", "Junior", "seniors")
x2 =  factor(x1, levels= x1_levels, ordered=TRUE)

# converting a column in DF to a factor
heights$race = factor(heights$race)
levels(heights$race)



###### DATE OBJECTS #######
today = Sys.Date()
# convert date into desired format
d1 = as.Date("2025/01/17", format="%Y/%m/%d")

# Get the day 
weekdays(today, abbreviate=FALSE)

# Get the month
months(today, abbreviate=TRUE)



####### PLOTTING #########

# normal plot
plot(cars$peed, cars$dist, 
  pch=2,# adjusts the shape of points 
  col= "maroon", # adjust the colour of points
  xlab="speed", ylabl="Distance",
  main='Relationship between Distance and Speed')
# adding a trend line
abline(reg=lm(dist~speed, data=cars),
        col="grey60",
        lty="dashed")


# Boxplot of temperature by month
boxplot(airquality$Temp ~ airquality$Month_abbr,
        xlab = "Month", 
        ylab = "Temperature", 
        main = "Monthly temperature in New York")



##### Aggregate function ######

# Aggregate all other columns by category
result <- aggregate(. ~ Category, data = df, FUN = sum)
# Aggregate by Region and Category
result <- aggregate(Sales ~ Region + Category, data = df, FUN = sum)


# %/% integer division
# %% modulo - remainder

apply()  # apply a function across rows or columns of a matrix or a data frame.
sapply() # apply a function across elements of a list, and return a vector or a matrix format
lapply() # apply a function across elements of a list, and return a list.
tapply() # apply a function on a subset of data frame broken down by factor levels.
tapply(df$Temp, df$season, summary)

library(tidyverse)

##### Reading Files #####

# Reading excel sheets
library(readxl)
# Read the sheets, one by one
pop_1 <- read_excel("urbanpop.xlsx", sheet = 1) # Can put sheet="<sheetname>"
pop_2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop_3 = read_excel("urbanpop.xlsx", sheet = 3)

# Read sheets with range name 

# Skipping lines
qn2_1 = read_excel("../data/tourist.xlsx", skip=9)

# Provide range
read_excel("../data/read_excel_01.xlsx",
           range = "C6:E8", 
           col_names = c("var1", "var2", "var3"))


# Reading in normal csv 
library("readr")
qn1_1 = read_csv("../data/Shanghai.csv")
# writing to csv
write.csv(results, "../data/rainfal.csv", row,names=FALSE)


# Reading in Json objects
library(jsonlite)
json1 = "[12,3,7]"
fromJSON(json1)
# accessing elements:
results_json[["result"]][["records"]]


# We want to continue submitting queries until the required number of rows are obtained
# use wile loop to repeatedly fetch data until number of rows matches the total available rows
# cur number of rows
results = results_json[["result"]][["records"]]
# total expected number of rows
total_records = resutls_json[["result"]][["total"]]

whiele (nrow(results) < total_records) {
  next_url = paste0("https://data.gov.sg",
    results_json[["result"]][["_links"]][["next"]])
  results_json = fromJson(next_url)
  results = rbind(results, results_json[["results"]][["records"]])
}




################ dplyr #############

### Useful Functions ###
# Measures of center: mean() and median()
# Measures of spread: sd(), var(), IQR()
# Measures of range: min(), quantile(), max()
# positions: first(x), nth(x,2), last(x)
# count: n(), n_distinct()

# displays entire dataset
data(starwards)

# head of data
glimpse(starwards)


# filter() function
filter(starwards,
        sex %in% c("female", "male"),
        skin_color == "blue")

# filters for either male or female, and must be blue skin color
filter(starwards, 
        (sex=="female" | sex=="male"),
        skin_color=="blue")


# select() function
# selects columns haircolor to eyecolor
select(starwars, hair_color: eye_color)
select(starwars, name, ends_with("color"))#  or starts_with

##### Mutate #####
mutate() 

# want to put the new column before the column "name"
df2 = mutate(df1, height_m = height/100, .before=name)

# add a column that calculates the cumulative sum within each group
mutate(df, sum_phones = cumsum(phones))


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
# Apply lag function
df <- df %>% # new column created called lagged_value
  mutate(lagged_value = lag(value, n = 1))  # Shift values by 1 row

# Renaming numerical month values to Abbreviated months
mutate(month_abb = month(Month, abbr = TRUE, label = TRUE))


# arrange() function
# arrange the df on mass column descending
arrange(df1, desc(mass))

# arrange the df first by mass in asc, then height in desc 
arrange(df, mass, desc(height))


# sort it by group
arrange(df, desc(age), by_group=TRUE)


# summarize() function
# get the mean of height and remove NAs, and then assign it in the col named mean_height
summarize(starwars, mean_height = mean(height, na.rm=TRUE))

# find first row
starwars %>%
  summarize(shortest=first(name, order_by = height))

# count the number of characters by gender
starwards %>% 
  group_byb(gender) %>%
  summarize(n = n())


# Accross column method
# calc mean of all the cols between height and mass
starwars %>% summarize(across(height: mass, min, na.rm = TRUE))

# for every numeric column
starwars %>% summarize(across(where(is.numeric), mean, na.rm =TRUE))


# Remove/ dealing with missing value
starwars %>% na.omit()
# remove missing values from a specified column
starwars %>% filter(!is.na(gender))

# reads none into NA across all columns
starwars %>% mutate(across(where(is.character), na.if, "none"))
starwars %>% mutate(across(where(is.character), na.if, "none")) %>% na.omit()


# Removing duplicate rows 
starwars %>% distinct()

# find all unique categories of sex
starwars %>% distinct(sex)

# get count of each category in column "sex"
starwars %>% count(sex)
# get count of unique combinations of sex and species
starwars %>% count(sex, species, sort=TRUE)


##### Column naming and selection #####
# rename the column "name" as "character_name"
starwars %>% rename(character_name = name) %>%

# <new_name> = col_num
rename(region = 1, country = 2, iso = 3,
       least_developed = 4, land_locked = 5, small_island = 6) 

# RELOCATE COLUMNS
# shifts Score to the first column
df%>% relocate(Score)

# Move column to a specific position
df %>% relocate(Score, .before = Name)

# relocate multiple columns
df %>% relocate(Age, Score, .before = Name)


# unnest() function
df <- tibble(
  name = c("Alice", "Bob", "Charlie"),
  hobbies = list(c("Reading", "Cycling"), "Swimming", c("Gaming", "Cooking"))
)

df %>% unnest(hobbies) # Output: (Each hobby gets its own row)


##### APPLYINGGGGG #####
starwards %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  summarize(mean_mass = mean(mean_mass))

# using case when or case_when for if else conditions 
flights %>%
  mutate(arr_status = 
          case_when(is.na(arr_delay) ~ "cancelled",
                    arr_delay <=0 ~ "on time",
                    arr_delay >0 ~ "late"))


##### Slicing dataframe #####
arrange() arrange(desc(density))
# slice max selects the rows with the highest value in the dataframe
slice_max() count(artist, sort = TRUE) %>% slice_max(n, n = 10)\

starwars %>%
  group_by(gender)%>%
  slice_max(mass, n=1)%>%
  relocate(gender, mass)


##### Pivoting Dataframe #####
# pivot longer
pivot_longer(`2022 Dec`:`2022 Jul`, names_to = "year_month", values_to = "arrivals") # Reduce columns
pivot_longer(1999:2000, names_to = "year", values_to = "cases")
# or if u want all except one clumn
pivot_longer(!1999, names_to = "year", values_to = "cases", values_drop_na=TRUE)


# pivot wider
pivot_wider(names_from = new_admission_type, values_from = count) # Reduce rows
pivot_wider(names_from = type, values_from = count) # reduce rows


# double pivot
table %>% pivot_wider(names_to = variable, 
                      values_to = c(estimate, moe))



# Additional: Splitting the columns into 3 columns: sp_m_1524 => (method - sp, sex - m, age - 1524) 
# Wk 7 slides for reference of how dataframe looks like
pivot_longer(cols = !(country:year), # All columns exluding those stated
             names_to = c("method", "sex", "age"), names_sep = "_",
             values_to = "count", values_drop_na = TRUE)



# separate() function, to separate column into 2 columns
table %>% separate(rate,
                    into = c("cases", "population"), # the delimiter for the separation is auto detected
                    convert=TRUE)) # incase the vars are in strings and need them to be ints

table %>% separate(rate,
                    into = c(NA, "population"), # or if u dont want the first column and only the second
                    convert=TRUE)) # incase the vars are in strings and need them to be ints


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


df = rainfall %>%
      mutate(month = lubridate:: ym(month),
              year = year(month),
              total_rainfall = as.numeric(total_rainfall)) %>%
      filter(year >= 200)



 ##### stringr methods #####

library(stringr)

# combines words in a vector by delimiter ","
str_c(fruits, collapse=", ")

# detect pattern "ana" in string
str_detect(fruits, "ana")


str_split_fixed(file_names, "_", 4)
mutate(month = str_remove(unlist(str_extract_all(year_month, "\\D+")), " "))

1. str_length() - Returns the number of characters in a string.

   str_length("hello")  # Outputs: 5

2. str_c() - Concatenates strings together, similar to `paste()`.
   
   str_c("Hello", "world")  # Outputs: "Helloworld"
   str_c("Hello", "world", sep = " ")  # Outputs: "Hello world"
   

3. str_sub() - Extracts or replaces substrings in a character vector.
   
   str_sub("Hello", 2, 4)  # Outputs: "ell"
   

4. tr_split() - Splits strings into a list of substrings based on a pattern.
   
   str_split("one,two,three", pattern = ",")  # Outputs: list(c("one", "two", "three"))
   

5. str_replace() - Replaces the first instance of a pattern in each string.
   
   str_replace("fuzzy", "z", "s")  # Outputs: "fuszy"
   

6. str_replace_all() - Replaces all instances of a pattern in each string.
   
   str_replace_all("fuzzy", "z", "s")  # Outputs: "fussy"
   

7. str_detect() - Detects the presence of a pattern in a string. Returns TRUE if pattern is found.
   
   str_detect("fruit", "ui")  # Outputs: TRUE
   

8. str_extract() - Extracts the first instance of a pattern in a string.
   
   str_extract("123 abc", "[a-z]+")  # Outputs: "abc"
   

9. str_extract_all() - Extracts all instances of a pattern in a string.
   
   str_extract_all("1a2b3c", "[a-z]")  # Outputs: list(c("a", "b", "c"))
   

10. str_trim() - Removes white space from start and end of a string.
    
    str_trim("   Text   ")  # Outputs: "Text"
    

11. str_pad() - Pads a string to a fixed width with a specified character.
    
    str_pad("123", width = 5, side = "left", pad = "0")  # Outputs: "00123"
    

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

### For inner_join, the by arg, left and right of equality follows the order in which table are presented. Original table is left side.
### Left join is used when you want to keep the NAs from the right table after joining. For example, which questions are unanswered?
### Left join questions tables with answers table, rows with NA from resultant table means unanswered.


######### HOW EACH JOIN WORKS #######
### Inner join matches observations whenever the keys are equal, and the rest are removed. 
### Left join (x,y) kepps all rows in x, including those not matched in y
### right join(x,y) keeps all rows in y, including those not matched inx
### full join(x, y) keeps all rows in both tables regardless of matches

x %>% left_join(y, by='carrier')
# syntax for joining on columns: by = c("key_x" = "key_y")
flights %>% left_join(airports, by = c("dest" = "faa"))


# Filtering joins
# semi join(x,y) keeps all observations in x that have a match in y. Similar to inner join just that no cols from y are added
# anti_join(x, y) keeps all observations in x that do not have a match in y
# means if x has [A,B,C] and y has [A, B, D, E], returns only row C in x


# concatenate/ concatenating 
# row bind - if we have 2 or more dfs with same column, can bind rows. 
# columns are matched by name, missing values in cols are filled with NA
df_1 = tibble(...)
df_2 = tibble(...)
bind_rows(df_1, df_2)

# col bind - rows are matched by position. df must have same num of rows
bind_cols(df_1, df_2)







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
factor(qn2_2$Data Series`, levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8-10", "11-14", "15-29", "30-59", "60+"), ordered=TRUE)




# Formatting Dates ####
# Define the date formats
formats <- c("%Y-%m-%d", "%B %d, %Y")
# Convert dates to the same format
accounts %>%
  mutate(date_opened_clean = parse_date_time(date_opened, orders = formats))


# Visualize the missing values by column
airlines %>% filter(is.na(carrier))

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
  slice_max(total_pop, n = 1) %>% # look at total_pop column, and get the top n 
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