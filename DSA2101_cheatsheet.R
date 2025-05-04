
##### Reading Files #####
library(readxl)
library(readr)
library(stringr)
library(lubridate)
library(tidyverse)

# Reading excel sheets
library(readxl)
# Read the sheets, one by one
pop_1 <- read_excel("urbanpop.xlsx", sheet = 1) # Can put sheet="<sheetname>"
pop_2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop_3 = read_excel("urbanpop.xlsx", sheet = 3)

# Read sheets with range name l

# Skipping lines
qn2_1 = read_excel("../data/tourist.xlsx", skip=9)

# Provide range
qn1_1 = read_excel("../data/read_excel_01.xlsx",
           range = "C6:E8", 
           col_names = c("var1", "var2", "var3"))
head(qn1_1)


# Reading in normal csv 

qn1_1 <- read.csv("../data/ges_sg.csv", stringsAsFactors = TRUE, na.strings = "na") %>% 
  # Remove column with IDs
  select(-X_id)


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


#### Essential syntax ####
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







##### Aggregate function ######

# Aggregate all other columns by category
result <- aggregate(. ~ Category, data = df, FUN = sum)
# Aggregate by Region and Category
result <- aggregate(Sales ~ Region + Category, data = df, FUN = sum)

# Create new column for seasons
airquality$Season <- as.factor(ifelse(airquality$Month <= 6, "Summer", "Autumn"))

# Summary of temperature in summer and autumn months
aggregate(Temp ~ Season, data = airquality, FUN = summary)

# %/% integer division
# %% modulo - remainder

apply()  # apply a function across rows or columns of a matrix or a data frame.
sapply() # apply a function across elements of a list, and return a vector or a matrix format
lapply() # apply a function across elements of a list, and return a list.
tapply() # apply a function on a subset of data frame broken down by factor levels.
tapply(df$Temp, df$season, summary)


apply(df, 1, sum)  # Sum of each row

# custom function
apply(df, 2, function(x) max(x) - min(x))  # Range of each column




################ dplyr #############

### Useful Functions ###
# Measures of center: mean() and median()
# Measures of spread: sd(), var(), IQR()
# Measures of range: min(), quantile(), max()
# positions: first(x), nth(x,2), last(x)
# count: n(), n_distinct()

hdb_2024 %>%
  filter(resale_price >= 1e6) %>%
  count(town, sort = TRUE) %>% # count the number of each value in the town column, sorted by the count
  mutate(total_transaction = sum(n)) # get the total number of the counts


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


# mutating across a series of columns, and applying the same function
mutate(across(c(Total:`85 & Over`), as.numeric)) %>%


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
  group_by(gender) %>%
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


# double column pivot
table %>% pivot_wider(names_to = variable, 
                      values_to = c(estimate, moe))

# another double pivot 
new_df <- qn1_1 %>%
  pivot_longer(`X2024Jul`: `X1985Jan`, names_to = "year_month", values_to = "revenue") %>% # Reduce columns
  select(year_month, DataSeries, revenue) %>%
  mutate(newym = str_c(substring(year_month, 2, 5), # get the year
                        substring(year_month, 6), # get the month
                        sep="-"), # separate it by a comma
          year_month = ym(newym))  %>%
  pivot_wider(names_from = DataSeries, values_from = revenue)%>% # Reduce rows
  select("year_month", "Total": "FastFoodOutlets")


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
   str_sub(column, 2) # gets second character from each row in the column onwards
   

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
# lwd - thickness; 
# lty - line type (dotted or not);
# las - vertical or horizontal axis values
# plot type="o" (line going through points) 
# type="l" (continuous line plot)

# par mar - margin adjust

####### PLOTTING #########

# normal plot
plot(cars$peed, cars$dist, 
  pch=2,# adjusts the shape of points 
  col= "maroon", # adjust the colour of points
  xlab="speed", ylab="Distance",
  main='Relationship between Distance and Speed')

# line through circular points
plot(qn2_1$Month_new, 
     qn2_1$Total/1000, 
     type = "b",  
     xlab = "Month", 
     ylab = "Number of arrivals (in thousands)", 
     main = "Monthly tourist arrivals in 2023", 
     lwd=1,
     col = "black")

# adding a trend line
abline(reg=lm(dist~speed, data=cars),
        col="grey60",
        lty="dashed")


# Boxplot of temperature by month
boxplot(airquality$Temp ~ airquality$Month_abbr,
        xlab = "Month", 
        ylab = "Temperature", 
        main = "Monthly temperature in New York")


df1 <- df %>% 
  count(artist, sort = TRUE) %>%
  slice_max(n, n = 10)


# df$1n is the count/ freq, then df1$ artist is the x axis categories
barplot(df1$n, names.arg = df1$artist,
        horiz = TRUE, las = 1,
        cex.names = 0.6, cex.axis = 0.6, border = NA,
        main = "The most popular artists in the year 2000")


barplot(qn1_3$median_gross_income, names.arg = qn1_3$school,
        horiz = TRUE, border = NA, las = 1, 
        cex.names = 0.7, cex.axis = 0.7,
        ylab="Number of transactions",
        xlab="Month",
        main = "Number of HDB resale transactions in 2024")

# Plot and additional lines() or points() on same plot, legend() on plot
plot(qn2_1$Hospitalised ~ qn2_1$date, 
  main="Covid-19 Weekly Statistics in Singapore", 
  ylab="Number of Cases", xlab="", type="l", col="steelblue", lwd=2)
lines(qn2_1$ICU ~ qn2_1$date, lwd=2)
#legend(ym("2023-3"),1000,legend=c("Hospitalisations", "ICU Admissions"),lty = c(1,1),col=c("steelblue", "black"))
legend(x="topleft",
  legend=c("Hospitalisations", "ICU Admissions"),
  lty = c(1,1),col=c("steelblue", "black")) 
# lty param makes sure legend displays line

# ab line, a is intercept, b is gradient
abline(a=average, b=0, lty=3) # this code draws a horizontal line at certain avg value

# horizontal line
abline(h=mean_months, lty=3)

# remove exponential in axis of plots
options(scipen = 999)


# boxplot
boxplot(airquality$Temp ~ airquality$Month_abbr,
        xlab = "Month", 
        ylab = "Temperature", 
        main = "Monthly temperature in New York")


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
factor(qn2_2$`Data Series`, 
  levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8-10", "11-14", "15-29", "30-59", "60+"),
   ordered=TRUE)


# Convert Month_abbr as an ordered factor variable with the correct levels
airquality$Month_abbr <- factor(airquality$Month_abbr,
                                levels = c("May", "Jun", "Jul", "Aug", "Sep"),
                                ordered = TRUE)


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





########## HOW TO CHOOSE A PLOT ###########
one variable:
- continuous X, visualise distribution of X:
- use histogram, or density ggplot

two variable:
- continuous X and Continuous y
- Visualise relationship between x and y
- gemo point, geom line, geom text(labelling data points), geom smooth, geom area(can be stacked)

two variable:
- discrete X, continuous y
- visualise distribution of Y with respect to X
- geom_col, geom boxplot, geom_jitter, geom_violin



############### ggplot() #################

# GEOM SMOOTH/ GEOM POINT

ggplot(mpg, aes(x=displ, y=hwy)) +
  # geom refers to a geometrical plot. Want a point plot
  geom_point(aes(color = class), # map class to the color aesthetics
              size = 4, # size of the point
              alpha = 0.5, # opacity 
              position = "jitter") # nudges similar points apart from each other so can see how many points are apart from each other


# overplotting occurs when multiple data points overlap with one another
# identical or very similar x and y values
# position = jitter fixes overplotting


# color aesthetic can be mapped to logical expressions
# here class = "SUB" takes values TRUE and FALSE
mpg %>%
filter (manufacturer == "chevrolet") %>%
ggplot (aes (x = displ, y = hwy, color = class == "suv")) +
geom_point (size = 4,
            alpha = 0.7, 
            position = "jitter") +
# adding labels to the graph
labs (title = "Fuel efficiency and engine size",
      subtitle = "... for Chevrolet",
      x = "Engine size (litres)",
      y = "Highway fuel efficiency (mph)",
      caption = "Source: Environment Protection Agency")


# Adding a smooth linear regression model line to the data
+ geom_smooth(method = "lm", formula  = y~x)
# grey line represents the 95% confidence interval

# Fitting a quadratic function (higher order polynomial to better fit the data)
+ geom_smooth (method = "lm", formula = y ~ poly(x, 2))

# but using method = "loess" fits a line to the scatter plot
# that helps us better see overall trend. forms smooth curve
+ geom_smooth(method = "loess", formula  = y~x, span=0.5)
# increasing the span makes the curve more smooth (generalises better)



# Loess smoother by group
ggplot (mpg, aes(x = displ, y = hwy, group = drv)) +
  geom_point (position = "jitter") +
  geom_smooth(formula = y ~ x, # can have formula = poly(x,2) for polynomial curve of 2 degree
              method = "loess")
  labs (x = "Engine Displacement (1)",
        y = "Highway Miles per Gallon")+
  # mapping a discrete variable to color. 
  scale_color_discrete(name = "Drive type",
                        labels = c("4-wheel", 
                          "Front-wheel", 
                          "Rear-wheel")) +
  theme (legend.position = "top") # includes new legend position
# here group only applies to geomsmooth. 
# if we do ggplot (mpg, aes(x = displ, y = hwy, color = drv))
# will color the points by groups in drv

# mapping a continuous variable to color. 
# scale_*_*
scale_color_continuous(name = "Drive type",
                        labels = c("4-wheel", 
                          "Front-wheel", 
                          "Rear-wheel")) +


# GEOM HISTOGRAM
  # width, number, locations of the bins

# default y is the count
ggplot (heights, aes (x = earn/1000) +
geom_histogram (binwidth = 10, 
              # change interior color of the bins
              fill = "maroon", 
              # position of first bar to start at 0
              boundary = 0,
              # colors the exterior of the bins white
              color = "white") +
labs (title = "Histogram of Earnings",
      x = "Earnings Per Annum (in Thousands)", y = "Frequency")

# adding density instead of count:
ggplot (heights, aes (x = earn/1000, y = after_stat(density)) +

# groupin the colors by sex (diff colors for each sex)
ggplot (heights, aes (x = earn/1000, y = after_stat(density), fill = sex) +
# TAKE NOTE: the lower layers variables overwrite the overarching settings



# GEOM DENSITY (smooth density)
ggplot (heights, aes(x = earn/1000, fill = sex)) +
  geom_density(alpha = 0.2) +
  labs (title = "Smooth Density Plots of Earnings",
        x = "Earnings Per Annum (in Thousands)", y = "Density") +
# changes the name of the legend on the side
scale_fill_discrete(name = "Gender", labels = c("Female", "Male"))

# smoothness of the densityplot. Oversmoothing, undersmoothing
  # oversmoothing: over generalise
  # undermsoothing: under generalise


# GEOM LINE
geom_line(group = 1) # group = 1 tells ggplot2 to treat all the points as belonging to one single group.
# if u dont see anything in the grom_line() plot


# group makes splits the plot into the diff groups in flat_type (diff flat hdb types)
ggplot (resale, aes (x = month, y = med_resale_price, group = flat_type)) + # can change to color = flat_type too
geom_line()


# add geom label
ggplot (resale, aes(x = month, y = med_resale_price/1000,
        color = flat_type)) +
geom_line(Iwd = 1, show. legend = FALSE) + 
# need to have a resale_text df
geom_label (data = resale_text, aes(label = flat_type), # overrides the gloabl mapping by defining a new mapping
            show.legend = FALSE, 
            size = 2.7,
            # vertical adjust
            vjust = "top", 
            # horizontal adjust
            hjust = "middle",
            nudge_y = -10,
            nudge_x = 15) +
labs(title = "Resale flat price trends, 2021 - 2024",
      x = "Year", y = "Median resale price (thousands)")
# CAN ALSO use geom_text() if dont want the border around it 


# ADDING REFERENCE LINES TO THE PLOT
# geom_vline() for vertical lines 
# geom_hline() for horizontal lines
geom_hline(aes(yintercept = 600), 
            lty = 2, # ranges from 1 to 6, just experiment. 
            # 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
            lwd = 0.3)

# geom_abline() for straight lines defined by a slope or an intercept

# Scaling:
ggplot() + .. + ..
+ scale_x_log10() 
+ scale_y_log10()




# geom_col() - barplot/ bar plot of count of each discrete variable or entity
# must manually count the variable
state_by_region <- murders %>% count(region)
ggplot(state_by_region, aes(x = region, y = n)) +
# Side-by-side bar plots for both genders
ggplot(qn1_6) +
  geom_col(aes(x = age_group, y = population/1000, fill = Gender), 
           position = "dodge") +
  labs(x = "", y = "Population (in thousands)",
       title = "Population Age Structure of Singapore (2015)",
       caption = "Source: Singapore Department of Statistics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top")

# geom_bar() - automatically counts the num of observations for each x value
ggplot(murders, aes(x = region)) +
geom_bar(stat = "identity") # to use the values directly from the data instead of the count


# geom_polygon() - easy way to draw maps, need latitude and longitude for the boundaries for different regions
library(maps)
us_states <- map_data("state") # must include latitude and longitudes

# just use a geom point to plot out the coordinates of the lat and longitude
# can turn the geom point into a map by using geom_polygon
ggplot(data = us_states, 
       aes(x = long, y = lat, group = group)) + # add a fill = region here if want to color by region
geom_polygon(color = "white",  # 
            fill = "lightblue")
# + geom_point(size = 0.2) use this if want points displayed

# can potentially add a fill that ranges with a variable
+ scale_fill_continuous(name = "Population (millions)", # name of the continuous column
                        low = "lightgray",
                        high = "steelblue")

# or scale_x_date(limits = as.Date(c("2020-01-01", "2021-01-31")))


# # Use geom_sf() hen have an sf object containing geometry column 
# when u have rds data available.
ggplot(qn1_7) +
  geom_sf(aes(geometry = geometry, fill = total/1000)) +
  scale_fill_continuous(name = "Population (thousands)",
                        low = "lightblue", high = "steelblue", na.value = "white") +
  labs(title = "Resident Population by Planning Area in 2015") +
  theme_void() +
  theme(legend.position = "bottom")
# add a theme void to make the theme clean slate





# use geom_polygon() when have explicit x/y columns and a group aesthetic for each polygon ring
ggplot(polygon_data, aes(x = x, y = y, group = group, fill = label)) +
  geom_polygon(color = "black") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Simple Polygons with geom_polygon()",
       fill = "Shape")


# You must manually specify group so each polygon is closed correctly






# Theme functions that change the appearance of the plot
theme_bw() - white background with grid lines
theme_light() - light areas and grid lines 
theme_classic() - classic theme, axes but no grid lines 
theme_linedraw() - only black lines
theme_dark() - dark background for contrast
theme_minimal() - minimal theme, no background
theme_gray() - grey_background (default)
theme_void() - empty theme, only gems are visible


# rotate the x axis to 90 degrees
theme(axis.text.x = element_text(angle = 90),



# facet_wrap() - Having multiple grids into one plot
ggplot(df1, aes(x = date, y = cases, color = country)) +
geom_line() +
facet_wrap(~ country, nrow = 2, ncol = 2) + # vairable that u want to separate on and visualise trend using multiple plots
labs(x = "", y = "Confirmed cases") +
theme(legend.position = "none") +
scale_x_date(limits = as.Date(c("2020-01-01", "2021-01-01"),
            date_breaks = "6 months", # interval between the x axis 
            date_labels = "%Y-%b") 

# facet_grid()
ggplot(mpg, aes(x = hwy)) +
  geom_histogram() +
  facet_grid(drv ~ .)  # One column, multiple rows (by 'drv')

ggplot(mpg, aes(x = hwy)) +
  geom_histogram() +
  facet_grid(. ~ drv)  # One row, multiple columns (by 'drv')

ggplot(mpg, aes(x = hwy)) +
  geom_histogram() +
  facet_grid(drv ~ cyl)  # Grid: rows = 'drv', columns = 'cyl'





# geom_tile - heat map: a numeric variable is mapped to a continous fill scale
ggplot(df1, aes(x = date, y = country)) +
  geom_tile(aes(fill = cases/1000)) +
  scale_fill_gradient(low = "white", high = "darkblue") + # continous fill scale
  labs(title = "Confirmed cases in 2020",
       fill = "Cases (thousands)", x = "", y = "") +
  theme(legend.position = "top")

# can always add a show.legend = False to remove the legend/ remove legend


# geom_area() - area plot
p2 <- ggplot(df_sg, aes(x = date, y = cases, fill = type)) +
  geom_area() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Confirmed cases", color = "",
  title = "Confirmed cases in Singapore, 2020") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("maroon", "gray"))

# SCALE FUNCTIONS

# ─── Setup ─────────────────────────────────────────────────────────────────────
library(ggplot2)
library(scales)       # for comma_format(), percent_format(), trans_format(), etc.
library(viridis)      # for scale_*_viridis_*
library(RColorBrewer) # for scale_*_brewer() and scale_*_distiller()

# ─── 1. AXIS SCALES ──────────────────────────────────────────────────────────────

# Continuous numeric x-axis: set exact tick positions & custom labels
scale_x_continuous(
  breaks = seq(0, 100, by = 20),      # where ticks appear
  labels = percent_format()           # label them as "0%", "20%", …
)

# Log10 transform on the y-axis (useful for wide‐ranging data)
scale_y_log10(
  breaks = c(0.1, 1, 10, 100),         # manual log‐scale ticks
  labels = trans_format("log10", math_format(10^.x))
)

# Date axis, monthly ticks with "Jan 2020" format
scale_x_date(
  date_breaks = "1 month",
  date_labels = "%b %Y"
)

# Discrete (categorical) x-axis: force order & relabel
scale_x_discrete(
  limits = c("Low","Medium","High"),  # fix the factor order
  labels = c("L","M","H")             # show custom tick text
)

# ─── 2. COLOR SCALES (color = …) ─────────────────────────────────────────────────

# 2a. Discrete manual palette
scale_color_manual(
  values = c(
    setosa     = "#E41A1C",
    versicolor = "#377EB8",
    virginica  = "#4DAF4A"
  )
)

# 2b. Continuous gradient (2 colors)
scale_color_gradient(
  low  = "lightblue",                 # color for smallest values
  high = "darkblue"                   # color for largest values
)

# 2c. Diverging gradient (midpoint)
scale_color_gradient2(
  low      = "red",                   # low end
  mid      = "white",                 # center
  high     = "blue",                  # high end
  midpoint = 0                        # where “mid” applies
)

# 2d. n-color gradient (custom stops)
scale_color_gradientn(
  colors = c("purple","pink","yellow","green")
)

# 2e. ColorBrewer qualitative (discrete)
scale_color_brewer(
  palette = "Set1"                    # “Set1”, “Dark2”, etc.
)

# 2f. ColorBrewer sequential (continuous)
scale_color_distiller(
  palette   = "YlGnBu",               # “YlGnBu”, “PuRd”, …
  direction = 1                       # reverse with –1
)

# 2g. Viridis perceptually uniform (continuous)
scale_color_viridis_c(
  option = "magma"                    # “viridis” (default), “plasma”, “inferno”, “cividis”
)

# 2h. Viridis for discrete
scale_color_viridis_d(
  option = "C"                        # choices A–D for discrete sets
)

# ─── 3. FILL SCALES (fill = …) ─────────────────────────────────────────────────

# Exactly the same variants exist for fill…
scale_fill_manual(...)        # discrete manual
scale_fill_gradient(...)      # 2-color gradient
scale_fill_gradient2(...)     # diverging
scale_fill_gradientn(...)     # n-color
scale_fill_brewer(...)        # ColorBrewer discrete
scale_fill_distiller(...)     # ColorBrewer sequential
scale_fill_viridis_c(...)     # continuous viridis
scale_fill_viridis_d(...)     # discrete viridis

# ─── 4. OTHER AESTHETIC SCALES ─────────────────────────────────────────────────

# Shapes (discrete)
scale_shape_manual(
  values = c(16, 17, 15)             # point shapes by index
)

# Line types (discrete)
scale_linetype_manual(
  values = c("solid","dashed","dotted")
)

# Point/line size (continuous)
scale_size_continuous(
  range = c(1, 6)                    # map data to size 1–6
)
# Or use area scaling
scale_size_area(
  max_size = 10                      # size proportional to area
)

# Transparency (alpha)
scale_alpha_manual(
  values = c(0.3, 0.6, 1)            # discrete mapping of alpha levels
)

# ─── 5. MISCELLANEOUS OPTIONS ─────────────────────────────────────────────────

# Remove the default expansion (padding) around axis limits
scale_y_continuous(
  expand = expansion(mult = c(0, 0.05))  # no pad below, 5% pad above
)


scale_y_continuous(
  limits = c(0,4500))  # no pad below, 5% pad above
)

# Reverse an axis
scale_x_reverse()                    # flips numeric x-axis

# Log-log combined transform
scale_x_log10() + scale_y_log10()    # both axes on log10 scales



# Manual discrete colors - scale_fill_manual
my_cols <- c("Fair" = "#E41A1C", "Good" = "#377EB8", "Very Good" = "#4DAF4A")

ggplot(diamonds, aes(cut, fill = cut)) +
  geom_bar() +
  scale_fill_manual(values = my_cols)

# Manual for color aesthetic
ggplot(mtcars, aes(factor(cyl), mpg, color = factor(cyl))) +
  geom_boxplot() +
  scale_color_manual(values = c("4" = "goldenrod", "6" = "turquoise", "8" = "orchid"))


# brewer palettes
# RColorBrewer discrete palette
ggplot(diamonds, aes(clarity, fill = clarity)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2")

# Brewer sequential for continuous
ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_distiller(palette = "YlGnBu", direction = 1)


# viridis palettes
# Discrete
ggplot(diamonds, aes(cut, fill = cut)) +
  geom_bar() +
  scale_fill_viridis_d(option = "C")

# Continuous
ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_viridis_c(option = "magma")


#Gradient palettes for continuous data
# Two-color gradient
ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "skyblue", high = "darkblue")

# Diverging gradient
ggplot(mtcars, aes(disp, mpg, color = mpg - hp/10)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 20)

# n-color gradient
ggplot(mtcars, aes(disp, mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_gradientn(colors = c("purple","pink","yellow","green"))





# conceptual stuff
Grid lines
Those light horizontal and/or vertical lines you often see behind a chart, 
used purely as a reading aid—they don’t carry any actual data values themselves, 
so Edward Tufte calls them “non-data ink.


Tufte’s rule of thumb is that a graphic should use as much of its ink as possible to show data, 
and as little as possible for anything else. 
Formally:
By removing grid lines (non-data ink), you shrink the denominator without touching the numerator—so the ratio goes up.


data ink ratio: (ink used to represent data) / (total ink used in graphic)

“lie factor” measures distortion in a graphic
lie factor: (size of effect shown in the graphic) / (size of effect in the data))

A perfectly honest chart has a lie factor of 1; anything else means the visual exaggerates or understates the data. 
Since grid lines don’t distort how the data itself is plotted, removing them has no impact on the lie factor.





### TUTORIAL mistakes #####


# Find the number of distinct in each column after a group by 
summer_games = olympics %>%
  filter(season == "Summer") %>%
  group_by(year)%>%
  summarize(n_nations = n_distinct(noc), 
            n_events = n_distinct(event))


# Finding max and min of a column and outputting them in 2 columns
years %>% # new column created called lagged_value
  mutate(lagged_value = lag(year),
        years_since = year - lagged_value) %>%  # Shift values by 1 row

mutate(lag_value = lag(index, n = 1, order_by = year), # be careful to edit n (how many to lag by)
        growth_rate = round((index - lag_value)*100/lag_value, 2)) %>%

  filter(!is.na(years_since)) %>%
  summarize(shortest_gap = min(years_since),
            longest_gap = max(years_since))



# summing up the number of diff entities in each column, for each group by
medals <- olympics %>%
  filter(season == "Summer", year >= 1980) %>%
  select(noc, year, event, medal) %>% unique() %>%
  group_by(noc, year) %>%
  rename(Country = noc, Year = year) %>%
  summarize(Gold = sum(medal == "Gold", na.rm = TRUE),
            Silver = sum(medal == "Silver", na.rm = TRUE),
            Bronze = sum(medal == "Bronze", na.rm = TRUE),
            Total = Gold + Silver + Bronze, .groups = "drop") %>%
  arrange(desc(Gold), desc(Silver), desc(Bronze))



# annotating a graph. Also plotting 2 lines on the same graph
ggplot(summer_games, aes(x = year)) +
  geom_line(aes(y = n_events), color = "maroon", linewidth = 1.5) +
  geom_line(aes(y = n_nations), color = "black", linewidth = 1.5) +
  labs(title = "Number of events and nations participated in the Summer Olympics", 
       x = "Year", y = "") +
  annotate("text", 
    x = 2010, 
    y = c(180, 280),
    label = c("Nations", "Events"),
    color = c("black", "maroon")) +
  scale_x_continuous(breaks = seq(1896, 2016, 20), limits = c(1896, 2016))



### creating data to use in geom label

# Data for the five countries in summer_top_5
golds_top_5 = df2004 %>% filter(noc %in% summer_top_5)

# Prepare the labels to use in the text geom
df_labels <- golds_top_5 %>% filter(year == 2012) %>%
    left_join(regions, by = c("noc" = "NOC"))
  
# Golds received across years
ggplot(golds_top_5, aes(x = year, y = n_gold, color = noc)) +
  geom_line(size = 2) +
  geom_point(size = 7) +
  geom_label(data = df_labels, aes(label = region),
            hjust = "left", nudge_x = 0.5, size = 3.5) +
  labs(x = "Year", y = "Number of golds received", color = "Region",
       title = "Gold medals received for selected countries, 2004 - 2012") +
  scale_x_continuous(breaks = seq(2004, 2012, 4), limits = c(2004, 2014)) +
  theme(legend.position = "none")
  # to remove legend from diagram


# Using rank min_rank
ranks_top_5 <- df2004 %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(n_gold))) %>%
  filter(noc %in% summer_top_5) 

# min_rank() gives every tie the same (smallest) value so that c(10, 20, 20, 30) 
# gets ranks c(1, 2, 2, 4). It's the way that ranks are usually computed in sports and is equivalent to rank(ties.method = "min").

# dense_rank() works like min_rank(), but doesn't leave any gaps, so that c(10, 20, 20, 30) 
# gets ranks c(1, 2, 2, 3).


qn1_1 <- read_excel("../data/t1-9.xls", sheet = "T7(Total)", 
                    range = "B6:V511", na = "-") %>%
  filter(Subzone == "Total", `Planning Area` != "Total") %>%
  mutate(across(c(Total:`85 & Over`), as.numeric)) %>%
  select(-Subzone)



# geom_segment()
ggplot(hdi_diff) +
  geom_segment(aes(x = 0, xend = diff, y = region, yend = region), linewidth = 1) +
  geom_point(aes(x = diff, y = region, color = diff > 0), size = 4, show.legend = FALSE) +
  geom_text(data = hdi_diff_text,
            aes(x = diff, y = region, label = diff_label), nudge_y = 0.3) +
  geom_vline(xintercept = 0, color = "gray") +
  labs(x = "Changes in median HDI compared to 1990 levels", y = "",
       title = "Changes in median Human Development Index in 2021",
       subtitle = "... compared to 1990 levels by continent")



# extract() function - to extract out into different columns
df <- data.frame(code = c("X123-A", "Y456-B", "Z789-C"))

extract(df, code, into = c("letter", "number", "suffix"), 
        regex = "([A-Z])([0-9]+)-([A-Z])")



# number of distinct in column:
n_distinct(df$course_code)



# geom_tile plot
ggplot(df3, aes(x = year, y = COUNTRY_CODE, fill = growth)) +
  geom_tile() +
  geom_text(aes(label = round(growth, 2)), size = 4) +
  scale_fill_gradient(name = "Growth Rates", na.value = NA,
                      low = "white", high = "steelblue") +
  labs(title = "Year-on-year growth (%) on housing price index", x = "", y = "") +
  scale_x_continuous(breaks = seq(2016, 2023, 1)) +
  theme(legend.position = "top")




# geom_area() plot

ggplot(sales_long, aes(x = month, y = revenue, fill = product, group = product)) +
  geom_area(
    position = "stack",      # stack | identity
    alpha = 0.7,             # transparency
    color = "black",         # outline color
    size = 0.5,              # outline thickness
    linetype = "solid"       # solid, dashed, dotted, etc.
  ) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  scale_x_date(
    date_labels = "%b",     # Jan, Feb, etc.
    breaks = "1 month"
  ) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$")
  ) +
  labs(
    title = "Monthly Revenue by Product - 2023",
    subtitle = "Using geom_area() with customization",
    x = "Month",
    y = "Revenue",
    fill = "Product"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







