############################## 1. DATES ##############################

help(strptime)

event <- strptime(event, format = "COPY THE STRING");
event <- difftime(event, Sys.time(), units = "mins"); #Change this so it is not negative
as.numeric(event);

# %a Abbreviated weekday name in the current locale on this platform
# %A Full weekday name in the current locale
# %b Abbreviated month name in the current locale on this platform
# %B Full month name in the current locale
# %c Date and time. Locale-specific on output, "%a %b %e %H:%M:%S %Y" on input.
# %C Century (00-99): the integer part of the year divided by 100.
# %d Day of the month as decimal number (01-31).
# %D Date format such as %m/%d/%y: the C99 standard says it should be that exact format (but not all OSes comply).
# %e Day of the month as decimal number (1-31), with a leading space for a single-digit number.
# %F Equivalent to %Y-%m-%d (the ISO 8601 date format).
# %g The last two digits of the week-based year (see %V). (Accepted but ignored on input.)
# %G The week-based year (see %V) as a decimal number. (Accepted but ignored on input.)
# %h Equivalent to %b.
# %H Hours as decimal number (00-23). As a special exception strings such as 24:00:00 are accepted for input, since ISO 8601 allows these.
# %I Hours as decimal number (01-12).
# %j Day of year as decimal number (001-366): For input, 366 is only valid in a leap year.
# %m Month as decimal number (01-12).
# %M Minute as decimal number (00-59).
# %n Newline on output, arbitrary whitespace on input.
# %p AM/PM indicator in the locale. Used in conjunction with %I and not with %H. An empty string in some locales (for example on some OSes, non-English European locales including Russia). The behaviour is undefined if used for input in such a locale.
# %P for output, which uses a lower-case version (%p may also use lower case): others will output P.
# %r For output, the 12-hour clock time (using the locale's AM or PM): only defined in some locales, and on some OSes misleading in locales which do not define an AM/PM indicator. For input, equivalent to %I:%M:%S %p.
# %R Equivalent to %H:%M.
# %S Second as integer (00-61), allowing for up to two leap-seconds (but POSIX-compliant implementations will ignore leap seconds).
# %t Tab on output, arbitrary whitespace on input.
# %T Equivalent to %H:%M:%S.
# %u Weekday as a decimal number (1-7, Monday is 1).
# %U Week of the year as decimal number (00-53) using Sunday as the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1). The US convention.
# %V Week of the year as decimal number (01-53) as defined in ISO 8601. If the week (starting on Monday) containing 1 January has four or more days in the new year, then it is considered week 1. Otherwise, it is the last week of the previous year, and the next week is week 1. (Accepted but ignored on input.)
# %w Weekday as decimal number (0-6, Sunday is 0).
# %W Week of the year as decimal number (00-53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention.
# %x Date. Locale-specific on output, "%y/%m/%d" on input.
# %X Time. Locale-specific on output, "%H:%M:%S" on input.
# %y Year without century (00-99). On input, values 00 to 68 are prefixed by 20 and 69 to 99 by 19 - that is the behaviour specified by the 2018 POSIX standard, but it does also say 'it is expected that in a future version the default century inferred from a 2-digit year will change'.
# %Y Year with century. Note that whereas there was no zero in the original Gregorian calendar, ISO 8601:2004 defines it to be valid (interpreted as 1BC): see https://en.wikipedia.org/wiki/0_(year). However, the standards also say that years before 1582 in its calendar should only be used with agreement of the parties involved.
# %z Signed offset in hours and minutes from UTC, so -0800 is 8 hours behind UTC. Values up to +1400 are accepted. (Standard only for output. For input R currently supports it on all platforms.)
# %Z (Output only.) Time zone abbreviation as a character string (empty if not available). This may not be reliable when a time zone has changed abbreviations over the years.

############################## 2.1 INDEXATION DATAFRAMES ##############################

df <- data.frame(name = c("Rex", "Tom", "Jerry"),
                 type = factor(c("dog", "cat", "mouse"), levels = c("dog", "cat", "mouse")),
                 age = c(3, 15, 2));
df;

############################## 2.1.1 Dataframes indexation by integer ##############################
df[1,2]; # One value
df[2,3];
df[2,]; # One row
df[,3]; # One column
df[2, c(1,2)]; # Combination

############################## 2.1.2. Dataframes indexation logical indexation ##############################
index <- c(FALSE, FALSE, TRUE);
df[index,];
df[index, index];

countries <- c("Spain", "Russia", "Japan", "India", "Colombia");
populations <- c(46.5, 144.5, 126.8, 1339, 49.07);
countries_pop <- data.frame(country = countries, pop = populations);
countries_pop;
# Filtering rows
countries_pop[countries_pop[,2] < 50, ]; # All the columns
countries_pop[countries_pop[,2] < 50, 1]; # Only the first column
countries_pop[which.max(countries_pop[,2]), 1]; # Just the first column of the max value

############################## 2.1.3. Dataframes indexation by name ##############################
colnames(countries_pop);
rownames(countries_pop);
countries_pop[, "country"]; # Filtering by column name
countries_pop["3",]; # Filtering by row name
countries_pop["3", "country"]; # Filtering by both column and row name

############################## 2.1.4. Dataframes indexation by variable ($ operator) ##############################
colnames(df); 
# Used to index columns

df$name # df[, "name"]
df$type; # df[, "type"]
df$age; # = df[,"age"]

############################## 2.2. INDEXATION DATA TABLES ##############################

############################## 2.2.0 dt[i, j, by]####################################
# Take DT, subset/reorder rows using 'i', then calculate 'j', grouped by 'by'.
# i = where/order
# j = select
# by = group by

############################## 2.2.1 [i] == where/order ####################################
head(iris);
dt <- as.data.table(iris);

### WHERE
# data.table using dt[i, j, by] operator
res_3 <- dt[Sepal.Length >= 5.4 & Petal.Length <= 2.0];
res_3;

### ORDER BY
dt_ordered <- dt[order(Sepal.Length, -Petal.Length)]; #Positive for ascending, negative for descending
head(dt_ordered);

############################## 2.2.2 [,j] == Select ####################################
### Get a column
# Standard method
dt[, "Sepal.Width"];
# List method
dt[, list(Sepal.Width)];

# Return as a vector
dt[, Sepal.Width];
class(dt[, Sepal.Width]);
dt[["Sepal.Width"]];

# Multiple columns
dt[, c("Sepal.Width", "Petal.Width")];
dt[, list(Sepal.Width, Petal.Width)]; # Using a list to return more than one value
dt[, Sepal.Width:Petal.Width]; # Range from one column to another including all between

# Multiple columns and rename
dt[, list(V1 = Sepal.Width, V2 = Petal.Width)];

# Remove columns
dt[, -c("Sepal.Width", "Petal.Width")];

### Compute only returning one value
dt[, mean(Sepal.Width)];
dt[, sum(Sepal.Width + Petal.Width)];
dt[, list(total_width = Sepal.Width + Petal.Width)];
dt[, sum(Sepal.Width + Petal.Width > 5)];

############################## 2.2.3 [i,j] == Select where ####################################
dt[Sepal.Length >= 5.4, list(Sepal.Length = Sepal.Length, total_width = Sepal.Width + Petal.Width)];

# Number of rows matching i condition
nrow(dt[Sepal.Length >= 5.4]);
dt[Sepal.Length >= 5.4, list(nrows = length(Sepal.Length))];
dt[Sepal.Length >= 5.4, list(nrows = .N)]; # Count method

############################## 2.2.4 [j, by] == Select group by ####################################
dt[, list(n = .N), by = "Species"]; #Count grouping per column values
dt[, list(avg_sepal_length = mean(Sepal.Length)), by = "Species"];

############################## 2.2.5 [i, j, by] == Select where group by ####################################
# One variable
dt[Sepal.Length > 5, list(mean(Sepal.Length), sum(Sepal.Length)), by = "Species"];
dt[Sepal.Length > 5, list(n = .N), by = c("Species", "Sepal.Length")];

############################## 2.2.6 [i, j, keyby] Select where group by order by ####################################
dt[Sepal.Length > 5, list(n = .N), by = "Sepal.Length"];
# Using keyby rather than by will make result rows ordered by the columns you are grouping on
dt[Sepal.Length > 5, list(n = .N), keyby = "Sepal.Length"];

############################## 2.2.7 Chaining [i, j, by] ##############################
# Without chaining
res <- dt[Sepal.Length > 5, list(n = .N), keyby = "Sepal.Length"];
res[n > 7];

# With chaining
dt[Sepal.Length > 5, list(n = .N), keyby = "Sepal.Length"][n > 7]; #You can add another condition to the result chaining

############################## 2.2.8 .SD ##############################
# Example 1
dt <- data.table(type = c("cat","dog", "mouse", "dog", "cat", "dog"),
                 name = c("Salem", "Rex", "Jerry", "Toby", "Misi", "Beast"),
                 age = c(7, 3, 1, 11, 9, 5),
                 owner_age = c(31, 29, 7, 28, 16, 15));
dt[, print(name), by = "type"];
dt[, print(.SD), by = "type"]; #.SD represents the entire datatable and we can call it to select all
dt[, print(.SD), by = "type", .SDcols = "owner_age"]; # But you can filter the columns too

# Example 2
dt[, list(mean_age = mean(age), mean_owner_age = mean(owner_age)), by = "type"];
dt[, list(mean_age = mean(.SD[,age]), mean_owner_age = mean(.SD[,owner_age])), by = "type"];
dt[, lapply(.SD, mean), by = "type", .SDcols = c("age", "owner_age")];
dt[, lapply(.SD, mean), by = "type", .SDcols = setdiff(colnames(dt), c("type", "name"))];

# Example 3
dt <- as.data.table(iris);
dt[, head(.SD, 2), by = "Species"]; # Getting the two first columns ordered by the group by

############################## 2.3. GREPL ##############################
# Search for matches to argument pattern within each element of a character vector
grepl(pattern, x)

############################## 3. FUNCTION CREATION ##############################
############################## 3.1 NO ARGUMENTS ##############################
my_fun <- function(){
  print("Hello world");
}
my_fun();

############################## 3.2 FIXED ARGUMENTS WITHOUT DEFAULTS ##############################
my_fun <- function(name, number){
  print(sprintf("Hello %s. Is %d your favourite number?", name, number)); #VALUES TO PRINT (S CHAR AND D INT)
}

############################## 3.3 FIXED ARGUMENTS WITH DEFAULTS ##############################
my_fun <- function(name, number = 100){
  print(sprintf("Hello %s. Is %d your favourite number?", name, number));
}

############################## 3.4 DINAMIC ARGUMENTS WITH DEFAULTS ##############################
# Admits any input type and number of arguments
my_fun <- function(name, number = 100, ...){
  arguments <- list(...); #we need to pass this to a list
  print(sprintf("Hello %s. Is %d your favourite number?", name, number));
  print(arguments[[1]]);
}
my_fun("Evey", 5, 1, c("a", "b"));

############################## 3.5 PASTE ##############################
# Concatenate vectors after converting to character.
help(paste)
paste (c("a", "b"), sep = " ", collapse = " ")
paste0(c("a", "b"), collapse = NULL)

############################## 4. LOOPS  ##############################
############################## 4.1. IF - ELSE IF - ELSE ############################################
x <- 1;

if (x < 0){
  x <- x + 2
} else if (x < 1){
  x <- x - 5
} else{
  x <- x - 1
}
print(x);

############################## 4.2. FOR CLAUSE ############################################
x <- 1;
for (iterator in 1:5){
  print(x);
  x <- x + iterator;
  print("-----");
}

v <- c("a", "b", "c");
iterators_list <- c();
for (iterator in v){
  print(iterator);
  iterators_list <- c(iterators_list, iterator); # Append
  print("-----");
}
print(iterators_list);

############################## 4.3. WHILE CLAUSE ############################################
############################## 4.3.1. USING LOGICAL CONDITION ######################################
x <- 1;
while (x <= 100){
  print(x);
  x <- x + 1;
  print("-----");
}
print(x);

############################## 4.3.2 USING FLAG ######################################
x <- 0;
flag <- TRUE; #Similar to break

while (flag){
  x <- x + 1;
  print(x);
  if (x == 14){
    flag <- FALSE;
  }
  print("-----");
}
print(x);

############################### 4.4. BREAK/NEXT ############################################
############################### 4.4.1. BREAK ############################################
# Difference between flag and break is that when you reach it just stops, with flag it finishes the iteration
for (iterator in 1:10){
  print(iterator);
  print("-----");
  if (iterator == 5){
    break
  };
}

############################### 4.4.2. NEXT ############################################
# For loop
for (iterator in 1:10){
  if (iterator == 5 | iterator == 7){
    next #Passes to the next iteration
  };
  print(iterator);
  print("-----");
}

# Infinite while loop again!
x <- 1;
flag <- TRUE
while (x < 10){
  print(x);
  if (x == 5){
    next #Passes to the next iteration and enters an infinite loop
    };
  x <- x + 1;
  print("-----");
}
print(x);

############################### 4.5 APPLY FUNCTIONS ############################################

df <- data.frame(v1 = 1:10, v2 = 11:20, v3 = 21:30);
dim(df);
df;
big_df <- as.data.frame(matrix(1:10^8, nrow = 10^4, ncol = 10^4));
dim(big_df);
big_df[1:2, 1:10];

############################### 4.5.1. USING FOR LOOP ############################################
res <- c();
for (column in 1:3){
  values <- df[, column]; # Extract the entire column
  res <- c(res, mean(values)); # Then calculate the mean
}

############################### 4.5.2. USING APPLY ############################################
# Function must be passed without () just function
apply(df, 2, mean); # By column
apply(df, 1, mean); # By row

############################### 4.5.3. USING LAPPLY ############################################
# Just for columns and much faster than apply and that a for loop
lapply(df, mean);

############################### 4.5.4. USING SAPPLY ############################################
# Returning a vector rather than a list
sapply(df, mean);

############################### 5. LISTS ############################################

############################### 5.1. CREATION ########################################
#Specifying objects and values. Concatenation of a set of objects with no structure of rows or columns
#Used when there is no point on having a structure (when you have missing values f.e.)
#You can put in whatever you want even functions etc

l <- list(type = c("cat","dog", "mouse", "dog", "cat", "dog"),
          name = c("Salem", "Rex", "Jerry", "Toby", "Misi", "Beast"),
          age = c(7, 3, 1, 11, 9, 5),
          owner_age = c(31, 29, 7, 28, 16, 15));

############################### 5.2. GET/SET STRUCTURE ####################################
# You have just a length
# Dimensions
length(l);

# There are not colnames and row names, but you can set names
names(l) <- c("V1", "V2", "V3", "V4");

# Get structure
str(l);

############################### 5.3. LIST TO DATA.FRAME/DATA.TABLE  ####################################
df <- as.data.frame(l); #If you have complex objects it would not work (if you have different lengths it is going to give you an error in df and
#for dt it is duplicating recycling values)
dt_1 <- as.data.table(l);

############################### 5.4. LIST vs DATA.FRAME/DATA.TABLE  ####################################

# Would you do this with a list? If it is about storing something of different lengths and types yes

### PROS
# Variables of different length
# Storing advanced R datatypes

### CONS
# No data.table functionality

############################### 5.5. INDEXATION ######################################
l <- list(type = c("cat","dog", "mouse", "dog", "cat", "dog"),
          name = c("Salem", "Rex", "Jerry", "Toby", "Misi", "Beast"),
          age = c(7, 3, 1, 11, 9, 5),
          owner_age = c(31, 29, 7, 28, 16, 15));

############################### 5.5.1. by integer ########################################

### Single position
# Get sublist
l[2]; #getting the second object list

# Get object type inside list
l[[2]]; # The content of the object directly

### Get multiple positions
# Sublist with multiple objects
l[c(2,3)]; #Both lines

# Specific object
# Third value of the second object
l[[c(2,3)]]; # Specific value from line
l[[2]][3]

### Negative indexing
l[-2]; #remove certain objects
l[c(-2,-3)]; #remove two lines

############################### 5.5.2. logical indexation #################################
index <- c(FALSE, TRUE, FALSE, FALSE);

# Get sublist
l[index];

############################### 5.5.3. by name #################################
# Get sublist
l["name"]

# Get object inside list
l[["name"]]

############################### 5.5.4 by variable #################################
# Get object inside list
l$name; #Equivalent to double bracket l[['name']]

# Partial matching
l$nam; #As there is no other option starting with name it automatically autocompletes
l[["nam"]];

############################### 5.6. MODIFICATION ######################################
l <- list(type = c("cat","dog", "mouse", "dog", "cat", "dog"),
          name = c("Salem", "Rex", "Jerry", "Toby", "Misi", "Beast"),
          age = c(7, 3, 1, 11, 9, 5),
          owner_age = c(31, 29, 7, 28, 16, 15));

############################### 5.6.1. using integer indexation ########################################
l[[1]][3] <- "lion";
l[2] <- list(c("Simba", "Timon", "Pumba", "Simba", "Timon", "Pumba"));

############################### 5.6.2. using logical indexation ########################################
l[[1]][l$age < 10] <- "baby";

############################### 5.6.3 using character indexation ########################################
l["age"] <- list(l[["age"]] + 1);

############################### 5.6.4. using variable name ###############################
l$age <- max(l$age);


############################### 5.7. EXPANSION ######################################
# Concatenate new list
l <- c(l, list(new_v = c(TRUE, FALSE))); #Concatenating / appending

# by index
l[6] <- list("new_v2"); #Creating a sixth element with the new values
#Same syntax as overwriting but we did not have the sixth

l["new_v2b"] <- c("new_v2b");

names(l)[6] <- "new_name" #Change name of object in a list

# by variable name
l$new_v3 <- c("new_v3"); #Easier

############################### 5.8. REDUCTION ######################################

# by index
l[2] <- NULL; #Assigning it to empty/null value will remove second position

# by variable name
l$new_v3 <- NULL;

############################### 5.9. CASES OF USE ######################################

# [1] Mixing objects of different lengths
l <- list(v1 = c(1, 2), v2 = c(1, 2, 3, 4));
l;

# [2] Store complex datatypes
structure <- summary(mtcars);
l <- list(structure = structure);
l;

# [3] Function arguments
library(xgboost);
model <- xgb.train(params = list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
                                 objective = "reg:linear", eval_metric = "rmse"),
                   xgb.DMatrix(as.matrix(mtcars[, setdiff(colnames(mtcars), "mpg")]),
                               label = mtcars$mpg), 
                   nrounds = 2);

# [4] ... argument
my_fun <- function(name, number, ...){
  arguments <- list(...); #Always to a list
}

# [5] For loop iterator
l <- list(type = c("cat","dog", "mouse", "dog", "cat", "dog"),
          name = c("Salem", "Rex", "Jerry", "Toby", "Misi", "Beast"),
          age = c(7, 3, 1, 11, 9, 5),
          owner_age = c(31, 29, 7, 28, 16, 15));

for (object in l){
  print(object[2]);
}

############################## 6. DPLYR ##############################
help(dplyr)


library(dplyr);
mtcars %>% #PIPING
  filter(cyl > 4) %>% # Where (filtering)
  group_by(carb) %>% # Group by (Always before summarise)
  summarise(mean_horsepower = mean(hp)) %>% # Select (Computations)
  top_n(3, desc(mean_horsepower));

diamonds_dt %>% #PIPING
  mutate(price_carat = price/carat) %>% # Adding a column
  group_by(price_carat) %>% # Group by (Always before summarise)
  summarise(counts = n()) %>% # Count occurrences
  arrange(desc(counts)) # Sort by column

############################## 7. IRIS AND MTCARS ##############################
### mtcars
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors

### iris
# iris is a data frame with 150 cases (rows) and 5 variables (columns) named 
# Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.

# Both are originally dataframes  and we need to cast them if we are going to work with them as datatables
library(data.table);
dt <- as.data.table(iris);

############################## 9.1. READ DATASET ##############################
dat <- readRDS("C:/Users/Rodrigo Morales/Documents/IE/SUBJECTS/2/R/quixote.RData"); #TRANSFORMATIONS REQUIRED?????????????????????
class(dat)

############################## 9.2. LESS FREQUENT ITEM ##############################
# dplyr or piping
dat %>%
  filter(50 > price) %>%
  group_by(destination) %>%
  count(destination, origin) %>%
  slice(which.min(n)) # Selects the minimum value

sort(table(dat$origin))[1] #Table returns all frequencies for all elements
sort(table(dat$origin), decreasing = T)[1] #Good practice to try this calculation outside and then remove $dat
dat[price<50, list(less_frequent = names(sort(table(origin), decreasing = T)[1]), by = X)]

############################## 10. (...) ARGUMENT ##############################

n_p <- function(...){ # Just a box where you put in a list of arguments which could be of any type
  arguments <- list(...); #Passing the arguments to a list is a must
  counter <- 0;
  for (object in arguments){ # Iterating over the objects
    if (class(object) == "character"){
      counter <- counter + sum(grepl("p", object)) #Search for p in object and count by summing
    }
  }
  return(counter);
}

############################## 11. OTHER EXERCISES ##############################
# Numbers from 0 to 9 can be defined like this:
names <- c("J3esus", "Mar1ia", "J0os4e");
gsub("[0-9]", "", names)

# Measure times
t1 <- system.time({
  values_1 <- c();
  for (col in colnames(df)){
    values_1 <- c(values_1, my_fun(df[, col]));
  }
})[3];

# Adding rows -> rbind, adding columns cbind
df <- rbind(df, new_pets)

# Value matching
found <- lost_dog %in% df$name

# (*) Exercise 4: Use the already loaded diamonds dataset and print the last two carat value
# of each cut.
diamonds_dt %>% #PIPING
  group_by(cut) %>% # Group by (Always before summarise)
  summarise(carat) %>%
  slice(tail(row_number(), 2)) #Last two for each group