# # Geography 4300/6300: Reading in data using CSVs and basic manipulation

# Before you start this script, create a new project on your computer/flash drive for the course.
# Projects make working with and sharing research a lot easier--always a good practice.
# Move this script and all other data for the course into that folder.

# You should have already installed the tidyverse package on your computer.
# Now we just need to call it.
library(tidyverse)

# Next download some county level data from GitHub
# You could read it in using the `read_csv` command in the tidyverse.

census_data<-read_csv("https://github.com/communitymaplab/geog4300_datascience/raw/refs/heads/main/data/ACSCtyData_2022ACS.csv")

# The data page of the project site lists multiple data sources in our course Github repo. 
# Link: https://communitymaplab.github.io/geog4300_datascience/course_data.html
# Pick another csv dataset and load it into R. 
# You have two options: right click and save the "Raw" link or (2) download the dataset to a course project folder and load it from there.

###

# Now open the data to look at it. Click on it in the Environment tab.
# Or use this:

View(census_data)

# Let's take a look at this file.
# "names" lists just the field
names(census_data)

# The "str" command gives you field names and types
str(census_data)

# "head" and "tail" give you the first and last rows
head(census_data)
tail(census_data)
