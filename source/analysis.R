library("tidyverse")
library("dplyr")
library("ggplot2")
library("plotly")
library("maps")
library("mapproj")
library("patchwork")
library("leaflet")

# The functions might be useful for A4
source("~/Documents/info201/assignments/a4-Gnorezgnaw/source/a4-helpers.R")
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns total jail population count grouped by years <todo:  update comment>
get_year_jail_pop <- function() {
  plot_data <- df %>% 
    select(year, total_jail_pop) %>% 
    group_by(year) %>% 
    summarise(all_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(plot_data)   
}

# This is the code for plotting the chart <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  plot1 <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = all_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
           x = "Year",
           y = "Total Jail Population")
  return(ggplotly(plot1))   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# This function returns total jail population count grouped by years and states
get_jail_pop_by_states <- function(states) {
  state_data <- df %>% 
    select(year, state, total_jail_pop) %>% 
    group_by(year, state) %>% 
    filter(state %in% states) %>% 
    summarise(state_jail_pop = sum(total_jail_pop, na.rm = TRUE), .groups = "drop")
  return(state_data)
}

# This is the code for plotting the chart on states
plot_jail_pop_by_states <- function(states){
  plot_state <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = state_jail_pop, colour = state)) +
    labs(title = "Growth of Prison Population by State (1970-2018)", 
         x = "Year",
         y = "State Total Jail Population")
  return(ggplotly(plot_state))   
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_race<- function(race) {
  
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_inequality_map <- function(){
  
}
## Load data frame ---- 


