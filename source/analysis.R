library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(mapproj)
library(maps)
library(patchwork)
library(leaflet)

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
# What year had the largest jail population
largest_year <- function(){
  largest_pop <- df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(all_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(all_jail_pop == max(all_jail_pop)) %>% 
    pull(year)
  return(largest_pop)
}

# What year had the least jail population?
least_year <- function(){
  least_pop <- df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(all_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(all_jail_pop == min(all_jail_pop)) %>% 
    pull(year)
  return(least_pop)
}

# What was the largest jail population for the location above?
l_pop <- function(){
  least <- df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(all_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(all_jail_pop == min(all_jail_pop)) %>% 
    pull(all_jail_pop)
  return(prettyNum(least,big.mark=",",scientific=FALSE))
}

# What was the jail population in 2008?
max_pop <- function(){
  max_p <- df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(all_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(all_jail_pop == max(all_jail_pop)) %>% 
    pull(all_jail_pop)
  return(prettyNum(max_p,big.mark=",",scientific=FALSE))
}



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
# Do some kinds of counties seem to imprison more people than other kinds of counties? 
# This function returns total jail population count grouped by years and urbanicity
get_jail_pop_by_urbanicity<- function(urbanity) {
  urbanity_data <- df %>% 
    select(year, total_jail_pop, urbanicity, fips) %>% 
    group_by(year, urbanicity) %>% 
    filter(urbanicity %in% urbanity) %>% 
    summarise(urbanaicity_all = sum(total_jail_pop, na.rm = TRUE), .groups = "drop")
  return(urbanity_data) 
}

# # This is the code for plotting the chart
plot_jail_pop_by_urbanicity <- function(urbanity){
  plot_urbanicity <- ggplot(data = get_jail_pop_by_urbanicity(urbanity)) +
    geom_line(mapping = aes(x = year, y = urbanaicity_all, colour = urbanicity)) +
    labs(title = "Growth of Prison Population by Urbanicity (1970-2018)", 
         subtitle = "urbanicity county comparison that reveals potential patterns of inequality",
         x = "Year",
         y = "Total Jail Population")
  return(ggplotly(plot_urbanicity))   
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# This function returns county map_data by state
df1 <- df %>% 
  select(fips, year, state, county_name, total_jail_pop) %>% 
  filter(year == "2018")
county_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")

get_map_data <- function(states){
  map_data <- county_shapes %>% 
    left_join(df1, by="fips") %>% 
    filter(state == states & county_name != "") 
  return(map_data)  
}

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

# This is the code for plotting the chart
plot_map <- function(states){
  cases_map <- ggplot(get_map_data(states)) +
    geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "gray",
    linewidth = .3        
    ) +
    scale_fill_continuous(limits=c(0, max(get_map_data(states)$total_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
    labs(title = "County Prison Population Map by State(2018)",
         subtitle = "A map shows county inequality by states",
         fill = "County jail population") +
    blank_theme +
    coord_map() 
  return(ggplotly(cases_map))
}


## Load data frame ---- 


