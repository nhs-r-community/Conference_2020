##### Using the NHSRdastasets and officeR packages to produce PowerPoints #####

# Really useful guides:
# https://cran.univ-paris1.fr/web/packages/officer/vignettes/powerpoint.html
# https://davidgohel.github.io/officer/reference/ph_with.html


##################### # 
# INSTALL LIBARIES ####
##################### #  

library(reshape2)
library(zoo)
library(NHSRdatasets)
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) 
library(scales)
library(gridExtra)
library(officer)
library(rvg)
library(magrittr)


################# #
# IMPORT FILES #### 
################# #


file_path <- '.'

# Read in org lookup file
orgs <- read_excel(paste(file_path, 'LUP.xlsx', sep = '/'))


####################### # 
# CHECK OUT THE DATA #### 
####################### #


head(ae_attendances) # attendance, breach and admissions data
head(orgs) # org lookup


# only select the NE&Y acute orgs
ae_attendances_NEY <- ae_attendances %>% 
  mutate(org_code = as.character(org_code),
         type = as.character(type)) %>%
  
  inner_join(orgs, by = c('org_code' = 'Org_Code')) %>%
  
  filter(Region_Name == 'NEY') %>%
  
  # unpivots the data - makes the data longer
  melt(measure.vars = c('attendances','breaches','admissions')) %>%  
  
  # calculates subtotals for each group
  dcast(period + Region_Name + STP_Name + Org_Name + type  ~ variable, 
        fun.aggregate = sum,
        margins = TRUE) 


# Change the factor columns to a character
i <- sapply(ae_attendances_NEY, is.factor)
ae_attendances_NEY[i] <- lapply(ae_attendances_NEY[i], as.character) 

# Create month name vector for use as the x-axis on the graph
my.month.name <- Vectorize(function(n) c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")[n])


# Create a data frame where all possible combinations are in the Org_Name variable
ae_attendances_NEY_wrangled <- ae_attendances_NEY %>% 
  mutate(Org_Name = case_when(STP_Name == '(all)' ~ Region_Name,
                              Org_Name == '(all)' ~ STP_Name,
                              TRUE ~ Org_Name)) %>%
  
  # remove extra rows
  filter(Region_Name != '(all)',
         type == '(all)') %>%
  dplyr::select(-`(all)`, -type) %>%
  
  # reshape the data into a tidy format with 1 value per row
  pivot_longer(5:7, names_to = 'Measure', values_to = 'Value') %>%
  
  # add in the financial year (for the graph groups) and the MonthName columns (for the x axis)
  mutate(Measure = as.character(Measure),
         FY = case_when(period < as.Date('2017-04-01') ~ 'FY 16/17',
                        period < as.Date('2018-04-01') ~ 'FY 17/18',
                        period < as.Date('2019-04-01') ~ 'FY 18/19',
                        TRUE ~ 'Check'),
         MonthName = my.month.name(month(period)),
         MonthName = factor(MonthName, #because the months aren't in alphabetical order a factor is required to order correctly
                            levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"), 
                            ordered = TRUE))


# have a look at the data
head(ae_attendances_NEY_wrangled)


###################### #
# CREATE THE GRAPHS ####
###################### #

# Choose the colours for each of the financial years
my_colours <- c(rgb(217, 217, 217, max=255), 
                rgb(141, 180, 226, max=255), 
                rgb(0, 112, 192, max=255))


# create a line graph template
function_line_chart <- function(org_to_use, measure_to_use){

  # create a line graph 
  chart_line <- ggplot(data = ae_attendances_NEY_wrangled %>%
                         # select the Org and Measure defined in the function
                         filter(Org_Name == org_to_use,
                                Measure == measure_to_use),
                       
                       # Choose the month name as the x-axis, Value as the y-axis
                       # Have different lines based on the financial years
                       aes(x = MonthName,
                           y = Value,
                           group = FY,
                           colour = FY)) +
    
    geom_line(size = 1.2) + 
    
    theme_minimal() + 
    
    theme(# add in legend specifications
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size=rel(1.0),
                                     colour = rgb(89,89,89,max = 255)),
          legend.spacing.y = unit(0, 'cm'),
          
          # add in x- and y-axis specifications
          axis.title.y = element_blank(),
          axis.text = element_text(
            #size = 12,
                                   colour = rgb(89,89,89,max = 255)),
          axis.title.x = element_blank(),
          
          # add in title specifications
          plot.title = element_text(hjust = 0.5,
                                    #size = 15,
                                    colour = rgb(89,89,89,max = 255))) + 
    
    ggtitle(paste(org_to_use, ' ', measure_to_use, ': Apr-16 to Mar-19', sep = '')) +
    
    # use the colours specified earlier
    scale_color_manual(values = my_colours) +
    
    # break up large values with a comma for readability
    scale_y_continuous(labels = comma_format())
  
  
  return(chart_line)
  
}



# create a text box template to draw out the max value for the selected measure and organisation
function_text <- function(org_to_use, measure_to_use){
  
  # create a line graph 
  data_filtered <- ae_attendances_NEY_wrangled %>% 
    filter(Org_Name == org_to_use,
           Measure == measure_to_use) 
  
  max_value <- data_filtered %>%
    group_by(Org_Name, Measure) %>%
    summarise(Value = max(Value)) %>%
    ungroup() %>%
    inner_join(data_filtered, by = c('Org_Name', 'Measure', 'Value')) %>%
    dplyr::select(period, MonthName, Value) %>%
    mutate(period = as.Date(period))


  text_for_slide <- paste(
    
                  paste(' ', sep = '\n'),
                  
                  paste('The maximum number of ',measure_to_use,' is ', comma(max_value$Value),
                        '; this was in ', max_value$MonthName, '-', year(max_value$period),'.',
                  sep = ''),
                  
                  sep = '\n')
 
  
  return(text_for_slide)
  
}



# Check the data
function_line_chart('NEY', 'attendances')
function_line_chart('NEY', 'admissions')
function_line_chart('NEY', 'breaches')


############################# #
# CREATE POWERPOINT SLIDES #### 
############################# #


# Read in the PowerPoint template 
this_ppt <- read_pptx(path = paste(file_path, 'ppt_template.pptx', sep = '/')) 

# You can also use the PowerPoint that is included within the package
x <- read_pptx() 

# see what different layouts are possible for your PPT template
layout_summary(this_ppt)
layout_summary(x)


?ph_location_type()
# placeholder type to look for in the slide layout, 
# one of 'body', 'title', 'ctrTitle', 'subTitle', 'dt', 'ftr', 'sldNum'



### Add a title slide 
this_ppt <- add_slide(x = this_ppt,
                      layout = 'Title Slide', 
                      master = 'Office Theme') %>%
  
  # Add a title using ph_with (to add content onto the slide)
  ph_with(value = 'A&E Example PowerPoint',
          location = ph_location_type(type = 'title')) %>%
  
  # Add a subtitle
  ph_with(value = 'Attendances, Breaches and Admissions',
          location = ph_location_type(type = 'subTitle'))



### Insert a new title and content slide with fullsize graph
this_ppt <- add_slide(x = this_ppt, 
                      layout = 'Title and Content', 
                      master = "Office Theme")  %>% 
  
  # add a full size graph
  ph_with(value = function_line_chart('NEY', 'attendances'),
          location = ph_location_fullsize())



### Insert a new title and content slide with graph and title
this_ppt <- add_slide(x = this_ppt, 
                      layout = 'Title and Content', 
                      master = "Office Theme")  %>% 
  
  # add title
  ph_with(value = 'A&E attendances',
          location = ph_location_type(type = 'title')) %>%

  # add graph
  ph_with(value = function_line_chart('NEY', 'attendances'),
          location = ph_location(left = 1,
                                 top = 2.5,
                                 width = 8,
                                 height = 4))



text_format_1 <- fp_text(bold = TRUE, color = rgb(0, 112, 192, max=255), font.size = 16)
text_format_2 <- fp_text(bold = FALSE, color = rgb(128, 128, 128, max=255), font.size = 14)

### Insert a new title and content slide with 3 graphs, a text box and a title
this_ppt <- add_slide(x = this_ppt, 
                      layout = 'Title and Content', 
                      master = "Office Theme")  %>% 
  
  # add title
  ph_with(value = 'Attendances, breaches & admissions',
        location = ph_location_type(type = 'title')) %>%
  
  # add graphs
  ph_with(value = dml(grid.arrange(function_line_chart('NEY', 'attendances'),
                               function_line_chart('NEY', 'breaches'),
                               function_line_chart('NEY', 'admissions'),
                               nrow =  3)), # show 3 graphs one underneath each other
          location = ph_location(left = 1,
                                 top = 1.5,
                                 width = 5,
                                 height = 6)) %>%
  
 
  
    # add text box
  ph_with(value = block_list(fpar(ftext('This slide shows information for North East and Yorkshire.',
                                        text_format_1),
                             ftext(function_text('NEY','attendances'),
                                   text_format_2),
                             ftext(function_text('NEY','breaches'),
                                   text_format_2),
                             ftext(function_text('NEY','admissions'),
                                   text_format_2))),
location = ph_location(left = 6,
                       top = 2,
                       width = 3,
                       height = 4))


### Insert a new title and content slide with 3 graphs, a text box and a title
this_ppt <- add_slide(x = this_ppt, 
                      layout = 'Title and Content', 
                      master = "Office Theme")  %>% 
  
  # add title
  ph_with(value = 'Attendances, breaches & admissions',
          location = ph_location_type(type = 'title')) %>%
  
  # add graphs
  ph_with(value = dml(grid.arrange(function_line_chart('Airedale NHS Foundation Trust', 'attendances'),
                                   function_line_chart('Airedale NHS Foundation Trust', 'breaches'),
                                   function_line_chart('Airedale NHS Foundation Trust', 'admissions'),
                                   nrow =  3)), # show 3 graphs one underneath each other
          location = ph_location(left = 1,
                                 top = 1.5,
                                 width = 5,
                                 height = 6)) %>%
  
  
  
  # add text box
  ph_with(value = block_list(fpar(ftext('This slide shows information for Airedale.',text_format_1),
                                  ftext(function_text('Airedale NHS Foundation Trust','attendances'),text_format_2),
                                  ftext(function_text('Airedale NHS Foundation Trust','breaches'),text_format_2),
                                  ftext(function_text('Airedale NHS Foundation Trust','admissions'),text_format_2))),
          location = ph_location(left = 6,
                                 top = 2,
                                 width = 3,
                                 height = 4))

####################### #
# EXPORTING THE DATA ####
####################### #

print(this_ppt, #what variable to print out
      paste(file_path, 'ppt_output.pptx', sep = '/')) #where to print it to

write.csv(ae_attendances_NEY_wrangled, 
          paste(file_path, 'ae_attendances_NEY_wrangled.csv', sep = '/'), 
          row.names = FALSE) #export the data in a CSV file


