### Analysis 
# This script defines functions used to perform analysis of Parvidlora sales
# create plots etc.
# please use ggplot2 library

get_period_header <- function(df){
  '
  This function creates a string that can be attached to any title and informs about the period which was analyzed.
  returns string e.g: "from January to March 2020"
  '
  first_month <- month.name[select(df, month) %>% min()]
  last_month <- month.name[select(df, month) %>% max()]
  year <- df %>% select(year) %>% max() %>% as.character()
  period_header <- paste("from", first_month, "to", last_month, year)
  
  return(period_header)
}


save_plot <- function(name) {
  '
  Function that saves the LAST GENERATED plot to output/plots directory
  use this function to safely save the plot (in case the path with "/" fails)
  '
  #TODO Is there an option that would make saved plots scale automatically?
  tryCatch(
    expr = {
      ggsave(paste("output", "plots", name, sep = '/'))
    }, # in case the above doesn't work - use recommended "here" library
    error = function(e){
      saveRDS(df_complete, here::here("output", "plots", name))
      message("File saved using here package, couldn't resolve path with '/'")
    }
  )
  
}

horizontal_bar_stores <- function(df, period, save = TRUE) {
  '
  This function creates the horizontal bar chart of Total Revenue by store sorted best to worse.
  Same plot for count is rather not necessary - counts are interesting on per flower basis
  '
  # create plot regarding total revenue
  # TODO make it more pretty (delete "Parviflora" maybe?) and add some space on the right side 
  plt <- ggplot(df, aes(x = reorder(store_name, rev_total) , y = rev_total)) + 
    geom_bar(stat="identity") + coord_flip() + 
    xlab("Store Name") + ylab("Total Revenue") + ggtitle(paste("Total Revenue of Parviflora stores", period))
  
  # If the argument is TRUE then save the plot, intended to save it inside main script, but don't do it when running .Rmd file
  if (save) {
    save_plot("tot_rev_stores.png")
  }
  
  return(plt)
}

bar_tot_flower_count <- function(df_analysis, period, save = TRUE){
  # selected a specific column range, we might want to replace it with a function so that the chart works with new inputs
  # two tables for both count and revenue, could be easier by making one table with flowers in columns
  df_col_flowers <- df_analysis %>% pivot_longer(6:13, names_to = "flower_types", values_to = "revenue/count")
  df_col_flowers_rev <- df_col_flowers %>% filter(str_detect(flower_types, 'rev')) %>% rename(revenue = `revenue/count`)
  df_col_flowers_cnt <- df_col_flowers %>% filter(str_detect(flower_types, 'count')) %>% rename(flower_count = `revenue/count`)

  # temp
  period <- get_period_header(df_analysis)
  
  plt3 <- ggplot(df_col_flowers_cnt, aes(x = flower_count, y = reorder(flower_types, flower_count))) +
    geom_bar(stat='identity') + xlab("Count") + ylab("Flower Types") + ggtitle(paste("Total Count by Flower", period)) + 
    scale_fill_brewer(palette = "Blues")

  if (save) {
    save_plot("bar_tot_flower_count.png")
  }
  
  return(plt3)
}
  
sep_flow_count <- function(df_analysis, period, save = TRUE){
  'Scatterplot revenue to count by flower'
  df_col_flowers <- df_analysis %>% pivot_longer(6:13, names_to = "flower_types", values_to = "revenue/count")
  df_col_flowers2 <- df_col_flowers %>% separate(flower_types, c("revenue", "count"))
  
  # temp
  period <- get_period_header(df_analysis)
  
  # creating lists for separate flowers and their counts per month
  pqflow <- df_analysis %>% select(month, count_Azalea, count_Begonia, count_Carnation, count_Daffodil)
  azaela <- aggregate(x = pqflow$count_Azalea, by = list(pqflow$month), FUN = sum)
  begonia <- aggregate(x = pqflow$count_Begonia, by = list(pqflow$month), FUN = sum)
  carnation <- aggregate(x = pqflow$count_Carnation, by = list(pqflow$month), FUN = sum)
  daffodil <- aggregate(x = pqflow$count_Daffodil, by = list(pqflow$month), FUN = sum)
  # naming the created columns for easy manipulation
  colnames(azaela) <- c("month", "count")
  colnames(begonia) <- c("month", "count")
  colnames(carnation) <- c("month", "count")
  colnames(daffodil) <- c("month", "count")
  
  # Creation of 4 separate charts for each flower
  # Azalea
  Tot_count_mo_azalea <- ggplot(azaela, aes(x = month, y = count)) +
    geom_bar(stat="identity") + ggtitle(paste("Total count of Azalea flowers", period)) + ylim(0, 30000)
  # Begonia
  Tot_count_mo_begonia <- ggplot(begonia, aes(x = month, y = count)) +
    geom_bar(stat="identity") + ggtitle(paste("Total count of Begonia flowers", period)) + ylim(0, 30000)
  # Carnation
  Tot_count_mo_carnation <- ggplot(carnation, aes(x = month, y = count)) +
    geom_bar(stat="identity") + ggtitle(paste("Total count of Carnation flowers", period)) + ylim(0, 30000)
  # Daffodil
  Tot_count_mo_daffodil <- ggplot(daffodil, aes(x = month, y = count)) +
    geom_bar(stat="identity") + ggtitle(paste("Total count of Daffodil flowers", period)) + ylim(0, 30000)
  
  list(Tot_count_mo_azalea, Tot_count_mo_begonia, Tot_count_mo_carnation, Tot_count_mo_daffodil)
}


horizontal_bar_stores_counts <- function(df_analysis, period, save = TRUE){
  'Amount of flowers sold in total over the three month period. Weirdly, high discrepancy in revenue and counts for Katowice or Rzesz?w'
  
  plt <- ggplot(df_analysis, aes(x = reorder(store_name, count_total) , y = count_total)) + 
    geom_bar(stat="identity") + coord_flip() + 
    xlab("Store Name") + ylab("Total Count") + ggtitle(paste("Total Flower Counts of Parviflora stores", period))
  
  if (save) {
    save_plot("tot_count_stores.png")
  }
  
  return(plt)
}

#===============================================================================================================================


diverging_bar_stores <- function(df, save = TRUE) {
  '
  Function creates Diverging bar chart as shown here: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
  Input:
    -> Parviflora dataframe in its final form
  Output:
    <- png. file with the chart
  '
  
  #preparing data
  df_plot_prep <- df %>%
    dplyr::group_by(store_name) %>% 
    dplyr::summarise(revenue=sum(rev_total)) %>% #grouping by store_name and summarizing with sum of total revenues for the stores
    mutate(revenue_norm = round((revenue - mean(revenue))/sd(revenue), 2)) %>% #calculating normalized revenue equation and rounding to two decimal places
    mutate(flag = case_when(revenue_norm < 0 ~ "below", revenue_norm > 0 ~ "above")) %>% #creating flag column with "above" for stores with higher revenue than average and "below" for lower
    dplyr::arrange(revenue_norm) %>% #sorting by revenue_norm
    dplyr::mutate(store_name = factor(store_name, level=store_name)) #converting store_name to factor, because the plot doesn't sort bars properly without it
  
  
  # Diverging Barcharts
  plt1 <- ggplot(df_plot_prep, aes(x=store_name, y=revenue_norm, label=revenue_norm)) + 
    geom_bar(stat='identity', aes(fill=flag), width=.5)  +
    scale_fill_manual(name="Revenue", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#53AD70", "below"="#FA9391")) + 
    labs(subtitle="Normalized Revenue per Store (0 = average revenue)", 
         title= "Revenues") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  +
    coord_flip()
  
  if (save) {
    save_plot("Normalized_Revenue_per_Store.png")
  }
  
  return(plt1)
}

bar_order_flower <- function(df, save = TRUE) {
  '
  Function first creates data with mean order value for each flower (revenues / count), then plots it
  NIE MAM ZBYTNIO POMYSŁU NA TEN WYKRES, BO NA TĄ CHWILĘ WYGLĄDA ŚREDNIO XD
  Input:
    -> Parviflora dataframe in its final form
  Output:
    <- png. file with the chart
  '
  
  df_order <- df %>%
    dplyr::mutate(Azalea = rev_Azalea/count_Azalea,
                  Begonia = rev_Begonia/count_Begonia,
                  Carnation = rev_Carnation/count_Carnation,
                  Daffodil = rev_Daffodil/count_Daffodil) %>%
    tidyr::pivot_longer(Azalea:Daffodil, names_to = 'flower', values_to = 'order_value')  %>%
    dplyr::group_by(flower) %>%
    dplyr::summarize(mean_order_value = mean(order_value, na.rm = TRUE))

   plt2 <- ggplot(df_order, aes(x = flower, y = mean_order_value)) + geom_bar(stat = "identity")
  
   if (save) {
     save_plot("Mean_Order_per_Flower.png")
   }
  
  return(plt2)
}
#total revenue of stores by month
Kuba_plot <- function(df_analysis, period){
  pltKuba <- df_analysis %>%
    select(store_name, month, year, rev_total) %>% 
    mutate(rev_total / 1000) 
  pltKuba1 <- pltKuba[-c(1,4)]
  pltKuba2 <- aggregate(x = pltKuba1$"rev_total/1000", by = list(pltKuba1$month), FUN = sum)
  colnames(pltKuba2) <- c("month", "revenue")
  
  wykresiq <- ggplot(pltKuba2, aes(x = month, y = revenue)) +
    geom_bar(stat="identity", fill = "#53AD70") + ggtitle(paste("Total Revenue of Parviflora stores in 1000zł"))
  return(wykresiq)
}

bar_flower_month <- function(df, pos = c("rev", "count"), daf = TRUE) {
  '
  Function creates a facet grid of bar charts with flower data for separate month in each plot.
  Inputs:
    -> df - Parviflora data.frame it its final form
    -> pos - whether the bar plot are going to show revenue per flower type (rev) or count of orders of flower type (count)
    -> daf - wheter or not to show Daffodil data, cause in revenue it is a very big outlier and so we may want to show chart without daffodil data
  Output:
    <- ggplot chart
  '
  
  df %>%
    tidyr::pivot_longer(count_Azalea:rev_Daffodil, names_to = 'flower', values_to = 'value') %>%  #pivoting data to make plotting possible
    dplyr::filter(stringr::str_detect(flower, pos)) %>% #filtering to rows with only count or revenue (depending on the pos argument)
    dplyr::filter(!flower == ifelse(daf == FALSE, 'rev_Daffodil', ''))  %>% #filtering out Daffodil data (depending on the daf argument)
    dplyr::mutate(month_new = factor(month, levels = c(min(month):max(month)))) %>% #creating new month column to later arrange months in order
    #dplyr::mutate(daffodil_flag = ifelse(str_detect(flower, '_Daffodil'), 1, 0)) %>% This line adds a flag to the data if we wanted to color only one of the bars (here Daffodils), then it is neccessary to add fill argument to aes in ggplot
    ggplot(aes(x = reorder(flower, -value), y = value)) + geom_bar(stat = "identity") + facet_grid(.~reorder(month.name[month_new], month)) #creating a facet grid
  
  
}