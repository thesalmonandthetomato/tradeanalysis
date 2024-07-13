#' Plot trade data
#' @param plot_type Type of plot, either line plot ('line') or 
#' area plot ('area'). Default is 'line'.
#' @examples
#' plot_trade(reporter =  "ESP",
#'            flow_direction = "export",
#'            partner = "all_countries",
#'            start_date = 2012,
#'            end_date = 2023,
#'            commodity_code = '0702',
#'            plot_type = 'area')
#' 
plot_trade <- function(reporter =  "ESP",
                       flow_direction = "export",
                       partner = "all_countries",
                       start_date = 2012,
                       end_date = 2023,
                       commodity_code = '0702',
                       plot_type = 'line') {
  
  set_primary_comtrade_key('e4f07fff200345e7b0239c259c7fb3f9')
  
  # Lookup codes for a product
  tomato_codes <- ct_commodity_lookup("tomato", return_code = TRUE, return_char = TRUE)
  ES_tomato_2012_2013 <- ct_get_data(
    reporter =  reporter,
    flow_direction = flow_direction,
    partner = partner,
    start_date = start_date,
    end_date = end_date,
    commodity_code = commodity_code
  )
  
  # combine all non-top-15 countries
  ES_tomato_2012_2013 <- ES_tomato_2012_2013[order(ES_tomato_2012_2013$net_wgt, decreasing = TRUE),]
  ES_top15 <- unique(ES_tomato_2012_2013$partner_desc)[1:15]
  ES_summary <- ES_tomato_2012_2013 %>%
    group_by(reporter_iso, partner_desc, ref_year) %>%
    summarise(net_wgt = sum(net_wgt))
  ES_summary_top15 <- subset(ES_summary, partner_desc %in% ES_top15)
  ES_summary_rest <- subset(ES_tomato_2012_2013, !partner_desc %in% ES_top15)
  ES_summary_rest <- ES_summary_rest %>%
    group_by(ref_year) %>%
    summarise(net_wgt = sum(net_wgt))
  ES_summary_rest$partner_desc <- "Other countries"
  ES_summary <- rbind(ES_summary_top15, ES_summary_rest)
  ES_summary$partner_desc <- factor(ES_summary$partner_desc, levels=c(ES_top15, "Other countries"))
  
  if(reporter == "all_countries"){
    reporter_p <- "All countries"
  } else {
    reporter_p <- reporter
  }
  if(partner == "all_countries"){
    partner_p <- "All countries"
  } else {
    partner_p <- partner
  }
  
  # plot
  if(plot_type == 'line'){
    # line plot
    plot <- ggplot(ES_summary, aes(y=net_wgt, 
                                   x=ref_year, 
                                   group=partner_desc, 
                                   text=paste0(partner_desc,": ",format(round(net_wgt,0),big.mark=","), " tonnes"))) +
      geom_line(aes(color=partner_desc)) +
      geom_point(aes(color=partner_desc)) + 
      #theme(legend.position="none") +
      labs(y="Weight (kilograms)", 
           x="Year",
           caption=paste0("Reporter: ", reporter_p, "\nPartner: ", partner_p, "\nCommodity: ", commodity_code, "\nYear(s): ", start_date, " - ", end_date, "\nFlow: ", flow_direction),
           colour="Partner") +
      scale_y_continuous(label=comma) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            plot.caption = element_text(hjust = 0)) + 
      scale_x_continuous(breaks = seq(min(ES_summary$ref_year), max(ES_summary$ref_year), by = 1)) +
      theme(legend.key.size = unit(0.5, "cm"))
  } else {
    # stacked area
    plot <- ggplot(ES_summary, aes(y=net_wgt, 
                                   x=ref_year, 
                                   fill=partner_desc)) + 
      geom_area(alpha=0.6 , linewidth=.5, colour="white") +
      scale_fill_viridis(discrete = T, direction = -1) +
      #theme_ipsum() +
      #theme(legend.position="none") +
      labs(y="Weight (kilograms)", 
           x="Year",
           caption=paste0("Reporter: ", reporter_p, "\nPartner: ", partner_p, "\nCommodity: ", commodity_code, "\nYear(s): ", start_date, " - ", end_date, "\nFlow: ", flow_direction),
           fill="Partner") +
      scale_y_continuous(label=comma) + 
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            plot.caption = element_text(hjust = 0)) + 
      scale_x_continuous(breaks = seq(min(ES_summary$ref_year), max(ES_summary$ref_year), by = 1)) +
      theme(legend.key.size = unit(0.5, "cm"))
  }
  
  return(plot)
  
}
