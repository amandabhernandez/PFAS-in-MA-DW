### Author: AHz
### Date published: 1/18/2022

library(shiny)

#load data and clean
pfas_ma <- readxl::read_xlsx("Drinking Water.xlsx")
pfas_ma_clean <- pfas_ma %>% 
    filter(`Chemical Name` == c("PERFLUOROHEPTANOIC ACID-PFHPA",
                                "PERFLUORONONANOIC ACID-PFNA",
                                "PERFLUOROHEXANESULFONIC ACID-PFHXS",
                                "PERFLUORODECANOIC ACID - PFDA",
                                "PFAS6",
                                "PERFLUOROHEXANOIC ACID - PFHXA",
                                "PERFLUOROOCTANESULFONIC ACID-PFOS",
                                "PERFLUOROBUTANESULFONIC ACID-PFBS",
                                "PERFLUOROOCTANOIC ACID-PFOA")) %>% 
    mutate(`Chemical Name` = factor(`Chemical Name`, levels = c("PFAS6",
                                                                "PERFLUOROOCTANESULFONIC ACID-PFOS",
                                                                "PERFLUOROOCTANOIC ACID-PFOA",
                                                                "PERFLUOROHEXANESULFONIC ACID-PFHXS",
                                                                "PERFLUORONONANOIC ACID-PFNA",
                                                                "PERFLUOROHEPTANOIC ACID-PFHPA",
                                                                "PERFLUORODECANOIC ACID - PFDA",
                                                                "PERFLUOROHEXANOIC ACID - PFHXA",
                                                                "PERFLUOROBUTANESULFONIC ACID-PFBS"
    ),
    labels = c("Sum of 6 PFAS in Massachusetts DEP Standard",
               "PFOS",
               "PFOA",
               "PFHxS",
               "PFNA",
               "PFHpA",
               "PFDA",
               "PFHxA",
               "PFBS"
    ))) %>% 
    filter(Class == "COM") %>% 
    mutate(result = case_when(Result == "ND" ~ -5,
                              TRUE ~ as.numeric(Result)),
           nd_flag = case_when(Result == "ND" ~ "ND",
                               TRUE ~ "Detect"),
           Result = case_when(Result == "ND" ~ "Not Detected",
                              TRUE ~ paste(Result, UOM)),
           MCL = case_when(`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" ~ 20),
           `Maximum Contaminant Level (MCL)` = case_when(`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" ~ "20 NG/L"),
           year = fct_rev(factor(year(`Collected Date`))),
           Town = factor(Town))

# run shiny server
shinyServer(function(input, output) {
    
    #filter water data based on user input
    dat <- reactive(
        pfas_ma_clean %>% 
            filter(year %in% c(input$year)) %>% 
            filter(`Chemical Name` %in% c(input$chemicals)) %>% 
            mutate(town_select = case_when(Town == input$town ~ "highlight",
                                           TRUE ~ "fade")
            )
    )
    ##### Add HTML files for left hand panel ####
    output$pfas_exp <- renderText({
        return(includeHTML("html/WhatArePFAS.html"))
    })
    output$health_effects <- renderText({
        return(includeHTML("html/healtheffects.html"))
    })
    output$treatment <- renderText({
        return(includeHTML("html/treatment.html"))
    })
    output$community <- renderText({
        return(includeHTML("html/community.html"))
    })
    
    # create dynamic summary text based on town results
    output$summary <- renderText({
        
        # show generic instruction text if user has not set "Town" yet
        if(!input$town %in% unique(levels(pfas_ma_clean$Town))){
            return("View recent PFAS testing by selecting your town in the dropdown box on the right.")
        }
        
        # get town-specific data: most recent test and highest level in that year
        summary_dat <- pfas_ma_clean %>% 
            filter(Town %in% c(input$town)) %>% 
            mutate(year = as.numeric(as.character(year))) %>% 
            group_by(Town, year, `Chemical Name`) %>% 
            summarize(max_result = max(result)) 
        
        #start rules for summary text -- will indicate if highest level is above/below MCL 
        if(max(summary_dat$max_result[which(summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" &
                                            summary_dat$year == max(summary_dat$year))]) > 20)
        {
            return(paste0("The most recent testing in ", unique(str_to_title(summary_dat$Town)),
                          " was conducted in ", max(summary_dat$year), ".",
                          "The state of Massachusetts standard for the sum of 
                           6 PFAS chemicals (PFOS, PFOA, PFHxS, PFNA, PFHpA, and PFDA) is 20 ng/L. The highest level reported in ",
                          max(summary_dat$year), " in your town for the sum of 6 PFAS chemicals is: ", 
                          "<font color=\"#6A5ACD\"><b>",
                          max(summary_dat$max_result[which(summary_dat$year == 
                                                               max(summary_dat$year) & 
                                                               summary_dat$`Chemical Name` == 
                                                               "Sum of 6 PFAS in Massachusetts DEP Standard")]),
                          " ng/L", "</b></font>",". This value <b>exceeds </b>the 
                          Massachusetts standard for the sum of 6 PFAS. ", 
                          "Look at the graphs on the right for more info or call 
                          your local water department."))  
        }
        
        else if(max(summary_dat$max_result[which(summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" &
                                                 summary_dat$year == max(summary_dat$year))]) < 20 & 
                max(summary_dat$max_result[which(summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" &
                                                 summary_dat$year == max(summary_dat$year))]) > 0
        ) 
        {
            return(paste0("The most recent testing in ", unique(str_to_title(summary_dat$Town)),
                          " was conducted in ", max(summary_dat$year), ".",
                          "The state of Massachusetts standard for the sum of 
                          6 PFAS chemicals (PFOS, PFOA, PFHxS, PFNA, PFHpA, and PFDA) is 20 ng/L. The highest level reported in ",
                          max(summary_dat$year), " in your town for the sum of 6 PFAS chemicals is: ", 
                          "<font color=\"#6A5ACD\"><b>",
                          max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year) & 
                                                               summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard")]),
                          " ng/L</b></font>. This value is <b>below</b> the 
                          Massachusetts standard for the sum of 6 PFAS. ", 
                          "Look at the graphs on the right for more info or call your local water department."))  
            
        }
        else if(max(summary_dat$max_result[which(summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" &
                                                 summary_dat$year == max(summary_dat$year))]) < 0 & 
                !is.infinite(max(summary_dat$max_result[which(summary_dat$`Chemical Name` == "Sum of 6 PFAS in Massachusetts DEP Standard" &
                                                              summary_dat$year == max(summary_dat$year))]))) 
        {
            return(paste0("The most recent testing in ", unique(str_to_title(summary_dat$Town)),
                          " was conducted in ", max(summary_dat$year), ".",
                          "The state of Massachusetts standard for the sum of  
                          6 PFAS chemicals (PFOS, PFOA, PFHxS, PFNA, PFHpA, and PFDA) is 20 ng/L. The highest level reported in ",
                          max(summary_dat$year), " in your town for the sum of 6 
                          PFAS chemicals is: <font color=\"#6A5ACD\"><b>Not Detected </b></font>. 
                          This value is <b>below</b> the Massachusetts standard for the sum of 6 PFAS. ", 
                          "Look at the graphs on the right for more info or call your local water department."))  
            
        }
        
        else if(!summary_dat$`Chemical Name` %in% c("Sum of 6 PFAS in Massachusetts DEP Standard")){
            return(paste0("The most recent testing for PFAS in ", unique(str_to_title(summary_dat$Town)),
                          " was conducted in ", max(summary_dat$year), ".",
                          "The public water system in ", unique(str_to_title(summary_dat$Town)),
                          " did not report a value for the sum of 6 PFAS in the Massachusetts DEP standard. ", 
                          "Look at the graphs on the right for more info or call your local water department."))
            
        }
        
        
    })
    
    
    ##### Graphs ####
    output$instructions <- renderText({
        return("<b>Use the dropdown boxes below to select your town, the testing year, 
               and PFAS chemicals of interest.</b><br>")
    })
    
    output$hint <- renderText({
        req(input$town)
        req(input$year)
        req(input$chemicals)
        return(paste0("<br><span style='color: #CD5B45'>Hint: Hover over the graphs below to learn more! All results
                      are shown in nanograms per liter (ng/L). For more info, 
                      see the <a href = ",'#FAQ' ,">FAQ</a></span><br><br>"))
    })
    
    #style inputs 
    # output$town <- renderUI({
    #     selectizeInput("town", "TOWN:", choices = as.list(unique(levels(pfas_ma_clean$Town))),
    #                    #selected = "AYER",
    #                    options = list(placeholder = 'Please select an option below',
    #                                   onInitialize = I('function() { this.setValue(""); }')),
    #                    multiple = FALSE)
    # })
    
    output$town <- renderUI({
        pickerInput("town", "TOWN:", choices = as.list(unique(levels(pfas_ma_clean$Town))), 
                    #selected = "AYER",
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select town"), 
                    multiple = FALSE)
    })
    
    
    output$year <- renderUI({
        pickerInput("year", "YEAR:", choices = as.list(unique(levels(pfas_ma_clean$year))), 
                    #selected = "2021",
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select year(s)"), 
                    multiple = TRUE)
    })
    
    output$chemicals <- renderUI({
        pickerInput("chemicals", "PFAS TO INCLUDE:", choices = as.list(unique(levels(pfas_ma_clean$`Chemical Name`))), 
                    #selected = c("Sum of 6 PFAS in Massachusetts DEP Standard"),
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select PFAS chemical(s)"), 
                    multiple = TRUE)
    })
    
    #download button not currently active, requires Rmd file that has not been set up yet
    observeEvent(input$download, {
        show_alert(title = "This feature is not currently available", text = "Stay tuned!")
    })
    
    
    
    # create interactive plot
    output$dw <- renderPlotly({
        req(input$town)
        req(input$year)
        req(input$chemicals)
        town_dat <- dat()

        pfas_plot <- ggplot(town_dat) + 
            geom_jitter(aes(x = as.factor(year), y = result,
                            color = town_select,
                            alpha = town_select,
                            label = `PWS Name`,
                            label2 = `Collected Date`,
                            label3 = Result),
                        width = 0.25) +
            geom_hline(aes(yintercept =  MCL,
                           label4 = `Maximum Contaminant Level (MCL)`), 
                       color = "#6E8B3D", linetype="dashed") +
            coord_flip() + 
            scale_shape_manual(values=c(16, 1),
                               breaks = c("Detect", "ND"),
                               labels = c("Detect", "Not Detected"), 
                               guide = "none")+
            scale_alpha_manual(values = c(0.5, 1), guide="none") +
            scale_color_manual(values = c("light blue", "#6A5ACD"),
                               breaks = c("fade","highlight"),
                               labels = c("Other towns in Massachusetts",
                                          "My town")) + 
            scale_y_continuous(limits = c(min(pfas_ma_clean$result[which(pfas_ma_clean$year %in% c(input$year))]), 
                                          max(pfas_ma_clean$result[which(pfas_ma_clean$year %in% c(input$year))])))+
            facet_wrap(~`Chemical Name`, ncol = 1, scales = "free_x") + 
            ggthemes::theme_pander() +
            xlab("")+
            ylab("Concentration (ng/L)") + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "snow2"),
              strip.text.x = element_text(color = "#556B2F", face = "bold"),
              text = element_text(family = "Arial"))
        
        #manually fix height of plot  
        if(length(unique(town_dat$`Chemical Name`)) < 2){
            fig <- ggplotly(pfas_plot, height = 300, tooltip = c("label", "label2", "label3", "label4")) 
        }
        else{
            fig <- ggplotly(pfas_plot, height = length(unique(town_dat$`Chemical Name`))*300, tooltip = c("label", "label2", "label3", "label4")) 
        }
        
        #manually fix the legend
        for(i in seq_along(1:length(unique(town_dat$`Chemical Name`)))){
            if(i > 1){
                fig$x$data[[1]]$name <- "Other Towns in MA"
                fig$x$data[[i]]$showlegend <- FALSE
                
                fig$x$data[[i+1]]$name <- paste(str_to_title(input$town))
                fig$x$data[[length(unique(town_dat$`Chemical Name`))+i]]$showlegend <- FALSE
                
            }
            else{
                fig$x$data[[1]]$name <- "Other Towns in MA"
                fig$x$data[[2]]$name <- paste(str_to_title(input$town))
                
                
            }
        }
        
        fig$x$layout$legend$title$text <- ""
        #print figure
        fig 
        
    })
    ## add table of results 
    dat_formatted <- reactive(
        dat() %>% 
            filter(Town == input$town) %>% 
            #filter(!town_select %in% c("fade")) %>% 
            select(-c(result:town_select))
    )
    
    output$dw_table <- DT::renderDataTable(
        dat_formatted() , options = list(pageLength = 25,
                                         autoWidth = TRUE, 
                                         order = list(list(2, 'desc')))
    )
    
    ##### Load HTML text on other pages ####
    output$FAQ_text <- renderUI(
        return(includeHTML("html/FAQ.html"))
    )
    
    output$about <- renderUI(
        return(includeHTML("html/about.html"))
    )
    
})
