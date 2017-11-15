
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

shinyServer(function(input, output, session) {
        
        
        ### Food tab ## Nutrient section
        
        output$intervalControls <- renderUI({
                #tagList(
                        sliderInput("rangeID", h4("Choose values range"),
                                    min = slider$values[1], max = slider$values[2],
                                    value = c(slider$values[2]/4, slider$values[2]/2))
                #)
               })
        
        output$intervalControls2 <- renderUI({
                sliderInput("rangeID2", h4("Choose values range"),
                            min = slider2$values[1], max = slider2$values[2],
                            value = c(slider2$values[2]/4, slider2$values[2]/2))
        })
        
        slider <- reactiveValues(values = NULL)

        observeEvent(input$nutChoiceID, {
               
                nutri_table <- NutriTable()
                # nutri_table <- nutri_new %>%
                #         filter(Nutrient %in% c(input$nutChoiceID)) 
                
                input_max <- range(nutri_table$Value)[2]
                input_min <- range(nutri_table$Value)[1]
                
                slider$values[2] <- input_max 
                slider$values[1] <- input_min
                
                
         
         })
        
        slider2 <- reactiveValues(values = NULL)
        
        observeEvent(input$nutChoiceID2, {
                
                nutri_table <- NutriTable2()
                # nutri_table <- nutri_new %>%
                #         filter(Nutrient %in% c(input$nutChoiceID2)) 
                
                
                input_max <- range(nutri_table$Value)[2]
                input_min <- range(nutri_table$Value)[1]
                
                slider2$values[2] <- input_max 
                slider2$values[1] <- input_min
                
        })
        
        NutriTable <- reactive({
                if(is.null(input$nutChoiceID)){
                        return(NULL)
                } else {
                        
                        nutri_table <- nutri_new %>%
                                filter(Nutrient %in% c(input$nutChoiceID)) 
                }
        })
        
        NutriTable2 <- reactive({
                if(is.null(input$nutChoiceID2)){
                        return(NULL)
                } else {
                        
                nutri_table <- nutri_new %>%
                        filter(Nutrient %in% c(input$nutChoiceID2)) 
                }
        })
        
        output$NutriTable <- renderDataTable({
                
                if(is.null(input$nutChoiceID) && is.null(input$nutChoiceID2)) { 
                        return(NULL) 
                        
                } else if (!is.null(input$nutChoiceID) && !is.null(input$nutChoiceID2)) {
                        
                        nutri_table1 <- NutriTable() %>%
                                #nutri_new %>%
                                #filter(Nutrient %in% c(input$nutChoiceID)) %>%
                                filter(between(Value, input$rangeID[1], input$rangeID[2])) %>%
                                select(Food, Quantity, Nutrient, Value, Unit) %>%
                                unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                                mutate(Nutrient = str_c(Nutrient, ")")) %>%
                                spread(Nutrient, Value)
                        
                        nutri_table2 <- NutriTable2() %>%
                                #nutri_new %>%
                                #filter(Nutrient %in% c(input$nutChoiceID2)) %>%
                                filter(between(Value, input$rangeID2[1], input$rangeID2[2])) %>%
                                select(Food, Quantity, Nutrient, Value, Unit) %>%
                                unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                                mutate(Nutrient = str_c(Nutrient, ")")) %>%
                                spread(Nutrient, Value)
                        
                        nutri_one <- inner_join(nutri_table1, nutri_table2, 
                                                by = c("Food", "Quantity"))
                        
                } else if (is.null(input$nutChoiceID) && !is.null(input$nutChoiceID2)){
                        
                        nutri_table2
                                
                } else {
                        
                        nutri_table1 
                        
                }
                
        })
        
        
        #### Recipes tab
        output$QuantitySelection <- renderUI({
                numericInput("QuantityID",
                             "Change the quantity for each food ingredient", "",
                             min = 0.1, max = 1000, width = '100%')})

        
        input_current <- reactiveValues(ingredients = NULL)
        
        observeEvent(input$AddIngredient, {
                print('update occured')
                input_prev <- session$userData$saveIng
                input_cur <- data_frame(Food = input$ingredientID, Portion = input$QuantityID)
                # NOTE: instead of a bind rows, should join and update the value instead. So that we dont get duplicates
                input_cur <- bind_rows(input_prev, input_cur) %>% distinct(Food, Portion)
                
                session$userData$saveIng <- input_cur
                input_current$ingredients <- input_cur
        })
        
        observeEvent(input$RemoveIngredient, {
                print('delete occured')
                input_prev <- session$userData$saveIng
                sel_rows <- input_prev[input$RecipeTable_rows_selected,]
                #input_cur <- setdiff(input_prev, sel_rows) 
                input_cur <- input_prev %>%
                        distinct(Food, Portion) %>%
                        anti_join(sel_rows)
                
                session$userData$saveIng <- input_cur
                
                input_current$ingredients <- input_cur
        })
        
        observeEvent(input$SaveTable, {
                print('save occured')
                #new_recipe <- input$RecipeTable
        })
        
        nutri_filtered <- reactive({
                if(is.null(input_current$ingredients)) 
                        return(NULL)
                
                nutri_new %>%
                right_join(input_current$ingredients, by = "Food") %>%
                filter(Nutrient %in% isolate(input$NutrientSub)) %>%
                mutate(Quantity = Portion,
                       Value = (Portion * Value)/100) %>%
                select(Food, Quantity, Nutrient, Unit, Value) 
        })
        
        nutri_sum <- reactive({
                if(is.null(nutri_filtered())) 
                        return(NULL)
                
                nutri_filtered() %>%
                group_by(Nutrient, Unit) %>%
                summarise(Total = sum(Value)) %>%
                right_join(nutri_filtered(), by = c("Nutrient", "Unit")) %>%
                select(Food, Quantity, Nutrient, Unit, Value, Total)
        })
        
        output$RecipeTable <- DT::renderDataTable({
                DT::datatable(nutri_sum(), options = list(orderClasses = TRUE))
        })
        

        #### Food tab
        ## Create reactive dataset to compare food ingredients
        nutri_compare <- reactive({
                if(is.null(input$inputID) & is.null(input$nutrientID)){
                        return(NULL)
                }

                 nutri_comp <- nutri_new %>%
                        select(Food, Quantity, Nutrient, Unit, Value) %>%
                        unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                        mutate(Nutrient = str_c(Nutrient, ")")) %>%
                        spread(Nutrient, Value) %>%
                        select(Food, Quantity,
                               str_subset(names(.),
                                          str_c(str_match(input$nutrientID,
                                                          "^[\\w-\\w+\\s]+"),
                                                          collapse = "|")))

        })
        
        values <- reactiveValues(compare_food = NULL)
        
        observeEvent(input$ResetID, {
                print('reset occured')
                
        })
        
        
        ## Output first table to compare food
        output$CompareFood <- renderDataTable(
                if(!is.null(input$inputID) && is.null(input$nutrientID)){
                                nutri_compare() %>%
                                filter(str_detect(Food, str_c(str_match(input$inputID,
                                                                        "^[\\w-\\w+\\s\\[\\w\\]]+"), collapse = "|")))



                                } else if (!is.null(input$nutrientID) && !is.null(input$inputID)) {
                                        nutri_compare() %>%
                                                filter(str_detect(Food, str_c(str_match(input$inputID, "^[\\w-\\w+\\s\\[\\w\\]]+"),
                                                                              collapse = "|")))

                                } else if (!is.null(input$nutrientID) && is.null(input$inputID)){
                                        nutri_compare()

                                } else {
                                        NULL

                                }

                        ,options = list(
                                pageLength = 5
                        )

        )
        
        ## Food section
        ### Food tab
        
        output$CompareFood_Plot <- renderPlotly({
                
                nutri_comp <- nutri_new %>%
                        select(Food, Quantity, Nutrient, Unit, Value) %>%
                        #unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                        #mutate(Nutrient = str_c(Nutrient, ")")) %>%
                        filter(Food %in% input$inputID) %>%
                        filter(Nutrient %in% input$nutrientID)
                
               if(!is.null(input$inputID) && !is.null(input$nutrientID)){ 
               
                       g <- ggplot(data = nutri_comp,
                                aes(x = Food, y = Value, fill = Nutrient)) +
                        geom_bar( 
                                 position=position_dodge(0.9), stat = "identity") +
                        coord_flip() + 
                               facet_wrap(~Unit, scales = "free", ncol = 1) +
                               labs(title = "Nutritional values of selected food items\n", 
                                  x = "", y = "", fill = "") + 
                               scale_fill_ptol() +
                               theme(legend.position = "right", 
                                     plot.title = element_text(vjust=2)) +
                        theme_minimal() 
                       
                        CompareFood_Plot <- plotly_build(g)
                        CompareFood_Plot$elementId <- NULL
                        CompareFood_Plot
               
             # plot_ly(nutri_comp, x = ~Nutrient, y = ~Value, color = ~Food, 
             #         type = "bar", text = ~paste("Food:", Food))
                       
                
                 } else {
                        plotly_empty()
                 }
                
        })

})
