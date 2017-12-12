
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

#library(profvis)
#profvis({runApp()})

shinyServer(function(input, output, session) {
        
        
        ## 1. Food section >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ### 1.1. Food tab
        
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
        
        #values <- reactiveValues(compare_food = NULL)
        
        # observeEvent(input$ResetID, {
        #         print('reset occured')
        #         # this is not working yet
        # })
        
        
        ## Output first table to compare food
        output$CompareFood <- renderDataTable(
                if(!is.null(input$inputID) && is.null(input$nutrientID)){
                        nutri_compare() %>%
                                filter(str_detect(Food, str_c(str_match(input$inputID,
                                                                        "^[\\w-\\w+\\s\\[\\w\\]]+"), 
                                                              collapse = "|")))
                        
                        
                        
                } else if (!is.null(input$nutrientID) && !is.null(input$inputID)) {
                        nutri_compare() %>%
                                filter(str_detect(Food, str_c(str_match(
                                        input$inputID, "^[\\w-\\w+\\s\\[\\w\\]]+"),
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
        
        ## Output plot to compare food items
        
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
        
        observeEvent(input$PlotButton, {
         toggle("CompareFood_Plot")
        })

        
        observeEvent(input$TableButton, {
                toggle("CompareFood")
        })
        
                 
        
        ## 1.2. Nutrient tab
        
        ## Define sliders
        
        output$intervalControls <- renderUI({
                
                nutri_table <- nutri_choice %>%
                        dplyr::filter(NutrientID %in% c(input$nutChoiceID)) %>%
                        summarise(min = min(Value, na.rm = TRUE), 
                                  max = max(Value, na.rm = TRUE)) %>%
                        select(min, max) %>%
                        unlist()
                
                        sliderInput("rangeID", h4(""),
                                    min = nutri_table[1], 
                                    max = nutri_table[2],
                                    value = c(nutri_table[1], 
                                              nutri_table[2]))
                
               })
        
        
        output$intervalControls2 <- renderUI({
                        
                if(!is.na(as.numeric(input$nutChoiceID2))){
                        
                        nutri_table2 <- nutri_new %>%
                                dplyr::filter(NutrientID %in% c(input$nutChoiceID2)) %>%
                                summarise(min = min(Value, na.rm = TRUE),
                                          max = max(Value, na.rm = TRUE)) %>%
                                select(min, max) %>%
                                unlist()
                        
                        
                        sliderInput("rangeID2", h4(""),
                                    min = nutri_table2[1], max = nutri_table2[2],
                                    value = c(nutri_table2[1], 
                                              nutri_table2[2]))
               } else {
                       NULL
                }
                
        })
        
        ## react to slider values
        #slider <- reactiveValues(values = NULL)

        # observeEvent(input$nutChoiceID, {
        #        
        #         #nutri_table <- NutriTable()
        #         nutri_table <- nutri_new %>%
        #                 dplyr::filter(NutrientID %in% c(input$nutChoiceID)) %>%
        #                 summarise(min = min(Value, na.rm = TRUE), 
        #                           max = max(Value, na.rm = TRUE)) %>%
        #                 select(min, max) %>%
        #                 unlist()
        #         
        #         # input_max <- max(nutri_table$Value, na.rm = TRUE)
        #         # input_min <- min(nutri_table$Value, na.rm = TRUE)
        #         
        #         slider$values[2] <- nutri_table[2] 
        #         slider$values[1] <- nutri_table[1]
        #  })
        
        # slider2 <- reactiveValues(values = NULL)
        # 
        # observeEvent(input$nutChoiceID2, {
        #         
        #         #nutri_table <- NutriTable2()
        #         nutri_table2 <- nutri_new %>%
        #                 dplyr::filter(NutrientID %in% c(input$nutChoiceID2)) %>%
        #                 summarise(min = min(Value, na.rm = TRUE), 
        #                           max = max(Value, na.rm = TRUE)) %>%
        #                 select(min, max) %>%
        #                 unlist()
        #         
        #         
        #         # input_max <- max(nutri_table2$Value, na.rm = TRUE)
        #         # input_min <- min(nutri_table2$Value, na.rm = TRUE)
        #         
        #         slider2$values[2] <- nutri_table2[2] 
        #         slider2$values[1] <- nutri_table2[1]
        #         
        # })
        
        
        ## Don't show the same nutrient in the second nutrient input

        observe({
                input_nut <- input$nutChoiceID

                        second_nut <- nutri_new %>%
                                select(NutrientID, Nutrient) %>%
                                dplyr::filter(NutrientID != input_nut) %>%
                                arrange(NutrientID)
                        
        
                updateSelectizeInput(session, "nutChoiceID2",
                                label = "Select second nutritional component:",
                                choices = set_names(nutri_wide$NutrientID, 
                                                    nutri_wide$Nutrient),
                                selected = character(0)
                                )
        })
        
        
        ## Output table for sorting foods based on nutrient selection
        
        # Correcting unexpected empty input values to avoid errors: 
        # https://shiny.rstudio.com/articles/req.html
        
        comp_one <- reactive({
                req(input$nutChoiceID, input$rangeID)
                
                
                if (isTruthy(input$nutChoiceID) &
                    !isTruthy(input$nutChoiceID2)) {
                        nutri_choice %>%
                                select(FoodID, Food, NutrientID, Nutrient, 
                                       Value, Quantity) %>%
                                dplyr::filter((NutrientID %in% c(input$nutChoiceID) & 
                                                       between(Value, input$rangeID[1], 
                                                               input$rangeID[2]))) %>%
                                select(-FoodID, -NutrientID)
                        
                } else {
                        req(input$nutChoiceID2, input$rangeID2) 
                        
                        nutri_choice %>%
                                select(FoodID, Food, NutrientID, Nutrient, 
                                       Value, Quantity) %>%
                                dplyr::filter(NutrientID %in% c(input$nutChoiceID) & 
                                                      between(Value, input$rangeID[1], 
                                                              input$rangeID[2]) |
                                                      (NutrientID %in% c(input$nutChoiceID2) 
                                                       & between(Value, input$rangeID2[1], 
                                                                 input$rangeID2[2]))) %>%
                                group_by(FoodID) %>%
                                dplyr::filter(n() %in% 2) %>%
                                ungroup() %>%
                                select(-FoodID, -NutrientID)
                }
        })
        
       
       
        
        output$NutriTable <- renderDataTable({
                if(nrow(comp_one()) == 0){
                        comp_one()
                
                } else {
                DT::datatable({
                         comp_one() %>%
                                spread(Nutrient, Value)
                        })      
                        } 
                })
                
        
        
        #### Recipes  section >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
        ##  Define quantity input
        output$QuantitySelection <- renderUI({
                numericInput("QuantityID",
                             "2. Select the quantity for each food ingredient", "",
                             min = 0.1, max = 1000, width = '100%')})

        ## make input reactive to user interactions
        input_current <- reactiveValues(ingredients = NULL, 
                                        nutrients = RecipeNutrients)
        
        # Button to add ingredients to the table
        observeEvent(input$AddIngredient, {
                req(input$AddIngredient)
                
                #print('update occured')
                input_prev <- session$userData$saveIng
                input_cur <- tibble(FoodID = as.numeric(input$ingredientID), 
                                        Portion = as.numeric(input$QuantityID))
                input_cur <- bind_rows(input_prev, input_cur) %>% 
                        distinct(FoodID, Portion)
                
                session$userData$saveIng <- input_cur
                input_current$ingredients <- input_cur
        })
       # Button to update nutrients shown in the table
        observeEvent({input$UpdateNutrient}, {
                if(!isTruthy(input$NutrientSub)) {
                        input_current$nutrients <- RecipeNutrients
                } else {
                        input_current$nutrients <- input$NutrientSub
                }
        })
        ## Button for deleting selected rows
        observeEvent(input$RemoveIngredient, {
                
                print('delete occured')
                
                #input_prev <- session$userData$saveIng
                #sel_rows <- input_prev %>% filter(row_number() %in% input$RecipeTable_rows_selected)
                 
                nutri_cur <- nutri_filtered()
                
                input_cur <- nutri_cur %>%
                        filter(!(row_number() %in% input$RecipeTable_rows_selected)) %>%
                        rename(Portion = Quantity) %>%
                        dplyr::distinct(FoodID, Portion) #%>%
                        #anti_join(sel_rows) 
                
                session$userData$saveIng <- input_cur
                
                input_current$ingredients <- input_cur
        })
        ## Save button to save user input - save recipe
        # observeEvent(input$SaveTable, {
        #         print('save occured')
        #         #new_recipe <- input$RecipeTable
        # })

        
        ## Create a reactive dataset dependent on the ingredients input
        nutri_filtered <- reactive({
                
                if(!isTruthy(input_current$ingredients)){
                        return(NULL)
                } else {
                recipe <- nutri_choice %>%
                                right_join(input_current$ingredients, by = "FoodID") %>%
                                filter(NutrientID %in% input_current$nutrients) %>%
                                mutate(Quantity = Portion,
                                Value = (Portion * Value)/100) %>%
                                select(FoodID, Food, Quantity, Nutrient, Value) }
                
                
                if (nrow(recipe > 0)){
                        
                sum <- recipe %>%
                        #filter(!(FoodID == totalFoodID)) %>%
                        group_by(Nutrient) %>%
                        summarise(Total = sum(Value)) %>%
                        ungroup() %>%
                        mutate(Food = "Total") %>%
                        rename(Value = Total) 
                        
        recipe_total <- recipe %>%
                  bind_rows(sum) %>%
                  #select(Food, Quantity, Nutrient, Unit, Value, Total) #%>%
                  spread(Nutrient, Value) #%>%
                  #select(-FoodID)
                
               
                        
               return(recipe_total)
        
                } else {
                        return(NULL)
                }
                
        })
        
        ## Output the recipes dataset 
        output$RecipeTable <- DT::renderDataTable({
                
                if(isTruthy(nutri_filtered())){
                        d <- nutri_filtered()
                        
                        DT::datatable(d, options = list(orderClasses = TRUE))
                        
                        } else {
                                return(NULL)
                }
                
        })
        
        # Show remove ingredient button only after table output
        output$RemoveIngredientUi <- renderUI({
                if(isTruthy(nrow(nutri_filtered()))){
                        actionButton("RemoveIngredient", "Remove selected row", icon("erase", lib = "glyphicon"),
                                     style = "color: #fff; background-color: #454140; border-color: #454140")
                } else {
                        return(NULL)
                }
        })
})
