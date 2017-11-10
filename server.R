
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Load data
# load("nutri_clean.RData", envir = .GlobalEnv)
# load("nutri_wide.RData", envir = .GlobalEnv)
# load("nutri_long.RData", envir = .GlobalEnv)

shinyServer(function(input, output, session) {
        
        
        ### Food tab ## Nutrient section
        
        output$intervalControls <- renderUI({
                tagList(
                        # radioButtons("sliderChoices", label = h4("Nutritional ideal value:"), 
                        #              choices = list("Above" = 1, "Below" = 2), selected = 1),
                        sliderInput("rangeID", h4(""),
                                     min = 0, 
                                     max = 200,
                                     value = 50)
                )
               })
        
        # http://shiny.leg.ufpr.br/daniel/065-update-input-demo/
        
        slider <- reactiveValues(values = NULL)
        
        observeEvent(input$nutChoiceID, {
                slider$values <- session$userData$saveValue
                
                print('update slider')
                
                updateSliderInput(session, "rangeID", 
                                  label = "something", min = slider$values/2, max = slider$values, value = slider$values/2)
                
        })
        
        output$NutriTable <- renderDataTable({
                # nutrient_wide <- nutri_long %>%
                #         select(Food, Quantity, Nutrient, Unit, Value) %>%
                #         unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                #         mutate(Nutrient = str_c(Nutrient, ")")) %>%
                #         spread(Nutrient, Value) 
                
                NutriTable <- nutri_new %>%
                        filter(str_detect(Nutrient, str_c(str_match(input$nutChoiceID,
                                                                    "^[\\w-\\w+\\s]+"),
                                                          collapse = "|"))) 
                max_value <- range(NutriTable$Value)[2]
                        #NutriTable %>% summarise(min = min(Value), max = max(Value))
                print(max_value)
                
                session$userData$saveValue <- max_value
                
                #updateSliderInput()
                               
                IntervalTable <- NutriTable %>%
                               filter(Value >= input$rangeID) %>%
                                arrange(desc(Value))
                IntervalTable
                
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
        
        output$CompareFood_Plot <- renderPlotly({
                
                nutri_comp <- nutri_new %>%
                        select(Food, Quantity, Nutrient, Unit, Value) %>%
                        #unite(Nutrient, Nutrient, Unit, sep = " (") %>%
                        #mutate(Nutrient = str_c(Nutrient, ")")) %>%
                        filter(Food %in% input$inputID) %>%
                        filter(Nutrient %in% input$nutrientID)
                
               if(!is.null(input$inputID) && !is.null(input$nutrientID)){ 
               
                       g <- ggplot(data = nutri_comp,
                                aes(x = Food, y = Value, fill = as_factor(Nutrient))) +
                        geom_bar(stat = "identity", position = "dodge") +
                        coord_flip() + 
                               facet_wrap(~Unit, scales = "free")
                        #ggthemes::theme_fivethirtyeight()
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
