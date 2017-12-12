
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



shinyUI(dashboardPage(
        skin = "black",
        
        
        dashboardHeader(title = "Food Decisions"),
        
        # Sidebar content
        dashboardSidebar(disable = F, width = 350,
                         img(src='recipe2.png', width = '350px'),
                         sidebarMenu(
                                 menuItem("Food", tabName = "Food", 
                                          icon = icon("search", lib = "glyphicon")),
                                 menuItem("Recipe", tabName = "Recipes", 
                                          icon = icon("th-list", lib = "glyphicon"))
                                 # menuItem("Meals", tabName = "Meals",
                                 #          icon = icon("cutlery", lib = "glyphicon")),
                                 # menuItem("24h Plan", tabName = "24hPlan",
                                 #        icon = icon("time", lib = "glyphicon")),
                                 # menuItem("Week Plan", tabName = "WeekPlan",
                                 #          icon = icon("calendar", lib = "glyphicon"))
                         )
                         
                #textAreaInput("textID", "Type in meal description", value = "banana com queijo", resize = "vertical")
        ),
        dashboardBody(
               
                tabItems(
                        #Food section content
                        tabItem(tabName = "Food",
                                fluidRow(
                                       
                                        tabBox(
                                                width = 12, 
                                                title = h2(strong("Explore and compare food items")),
                                                # The id lets us use input$tabset1 on the server to find the current tab
                                                id = "Food", height = "250px",
                                        column(12,        
                                        tabsetPanel(
                                                # Food tab
                                                tabPanel("Food", br(), h3("Compare food items' nutritional composition"),
                                                         br(), 
                                                         selectizeInput("inputID", h4("Select one or more food items to compare"), 
                                                                        choices = unique(nutri_new$Food), multiple = TRUE, selected = NULL,
                                                                        width = 350),
                                                         selectizeInput("nutrientID", h4("Select nutritional component"), 
                                                                        choices = unique(nutri_new$Nutrient), multiple = TRUE,
                                                                        width = 350),
                                                         
                                                         #actionButton("ResetID", "Reset", icon("repeat", lib = "glyphicon")), br(), br(),
                                                         
                                                         #submitButton("Update table", icon("refresh")), br(), br(),
                                                         useShinyjs(),
                                                         actionButton("PlotButton", "Hide/Show Plot",
                                                                      icon = icon("bar-chart"), lib = "font-awesome",
                                                                      style = "color: #fff; background-color: #454140; border-color: #454140"),
                                                         actionButton("TableButton", "Hide/Show Table", 
                                                                      icon = icon("table", lib = "font-awesome"), 
                                                                      style = "color: #fff; background-color: #2E3331; border-color: #2E3331"),
                                                         
                                                         br(), br(),
                                                         dataTableOutput("CompareFood"),
                                                         hr(),
                                                         plotlyOutput("CompareFood_Plot")),
                                                         
                                                
                                                # Nutrient tab
                                                tabPanel("Nutrient", br(), h3("Manipulate nutritional values"), br(),
                                                         box(selectInput("nutChoiceID", 
                                                                        label = "Search food with:",
                                                                        choices = set_names(unique(nutri_new$NutrientID), unique(nutri_new$Nutrient)), 
                                                                        multiple = FALSE,
                                                                        selectize = FALSE,
                                                                        selected = 14),
                                                         uiOutput("intervalControls")),
                                                         box(selectizeInput("nutChoiceID2", label = "Select nutritional component and range values", 
                                                                        choices = set_names(unique(nutri_new$NutrientID), unique(nutri_new$Nutrient)), 
                                                                        multiple = FALSE,
                                                                        selected = character(0),
                                                                        options = list(placeholder = "Select second nutrient")
                                                                        ),
                                                             uiOutput("intervalControls2")),
                                                                        
                                                         dataTableOutput("NutriTable"))
                                        )
                                        ))
                                )),
                                        
                                        
                                        # )
                        #),
                        
                        # Recipes section content
                        tabItem(tabName = "Recipes",
                                
                                fluidRow(
                                        box( 
                                            
                                            collapsible = TRUE, collapsed = FALSE, 
                                            h2(strong("Create and compare recipes"),
                                               align = "center"), 
                                            #img(src='pasta_noodles.jpg', width = '680px', align = 'center'),
                                            hr(),
                                            width = 12,
                                            p(h4(strong("Include the ingredients for the recipe and click on
                                                        'Add ingredient'"))),
                                            p(h4("Visualize different nutritional components and click on 'update nutritional summary'")),
                                            br(), br(),
                                            box(status = "success", width = 12,
                                                selectizeInput("ingredientID",
                                                           label = "1. Select food ingredients",
                                                           #choices = unique(nutri_clean$foodItem),
                                                           choices = set_names(food_wide$FoodID, food_wide$Food),
                                                           multiple = FALSE,
                                                           selected = NULL, 
                                                           options = list(placeholder = 'select one or more ingredients')), br(),
                                                 uiOutput("QuantitySelection"),
                                                selectizeInput("NutrientSub","3. Select nutritional component for the calculation",
                                                               choices = set_names(nutri_wide$NutrientID, nutri_wide$Nutrient), 
                                                               selected = RecipeNutrients, 
                                                               multiple = TRUE, size = 5,
                                                               options = list(placeholder = "Select one or more nutritional components")),
                                                actionButton("AddIngredient", "Add ingredient", icon("grain", lib = "glyphicon"), 
                                                                style = "color: #fff; background-color: #bd5734; border-color: #bd5734"),
                                                actionButton("UpdateNutrient", "Update nutritional summary", icon("refresh", lib = "glyphicon"), 
                                                             style = "color: #fff; background-color: #454140; border-color: #454140")
                                                ),
                                           
                                            br(), 
                                            box(width = 12, dataTableOutput("RecipeTable"),
                                            #actionButton("RemoveIngredient", "Remove selected ingredients", icon("erase", lib = "glyphicon")),
                                            br(), 
                                            #uiOutput("removeRows"),
                                            # conditionalPanel(
                                            #         condition = "output.RecipeTable",
                                                    actionButton("RemoveIngredient", "Remove selected row", icon("erase", lib = "glyphicon"),
                                                                 style = "color: #fff; background-color: #454140; border-color: #454140")),
                                            #),
                                            br(), br(),
                                            # textInput("recipeID", 
                                            #           "Recipe name:", value = "Bolo de iogurte"),
                                            # br(),
                                            # actionButton("SaveTable", "Save recipe", icon("floppy-save", lib = "glyphicon"),
                                            #              style = "color: #fff; background-color: #7a3b2e; border-color: #7a3b2e")),
                                            #uiOutput("load"),
                                            # actionButton("trashID", "Delete recipe", icon("trash", lib = "glyphicon")),
                                            # actionButton("ResetID", "Reset", icon("repeat", lib = "glyphicon")), br(), br(),
                                            verbatimTextOutput('row_selected'),
                                            br(),
                                            hr(),
                                            #p(h4(strong("Get summaries for each nutritional component"))),
                                            br()
                                            
                                        
                                            
                                            # http://rstudio.github.io/DT/shiny.html
                                            # https://community.rstudio.com
                                        
                                            #dataTableOutput("RecipeSum"),
                                            #verbatimTextOutput("Inglist"))
                                ))
                        ),
                        
                        # Meals section content
                        tabItem(tabName = "Meals",
                                fluidRow(
                                        box(title = "Make a meal",
                                            collapsible = TRUE, collapsed = FALSE,
                                            width = 12,
                                            h3(strong("Create and compare meals"),
                                               align = "center"), hr(),
                                            p("Prepare a meal choosing ingredients and recipes"),
                                            textInput("mealID",
                                                      "Give a name to the meal (e.g Lunch)"))
                                )
                        )
                        
                        # 24h Plan section content
                        # tabItem(tabName = "24hPlan",
                        #         fluidRow(
                        #                 box(title = "Make a 24h Plan",
                        #                     collapsible = TRUE, collapsed = FALSE,
                        #                     width = 12,
                        #                     h3(strong("Create and compare 24h Plans"),
                        #                        align = "center"), hr(),
                        #                     p("Prepare a 24h Plan choosing meals, ingredients and recipes"),
                        #                     textInput("dayID",
                        #                               "Give a name and date to the 24h plan"))
                        #         )
                        # ),
                        
                        # Week Plan tab content
                        # tabItem(tabName = "WeekPlan",
                        #         fluidRow(
                        #                 box(title = "Make a Week Plan",
                        #                     collapsible = TRUE, collapsed = FALSE,
                        #                     width = 12,
                        #                     h3(strong("Create and compare Week Plans"),
                        #                        align = "center"), hr(),
                        #                     p("Prepare a Week Plan choosing 24h plans or meals and recipes"),
                        #                     textInput("weekID",  "Give a name and date to the week plan"))
                        #         )
                        # )
                )
        )
)
)
                
