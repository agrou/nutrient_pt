
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



shinyUI(dashboardPage(
        skin = "black",
        
        
        dashboardHeader(title = "Food Decisions"),
        
        # Sidebar content
        dashboardSidebar(disable = F,
                         img(src='pen_paper.jpg', width = '230px'),
                         sidebarMenu(
                                 menuItem("Food", tabName = "Food", 
                                          icon = icon("search", lib = "glyphicon")),
                                 menuItem("Recipe", tabName = "Recipes", 
                                          icon = icon("th-list", lib = "glyphicon")),
                                 menuItem("Meals", tabName = "Meals",
                                          icon = icon("cutlery", lib = "glyphicon")),
                                 menuItem("24h Plan", tabName = "24hPlan",
                                        icon = icon("time", lib = "glyphicon")),
                                 menuItem("Week Plan", tabName = "WeekPlan",
                                          icon = icon("calendar", lib = "glyphicon"))
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
                                                
                                                # Food tab
                                                tabPanel("Food", h3("Compare food items' nutritional composition"),
                                                         br(), 
                                                         selectizeInput("inputID", h4("Select one or more food items to compare"), 
                                                                        choices = unique(nutri_new$Food), multiple = TRUE, selected = NULL,
                                                                        width = 350),
                                                         selectizeInput("nutrientID", h4("Select nutritional component"), 
                                                                        choices = unique(nutri_new$Nutrient), multiple = TRUE,
                                                                        width = 350),
                                                         
                                                         actionButton("ResetID", "Reset", icon("repeat", lib = "glyphicon")), br(), br(),
                                                         
                                                         #submitButton("Update table", icon("refresh")), br(), br(),
                                                         dataTableOutput("CompareFood"),
                                                         plotlyOutput("CompareFood_Plot")),
                                                         
                                                
                                                # Nutrient tab
                                                tabPanel("Nutrient", h3("Get a list of food items by manipulating nutritional values"),
                                                         box(selectizeInput("nutChoiceID", h4("Select nutritional component"), br(),
                                                                        choices = unique(nutri_new$Nutrient), multiple = FALSE),
                                                         uiOutput("intervalControls")),
                                                         box(selectizeInput("nutChoiceID2", h4("Select nutritional component"), br(),
                                                                        choices = unique(nutri_new$Nutrient), multiple = FALSE),
                                                             uiOutput("intervalControls2")),
                                                                        
                                                         dataTableOutput("NutriTable"))
                                        )
                                        )
                                ),
                                        
                                        
                                        # )
                        #),
                        
                        # Recipes section content
                        tabItem(tabName = "Recipes",
                                
                                fluidRow(
                                        box( 
                                            
                                            collapsible = TRUE, collapsed = FALSE, 
                                            h3(strong("Create and compare recipes"),
                                               align = "center"), 
                                            #img(src='pasta_noodles.jpg', width = '680px', align = 'center'),
                                            hr(),
                                            width = 12,
                                            p(h4(strong("Include all the ingredients you need for the recipe"))),
                                            br(),
                                           
                                            selectizeInput("ingredientID",
                                                           label = "Select food ingredients",
                                                           #choices = unique(nutri_clean$foodItem),
                                                           choices = unique(nutri_new$Food),
                                                           multiple = FALSE, width = '100%', selected = NULL, 
                                                           options = list(placeholder = 'select one or more ingredients')),
                                            uiOutput("QuantitySelection"),
                                           
                                            # numericInput("QuantityID",
                                            #              "Change the quantity for each food ingredient", "",
                                            #              min = 0.1, max = 1000, width = '100%'),
                                            selectizeInput("NutrientSub","Select nutrients for the calculation",
                                                           unique(nutri_clean$Nutrient), 
                                                           selected = "Energia [kcal] (ENERCC)",
                                                           multiple = FALSE, size = 5),
                                            
                                            #submitButton("Update table", icon("refresh", lib = "glyphicon")), br(),
                                            
                                            actionButton("AddIngredient", "Update table", icon("refresh")),
                                            actionButton("RemoveIngredient", "Delete rows", icon("erase", lib = "glyphicon")),
                                            br(), 
                                            dataTableOutput("RecipeTable"),
                                            br(), br(),
                                            textInput("recipeID", 
                                                      "Save recipe as:", value = "Bolo de iogurte"),
                                           
                                            actionButton("SaveTable", "Save recipe", icon("floppy-save", lib = "glyphicon")),
                                            uiOutput("load"),
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
                        ),
                        
                        # 24h Plan section content
                        tabItem(tabName = "24hPlan",
                                fluidRow(
                                        box(title = "Make a 24h Plan",
                                            collapsible = TRUE, collapsed = FALSE,
                                            width = 12,
                                            h3(strong("Create and compare 24h Plans"),
                                               align = "center"), hr(),
                                            p("Prepare a 24h Plan choosing meals, ingredients and recipes"),
                                            textInput("dayID",
                                                      "Give a name and date to the 24h plan"))
                                )
                        ),
                        
                        # Week Plan tab content
                        tabItem(tabName = "WeekPlan",
                                fluidRow(
                                        box(title = "Make a Week Plan",
                                            collapsible = TRUE, collapsed = FALSE,
                                            width = 12,
                                            h3(strong("Create and compare Week Plans"),
                                               align = "center"), hr(),
                                            p("Prepare a Week Plan choosing 24h plans or meals and recipes"),
                                            textInput("weekID",  "Give a name and date to the week plan"))
                                )
                        )
                )
        )
)
)
                
