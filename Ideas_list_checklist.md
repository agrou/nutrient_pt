# Ideas list:

### General:

- [x] Add sections to the dashboard and add icons 
- [x] Correct for filtering Energia - It's matching both [kcal] and [kJ]
- [ ] Correct conditional UIs  
- [ ] Creating factors of variables (Food name + Food id number; Nutrient name + Nutrient id number)


### Sections:

**1. Food section** 

  **1.1 Food tab**
  
- [x] Compare Food by Nutrient
- [x] Add a graph visualization to find and compare food items 
- [x] Correct graph display (1 column and bars together)
- [x] Correct labels in the plot
- [ ] Add radio button to visualize table and/or plot
- [ ] Add radio button to visualize graph faceting by unit or by nutrient+unit
- [ ] Add radio buttons to compare by 100g or by portion size (household measures)
- [ ] Add food groups selection to the selectize.js button 
- [ ] Add option of only showing information for macronutrients. Calculate the convertion of each macronutrient into energy with percentage of each macronutrient for the total energetic value of each food item.

  **1.2. Nutrient tab**
  
Objective: get lists of food items per nutrient selection: Nutrient quantity in grams and percentage of energy of each macronutrient 
- [ ] Correct slider updated range numeric labels (getting value from previous and not current input) 
- [ ] Add button to select by FoodGroup 
- [ ] Don't show FoodID variable
- [ ] Enable changing the quantity of food items (portion sizes) - later

**2. Recipe section**

- [x] Create new recipe with food ingredient input and dynamic quantities changing the nutritional values 
- [x] Change one food ingredient's quantity at a time and add to the same output table 
- [x] Correct for quantity input changes in all. input should only change current session not previous sessions
- [ ] Get button Save recipe working 
- [ ] Get button delete rows to work
- [ ] Select recipes already created and activate delete recipe button
- [ ] Enable changing only some ingredients and quantities in a specific recipe
- [ ] Get statistical summaries for each recipe: Add progressive bars to recipe table or graph visualization for each nutrient 


**3. Meal section**

- [ ] Start by setting up nutritional values and matching those (e.g. max carbohydrates for that specific meal, in absolute numbers and percentage)
- [ ] Select ingredients and recipes for the meal
- [ ] Summaries of the data
- [ ] Compare meals option


### Later

- [ ] Complete other sections
- Apply UI design - http://appsilon.github.io/shiny.semantic - App interface with modern look
- [ ] Add wide and long format view of the tables
- [ ] Add option to upload an image for a recipe and meal
- [ ] Filters for list of preferences/allergies
- [ ] Add network or other graph visualisations representing frequency and variability of choices 
