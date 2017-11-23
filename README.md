# nutrient_pt

This is a collaborative project with the aim of tidying and visualising nutritional data. The goal is to develop a simple shiny app to help the user quickly search, compare and visualize nutritional data of single or combined food ingredients. 

In order to help manage the development of the Shiny App a checklist with ideas and tasks is available: `Ideas_list_checklist.md` 

This is how the app looks like:
 
![nutrient_pt app][appimage]

[appimage]: https://github.com/agrou/nutrient_pt/blob/master/www/screeshot_20171113.png?raw=true "Nutrient App - First screenshot"

*******************************************************************************

## Instructions to run the app

### 1. Download the project

- To run the app you need to download this project, open the `nutrient_pt.Rproj` in RStudio. 

- Before running the app you need to create a new subdirectory `data` under `nutrient_app` (`nutrient_app/data`) and populate the  subdirectory with the data that is obtained by running `Tidy_Nutri_Data.Rmd` file (see more instructions below in *2. About the data*).

### 2. About the data

Food composition data is publically available and provided by **Instituto Nacional de Saúde Dr. Ricardo Jorge** (portuguese "National Institute of Health" - INSA): http://portfir.insa.pt/foodcomp/introduction 
**Under INSA's legal requirements, the data should not be used with any commercial intent.**

To **download the data** you can follow these steps:

- Access to "Composição dos alimentos" tab, then select 'Pesquisa de Alimentos' >> 'Download da TCA' 
- Save the data `insa_tca.xlsx` under the already opened project `nutrient_pt/data` and run the code chunks from the file `Tidy_Nutri_Data.Rmd`. This will save the imported, cleaned and transformed datasets into the project `nutrient_pt/data` subdirectory.

### 3. Run the app

After following the previous instructions, open the files `ui.R`, `server.R` and `global.R` in RStudio. You may need to install or load the libraries listed in `global.R` as well. Then you can click on the >Run App button on the top of the screen.  
