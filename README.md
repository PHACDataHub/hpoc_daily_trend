# HPOC Daily Epi Trend Report

This a tool that leverages R Markdown to automate HPOC's Daily Epi Trend Report 

## Instructions
1. Please clone/download and subsequently extract this repository onto your machine.
2. Ensuring that you have R and R-Studio installed on your machine, please open the file with the .rmd extension.
3. The first time you open the file, R-Studio will prompt you to install all the R pacakges for this progream that are not already installed on your machine. Please proceed with installing them all.
4. Additionally, you will also need to install a couple of additional Python packages. In order to do so, please run the following commands in your R-Studio console: `reticulate::py_install("pandas")` and `reticulate::py_install("xlrd")` 
5. At this stage and for all the subsequent times you open the .rmd file, you can directly proceed to pressing the 'Knit' button. This might take a few minutes and once executed, a new PowerPoint file will be generated in the folder with all the relevant graphs, tables, and narrative.
