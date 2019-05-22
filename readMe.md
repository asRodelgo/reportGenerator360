# Generate TCdata360 Thematic Reports 

## Basic instructions on how to generate 2 pager PDF reports based on TCdata360 data. 

## Pre-requisites:

- Install the R packages listed in: **global_utils.R** plus **jsonlite** to process the TCdata360's API responses.
- TexLive installation (To be able to knit LaTeX code into PDF): <https://www.tug.org/texlive/quickinstall.html>

## Setup

1. This is meant to be run on a local environment, for now. So download this project and change accordingly all local paths in:

- **Writer_Report_data.R**
- **Report_Generator.R**
- **datapull_TCdata360.R**

2. The **templates** folder contains the custom report for instance, Entrepreneurship. Ideally you don't need to change anything outside of this **templates** folder. The parameter **input_reportID** defines what report you are working with. To edit an existing report or to create a new one, you must create 4 files within the templates folder:
  
  - **input_reportID_charts.R** calls to functions in helper_functions.R (charts, tables, etc.)
- **input_reportID_DataDescription.csv** tell the report what indicators, units, sections, sources you will use. In more detail:
  
  ***Section**: Main topics within the report
***Subsection**: group tag indicators. Example: Use the same name for indicators you want to appear in the same chart/table. 
***Subsection2**: in case indicators are used in more than 1 place. In case indicators are used in 3 places, then a new column would need to be added (Subsection3), although I'm not sure this will work without further changes. The rest of the columns should be self explanatory
***tcdata360_id**: TCdata360 indicator ID per the API.
***Source_Name**: Name of data source to appear on report footnotes. Ex: WDI 
***Source_Link**: URL pointing to data source (as hyperlink to Source_Name)

- **input_reportID_ReportConfiguration.csv** configuration file needed for the overall attributes of each report. Details:

***SectionID**: Identify main sections in the report. The bigger font titles. Section 9 is a place holder for the main header banner and color styles.
***Order**: Identify secondary sections within each main sectionID. Order 9 in sectionID 9 defines the color for the report title and header banner and main titles. Banner will look for a file **header_background_[hexacolor].png** in folder www. You can create this file by editing the file templates/Powerpoint_styles.pptx. Pick a header, a color (hexacolor) and save it as PNG file into www folder. Make sure hexacolor in the .png filename is the same as in the color column corresponding to SectionID = 9. 
Order 10 defines the text color in the report.
***Section_Level**: Main sections = 1, secondary sections = 2
***Section**: Section title. For sections without subsection title, you must create a new row and leave this column empty. 
***Section_Description**: For Section_Level = 1, a short description in smaller font to accompany the main title of the section. For Section_Level = 2, this is the footnote, normally corresponding to the data source.

- **input_reportID_PDF_LaTeX.Rnw** layout of the report. Here you will layout the elements you described on **_charts.R** 

If you get lost please take a look at the 2 examples built in the project right now (Entrepreneurship and Tourism). This should help.

## Steps to generate PDF reports:

0. Create a new folder inside /templates/ where the newly generated PDFs will go. With the format: **/input_reportID_final_pdf/**

1. Run **Writer_Report_data.R** to pull and store the data from TCdata360's API. Important: If you change the downloaded data file location, it is usually a good practice to keep it outside the project directory (especially if you keep versions on Github, large files will make it crash)

2. The program is prepared to ingest data from other sources as well, you will have to define it in a similar way as in **Entrepreneurship_extraData.R**
  
  3. Update the List topics on global_utils.R:
  topics <- c("Entrepreneurship","Tourism",**"my_new_report"**)

4. Run **Report_Generator.R**. You can run it for 1 country or loop through all of them or through your custom list. 

**NOTE**: You will most likely run several iterations of your report until you get it right. **Every time you update Data_Description.csv or Report_Configuration.csv files you need to re-run datapull_TCdata360.R**. Not all the loops in it, you can save time by just running the topic for your report. 


