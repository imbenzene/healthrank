library(shiny)

load(file = "diagnosis.rda"); #load(file = "counties.rda");
#diagnosis<- as.factor(c(as.character("Search..."), as.matrix(diagnosis)));
#save(diagnosis, file = "diagnosis.rda")

shinyUI(fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
  # Application title
  titlePanel(HTML("<img src='logo.png' width='300' height='85'/>"), 
             windowTitle = "HealthRank"),
  sidebarLayout(
    sidebarPanel(width = 3,                 
      tags$div(HTML("<strong><h4> Step 1 </h4> </strong>") ),                 
      textInput("caption", HTML(" <h6>Where are you located?</h6>"), "5 Times Square, New York, NY"),
      tags$div(HTML("<strong><h4> Step 2 </h4> </strong>") ),                 
      selectInput('variable',HTML(" <h6>Search your diagnosis</h6>"), diagnosis, 
                  selected= diagnosis[1]), 
      tags$div(HTML("<strong><h4> Step 3  </h4> </strong> <h6>Prioritize your ranking criteria <br> 1 = Least Important; 5 = Most Important</h6>") ),                 
      #tags$div(HTML("<strong><h6>1 = Least Important; 5 = Most Important</h6> </strong>") ),
      sliderInput("LOSWeight", label = HTML(" <h7>Length of Stay</h7>"),
                         min = 1, max = 5, value = 3),
      sliderInput("CostWeight", label = HTML(" <h7>Cost</h7>"),
                  min = 1, max = 5, value = 3),
      sliderInput("NumberWeight", label = HTML(" <h7>Number of seen cases</h7>"),
                  min = 1, max = 5, value = 3),
      sliderInput("DistanceWeight", label = HTML(" <h7>Distance</h7>"),
                  min = 1, max = 5, value = 3)
#       ,    
#       selectInput('county',HTML(" <br> <h7>Optional</h7> <br> <h7> Filter by your county</h7>"), counties, 
#                   selected= counties[1]) 
      ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      
      tabsetPanel(
        tabPanel('Table',
                 dataTableOutput(outputId="table"),
                 tags$style(type="text/css", '#myTable tfoot {display:none;}')),
        tabPanel('Map',
                 htmlOutput("mapped")),
        tabPanel('Cost',
                 plotOutput("graphCost")),
        tabPanel('Number of Cases',
                 htmlOutput("graphNumber")),
        tabPanel('About HealthRank', HTML("<h4>      Welcome to HealthRank  </h4>
  <p>     HealthRank is an interactive web application for New York State healthcare consumers to find the best hospital match for their individual health needs.
</p>
                                           <h4>      To Use the Application  </h4>
                                           <p>   Simply input your address and diagnosis on the “Table” view to generate the list of the best hospitals to treat your specific case. You can set the
                                           importance of each of the four ranking criteria (length of stay, cost, number of cases seen, and distance) by adjusting their weightings on a 5-point scale
                                           from “Not Important” to “Very Important.”
                                           </p>
                                           <br/>
                                           <p>
                                           Based on your specifications, each hospital is assigned a score from 0-100%, generating a list of hospitals ranked according to your best match. You also
                                           have the option of sorting the list in ascending or descending order for each of the columns by clicking on the column name.
                                           </p>
                                           <br/>
                                           <p>
                                           For visual displays on how each of the top ranked hospitals compare for the four criteria, select the “Plots” tab. The map tool shows where each hospital
                                           is located geographically with respect to your given location. The bar chart on number of cases seen illustrates which hospitals have experienced the
                                           greatest volume in treating patients with your same diagnosis in the past. The violin plot displays length of stay and cost information at each of the top
                                           matched hospitals, also based on past data.
                                           </p>
                                           
                                           <h4>
                                           Data Source
                                           </h4>
                                           <p>
                                           The criteria used to generate the hospital rankings, as well as the hospital names and list of diagnoses, are based on data made publicly available on
                                           health.data.ny.gov. (Title of dataset:
                                           <em>
                                           <strong>
                                           <a href='https://health.data.ny.gov/Health/Hospital-Inpatient-Discharges-SPARCS-De-Identified/u4ud-w55t'>
                                           Hospital Inpatient Discharges (SPARCS De-Identified): 2012
                                           </a>
                                           </strong>
                                           </em>
                                           <strong>)</strong>
                                           <img src='nyhealth.png' name='Picture 2' border='0' align='left' width='81' height='81' hspace='9'/>
                                           </p>
                                           <p>
                                           The “cost” measure is based on the median charge at the hospital for the user’s diagnosis in 2012. The “length of stay” measure is based on median LOS in days at the hospital for the user’s diagnosis in 2012. The “number of cases seen” represents the actual count of cases for the user’s diagnosis admitted to the hospital in 2012. 
                                           </p>")),
        tabPanel('Contact Us', HTML("
                                   <h4>
                                   About us!
                                   </h4>
<p>
    This web application was developed in Summer 2014 at Weill Cornell Medical College for the NYS Health Innovation Challenge.
</p>
                                   <h4>
    Team Members
</h4>
<p>
    <a target='_blank' href= 'https://twitter.com/imbenzene'>Tushar</a>, Master of Engineering student at Cornell University, concentration in Information Science; Anna Zhu, Master of Health Administration student in the
    Sloan Program at Cornell University; Max Nowicki, 4<sup>th</sup> year medical student at Weill Cornell Medical College.
</p>
<p>
    The three student team members worked under the guidance of Ramin Zabih, Professor of Computer Science at Cornell Tech, and Dr. George Shih, Associate Professor of Clinical Radiology at Weill Cornell Medical College and Associate Attending Radiologist at NewYork-Presbyterian Hospital-Weill Cornell Campus.
</p>
<p>
    For questions or feedback, please email <a href='mailto:info@healthrank.io'>info@healthrank.io</a>.
</p>"))

      
    
  )))
))