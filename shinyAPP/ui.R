library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Salt Reduction Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("current_salt", 
                   label = h5("Current salt intake (g/d)"), 
                   value = 9.6),
      numericInput("reference_salt", 
                   label = h5("Reference salt intake (g/d)"), 
                   value = 5),
      numericInput("year_end", 
                   label = h5("Ending year of simulation (up to 2049)"), 
                   value = 2016),
      numericInput("cost_ht_treatment", 
                   label = h5("Cost of yearly treatment of hypertension (zl)"), 
                   value = 623.28),
      numericInput("interest_rate", 
                   label = h5("Interest rate (%)"), 
                   value = 0.03),
      numericInput("hypertension_treated_eff", 
                   label = h5("Rate of effective hypertension treatment (%)"), 
                   value = 0.26),
      numericInput("hypertension_treated_all", 
                   label = h5("Rate of all hypertension treatment (%)"), 
                   value = 0.62),
      numericInput("working_days", 
                   label = h5("Number of working days"), 
                   value = 251),
      numericInput("value_year_work",
                   label = h5("Average value of 1-year work (zl)"), 
                   value = 3650*12),
      numericInput("campaign_cost1",
                   label = h5("Cost of salt-reduction campaign - first 5 years (zl)"), 
                   value = 5*10^7),  
      numericInput("campaign_cost2",
                   label = h5("Cost of salt-reduction campaign - subsequent years (zl)"), 
                   value = 2.5*10^7)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("df"),
      p("Opis zmienych:"),
      p("mean_death - srednia roczna liczba zgonow uniknietych w wyniku obnizenia spozycia soli"),
      p("total_death - sumaryczna liczba zgonow uniknietych w wyniku obnizenia spozycia soli w ciagu modelowanego okresu"),
      p("total_cost_direct - zdyskontowana oszczednosc w wydatkach na leczenie szpitalne z powodu chorob ukladu krazenia "),
      p("total_qaly - zdyskontowana liczba lat w pelnym zdrowiu uzyskana dzieki redukcji spozycia soli"),
      p("total_cost_public - calkowite oszczednosci budzetowe wynikajace z redukcji spozycia soli (leczenie szpitalne+ZUS)" ),
      p("total_cost_indirect - calkowite oszczednosci spoleczne (abencja choroba + niezdolnosc do pracy)" ),
      p("total_campaign_cost - przewidywane koszty programu redukcji spozycia soli")
      )
  )
))