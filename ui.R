source("screen_detector_script.R") # Loads a script to adjust UI based on the screen size

ui <- fluidPage(
  
  # Add language attribute to HTML
  tags$html(lang = "en"),
  

  
  useShinyjs(),
  theme = shinytheme("cerulean"),
  includeCSS("www/tephi_ww_dashboard.css"),  # Include your existing CSS file
  
  tags$head(
    
    # Add page title
    tags$title("SeqBoard - Real-time Virus Monitoring for Texas"),
    
    #Java for mobile device detection
    tags$script(HTML("
      function isMobileDevice() {
        return (typeof window.orientation !== 'undefined') || (navigator.userAgent.indexOf('IEMobile') !== -1);
      }
      
      $(document).ready(function() {
        if (isMobileDevice()) {
          $('#desktop-content').hide();
          $('#mobile-content').show();
        } else {
          $('#desktop-content').show();
          $('#mobile-content').hide();
        }
      });
    ")),
    #Java for video guide window
    tags$script(HTML('
    $(document).ready(function() {
      $("#toggleVideoBtn").on("click", function() {
        $("#videoContainer").slideToggle();
        var $btn = $(this);
        var isVisible = $("#videoContainer").is(":visible");
        $btn.text(isVisible ? "Hide Video Guide" : "Show Video Guide");
        if (!isVisible) {
          $("#videoContainer video")[0].pause();
        }
      });
      
      $(window).on("resize", function() {
        var video = $("#videoContainer video");
        if (video.length) {
          video.css("max-width", "100%");
        }
      });
    });
  ')),
    
    
    
    
    
    
    # CSS style
    tags$style(HTML("
      body, p, h1, h2, h3, h4, h5, h6 {
        color: black !important;
      }
      .full-width-plot .plotly {
        width: 100% !important;
      }
      .plot-description {
        margin-top: 20px;
        margin-bottom: 20px;
        padding: 15px;
        background-color: #f8f9fa;
        border-radius: 5px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        line-height: 1.5;
      }
      .rpkmf-explanation {
        margin-top: 10px;
        font-style: italic;
        line-height: 1.5;
      }
      .shiny-notification {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        background-color: #ffd700;
        color: #000000;
        font-size: 24px;
        font-weight: bold;
        text-align: center;
        padding: 20px;
        z-index: 9999;
      }
      #videoContainer {
        display: none;
      }
      .video-container {
        position: relative;
        padding-bottom: 18.75%;
        height: 0;
        overflow: hidden;
      }
      .video-container video {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
      }
      .species-box {
        background-color: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
        font-size: 0.9em;
      }
      .species-title {
        font-weight: bold;
        margin-bottom: 5px;
      }
      h5 {
        font-weight: bold;
      }
      .species-box.high {
        background-color: #FFDAB9;
        border: 1px solid #FFB74D;
      }
      .species-box.medium {
        background-color: #FFFFE0;
        border: 1px solid #FFF59D;
      }
      .species-box.low {
        background-color: #FFFFFF;
        border: 1px solid #E0E0E0;
      }
      .species-level {
        display: inline-block;
        padding: 2px 6px;
        border-radius: 3px;
        font-weight: bold;
        margin-bottom: 5px;
      }
      .species-description {
        font-size: 0.85em;
        color: #333;
      }
      .overview-column {
        padding-right: 10px;
      }
      .main-content-column {
        padding-left: 10px;
      }
      .section-separator {
        margin-top: 50px;
        margin-bottom: 50px;
        border-top: 2px solid #e0e0e0;
        width: 100%;
      }
      #map {
        height: 600px !important;
        margin-top: 30px;
      }
      .map-container {
        margin-top: 50px;
      }
      .reactable-table {
        font-family: 'Oswald', sans-serif;
      }
      .disclaimer-list {
        padding-left: 20px;
      }
      .disclaimer-item {
        margin-bottom: 15px;
      }
      .methods-image-container {
        text-align: center;
        margin-bottom: 20px;
      }
      .methods-image {
        max-width: 60%;
        height: auto;
        max-height: 300px;
        object-fit: contain;
      }
      @media (max-width: 768px) {
        .methods-image {
          max-width: 60%;
        }
      }
      .visually-hidden {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        margin: -1px;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border: 0;
      }
      #mobile-content {
    padding-top: 50px; /* Space for fixed top menu */
    margin-bottom: 0; /* Remove bottom margin */
  }

  #top-menu {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    background-color: #f8f9fa;
    display: flex;
    justify-content: space-around;
    border-bottom: 1px solid #dee2e6;
    z-index: 1000;
  }

  #top-menu a {
    flex: 1;
    text-align: center;
    padding: 15px;
    text-decoration: none;
    color: #007bff;
    border-right: 1px solid #dee2e6;
  }

  #top-menu a:last-child {
    border-right: none;
  }

  #top-menu a:hover,
  #top-menu a.active {
    background-color: #e9ecef;
  }

  .tab-content {
    padding: 15px;
    margin-bottom: 0;
  }

  .tab-content > .tab-pane {
    display: none;
    margin-bottom: 0;
  }

  .tab-content > .active {
    display: block;
  }

    #footer-area,
  #bottom-menu {
    display: none;
  }
    #bottom-menu a {
      flex: 1;
      text-align: center;
      padding: 10px;
      text-decoration: none;
      color: #007bff;
    }
    #bottom-menu a:hover, #bottom-menu a.active {
      background-color: #e9ecef;
    }
      #COVID .covid-content {
      padding: 15px;
      background-color: #ffffff;
    }
    #COVID .plot-container {
      margin-bottom: 20px;
      background-color: #ffffff; /* Changed from #f8f9fa to white */
      border-radius: 8px;
      padding: 0; /* Removed padding */
    }
    #COVID .plot-container h4 {
      margin: 0 0 10px 0; /* Adjusted margins */
      padding: 10px 15px; /* Added padding to the title itself */
      color: #333;
      font-size: 16px; /* Slightly reduced font size */
      background-color: #f8f9fa; /* Light grey background for just the title */
      border-radius: 8px 8px 0 0; /* Rounded corners only at the top */
    }
    #COVID .plot-wrapper {
      width: 100%;
      overflow: hidden;
      padding: 0 15px; /* Added padding to the sides of the plot */
    }
    #COVID .plot-description {
      font-size: 14px;
      line-height: 1.5;
      color: #333;
      background-color: #f8f9fa;
      border-radius: 8px;
      padding: 15px;
      margin-top: 15px; /* Added margin to separate from plot */
    }
    /* Make tabs larger and more prominent */
  .nav-tabs > li > a {
  font-size: 16px;  /* Increase font size */
  padding: 15px 20px;  /* Add more padding around tab text */
  font-weight: 500;  /* Make font slightly bolder */
  }

  /* Style for active tab */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  font-weight: 600;  /* Make active tab bolder */
  border-top: 3px solid #007bff;  /* Add a thicker top border for active tab */
}

/* Style for hover state */
.nav-tabs > li > a:hover {
  background-color: #f8f9fa;  /* Light background on hover */
}

.box .box-body p {
  line-height: 1.5;
  margin-top: 15px;
  margin-bottom: 15px;
}

.method-description,
.method-citation {
  line-height: 1.5;
  margin-bottom: 1em;
}
.faq-link {
  line-height: 1.5;
  margin-top: 1.5em;
  margin-bottom: 1.5em;
}

.faq-link a {
  font-size: 70px;  /* Makes link text larger */
  line-height: 1.5;
  padding: 5px 0;   /* Adds some vertical padding around the link */
  display: inline-block;  /* Helps maintain the line height */
}

.faq-link a:hover {
  text-decoration: none;
  opacity: 0.8;
}
    
    "))
  ),
  
  
  #############JavaScript for mobile menu
  
  tags$script(HTML("
    $(document).on('click', '#top-menu a', function(e) {
      e.preventDefault();
      var tabId = $(this).data('tab');
      
      // Hide all tab panes
      $('.tab-pane').removeClass('active');
      
      // Show the selected tab pane
      $('#' + tabId).addClass('active');
      
      // Update active state in top menu
      $('#top-menu a').removeClass('active');
      $(this).addClass('active');
    });
  ")),
  
  
  ###################################### Mobile Content
  div(id = "mobile-content",
      # Top menu
      div(id = "top-menu",
          tags$a(href = "#", "Overview", `data-tab` = "Overview", class = "active"),
          tags$a(href = "#", "COVID", `data-tab` = "COVID"),
          tags$a(href = "#", "Methods", `data-tab` = "Methods")
      ),
      
      # Tabset Panel with hidden navigation
      div(class = "tab-content",
          ############################## Mobile Overview tab
          div(class = "tab-pane active", id = "Overview",
              div(class = "overview-column",
                  style = "text-align: center;",
                  h3("SeqBoard Mobile Overview"),
                  uiOutput("mobile_species_summary"),
                  
                  div(class = "disclaimer",
                      p("This version of the dashboard has been optimized for mobile devices. There is additional information available on the desktop version.", 
                        style = "color: #666; font-weight: bold; text-align: center;")
                  )
              )
          ),
          
          ############################## Mobile COVID tab
          div(class = "tab-pane", id = "COVID",
              h3(HTML("SARS-CoV-2<br>Lineages Over Time"), 
                 style = "text-align: center;"),
              div(class = "covid-content",
                  div(class = "plot-container",
                      tags$img(src = "covid.png", 
                               alt = "SARS CoV-2 Lineages", 
                               style = "width: 100%; max-width: 600px; height: auto; margin: 20px 0;"),
                      p("This plot shows the prevalence of different SARS-CoV-2 lineages over time. The size of each point represents the number of sites where the lineage was detected.")
                  )
              )
          ),
          
          ############################## Mobile Methods tab
          div(class = "tab-pane", id = "Methods",
              h3("Methods"),
            # Introduction with consistently formatted FAQ link
            h4("Introduction to Wastewater Analysis Data"),
            p(class = "method-description","The data in this dashboard and related reports provides a big-picture summary of viral shedding present in community wastewater."),
            tags$ul(
              tags$li("Collection methods prioritize personal privacy by anonymizing and aggregating samples, preventing identification of specific households or individuals."),
              tags$li("This data does not represent individual tests or diagnoses and should not substitute the need for individual testing."),
              tags$li("Wastewater analysis is meant to serve as one piece of information that can help fill gaps in knowledge about the behavior of viruses on a population-level.")
            ),
            tags$img(src = "methods.png", alt = "Methods illustration", style = "width: 100%; max-width: 600px; height: auto; margin: 20px 0;"),
            
            p("Our team has developed methods for detecting the presence of thousands of unique viral markers and illustrating their relative change over space and time. Though great consideration has been taken to normalize this data for public health insights, environmental factors such as water runoff may affect viral load in wastewater."),
            p("Learn more about our methods and disclaimers below, or visit our FAQ's for general questions on wastewater-based epidemiology:"),
            p(
              tags$a(
                href = "https://tephi.texas.gov/faqs",
                "FAQS",
                target = "_blank",
                style = "color: #0000EE; text-decoration: underline; font-weight: bold;",
                `aria-label` = "https://tephi.texas.gov/faqs - Link to Frequently Asked Questions about wastewater-based epidemiology"
              )
            ),
            
            h4("Detailed Methodology"),
            h5("Sample Collection and Extraction"),
            p("Between 200-500 mL of raw wastewater is collected by participating Wastewater Treatment Facilities from multiple cities in Texas. Samples are collected in 500 mL leak-proof prelabeled bottles, decontaminated, and sealed into biohazard bags. Then they are shipped overnight with ice packs to the Alkek Center of Metagenomics and Microbiome Research at Baylor College of Medicine in Houston, TX. Sample bottles are barcoded upon arrival and refrigerated until they undergo filtration and DNA/RNA extraction using a Qiagen Qiamp VIRAL RNA Mini Kit."),
            h5("Quantitative Polymerase Chain Reaction (PCR)"),
            p("Real-time RT-PCR for select targeted viruses is performed using the TaqPath 1-step Multiplex Master Mix (A28523 Applied Biosystems) on the 7500 Fast Dx Real-Time PCR Instrument (4406985, Applied Biosystems) with SDS version 1.4 software. Samples are considered positive if CT values are less than 45. The real-time RT-PCR protocol includes negative extraction and no template controls. A standard curve with amplicons targeting the primers and probe sequences of each target is used to determine the genomic copy numbers used in downstream analysis."),
            h5("Comprehensive Deep Sequencing (CDS)"),
            p("RNA extracts are converted to cDNA, and a probe-based viral capture is performed on the DNA/cDNA mix using the Twist Comprehensive Viral Research Panel (Twist Biosciences). Sequencing library construction is performed using the Twist Library Preparation EF 2.0 Kit and Twist Universal Adaptor System. Samples are then sequenced on an Illumina NovaSeq 6000 SP flow cell, to generate 2×150 bp paired-end reads."),
            h5("Bioinformatic Data Processing of CDS"),
            p("A custom mapping and taxonomic classification pipeline and Virus Database have been developed for processing the comprehensive deep sequencing data. The pipeline generates per sample abundance and coverage metrics, and taxonomic information is used to create the visualizations in this report. The Virus Database is composed of a dereplicated set of reference genomes downloaded from NCBI that encompass the species and strains covered by the Twist Comprehensive Virus Research Panel. Additionally, 97 viral pathogens of particular public health interest were identified, their RefSeq genomes downloaded and included in the database, independently of the prior dereplication."),
            h5("Citation"),
            p('For a more detailed explanation of the methods used to generate and analyze the data please see: '),
            p(
              tags$a(
                href = "https://doi.org/10.1038/s41467-023-42064-1",
                "Wastewater Sequencing Reveals Community and Variant Dynamics of the Collective Human Virome.",
                target = "_blank",
                style = "color: #0000EE; text-decoration: underline; font-weight: bold;",
                `aria-label` = "https://doi.org/10.1038/s41467-023-42064-1 - Link to research paper: Wastewater Sequencing Reveals Community and Variant Dynamics of the Collective Human Virome"
              )
            ),
            h4("Important Disclaimers:"),
            tags$ol(
              tags$li("This report is an aggregate of viral shedding in community wastewater and does not represent individual tests or diagnoses."),
              tags$li("Absolute viral levels are difficult to measure, vary by viral species, and are impacted by many factors, including number of people infected, the stability of the virus over time, the composition of wastewater from a given site, and the detection method itself. However, the relative changes of the viral signal may be used as an approximate gauge of viral trends in space and time."),
              tags$li("Data in this report should not substitute the need for individual testing. Instead, it can be used as one piece of information in filling population-level gaps in the behavior of viruses at the various sites."),
              tags$li("Sequencing data output is in reads per kilobase of transcript per million reads filtered (RPKMF). This represents the relative abundance of a virus genome in a sample. Generally, the higher this number, the more of that sequence and hence the more virus present."),
              tags$li("The data displayed on this dashboard represents the moving average."),
              tags$li("Percent covered represents the fraction of the genome detected in the sequencing data."),
              tags$li("Accession number is the unique identifier for that genome in NCBI."),
              tags$li("Viral load in wastewater is lowered by water runoff.")
            ),
            h4("How Overview is calculated:"),
            tags$ul(
              tags$li("If the RPKMF is greater than 325% of the historic median (excluding observations of zero), it indicates a high historic concentration."),
              tags$li("If the RPKMF falls between 50% and 325% of the median value, it represents a medium historic concentration."),
              tags$li("If the RPKMF is less than 50% of the median value, it indicates a low historic concentration."),
              tags$li("If the current RPKMF exceeds the previous month's average by more than 100%, it is classified as increasing."),
              tags$li("If the current RPKMF is below the previous month's average, it is classified as decreasing."),
              tags$li("A \"high level\" is defined as any high historic concentration, or a medium historic concentration with an upward trend."),
              tags$li("A \"medium level\" is defined as a medium historic concentration, or any low historic concentration with an upward trend."),
              tags$li("A \"low level\" is defined as a low historic concentration with no trend or a downward trend.")
            )
        )
      ),
      
      
      
      
  
      
      # Footer area (only contains the menu)
      div(
        id = "footer-area",
        
        # Bottom menu
        div(
          id = "bottom-menu",
          a(href = "#", "Overview", `data-tab` = "Overview", class = "active"),
          a(href = "#", "COVID", `data-tab` = "COVID"),
          a(href = "#", "Methods", `data-tab` = "Methods")
        )
      )
  ),
  

    

  


  
  
  
  
  #######################################Desktop Version
  div(id = "desktop-content",
      fluidRow(
        column(2, class = "overview-column",
               h3("Overview for Texas"),
               uiOutput("species_summary")
        ),
        column(10, class = "main-content-column",
               tags$h2(tags$b("SeqBoard")),
               tags$h5("Real-time virus monitoring for Texas"),
               tags$hr(),
               
               tabsetPanel(
                 tabPanel("Main Dashboard",
                          # Main virus plot
                          fluidRow(
                            box(
                              width = 12,
                              status = "primary",
                              solidHeader = TRUE,
                              fluidRow(
                                column(width = 6, align = "left",
                                       selectInput("virus", "Pathogen:", 
                                                   choices = sort(unique(sense_path_expand_dt$species)), 
                                                   selected = "SARS-CoV-2")
                                ),
                                column(width = 6, align = "left",
                                       selectInput("dateRange", "Date Range:",
                                                   choices = c("Last 6 weeks" = "6w",
                                                               "Last 3 months" = "3m",
                                                               "Last 6 months" = "6m",
                                                               "All available" = "all"),
                                                   selected = "all")
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       div(class = "full-width-plot",
                                           plotlyOutput("virusPlot")
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       div(class = "rpkmf-explanation",
                                           p("RPKMF shows the relative amounts of different viruses present, accounting for differences in virus genome sizes and total sequencing depth, giving a standardized view of the viral composition in the wastewater sample.")
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       div(class = "plot-description",
                                           h4("For full functionality please use a laptop or desktop computer."),
                                           actionButton("toggleVideoBtn", "Toggle Video Guide on How to Interact with the Plot"),
                                           div(id = "videoContainer", style = "display: none;",
                                               div(class = "video-container",
                                                   tags$video(
                                                     src = "videos/video_guide_cropped.mp4",
                                                     type = "video/mp4",
                                                     controls = TRUE,
                                                     width = "100%",
                                                     height = "auto",
                                                     preload = "auto"
                                                   )
                                               )
                                           )
                                       )
                                )
                              )
                            )
                          ),
                          
                          # Add a clear separator
                          div(class = "section-separator"),
                          
                          # Texas-wide Pathogen Levels Plot
                          fluidRow(
                            column(width = 12,
                                   h3("Texas-wide Pathogen Levels"),
                                   div(class = "full-width-plot",
                                       plotlyOutput("TX_wide_plot", height = "800px")
                                   )
                            )
                          ),
                          
                          # Add another clear separator
                          div(class = "section-separator"),
                          
                          # Collection Site section
                          fluidRow(
                            column(width = 12, class = "map-container",
                                   tags$h2(tags$b("Wastewater Treatment Cities")),
                                   tags$h4("Map of wastewater treatment cities within each public health region of Texas."),
                                   tags$hr(),
                                   imageOutput("map_image", height = "600px"),
                                   tags$p(id = "map-alt-text", class = "visually-hidden",
                                          "Map of Texas showing the locations of cities. Each city is represented by a colored marker on the map.")
                            )
                          )
                 ),
                 
                 
                 
                 
                 ################ New tab for SARS-CoV-2 lineage plot
                 tabPanel("SARS-CoV-2 Lineages",
                          fluidRow(
                            box(
                              width = 12,
                              title = "SARS-CoV-2 Lineages Over Time",
                              status = "primary",
                              solidHeader = TRUE,
                              plotlyOutput("sars_linp", height = "600px")
                            )
                          ),
                          fluidRow(
                            column(12,
                                   p(class = "plot-description","This plot shows the prevalence of different SARS-CoV-2 lineages over time. 
                  The size of each point represents the number of sites where the lineage was detected. 
                  Hover over points for more details.")
                            )
                          )
                 ),
                 
                 
                 
                 ############################New tab for Genome Coverage
                 tabPanel("Genome Coverage",
                          fluidRow(
                            column(width = 12,
                                   h3("Detailed Pathogen Data"),
                                   reactableOutput("nice_table")
                            )
                          )
                 ),
                 
                 ####################New tab for Methods and Disclaimers
                 tabPanel("Methods and Disclaimers",
                          fluidRow(
                            column(width = 12,
                                   # Introduction with consistently formatted FAQ link
                                   h4("Introduction to Wastewater Analysis Data"),
                                   p(class = "method-description","The data in this dashboard and related reports provides a big-picture summary of viral shedding present in community wastewater."),
                                   tags$ul(
                                     tags$li("Collection methods prioritize personal privacy by anonymizing and aggregating samples, preventing identification of specific households or individuals."),
                                     tags$li("This data does not represent individual tests or diagnoses and should not substitute the need for individual testing."),
                                     tags$li("Wastewater analysis is meant to serve as one piece of information that can help fill gaps in knowledge about the behavior of viruses on a population-level.")
                                   ),
                                   p(class = "method-description","Our team has developed methods for detecting the presence of thousands of unique viral markers and illustrating their relative change over space and time. Though great consideration has been taken to normalize this data for public health insights, environmental factors such as water runoff may affect viral load in wastewater."),
                                   p(class = "method-description","Learn more about our methods and disclaimers below, or visit our FAQ's for general questions on wastewater-based epidemiology:"),
                                   p(class = "faq-link",
                                     tags$a(
                                       href = "https://tephi.texas.gov/faqs",
                                       "FAQS",
                                       target = "_blank",
                                       style = "color: #0000EE; text-decoration: underline; font-weight: bold; font-size: 25px;",  # Increased font size
                                       `aria-label` = "https://tephi.texas.gov/faqs - Link to Frequently Asked Questions about wastewater-based epidemiology"
                                     )
                                   ),
                                   
                                   imageOutput("methodsImage", width = "1284px", height = "1240px"),
                                   h4("Detailed Methodology"),
                                   h5("Sample Collection and Extraction"),
                                   p(class = "method-description", 
                                     "Between 200-500 mL of raw wastewater is collected by participating Wastewater Treatment Facilities from multiple cities in Texas. Samples are collected in 500 mL leak-proof prelabeled bottles, decontaminated, and sealed into biohazard bags. Then they are shipped overnight with ice packs to the Alkek Center of Metagenomics and Microbiome Research at Baylor College of Medicine in Houston, TX. Sample bottles are barcoded upon arrival and refrigerated until they undergo filtration and DNA/RNA extraction using a Qiagen Qiamp VIRAL RNA Mini Kit."),
                                   
                                   h5("Quantitative Polymerase Chain Reaction (PCR)"),
                                   p(class = "method-description",
                                     "Real-time RT-PCR for select targeted viruses is performed using the TaqPath 1-step Multiplex Master Mix (A28523 Applied Biosystems) on the 7500 Fast Dx Real-Time PCR Instrument (4406985, Applied Biosystems) with SDS version 1.4 software. Samples are considered positive if CT values are less than 45. The real-time RT-PCR protocol includes negative extraction and no template controls. A standard curve with amplicons targeting the primers and probe sequences of each target is used to determine the genomic copy numbers used in downstream analysis."),
                                   
                                   h5("Comprehensive Deep Sequencing (CDS)"),
                                   p(class = "method-description",
                                     "RNA extracts are converted to cDNA, and a probe-based viral capture is performed on the DNA/cDNA mix using the Twist Comprehensive Viral Research Panel (Twist Biosciences). Sequencing library construction is performed using the Twist Library Preparation EF 2.0 Kit and Twist Universal Adaptor System. Samples are then sequenced on an Illumina NovaSeq 6000 SP flow cell, to generate 2×150 bp paired-end reads."),
                                   
                                   h5("Bioinformatic Data Processing of CDS"),
                                   p(class = "method-description",
                                     "A custom mapping and taxonomic classification pipeline and Virus Database have been developed for processing the comprehensive deep sequencing data. The pipeline generates per sample abundance and coverage metrics, and taxonomic information is used to create the visualizations in this report. The Virus Database is composed of a dereplicated set of reference genomes downloaded from NCBI that encompass the species and strains covered by the Twist Comprehensive Virus Research Panel. Additionally, 97 viral pathogens of particular public health interest were identified, their RefSeq genomes downloaded and included in the database, independently of the prior dereplication."),
                                   
                                   h5("Citation"),
                                   p(class = "method-citation",
                                     'For a more detailed explanation of the methods used to generate and analyze the data please see: '),
                                   p(class = "method-citation",
                                     tags$a(
                                       href = "https://doi.org/10.1038/s41467-023-42064-1",
                                       "Wastewater Sequencing Reveals Community and Variant Dynamics of the Collective Human Virome.",
                                       target = "_blank",
                                       style = "color: #0000EE; text-decoration: underline; font-weight: bold; font-size: 25px; padding: 10px 0; display: inline-block;",
                                       `aria-label` = "https://doi.org/10.1038/s41467-023-42064-1 - Link to research paper: Wastewater Sequencing Reveals Community and Variant Dynamics of the Collective Human Virome"
                                     )),
                                   h4("Important Disclaimers:"),
                                   tags$ol(
                                     tags$li("This report is an aggregate of viral shedding in community wastewater and does not represent individual tests or diagnoses."),
                                     tags$li("Absolute viral levels are difficult to measure, vary by viral species, and are impacted by many factors, including number of people infected, the stability of the virus over time, the composition of wastewater from a given site, and the detection method itself. However, the relative changes of the viral signal may be used as an approximate gauge of viral trends in space and time."),
                                     tags$li("Data in this report should not substitute the need for individual testing. Instead, it can be used as one piece of information in filling population-level gaps in the behavior of viruses at the various sites."),
                                     tags$li("Sequencing data output is in reads per kilobase of transcript per million reads filtered (RPKMF). This represents the relative abundance of a virus genome in a sample. Generally, the higher this number, the more of that sequence and hence the more virus present."),
                                     tags$li("The data displayed on this dashboard represents the moving average."),
                                     tags$li("Percent covered represents the fraction of the genome detected in the sequencing data."),
                                     tags$li("Accession number is the unique identifier for that genome in NCBI."),
                                     tags$li("Viral load in wastewater is lowered by water runoff.")
                                   ),
                                   h4("How Overview is calculated:"),
                                   tags$ul(
                                     tags$li("If the RPKMF is greater than 325% of the historic median (excluding observations of zero), it indicates a high historic concentration."),
                                     tags$li("If the RPKMF falls between 50% and 325% of the median value, it represents a medium historic concentration."),
                                     tags$li("If the RPKMF is less than 50% of the median value, it indicates a low historic concentration."),
                                     tags$li("If the current RPKMF exceeds the previous month's average by more than 100%, it is classified as increasing."),
                                     tags$li("If the current RPKMF is below the previous month's average, it is classified as decreasing."),
                                     tags$li("A \"high level\" is defined as any high historic concentration, or a medium historic concentration with an upward trend."),
                                     tags$li("A \"medium level\" is defined as a medium historic concentration, or any low historic concentration with an upward trend."),
                                     tags$li("A \"low level\" is defined as a low historic concentration with no trend or a downward trend.")
                                   )
                                   
                            )
                          )
                 )
                 
                 
                 
                 
               )
        )
      )
  )
)

