#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("Analiza krzywych wzrostu bioscreen",
               theme = shinythemes::shinytheme("united"),
               tabPanel("Wczytanie danych",
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons('czy_filtr', 'Czy chcesz wczytać wcześniej odfiltrowane dane?', 
                                         choices = c('Tak', 'Nie'), inline = TRUE, selected = 'Nie'),
                            conditionalPanel(condition = 'input.czy_filtr == "Nie"',
                                             "Wczytaj plik zawierający mapę i dane z bioscreen w formacie .xlsx lub .xls", 
                                             fileInput('dane_mapa', 'Wybierz plik excel zawierający mapę płytki'),
                                             radioButtons('format', "Czy dane do wczytania są w formacie xlsx (zapisane w excel) lub csv (wygenerowane przez bioscreen)?", 
                                                          choices = c('xlsx', 'csv'), inline = TRUE, selected = 'csv'),
                                             fileInput('dane_bio', 'Wybierz plik excel lub csv zawierający dane z bioscreena'),
                                             numericInput('linie_usun', "Ile linijek pominąć podczas wczytywania pliku xlsx (zwykle 2)", 2),
                                             numericInput('czas_bio', 'Co ile minut wykonywano pomiar absorbancji?', 10),
                                             numericInput("usun_blank", "Usuń pomiary dla blank jeżeli różnica pomiędzy minimu a maksimum przekracza:",
                                                          value = 0.1, min = 0, max = 1, step = 0.05),
                                             checkboxInput('pomin_blank', "Czy pominąć odejmowanie wartości blank?", value = FALSE)
                            ),
                            conditionalPanel(condition = 'input.czy_filtr == "Tak"',
                                             fileInput('dane_filtr', 'Wybierz plik csv zawierający odfiltrowane dane, zapisane wcześniej z shiny')
                            ),
                            width = 3
                            
                          ),
                          
                          
                          mainPanel(
                            h4("Dane po zmianie formatu na wąski"),
                            tableOutput("dane"),
                            h4("Wczytana mapa płytki"),
                            tableOutput("mapa"),
                            conditionalPanel(condition =  '!input.pomin_blank', 
                                             h4("Absorbancja studzienek blank - czysta pożywka"),
                                             plotOutput("wykres_blank")
                            ),
                            width = 9
                          )
                        )
               ),
               tabPanel("Filtrowanie",
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            numericInput('filtr_up', 'Usunąć krzywe przekraczające podaną wartość absorbancji', 1.25, step = 0.1),
                            numericInput('filtr_down', 'Usunąć krzywe poniżej podanej wartości absorbancji', 0.01, step = 0.01),
                            numericInput('filtr_mediana', 'Usunąć odstające krzywe - podaj maksymalną średnią odległość od mediany', 0.08, step = 0.01),
                            radioButtons('wyrownac', 'Czy wyrównać wszystkie krzywe do zera', 
                                         choices = c('Tak', 'Nie'), inline = TRUE, selected = 'Tak'),
                            downloadButton("downloadData", "Pobierz odfiltrowane dane"),
                            width = 3
                            
                            
                          ),
                          
                          
                          mainPanel(
                            plotOutput("krzywe", height = "700px"),
                            width = 9
                          )
                        )
               ),
               tabPanel("Krzywe wzrostu",
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("filtr_czas"),
                            uiOutput("szczepy"),
                            uiOutput("warunki"),
                            radioButtons('sd', 'Czy pokazać odchylenie standardowe?', 
                                         choices = c('Tak', 'Nie'), inline = TRUE, selected = 'Nie'),
                            radioButtons('ll', 'Czy pokazać dopasowanie modelu loglogistycznego? (tylko gdy wybrane są pojedyncze warunki)', 
                                         choices = c('Tak', 'Nie'), inline = TRUE, selected = 'Nie'),
                            downloadButton('download_wykres', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                            numericInput('width', 'Szerokość obrazka [cm]', 20, min = 5, max = 35),
                            numericInput('height', 'Wysokość obrazka [cm]', 14, min = 5, max = 35),
                            numericInput('res', 'Rozdzielczość', 200, min = 100, max = 500),
                            textInput('xlab', 'Podaj tytuł osi X', value = 'Czas [h]'),
                            textInput('ylab', 'Podaj tytuł osi Y', value = 'OD'),
                            textInput('legend', 'Podaj tytuł legendy', value = 'Szczepy'),
                            width = 3
                          ),
                          
                          
                          mainPanel(
                            plotOutput("krzywe_final", height = "700px", width = '100%'),
                            width = 9
                          )
                        )
               ),
               tabPanel('Pomoc',
                        p(strong('1. Zakładka Wczytanie danych -'),' - potrzebne są:'),
                        p('* plik .csv wygenerowany z programu bioscreena albo wcześniej zapisany w excelu plik .xlsx'),
                        p('* plik .xlsx zawierający mapę płytki - konieczny do poprawnego przyporządkowania kolumn w pliku z danymi do nazw szczepów'),
                        p('każda nazwa szczepu powinna trzymać się formatu: szczep/warunki. Jak wszystkie szczepy są w jednych warunkach - to można po / napisać np. 0.'),
                        p('Jeżeli dana studzienka zawiera samą pożywkę i chcemy jej wartość odjąć od otrzymanych wyników to powinna nazywać się blank,
może być więcej niż jeden blank na płytce, bez żadnego blanka program nie zadziała.'),
                        p('Przykładowe pliki z z mapą i danymi znajdują się na stronie GitHub'),
                        p(strong('2. Zakładka Filtrowanie -'),' pozwala odrzucić z analizy te powtórzenia które nie pasują pod względem
maksymalnej lub minimalnej absorbancji oraz odrzucić krzywe odstające pod względem ich podobieństwa
do mediany z wszystkich powtórzeń'),
                        p('Odfiltrowane dane można zapisać do innych analiz w formacie .csv'),
                        p(strong('3. Zakładka Krzywe wzrostu'),' - Rysuje uśredniony wykres dla wszystich odfiltrowanych powtórzeń,
można wybrać które szczepy lub warunki będą widoczne na wykresie
można dodać do wykresu odchylenie standardowe i/lub dopasowany model wzrostu
(to drugie tylko gdy wybrano pojedyncze warunki)'),
                        p('Przygotowany wykres można zapisać jako .png')
                        
               )
    )
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'bioscreen'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

