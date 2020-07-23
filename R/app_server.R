#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  library(magrittr)
  
  dane_bio <- reactive({    
    
    inFile_excel_mapa <- input$dane_mapa
    if (is.null(inFile_excel_mapa))
      return(NULL)
    # plik zawierający mapę płytki
    mapa <- readxl::read_excel(path = inFile_excel_mapa$datapath)
    
    inFile_excel_bio <- input$dane_bio
    if (is.null(inFile_excel_bio))
      return(NULL)
    # plik zawierający dane z bioscreen
    
    if(input$format == 'xlsx'){
      bioscreen <- readxl::read_excel(path = inFile_excel_bio$datapath, skip = input$linie_usun)
    }
    # bioscreen <- read.xls(xls = inFile_excel_bio$datapath, skip = input$linie_usun)
    
    # gdy plik csv pochodzi od razu z bioscreena
    if(input$format == 'csv'){
      neg = scan(inFile_excel_bio$datapath, what = 'character', comment.char = '',
                 encoding = "UTF-8", skipNul = TRUE, skip = 3)
      
      cols = strsplit(neg[1], ',')[[1]]
      data <- matrix(nrow = length(neg) - 1, ncol = length(cols))
      
      for(i in 2:length(neg)){
        wiersz <- strsplit(neg[i], ',')[[1]]
        #print(wiersz)
        
        for(j in 1:length(wiersz)){
          
          data[i-1, j] <- wiersz[j]
        }
      }
      
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      colnames(data) <- cols
      
      for(i in 2:ncol(data)){
        data[,i] <- as.numeric(data[,i])
      }
      
      bioscreen <- data
    }
    # tabela z nazwami kolumn z bioscreena
    wzor <- mapa[13:22, 2:21]
    
    # tabela z wpisanymi szczepami
    mapa <- mapa[1:10,2:21]
    
    # przygotowanie tabeli z nazwami szczepów odpowidającymi nazwom kolumn z danych bioscreen
    wzor <- subset(as.data.frame(cbind(unlist(wzor), unlist(mapa)), stringsAsFactors = FALSE), !is.na(V2))
    
    # dodanie kolumny z powtórzeniami dla każdego szczepu
    wzor <- wzor %>% dplyr::group_by(V2) %>% dplyr::mutate(powtorzenie = 1:dplyr::n())
    
    # zmiana nazw kolumn - żeby można było póżniej połączyć z danymi
    colnames(wzor) <- c("nazwa", "szczep", "powtorzenie")
    
    # wybranie nazw kolumn, które zawierają kontrolę (sama pożywka)
    kontrola <- unlist(wzor[which(wzor[,2] == "blank"),1])
    
    # usunięcie kolumny blank z danych - zawiera zazwyczaj same 0
    bioscreen <- bioscreen[,-2]
    
    # dodanie kolumny czas zwierającej kolejne wartości co 10 minut zaczynając od 0
    bioscreen$czas <- seq(0, (nrow(bioscreen)-1)*input$czas_bio, input$czas_bio)
    
    # przejście do formatu wąskiego, kolumny czas, Time i wszystkie zawierające pomiary blank są traktowane jako 
    # wartości identyfikujące. Kolumna nazwa będzie zawierać dotychczasowe nazwy kolumn, a kolumna absorbancja 
    # wartości zmierzone przez bioscreen
    bioscreen <- tidyr::gather(bioscreen, "nazwa", "absorbancja", -Time, -czas, -which(colnames(bioscreen) %in% kontrola))
    
    # łączenie tabeli z wynikami z przygotowanym wzorem nazw według nazw kolumn np. 101
    bioscreen <- dplyr::left_join(bioscreen, wzor, by = "nazwa")
    
    # podział‚ na grupy według szczepu, czasu i powtorzenia, wybieramy tylko kolumny zawierające X w nazwie(blanki)
    # łączymy blanki do formatu wąskiego (o ile jest więcej niż jedna), ponownie grupujemy i wyciągamy średnią - 
    # kolumna blank, dołączamy do wejściowych danych (test3) i odrzucamy kolumny zawierające X (nie będą już 
    # potrzebne), na koniec odejmujemy od wartości absorbancji blank
    
    bioscreen %>% dplyr::group_by(szczep, powtorzenie, czas) %>%
      dplyr::select(kontrola) %>%
      tidyr::gather("well", "blank", -szczep, -powtorzenie, -czas) -> 
      dane_blank
    
    #sprawdzenie czy w w blankach któryś nie przerósł - absorbancja powinna być w miarę stała w czasie eksperymentu
    dane_blank %>% dplyr::group_by(szczep, powtorzenie, well) %>%
      dplyr::mutate(range_blank = max(blank) - min(blank),
                    blank = ifelse(range_blank >= input$usun_blank, NA, blank)) %>%
      dplyr::filter(!is.na(blank)) -> 
      dane_blank
    
    dane_blank %>%
      dplyr::group_by(szczep, powtorzenie, czas) %>%
      dplyr::summarize(blank = mean(blank)) %>%
      dplyr::left_join(bioscreen, by = c("szczep", "powtorzenie", "czas")) %>%
      dplyr::select(szczep, powtorzenie, czas, blank, absorbancja) %>%
      dplyr::mutate(value = absorbancja - blank) -> 
      dane
    
    return(list(dane, 
                mapa,
                dane_blank))
    
  })
  
  
  
  output$dane <- renderTable({
    
    if(input$czy_filtr == 'Nie'){
      head(dane_bio()[[1]], n = 10)
    } else {
      head(dane_wczytane(), n = 10)
    }
  })
  
  output$mapa <- renderTable({
    
    if(input$czy_filtr == 'Nie'){
      dane_bio()[[2]]
    } 
  })
  
  dane_filtr <- reactive({    
    
    dane <- dane_bio()[[1]]
    
    # podział‚ na grupy według szczepu i powtórzenia, następnie dodanie kolumny z maksymalną wartością 
    # absorbancji, usunięcie powtórzeń, których maksima nie mieszczą się w zakresie podanym przez użytkownika
    dane_1 <- dane %>% dplyr::group_by(szczep, powtorzenie) %>%
      dplyr::mutate(maksimum = max(value)) %>%
      dplyr::filter(maksimum >= input$filtr_down, maksimum <= input$filtr_up)
    
    # Obliczenie odległości od mediany absorbancji każdego powtórzenia i odrzucenie wszystkich z odległością 
    # powyżej podanej wartości
    
    dane_2 <- dane_1 %>% dplyr::group_by(szczep, czas) %>%
      dplyr::mutate(mediana = median(value)) %>%
      dplyr::group_by(szczep, czas, powtorzenie) %>%
      dplyr::mutate(odl_med = value - mediana) %>%
      dplyr::group_by(szczep, powtorzenie) %>%
      dplyr::mutate(sr_odl_med = mean(abs(odl_med))) %>%
      dplyr::filter(sr_odl_med <= input$filtr_mediana)
    
    dane <- dane_2
    
    if(input$wyrownac == 'Tak'){
      dane <- dane %>% dplyr::group_by(szczep, powtorzenie) %>%
        dplyr::mutate(value = value - min(value))
    }
    
    return(dane)  
  })
  
  krzyweInput <- reactive({
    
    envir <- environment()
    
    dane <- dane_filtr()
    
    p <- ggplot2::ggplot(dane, environment = envir, ggplot2::aes(x = czas, y = value, color = factor(powtorzenie)))
    
    p <- p + ggplot2::geom_line()+
      ggplot2::facet_wrap(~ szczep)+
      ggplot2::theme_bw()+
      ggplot2::xlab('Czas')+
      ggplot2::ylab('Absorbancja')+
      ggplot2::scale_color_discrete(name = '')
    
    print(p)
  })  
  
  output$krzywe <- renderPlot({
    if (is.null(input$dane_mapa)&is.null(input$dane_bio))
      return(NULL)
    print(krzyweInput())
  })
  
  dane_final <- reactive({    
    
    dane <- dane_filtr()
    
    dane <- dane %>% dplyr::group_by(szczep, czas) %>%
      dplyr::summarize(pomiar = mean(value), odch = sd(value), n = dplyr::n(),
                       min = pomiar - odch, max = pomiar + odch) %>%
      tidyr::separate(szczep, into = c('szczep', 'warunki'), sep = '/')
    
    return(dane)
  })
  
  dane_wczytane <- reactive({
    
    inFile_filtr <- input$dane_filtr
    if (is.null(inFile_filtr)){
      return(NULL)
    }
    dane <- read.csv(file = inFile_filtr$datapath)
    return(dane)
  })
  
  krzywe_blank <- reactive({
    
    dane_blank <- dane_bio()[[3]]
    
    p <- ggplot2::ggplot(dane_blank, ggplot2::aes(x = czas, y = blank, color = well))
    p <- p + ggplot2::geom_line()
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::xlab("Czas") + ggplot2::ylab('Absorbancja blanka')
    
    return(p)
  })
  
  output$wykres_blank <- renderPlot({
    if(input$czy_filtr == 'Nie'){
      if (!is.null(input$dane_mapa)&!is.null(input$dane_bio))
        print(krzywe_blank())
    }
    
  })
  
  krzywefinalInput <- reactive({
    
    envir <- environment()
    
    if(input$czy_filtr == 'Nie'){
      dane <- dane_final()
    } else {
      dane <- dane_wczytane()
    }
    
    dane %>% dplyr::filter(szczep %in% input$szczepy) %>%
      dplyr::filter(warunki %in% input$warunki) %>%
      dplyr::filter(czas <= input$filtr_czas[2]*60, czas >= input$filtr_czas[1]*60) -> dane
    
    p <- ggplot2::ggplot(dane, environment = envir, ggplot2::aes(x = czas/60, y = pomiar, color = szczep))
    
    p <- p + ggplot2::geom_line(size = 1)+
      ggplot2::facet_wrap(~ warunki)+
      ggplot2::theme_bw()+
      ggplot2::theme(text = ggplot2::element_text(size = 15))+
      ggplot2::scale_color_viridis_d(name = input$legend)
    
    if(input$sd == 'Tak'){
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(x = czas/60, fill = szczep, ymin = min, ymax = max),
                                    alpha = 0.33)+
        ggplot2::scale_fill_viridis_d(name = input$legend)
    }
    
    if(input$ll == 'Tak'){
      
      #library(drc)
      model <- drc::drm(pomiar~czas, curveid = szczep, data = dane, fct = drc::LL.3())
      
      nowe_dane <- predict(model)
      
      dane2 <- cbind(dane, nowe_dane)
      
      p <- p + ggplot2::geom_line(data = dane2, ggplot2::aes(x = czas/60, y = nowe_dane, color = szczep),
                                  size = 2)
      
    }
    p <- p + ggplot2::xlab(input$xlab)+
      ggplot2::ylab(input$ylab)
    print(p)
  })  
  
  output$krzywe_final <- renderPlot({
    if(input$czy_filtr == 'Nie'){
      if (is.null(input$dane_mapa)&is.null(input$dane_bio))
        return(NULL)
    }
    print(krzywefinalInput())
  })
  
  output$filtr_czas <- renderUI({
    
    if(input$czy_filtr == 'Nie'){
      dane <- dane_final()
    } else {
      dane <- dane_wczytane()
    }
    
    max_czas <- round(max(dane$czas)/60)
    
    sliderInput('filtr_czas', 'Podaj zakres czasu w godzinach', 0, max_czas, value = c(0, max_czas))
    
  })
  
  output$szczepy <- renderUI({
    
    if(input$czy_filtr == 'Nie'){
      dane <- dane_final()
    } else {
      dane <- dane_wczytane()
    }
    
    szczepy <- unique(dane$szczep)
    
    checkboxGroupInput("szczepy", label = ("Wybierz szczepy"), 
                       choices = szczepy,
                       selected = szczepy)
    
  })
  
  output$warunki <- renderUI({
    
    if(input$czy_filtr == 'Nie'){
      dane <- dane_final()
    } else {
      dane <- dane_wczytane()
    }
    
    warunki <- unique(dane$warunki)
    
    checkboxGroupInput("warunki", label = ("Wybierz warunki"), 
                       choices = warunki,
                       selected = warunki)
    
  })
  
  output$download_wykres <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res, width = input$width, input$height, unit = 'cm')
      print(krzywefinalInput())
      dev.off()
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dane_final(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  
}
