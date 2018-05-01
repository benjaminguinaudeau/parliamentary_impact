

scrape_index <- function(url){
  #This function takes an index page of the bundestags archives, select only the bills 
  # and return the links to the page summarizing the process of one bill
  
  page <- xml2::read_html(url)
  
  #Scrape of the tables
  tmp <- page %>% 
    html_nodes( xpath = '//*[@id="wrap"]/div[2]/div/fieldset/div/fieldset/div/table') %>%
    html_table() %>%
    .[[1]]
  
  #Scrape of the links
  tmp1 <- page %>% 
    html_nodes( xpath = '//*[@id="wrap"]/div[2]/div/fieldset/div/fieldset/div/table') %>%
    html_nodes( xpath = "//td/a") %>%
    html_attr("href")
  
  #Merge the table and the links
  df <- data.frame(tmp, tmp1, url)
  
  #Select only the process refering to laws
  df %<>% filter(df[,1] == "Gesetzgebung")
  
  return(df)
}

extract_important_information <- function(url){
  
  page <- xml2::read_html(url)
  
  name <- page %>% 
    html_nodes('.linkDokument') %>% 
    html_text() %>%
    str_replace_all(c('\n ' = ' ', '\t' = ' ', " \\s+" = ' '))
  
  links <- page %>% html_nodes('.linkDokument') %>% html_attr("href")
  
  if(length(name) == 0){
    name <- "None"
    links <- "None"
  } else {
    if(length(name %>% grep(pattern = "^S. [[:digit:]]+.")) > 0 ){
      links <- links[-(name %>% grep(pattern = "^S. [[:digit:]]+."))]
      name <- name[-(name %>% grep(pattern = "^S. [[:digit:]]+."))]
    }}
  
  drucksache_numm <- page %>% 
    html_nodes(".ddBreit") %>%
    html_text %>%
    str_replace_all("(\n)|(\t)", " ") %>%
    str_replace_all(" {2,}", " ") 
  
  drucksache_numm %<>% .[grepl(drucksache_numm, pattern ="\\d/\\d")] %>% paste0(collapse = "")
  
  general_information <- page %>% 
    html_nodes(xpath = '//*[@id="wrap"]/div[2]/div/fieldset/div/fieldset/div[1]') %>%
    html_text() %>%
    str_replace_all(c('\t+' = ' ', '\n' = ' ', ' \\s+' = ''))
  
  title <- general_information  %>%
    str_extract(regex("(?<=Vorgangstyp:Gesetzgebung).*(?=Initiative:)"))
  
  initiative <- general_information  %>%
    str_extract(regex("(?<=Initiative:).*(?=Aktuelle)"))
  
  stand <- general_information %>%
    str_extract(regex("(?<=Aktueller Stand:).*(?=Archivsignatur)"))
  
  signatur <- general_information %>%
    str_extract(regex("(?<=Archivsignatur:).*(?=GESTA-Ordnungsnummer:)"))
  
  if(is.na(stand)){
    stand <- general_information %>%
      str_extract(regex("(?<=Aktueller Stand:).*(?=GESTA-Ordnungsnummer:)"))
    
    signatur <- "None"
  }
  
  gesta <- general_information %>%
    str_extract(regex("(?<=GESTA-Ordnungsnummer:).*(?=Zustimmungsbedürftigkeit:)"))
  
  zustimmung <- general_information %>%
    str_extract(regex("(?<=Zustimmungsbedürftigkeit:).*(?=Wichtige)"))
  
  verkundung <- general_information %>%
    str_extract(regex("(?<=Verkündung:).*(?=Sachgebiete:)"))
  if(is.na(verkundung)){verkundung <- "None"}
  
  
  inkrafttreten <- general_information %>%
    str_extract(regex("(?<=Inkrafttreten:).*(?=Sachgebiete:)"))
  
  if(is.na(inkrafttreten)){inkrafttreten <- "None"}
  
  tmp <- page %>% 
    html_nodes(xpath = '//*[@id="wrap"]/div[2]/div/fieldset/div/fieldset/div[2]/fieldset/div') %>%         html_text()%>%
    str_replace_all(c('\t+' = ' ', '\n' = ' ', ' \\s+' = '')) 
  
  process <- tmp
  
  
  
  df <- data.frame(url, name, links, initiative, stand, gesta, zustimmung, verkundung, inkrafttreten, process, drucksache_numm, stringsAsFactors = F)
  
  return(df)
}

get_links <- function(drs){
  drs %<>% str_extract("14/[[:digit:]]+")
  tmp2 <- drs %>% str_extract("/[[:digit:]]+") %>% str_replace("/", "")
  tmp1 <- tmp2 %>% str_replace("..$", "") %>% str_pad(3, side = "left", "0")
  
  links <- paste0("http://dip21.bundestag.de/dip21/btd/14/",  tmp1, "/14", tmp1, tmp2, ".pdf")
  return(links)
}

extract_selenium_pdf <- function(url, jj, wait, filenames){
  remDr$navigate(url)
  
  webElem <- list()
  k <- 1
  
  while(length(webElem) == 0 && k < 20){
    Sys.sleep(wait*2)
    webElem <- remDr$findElements(using = "css selector", 'iframe')
    k <- k+1 
  }
  Sys.sleep(wait)
  k <- 1
  linkse <- list()
  while(length(linkse) == 0 && k < 20){
    Sys.sleep(wait)
    linkse <- sapply(webElem, function(x){x$getElementAttribute("src")})
    k <- k + 1
  }
  
  linkse <- linkse[[1]][1]
  download.file(linkse, destfile=paste0("Data/Bills/", filenames[jj], ".pdf"), quiet = T)
  return(linkse)
}

clean <- function(x, type, char = ""){
  if(type == "space"){y <- x %>% str_replace_all(pattern = " ", "")}
  if(type == "multiple"){y <- x %>% str_replace_all(pattern = paste0(char, "{2,}", char))}
  if(type == "multiplespace"){y <- x %>% str_replace_all(pattern = " {2,}", " ")}
  if(type == "breaklines"){y <- x %>% str_replace_all(pattern = "\n", " ")}
  if(type == "multiplebreaks"){y <- x %>% str_replace_all(pattern = "\n{2,}", "\n")}
  if(type == "head"){y <- x %>% str_replace(pattern = ".*?(\n)", "")}
  if(type == "tiret"){y <- x %>% str_replace_all(pattern = "\\-(\\s+)?\n", "\n")}
  return(y)
}

split_page <- function(text){
  tmp <- paste0(text, sep = "\n", collapse = "\n") %>%
    str_split("\f") %>% .[[1]]
  return(tmp)
}

preprocess <- function(doc, method = "pdftool", type="bill"){
  
  text <- c()  
  
  tryCatch({
    if(method == "pdftool"){
      text <- pdf_text(doc)  
    } else {
      if(method %in% c("Rpoppler", "ghostscript")){
        tmp <- readPDF(engine = method, 
                       control = list(text = "-layout -fixed 3 -enc UTF-8")
        )(elem = list(uri = doc), language = "de")
        text <- tmp$content
      } else {
        #"Rcampdf" , "xpdf"
        tmp <- readPDF(engine = method, 
                       control = list(text = "-layout -fixed 3 -enc UTF-8")
        )(elem = list(uri = doc), language = "de")
        text <- split_page(tmp$content) 
        if(type != "anfrage"){
        text %<>% clean("multiplebreaks")
        }
      }
    }
    
  }, error = function(e){
  }, warning = function(war){
  })
  
  
  error <- T
  error <- !length(text) == 0
  if(error){error <- !str_length(text[1]) == 0}
  
  if(!error & method == "pdftool"){
    text <- c()
    tryCatch({
      tmp <- readPDF(engine = "xpdf", 
                     control = list(text = "-layout -fixed 3 -enc UTF-8")
      )(elem = list(uri = doc), language = "de")
      text <- split_page(tmp$content) 
      if(type != "anfrage"){
      test %<>% clean("multiplebreaks")
      }
    }, error = function(e){
    }, warning = function(war){
    })     
  }
  
  error <- T
  error <- !length(text) == 0
  if(error){error <- !str_length(text[1]) == 0}
  
  if(!error){
    message("PDF could not be read")
    return("The bill was problematic (1)")}
  
  if(type == "anfrage"){
    text[text %in% c("", "\n")] <- NA
    text %<>% na.omit
    
    
    #text <- pdf_text(doc)
    tmp <- text[1] %>% 
      str_split("\n\n\n+") %>%
      unlist
    tmp %<>% grep(pattern = "Anfrage")
    
    if(length(tmp) == 0){
      message(
        paste0("\nFirst Pattern could not be matched for document \n", doc, "\n")
      )
      return("Problematic Anfrage")
    }
    
    text[1] %<>% 
      str_split("\n\n\n+") %>%
      unlist %>% .[-1:(-tmp[1])] %>%
      paste0(collapse="")
    
    step2 <- text
    
    step3 <- step2 %>% clean("multiplespace") %>% clean("multiplebreaks")
    
    tmp <- grep(step3 %>% str_extract(".*\n"), pattern = "Deutscher\\s+Bundestag\\s+.*\\d{1,2}\\.\\s+Wahlperiode")
    step3[tmp] %<>% clean("head")
    
    step4 <- step3 
    pattern <- " +((Bonn)|(Berlin))(.{1,3})? +(den +)?\\d{1,2}.{3,12}\\d{4}((\n)|( +))"
    tmp <- step4 %>% grep(pattern = pattern)
    if(length(tmp) == 0){
      message(
        paste0("\nLast Pattern could not be matched for document \n", doc, "\n")
              )
        return("Problematic Anfrage")
        }
    step4[tmp[length(tmp)]] %<>% str_split(pattern = pattern) %>% unlist %>% .[1]
    if(tmp[length(tmp)]!= length(step4)){
      step4 %<>% .[(-tmp[1]-1):-length(step4)]
      }
    
    
    step4 %<>%
      str_replace_all("\n\\s+", "\n ") %>%
      str_replace_all("\n", " ") %>%
      str_replace_all("-\\s+", "") %>%
      paste0(collapse="")
    
    return(step4)
  }
  
  if(type == "bill"){
    #Clean heading and last line
    for(jj in seq_along(text)){
      text[jj] %<>% 
        str_replace("\n.*(Bundesgesetzblatt(,)?\\s+Jahrgang)|.*\n", "\n") %>% 
        str_replace("\n.*(Das\\s+Bundesgesetzblatt\\s+im\\s+Internet).*\n", "\n")
    }
    
    #Extract date and title of the bill
    tmp <- text[1] %>% str_extract("(.*)?(\n(^((G|g)esetz).*)?)*\n.*(G|g)esetz") %>% str_split("\n") %>% .[[1]]
    if(length(tmp) > 3){
      message("Heading of pdf is not regular")
      return("The bill was problematic (7)")
    }
    
    tmp <- text[1] %>% str_extract("\n.*(G|g)esetz(\n|.)*(Vom.*\\d{4})")
    date <- tmp %>% str_extract("Vom.*\\d{4}") %>% str_replace("Vom(\\s+)?", "")
    gesetz_title <- tmp %>% str_replace("Vom.*\\d{4}", "") %>% clean("breaklines") %>% clean("multiplespace")
    
    text[1] %<>% str_replace("\n.*(G|g)esetz(\n|.)*(Vom.*\\d{4})", "\n")
  }
  
  if(type == "proposal"){
    #Extract name of the institution
    institution <- str_extract(text[1], 
                               pattern = "Bundes.*?(\\s)") %>% 
      clean("space")
    
    date <- str_extract(text[1], pattern = "(\\d\\d\\.){2}\\d\\d")
    
    author <- str_extract(text[1], pattern = "Gesetzentwurf(.|\n)*Entwurf") %>% 
      clean("breaklines") %>% clean("multiplespace") %>% 
      str_replace_all("Gesetzentwurf|Entwurf| d.. ", "") 
    
    #Extract title of the bill
    gesetz_title <- str_extract(paste0(text[1], text[2]), 
                                pattern = "(Entwurf).*(.\n?.+)*(\n\\s*A.\\s*)") %>%
      clean("breaklines") %>% clean("multiplespace")
  }
  
  int.abk <- "international|Abkommen"
  if(grepl(gesetz_title, pattern = int.abk)){
    message("The bill seems to be an international treaty")
    return("The bill was problematic (5)")
  }
  
  tmp <- "Der(\\s+|\n)Text(\\s+|\n)des(\\s+|\n)Gesetzentwurfs(\\s+|\n)und(\\s+|\n)der(\\s+|\n)Begründung
  ist(\\s+|\n)gleichlautend(\\s+|\n)mit"
  if(length(grep(text, pattern = tmp)) != 0){
    message("The content of the bill was not in the document")
    return("The bill was problematic (4)")
  }
  
  if(type == "bill"){
    #Extract the bill's content from the whole document
    tmp1 <- c("Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+mit\\s+Zustimmung\\s+d(a|e)s",
              "Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+das\\s+(folgende|nachstehende)\\s+Gesetz",
              "Der\\s+(Deutsche\\s+)?Bundestag\\s+wolle\\s+beschließen")
    tmp1 <- paste(tmp1, sep="|", collapse = "|")
    
    tmp2 <- c("(Bonn|Berlin),\\s+den\\s+[[:digit:]]+\\. ")
    
    starting_page <- text %>% grep(pattern=tmp1) %>% .[1]
    final_page <- text %>% grep(pattern = tmp2) %>% .[1]
    
    if(is.na(starting_page)){
      message("starting_page is missing")
      return("The bill was problematic (2B)")
    }
    
    if(is.na(final_page)){
      message("final_page is missing")
      return("The bill was problematic (2C)")
    } 
    
    if(final_page < starting_page){
      message("No final after the starting page")
      return("The bill was problematic (3)")
    } 
  }
  
  if(type == "proposal"){
    #Clean the heading for each page
    for(jj in seq_along(text)){text[jj] %<>% clean("head")}
    text[5]
    #Extract the bill's content from the whole document
    tmp1 <- c("Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+mit\\s+Zustimmung\\s+d(a|e)s",
              "Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+das\\s+(folgende|nachstehende)\\s+Gesetz",
              "Der\\s+(Deutsche\\s+)?Bundestag\\s+wolle\\s+beschließen")
    tmp1 <- paste(tmp1, sep="|", collapse = "|")
    
    tmp2 <- c("(Bonn|Berlin), den [[:digit:]]+\\. ")
    
    starting_page <- text %>% grep(pattern=tmp1) %>% .[1]
    final_page <- text %>% grep(pattern = tmp2) %>% .[1]
    final_page1 <- text %>% grep(pattern = "Begründung\\s*(:| zum Vertragsgesetz|-)?\\s*\n") %>% .[1]
    
    if(is.na(starting_page)){
      message("starting_page is missing")
      return("The bill was problematic (2B)")
    }
    
    if(is.na(final_page1)){
      message("final_page1 is missing")
      return("The bill was problematic (2A)")
    }
    
    if(starting_page < final_page1){
      final_page1 <- final_page1 - 1
    }
    
    if(is.na(final_page)){
      final_page <- final_page1
    } else {
      if(final_page == final_page1 && final_page < starting_page){
        message("No final after the starting page")
        return("The bill was problematic (3)")
      } else {
        if(final_page < starting_page | final_page1 < final_page){
          final_page <- final_page1
        }
      }
    }
  }
  
  text_new <- text[starting_page:final_page]
  
  if(type == "bill"){
    tmp <- paste0("((Die\\s+verfassungsmäßigen\\s+Recht.\\s+des\\s+Bundesrate)",
                  "|(Das\\s+vorstehende\\s+Gesetz\\s+wird\\s+hiermit\\s+ausgefertig)", 
                  ").*(\n.*)*")
    if(is.na(text_new[length(text_new)] %>% str_extract(tmp))){
      message("The final page could not be cleaned")
      return("The bill was problematic (6)")
    }
    text_new[length(text_new)] %<>% str_replace(tmp, " ")
    
    return(list(text=text_new, title = gesetz_title, date = date))
  }
  
  if(type == "proposal"){
    text_new <- text[starting_page:final_page]
    
    
    tmp <- grep(text_new, pattern = "Begründung zum Vertragsgesetz")
    if(!is.na(tmp[1])){
      text_new[[tmp]] %<>% str_split("(Begründung zum Vertragsgesetz)") %>% .[[1]] %>% .[1]
    }
    
    return(list(text=text_new, instit = institution, title = gesetz_title, date = date, author = author))
  }       
  return(NA)
}

desc_errors <- function(text_dt){
  errors <- text_dt[grep(text_dt, pattern = "The bill was problematic")]
  errors %<>% unlist
  errors %<>% str_extract("\\(.*\\)")
  
  e1 <- length(grep(errors, pattern = "(1)"))
  e2a <- length(grep(errors, pattern = "(2A)"))
  e2b <- length(grep(errors, pattern = "(2B)"))
  e2c <- length(grep(errors, pattern = "(2C)"))
  e3 <- length(grep(errors, pattern = "(3)"))
  e4 <- length(grep(errors, pattern = "(4)"))
  e5 <- length(grep(errors, pattern = "(5)"))
  e6 <- length(grep(errors, pattern = "(6)"))
  e7 <- length(grep(errors, pattern = "(7)"))
  
  perc <- function(x){return(paste0(" (",round(x*100/length(errors),0), "%) "))}
  
  perc_errors <- length(errors)*100/length(text_dt)
  
  report <- paste0("There was ", perc_errors, " % of errors. \n\n",
             "The repartition of errors is as follow: \n",
             "     ", e1 , perc(e1), " : PDF not read (1) \n",
             "     ", e2a , perc(e2a), " : 'Begründung' is missing (2A) \n",
             "     ", e2b , perc(e2b), " : 'Starting page' is missing (2B) \n",
             "     ", e2c , perc(e2c), " :  'final_page' is missing (2C) \n",
             "     ", e3 , perc(e3), " : Finals are before start (3) \n",
             "     ", e4 , perc(e4), " : Content not present (4) \n",
             "     ", e5 , perc(e5), " : International Treaty (5) \n",
             "     ", e6 , perc(e6), " : The bill was problematic (6) \n",
             "     ", e7 , perc(e7), " : Heading of pdf is not regular (7) \n"
  )
  cat(report)
  return(report)
}

clean_start <- function(list){
  
  text <- list$text
  
  for(jj in seq_along(text)){
    if(text[jj] == ""|(text[jj] %>% str_count("\n")) <= 6){next}
    tmp <- check_space_structure(text[jj], plot = F)
    tmp1 <- text[jj] %>% str_split("\n") %>% .[[1]]
    if(tmp1[length(tmp1)] == ""){tmp1 <- tmp1[-length(tmp1)]}
    start <- as.character(tmp == 1) %>% grep(pattern = "FALSE") %>% .[1]
    text_b <- text[jj] %>% str_split("\n") %>% .[[1]]
    text_b %<>% .[1:(length(text_b)-4)]
    tmp_b <- check_space_structure(text_b , plot = F)
    start_b <- as.character(tmp_b == 1) %>% grep(pattern = "FALSE") %>% .[1]
    if((start + 15) < start_b){
      for(kk in (length(tmp1)-5):length(tmp1)){
        tmp1[kk] <- paste0(strrep(" ", start_b - start), tmp1[kk])
      }
      start <- start_b
    }
    
    tmp1 <- substring(tmp1, first = start, last = 10000)
    tmp1 %<>% paste0(sep = "\n",collapse = "\n")
    text[jj] <- tmp1
  }
  
  list$text <- text
  
  return(list)
}

clean_title <- function(text){
  
  tmp1 <- c("Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+mit\\s+Zustimmung\\s+d(a|e)s",
            "Der\\s+(Deutsche\\s+)?Bundestag\\s+hat\\s+das\\s+(folgende|nachstehende)\\s+Gesetz",
            "Der\\s+(Deutsche\\s+)?Bundestag\\s+wolle\\s+beschließen")
  tmp1 <- paste(tmp1, sep="|", collapse = "|")
  
  med <- median(sapply(text$text[1] %>% str_extract_all("\n(\\s)+[^\\s]"), str_length))
  
  pattern <- text$text[1] %>% str_locate(tmp1) %>% .[1,1] %>% as.numeric
  text$text[1] <- substring(text$text[1], first = pattern, last = str_length(text$text[1]))
  text$text[1] <- paste0(strrep(" ", med), text$text[1])
  return(text)
}

split_column <- function(text, jj){
  for(kk in seq_along(text)){
    trim <- trim_column(text[kk])
    cat(paste0(jj, " | ", kk, " | ", trim, "\n"))
    if(!is.na(trim)){
      text1 <- text[kk] %>% clean("multiplebreaks") %>% str_split("\n") %>% .[[1]]
      col1 <- text1 %>% substring(first = 1, last = trim)
      col2 <- text1 %>% substring(first = trim, last = 10000)
      text1 <- c(col1, col2)
      text1 %<>% paste(collapse = "\n") %>% clean("multiplebreaks")
      text[kk] <- text1
    }
  }
  return(text)
}

trim_column <- function(text, benchmark = .94){
  if(text == ""){return(NA)}
  text %<>% str_replace_all("\n\n", "\n") %>% str_split("\n") %>% .[[1]]
  leng_text <- length(text)
  le <- ceiling(leng_text)/5
  med <- median(sapply(text, str_length, USE.NAMES = F) %>% sort(decreasing = T) %>% .[1:le])/2
  penalty <- ifelse(leng_text <= 7, 0, 5)
  tmp <- substring(text[1:(length(text)-penalty)], first = med - 20, last = med + 20)
  if(mean(sapply(tmp, str_length), na.rm = T) == 0){ return(NA)}
  tmp <- check_space_structure(tmp)
  tmp1 <- (tmp > benchmark) %>% as.character %>% grep(pattern = "TRUE")
  max <- (tmp == max(tmp)) %>% as.character %>% grep(pattern = "TRUE") %>% .[1]
  max <- max + med - 20
  if(length(tmp1) == 0){
    return(NA)
  } else {
    return(max)
  }
  
}

check_space_structure <- function(text, plot = F, round = F){
  if(length(text) == 1){
    text <- text %>% str_split("\n") %>% .[[1]]
  }
  
  len <- sapply(text, str_length, USE.NAMES=F)
  med_length <- median(len[len > 0], na.rm=T)
  
  tmp <- c()
  for(k in 1:med_length){
    tmp[k] <- paste0(substring(text, first = k, last = k), collapse = "")
  }
  tmp <- round(sapply(tmp, str_count, " ", USE.NAMES = F)/sapply(tmp, str_count, ".", USE.NAMES = F),2)
  
  if(round){tmp <- ifelse(tmp < .8, 0, tmp)}
  if(plot){plot(tmp)}
  
  return(tmp)
}

prepare_comparison <- function(text){
  names <- names(text)
  
  tmp <- paste0(text, collapse = "\n")
  
  bound <- tmp %>% str_locate_all("\n") %>% .[[1]] %>% .[,1]
  bound_low <- c(1, bound)
  bound_up <- c(bound, tmp %>% str_length)
  
  tmp %<>% str_replace_all("\n", " ") %>% .[[1]]
  
  return(list(names = names, bound_low = bound_low, bound_up = bound_up, text = tmp))
}

# check_time <- function(start, timelimit){
#   return(difftime(Sys.time(), start) %>% as.numeric() > timelimit)
# }



check_similarity <- function(text1, text2, n = 100, limit = 100000){ #, timelimit = 30
  #tart <- Sys.time() 
  
  final <- strrep(".", str_length(text2))
  
  while(n > 15){
  
    #print(n)
    
    #Check whether there is still space for match
    trig <- check_resting_place(final, 15)
    if(trig == "stop"){break}
    #Generate the vectors for each text
    tmp1 <- generate_vectors(text1, n, limit)
    if(length(tmp1) == 1){return(NA)}
    tmp2 <- generate_vectors(text2, n, limit)
    if(length(tmp2) == 1){return(NA)}
    
    #Identify match
    tmp <- match_two_vectors(tmp1, tmp2)
    tmp <- tmp[order(str_length(tmp), decreasing = T)]
    
    if(length(tmp) != 0 ){
      #print("Match found")
      
      for(jj in seq_along(tmp)){ 
        final <- replace_pos_match(text2, pattern = tmp[jj], final)
        text1 <- replace_pos_match(text1, pattern = tmp[jj])
        text2 <- replace_pos_match(text2, pattern = tmp[jj])
      }
    }
    
    n <- n - 10
    
  }
  return(list(final = final, text1 = text1, text2 = text2))
}

check_resting_place <- function(final, bound){
  tmp <- str_extract_all(final, "\\.{2,}") %>% .[[1]]
  if(length(tmp) == 0){
    message("The whole string has been matched")
    return("stop")}
  tmp <- max(sapply(tmp, str_length))
  
  if(tmp <= bound){
    message("The whole string has been matched")
    return("stop")}
  
  if(tmp < n){
    n <- tmp
    return(n)}
  return(n)
}

replace_pos_match <- function(string, pattern, string2 = NA){
  if(is.na(string2)){tmp <- string} else {tmp <- string2}
  
  position <- str_locate_all(string, fixed(pattern)) %>% .[[1]]
  
  if(nrow(position) != 1){
    return(tmp)
  }
  
  if(position[1,1] == 1 ){
    pre <- ""
  } else {
    pre <- tmp %>% substring(1 , position[1,1] - 1)
  }
  
  if(is.na(string2)){
    repl <- strrep(".", str_length(pattern))
  } else {
    repl <- pattern
  }
  
  if(position[1, 2] == str_length(string)){
    post <- ""
  } else {
    post <- tmp %>% substring(position[1,2] + 1,str_length(string))
  }
  
  tmp <- paste0(pre, repl, post)
  
  return(tmp)
}

generate_vectors <- function(text2, n, limit = 100000){
  if(grepl(text2, pattern = "\\.{10,}")){
    text2 %<>% str_split("(\n)|(\\.{10,})") %>% .[[1]]
    text2 <- text2[!str_length(text2) < n]
    tmp <- sapply(text2, generate_vectors, n, limit) %>% unlist(use.names = F)
    return(tmp)
  }
  
  tmp1 <- 1:(str_length(text2) - n + 1)
  tmp2 <- tmp1 + n - 1
  
  if(length(tmp1) > limit){
    message("String is to long")
    return(NA)}
  tmp <- substring(text2, first = tmp1, last = tmp2)
  return(tmp)
}

compare_length <- function(jj){
  
  tmp1 <- reprocess(readtext(file = names_entwurf[jj])[,2])
  tmp2 <- reprocess(readtext(file = names_gesetz[jj])[,2])
  
  print(sapply(tmp1, str_length))
  print(sapply(tmp2, str_length))
  
}

match_two_vectors <- function(tmp1, tmp2){
  tmp <- c()
  kk <- 1
  ll <- 1
  
  for(jj in seq_along(tmp1)){
    
    if(tmp1[jj] %in% tmp2) {
      tmp2[tmp2 == tmp1[jj]][1] <- "NA"
      
      tmp[kk] <- fixed(tmp1[jj])
      
      #Check if tmp[kk]is almost equal to tmp[kk+1]
      if(kk > 1){
        if(!is.na(tmp[kk-ll])){
          tmp3 <- substring(tmp[kk-ll], ll + 1, str_length(tmp[kk-ll]))
          tmp4 <- substring(tmp[kk],1, str_length(tmp[kk])-1)
          if(tmp3 == tmp4){
            tmp[kk-ll] <- paste0(tmp[kk-ll],tmp[kk] %>% str_extract(".$"))
            tmp[kk] <- NA
            ll <- ll + 1
          }
        }
      }
      kk <- kk + 1
      
    } else {
      ll <- 1
    }
  }
  if(length(tmp) == 0){
    return(tmp)
  } else {
    return(tmp[!is.na(tmp)])
  }
}

compute_change <- function(text){
  text %<>% str_replace_all("\\.{4}..\\.{4}", strrep(".", 10)) %>% .[[1]]
  changed <- text %>% str_split("\\.{4,}") %>% .[[1]] %>% str_length %>% sum
  total_length <- str_length(text)
  value <- changed/total_length
  return(value)
}

reprocess <- function(text, title = F){
  text %<>% split_article(title)
  if(title){return(text)}
  tmp <- names(text)
  text %<>% clean("tiret") %>% str_trim %>% clean("breaklines") %>% clean("multiplespace")
  names(text) <- tmp
  return(text)
}

split_article <- function(text, title = F){
  text %<>% clean("multiplebreaks") %>% str_split("\n") %>% .[[1]]
  bound <-  text %>% clean("space") %>% grep( pattern = "^((Artikel)|(Abschnitt)|(§))(\\d*)$")
  tit <- c("", text[bound +1])
  names <- text[bound]
  bound <- c(2, bound + 1)
  tmp <- c()
  for(jj in seq_along(bound)){
    if(jj == length(bound)){
      tmp[jj] <- paste0(text[(bound[jj]-1):length(text)], collapse = "\n")
      next
    }
    tmp[jj] <- paste0(text[(bound[jj]-1):(bound[jj+1]-2)], collapse = "\n")
  }
  
  names(tmp) <- c("", names)
  if(length(tmp) == 1){return(tmp)}
  if(tmp[1] == tmp[2]){
    tmp <- tmp[-1]
    tit <- tit[-1]
  }
  if(title){return(tit)}
  return(tmp)     
}
