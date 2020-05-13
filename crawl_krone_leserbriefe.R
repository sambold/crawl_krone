################################################################################
#
#   KRONE LESERBRIEF-CRAWLR
#
#   Crawlt und speichert die Krone-Leserbrief-Teaser in einer Tabelle
#   (crawl_krone). Bietet Optionen den Crawl-Zeitraum einzuschr?nken bzw. 
#   die Leserbrief-Teaser nur bis zum zuletzt gecrawlten Datum zu druchsuchen.
#   In einem zweiten Schritt wird der Volltext der Leserbriefe + Zusatzinfos
#   abgerufen, ?ber den Link mit der Teaser-Tabelle gematcht, gespeichert und 
#   wieder zur?ckgegeben (crawl_leserbriefe). 
# 
#   Es besteht die M?glichkeit ?ber das TOR-Netzwerk zu crawlen (hidden_crawl).
#   Die ben?tigten User Agents k?nnen entweder selbst definiert, leer gelassen 
#   oder ?ber die Funktion get_ua eingelesen werden.
#
################################################################################

################################################################################
#   INIT
################################################################################

# Packages laden
library(magrittr)

# Funktionen laden
# Crawl ?ber TOR-Netzwerk oder eigene IP
hidden_crawl <- function(url,
                         ua="",
                         proxy="socks5h://127.0.0.1:9153",
                         hidden=T,
                         info=F,
                         verbose=F,
                         timeout=5,
                         encoding="UTF-8",
                         wait=0,
                         try.con=10,
                         ssl=F){
    # packages: RCurl, rvest, magrittr,
    Sys.sleep(wait) # Pause vor jedem Crawl
    webpage <- NA
    # Optionen f?r Crawl ?ber TOR
    opt <- list(proxy=proxy,
                useragent=ua,
                followlocation=TRUE,
                referer="",
                timeout=timeout,
                verbose=verbose,
                ssl.verifypeer=ssl)
    # Optionen f?r Crawl mit eigener IP
    opt.unhide <- list(followlocation=TRUE,
                       referer="",
                       timeout=timeout,
                       verbose=verbose,
                       ssl.verifypeer=ssl)
    if (hidden == TRUE){ # Crawl ?ber TOR-Netzwerk (wenn m?glich)
        con.tries <- 0
        #Verbindungsversuche bis threshold erreicht/Verbindung erfolgreich
        while ((length(webpage)==1) & (con.tries < try.con)){
            con.tries <- con.tries + 1
            webpage <- tryCatch({
                webpage <- RCurl::getURL(url=url,.opts=opt) %>%
                    xml2::read_html(encoding=encoding)
            }, error=function(e){
                webpage <- NA
            })
        }
        if (length(webpage)==2){ # TOR-Verbidung erfolgreich
            if (info == TRUE) cat(" # TOR-IP\n")
        } else { # TOR-Verbindung fehlgeschlagen, eigene IP verwenden
            if (info == TRUE) cat(" # Eigene IP\n")
            webpage <- RCurl::getURL(url=url,.opts=opt.unhide) %>%
                xml2::read_html(encoding=encoding)
        }
    } else { # eigene IP f?r Crawl verwenden
        if (info == TRUE) cat(" # Eigene IP\n")
        webpage <- RCurl::getURL(url=url,.opts=opt.unhide) %>%
            xml2::read_html(encoding=encoding)
    }
    return(webpage)
}

# Liste mit User Agents f?r bestimmte Browser abrufen
get_ua <- function(url="https://developers.whatismybrowser.com/useragents/explore/software_name/",
                   browser=c("chrome","firefox")){
    # Liste mit unterschiedlichen User Agents einlesen
    # packages: magrittr, rvest
    ua.list <- list()
    for (i in browser){
        ua.list <- xml2::read_html(paste0(url,i)) %>%
            rvest::html_nodes(css=".useragent a") %>%
            rvest::html_text(trim=T) %>%
            append(ua.list)
    }
    return(unlist(ua.list))
}

# Krone-Leserbrief-Teaser crawlen
crawl_krone <- function(url.base = "https://www.krone.at/das-freie-wort/",
                        start.date = format(Sys.Date(),"%Y/%m/%d"),         # Format: yyyy/mm/dd
                        ende.date = "1990/01/01",                           # Format: yyyy/mm/dd
                        ende.thld = 7,                                      # Tage ohne Leserbrief (Abbruch)
                        ua.list=get_ua(),                                   # Liste mit User Agents
                        bu=T,                                               # Ergebnisse zwischenspeichern
                        temp=F,                                             # Zwischengespeicherte Ergebnisse verwenden
                        temp.date=F,                                        # Von aktuellstem Datum in zwischengespeicherten Ergebnissen starten
                        bu.step=10,                                         # Alle 10 Crawls zwischenspeichern
                        bu.path=getwd(),                                    # Pfad f?r gespeicherte Ergebnisse
                        ...){                                               # Optionen f?r hidden_crawl
    # INIT
    # Start + 1 (vermeidet Abfrage f?r ersten Durchgang in Schleife)
    url.date <- lubridate::as_date(start.date,"%Y/%m/%d") %>%
        magrittr::add(1) %>%
        format("%Y/%m/%d")
    ende <- FALSE
    ende.cnt <- 0
    i <- 0
    # Temp-Dir erzeugen
    if (!file.exists(paste0(bu.path,"/RData"))) dir.create(paste0(bu.path,"/RData"))
    
    # Temp laden, falls vorhanden
    if (temp & file.exists(paste0(bu.path,"/RData/krone_leserbriefe.RData"))){
        load(paste0(bu.path,"/RData/krone_leserbriefe.RData"))
    } else {
        brief.df <- dplyr::data_frame()
    }
    # Enddatum aus Temp-Bestand (aktuellstes Datum des Temp-Bestands)
    if (temp & temp.date & "date" %in% colnames(brief.df)){
        ende.date <- lubridate::as_date(brief.df$date,"%Y/%m/%d") %>%
            max() %>%
            format("%Y/%m/%d")
    }
    # Startdatum <= Enddatum: so setzen, dass zumindest 1 Tag gecrawlt wird
    if (start.date<=ende.date) {
        ende.date <- lubridate::as_date(start.date,"%Y/%m/%d") %>%
            magrittr::subtract(1) %>%
            format("%Y/%m/%d")
    }
    
    # Crawl starten
    while (!ende){
        # Link zusammensetzen
        url.date <- lubridate::as_date(url.date,"%Y/%m/%d") %>%
            magrittr::subtract(1) %>%
            format("%Y/%m/%d")
        if (url.date < ende.date){
            cat("## Angegebenes Enddatum erreicht. Es wurden die Leserbriefe vom",
                start.date,"bis zum",ende.date,"gespeichert\n")
            ende <- TRUE
        } else {
            url <- paste0(url.base,url.date)
            # Info zum aktuellen Tag
            cat("## Leserbriefe vom",url.date,"abrufen ... \n")
            # Webpage abrufen
            wp <- hidden_crawl(url,ua=sample(ua.list,size=1),...)
            # Pr?fen, ob Daten vorhanden sind
            ende.check <- wp %>%
                rvest::html_nodes(css=".reader-letter__list-headline") %>%
                length()
            if (ende.check>0) {
                # Z?hler f?r leere Seiten zur?cksetzen
                ende.cnt <- 0
                # relevante Bereiche auslesen
                header <- wp %>%
                    rvest::html_nodes(css=".reader-letter__list-headline") %>%
                    rvest::html_text()
                teaser <- wp %>%
                    rvest::html_nodes(css=".reader-letter__list-text") %>%
                    rvest::html_text()
                author <- wp %>%
                    rvest::html_nodes(css=".reader-letter__list-author") %>%
                    rvest::html_text()
                link <- wp %>%
                    rvest::html_nodes(css=".reader-letter__list-link") %>%
                    rvest::html_attr("href")
                # relevante Bereiche in data_frame zusammenf?hren u an Bestand anh?ngen
                brief.df <- brief.df %>%
                    dplyr::bind_rows(dplyr::data_frame(date=url.date,
                                                       header=header,
                                                       teaser=teaser,
                                                       author=author,
                                                       link=link,
                                                       timestamp.teaser=Sys.time()))
                # Schleifenz?hler erh?hen/Ergebnisse zwischenspeichern
                i <- i+1
                if (bu & (i==1 || i%%10==0)){
                    save(brief.df,file=paste0(bu.path,"/RData/krone_leserbriefe.RData"))
                }
            } else {
                # Z?hler f?r leere Seiten erh?hen
                # bei ende.thld-?berschreitung, Abbruch
                cat(" # An diesem Tag wurden keine Leserbriefe gefunden ... \n")
                ende.cnt <- ende.cnt + 1
                if (ende.cnt > ende.thld){
                    cat("## An",ende.thld,"aufeinanderfolgenden Tagen wurde kein",
                        "Leserbrief gefunden. Der Crawl wurde daher beendet... \n")
                    ende <- TRUE
                } 
            } 
        }
    }
    # Endergebnis zwischenspeichern/ausgeben
    if (bu) save(brief.df,file=paste0(bu.path,"/RData/krone_leserbriefe.RData"))
    return(brief.df)
}                                           

# Volltext-Leserbriefe crawlen
crawl_leserbriefe <- function(df,                                           # data_frame (min. Spalte 'link')
                     ua.list=get_ua(),                                      # Liste mit User Agents
                     bu=T,                                                  # Ergebnisse zwischenspeichern
                     temp=F,                                                # Zwischengespeicherte Ergebnisse verwenden
                     bu.step=10,                                            # Alle 10 Crawls zwischenspeichern
                     bu.path=getwd(),                                       # Pfad f?r gespeicherte Ergebnisse
                     ...){                                                  # Optionen f?r hidden_crawl
    
    # INIT
    i <- 0
    # Tempbestand laden, falls m?glich
    if (temp & file.exists(paste0(bu.path,"/RData/krone_leserbriefe_ext.RData"))){
        load(paste0(bu.path,"/RData/krone_leserbriefe_ext.RData"))
    } else {
        brief.df <- dplyr::data_frame()
    }
    # Nur Volltext-Leserbriefe crawlen, die nicht schon gecrawlt wurden
    if ("link" %in% colnames(brief.df)){
        url.list <- df$link[!df$link %in% brief.df$link]
    } else {
        url.list <- df$link
    }
    if (length(url.list)==0) cat("## Keine neuen Leserbriefe vorhanden ... \n")
    # Crawl starten
    for (url in url.list){
        wp <- hidden_crawl(url,ua=sample(ua.list,size=1),...)
        # Schleifenz?hlerInfo zum aktuellen Tag
        i <- i+1
        cat("## [",i,"/",length(url.list),"] Leserbriefe einlesen (url:",url,") ... \n")
        # Daten aufbereiten
        header <- wp %>%
            rvest::html_nodes(css=".kmm-reader-letter-box") %>% 
            rvest::html_nodes(css="h1") %>%
            rvest::html_text()
        text <- wp %>%
            rvest::html_nodes(css=".kmm-reader-letter-box") %>% 
            rvest::html_nodes(css=".reader-letter__text") %>%
            rvest::html_text(trim=T)
        detail <-  wp %>%
            rvest::html_nodes(css=".kmm-reader-letter-box") %>% 
            rvest::html_nodes(css=".reader-letter__authordate") %>%
            rvest::html_text(trim=T)
        # ort <- detail %>%
        #     strsplit(",|\n|\t") %>%
        #     unlist() %>%
        #     .[.!=""] %>%
        #     .[2] %>%
        #     gsub("^ *| *$","",.)
        ort <- detail %>%
                stringr::str_match("(?:^.*, )(.*)(?:\\n)") %>%
                .[,2]
        brief.df <- brief.df %>%
            dplyr::bind_rows(dplyr::data_frame(header=header,
                                               text=text,
                                               ort=ort,
                                               detail=detail,
                                               link=url,
                                               timestamp.ext=Sys.time()))
        
        # Ergebnisse zwischenspeichern
        if (bu & (i==1 || i%%10==0)){
            save(brief.df,file=paste0(bu.path,"/RData/krone_leserbriefe_ext.RData"))
        }
    }
    # Endergebnis zwischenspeichern/ausgeben
    if (bu) save(brief.df,file=paste0(bu.path,"/RData/krone_leserbriefe_ext.RData"))
    # Volltextleserbriefe + Zusatzinfo mit Teaser-Bestand matchen 
    # Duplikate bereinigen
    brief.df <- df %>%
        dplyr::left_join(brief.df %>%
                             dplyr::select(link,text,ort,detail,timestamp.ext),
                         by=c("link"="link")) %>%
        #dplyr::filter(!duplicated.data.frame(.))
        dplyr::arrange(desc(timestamp.ext)) %>%
        dplyr::distinct(date,header,teaser,author,link,text,ort,detail)
    
    return(brief.df)
}

################################################################################
#   KRONE-CRAWL starten
################################################################################

brief.df <- crawl_krone(info=T,temp=T,temp.date=T) %>%
    crawl_leserbriefe(info=T,temp=T)

################################################################################
#   ?BERSICHT
################################################################################

# ?bersicht
str(brief.df)
summary(brief.df)
# inhaltliche Duplikate
brief.df %>%
    dplyr::select(header,teaser,author,text,ort) %>%
    duplicated.data.frame(.) %>%
    sum()

# Crawl-Duplikate
brief.df %>%
    dplyr::select(date,header,teaser,author,link,text,ort,detail) %>%
    duplicated.data.frame(.) %>%
    sum()

# Verteilungen
brief.df %>%
    dplyr::mutate(date=lubridate::as_date(date,"%Y/%m/%d")) %>%
    ggplot2::ggplot(ggplot2::aes(date)) + 
    ggplot2::geom_histogram(stat="count")
brief.df %>%
    dplyr::count(date) %>%
    dplyr::arrange(-n) %>%
    dplyr::top_n(25,n) %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(date,n),y=n)) + 
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip()

brief.df %>%
    dplyr::count(author) %>%
    dplyr::arrange(-n) %>%
    dplyr::top_n(25,n) %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(author,n),y=n)) + 
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip()

brief.df %>%
    dplyr::count(header) %>%
    dplyr::arrange(-n) %>%
    dplyr::top_n(25,n) %>%
    dplyr::mutate(teaser=substr(header,1,20)) %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(header,n),y=n)) + 
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip()

brief.df %>%
    dplyr::filter(!is.na(ort)) %>%
    dplyr::count(ort) %>%
    dplyr::arrange(-n) %>%
    dplyr::top_n(25,n) %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(ort,n),y=n)) + 
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip()

brief.df %>%
    dplyr::count(header) %>%
    dplyr::arrange(-n)
brief.df %>%
    dplyr::count(teaser) %>%
    dplyr::arrange(-n) %>%
    dplyr::mutate(teaser=substr(teaser,1,100))
brief.df %>%
    dplyr::count(text) %>%
    dplyr::mutate(text=substr(text,1,100)) %>%
    dplyr::arrange(-n)

brief.df %>%
    dplyr::filter(!is.na(ort)) %>%
    dplyr::count(ort,header) %>%
    dplyr::arrange(-n)

# Textanalyse

# Bigram je Tag
p <- brief.df %>%
    dplyr::select(date,text) %>%
    tidytext::unnest_tokens(bigram, text, token="ngrams",n=2) %>%
    dplyr::count(date,bigram,sort=T) %>%
    tidytext::bind_tf_idf(bigram,date,n) %>%
    dplyr::group_by(date) %>%
    dplyr::top_n(10,tf_idf) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date,tf_idf) %>%
    dplyr::mutate(order=row_number())
p %>%
    ggplot2::ggplot(ggplot2::aes(y=tf_idf,x=order,fill=date)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::facet_wrap(~date,scale="free") +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(breaks=p$order,
                       labels=gsub("(.{19})", "\\1\n", p$bigram),
                       expand=c(0,0)) 

# Sentiment
path <- "C:/Users/sambo/Dropbox/SentiWS"
neg <- read.csv(file=paste0(path,"/SentiWS_v1.8c_Negative.txt"),
                sep="\t") %>%
    dplyr::as_data_frame() %>%
    magrittr::set_colnames(c("WORT","WERT","FORMEN")) %>%
    dplyr::select(WORT,WERT) %>%
    dplyr::mutate(WORT=stringr::str_match(WORT,"(^\\w*)(?:|)")[,2])
pos <- read.csv(file=paste0(path,"/SentiWS_v1.8c_Positive.txt"),
                sep="\t") %>%
    dplyr::as_data_frame() %>%
    magrittr::set_colnames(c("WORT","WERT","FORMEN")) %>%
    dplyr::select(WORT,WERT) %>%
    dplyr::mutate(WORT=stringr::str_match(WORT,"(^\\w*)(?:|)")[,2])
ges <- neg %>%
    dplyr::bind_rows(pos)

brief.df %>%
    dplyr::select(date,text) %>%
    tidytext::unnest_tokens(word,text) %>%
    dplyr::count(date,word) %>%
    dplyr::inner_join(ges,by=c("word"="WORT")) %>%
    dplyr::mutate(senti_score=n*WERT) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(senti_sum=sum(senti_score)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date,senti_sum) %>%
    dplyr::distinct() %>%
    dplyr::mutate(date=lubridate::as_date(date,"%Y/%m/%d")) %>%
    ggplot2::ggplot(ggplot2::aes(y=senti_sum,x=date,fill=senti_sum)) +
    ggplot2::geom_bar(stat="identity") + 
    ggplot2::geom_smooth()
