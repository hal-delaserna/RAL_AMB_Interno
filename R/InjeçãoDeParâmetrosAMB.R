
############## ISTO AQUI FUNCIONOU !!!!! ###########################
rm(list = ls())
library(rvest)
library(httr)


# Desbugando o Rvest: O Rvest falha quando há campos sem atributo de nome preenchido ----

custom.submit_request <- function (form, submit = NULL) 
{
   is_submit <- function(x) {
      if (!exists("type", x) | is.null(x$type)){
         return(F);
      }
      tolower(x$type) %in% c("submit", "image", "button")
   } 
   submits <- Filter(is_submit, form$fields)
   if (length(submits) == 0) {
      stop("Could not find possible submission target.", call. = FALSE)
   }
   if (is.null(submit)) {
      submit <- names(submits)[[1]]
      message("Submitting with '", submit, "'")
   }
   if (!(submit %in% names(submits))) {
      stop("Unknown submission name '", submit, "'.\n", "Possible values: ", 
           paste0(names(submits), collapse = ", "), call. = FALSE)
   }
   other_submits <- setdiff(names(submits), submit)
   method <- form$method
   if (!(method %in% c("POST", "GET"))) {
      warning("Invalid method (", method, "), defaulting to GET", 
              call. = FALSE)
      method <- "GET"
   }
   url <- form$url
   fields <- form$fields
   fields <- Filter(function(x) length(x$value) > 0, fields)
   fields <- fields[setdiff(names(fields), other_submits)]
   values <- pluck(fields, "value")
   names(values) <- names(fields)
   list(method = method, encode = form$enctype, url = url, values = values)
}
library(R.utils)
reassignInPackage("submit_request", "rvest", custom.submit_request)
#_____________________________________________________________________________

#     Tela Inicial ----
url       <-"http://amb.dnpm.gov.br/SCA/Site/Login.aspx?ReturnUrl=%2fAMB%2fSite%2fPesquisar%2fPesquisarRals.aspx"   
pgSession <- rvest::html_session(url = url)
pgForm    <- html_form(x = pgSession)[[1]]  # x = node, node set or document

#_____Logando ----
filled_form <- set_values(form = pgForm,
                          `ctl00$ContentPlaceHolderCorpo$TextBoxUsuario` = "humberto.serna", 
                          `ctl00$ContentPlaceHolderCorpo$TextBoxSenha` = "finiscoronatopus")

PesquisarRalSession <- submit_form(session = pgSession,
                                    form = filled_form,
                                    submit = 'ctl00$ContentPlaceHolderCorpo$ButtonOk')
#_____________________________________________________________________________

# Tela Pesquisar RALs ----

# html_form retornará uma lista. Assim, para um objeto 'form' devemos usar [[1]]
PesquisarRalForm <- html_form(x = PesquisarRalSession)[[1]]  
filled_form <- set_values(form = PesquisarRalForm,
                          `ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$txtDNPM` = '826.947/2013')
#_____Pesquisar                      
PesquisarRalSession <- submit_form(session = PesquisarRalSession, 
                          form = filled_form,
                          submit = "ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$btnPesquisar")


#_____Abrir RAL

# html_form retornará uma lista. Assim, para um objeto 'form' devemos usar [[1]]
PesquisarRalForm <- html_form(x = PesquisarRalSession)[[1]]  
# Pesquisar                      
AbrirRalSession <- submit_form(session = PesquisarRalSession, 
                               form = PesquisarRalForm,
                               submit = "ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$gridViewRal$ctl02$btnEditar")





#  Aqui é necessário uma autenticação: unir '(...)br/AMB/Site/Default.aspx?' com o 'token=%2FzL(...)'. 
RAL_Session <- jump_to(x = AbrirRalSession,
                       url = 'http://amb.dnpm.gov.br/AMB/Site/Default.aspx)?token=%2FzLO%2F4%2F%2F%2BLyc8z7llgN0pUhjcDmIgetx9gON%2BqV5GUmKMP4ImQHSkjG0WCcsT7AMUF3boxHSA4XHs3ALp6XqVysSSSqBYgID827Lf9e4OoVh9pAjW8WV1RGwjnQcxzq0D%2FIWqM4U6mAYSyqH7Gicu7A0Bk6uqGT5fFecKt36lQV5kmzTOEnkw8MGAv0qBohwb1Pslt1ug2o%3D',
                        
                        )








#_______________________________________________________________________________
# EXEMPLO Rvest

movie <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(movie, "#titleCast span")
html_text(cast)
html_name(cast)
html_attrs(cast)
html_attr(cast, "class")






### Expressão para pegar o nome do campo
AbrirRalSession %>% 
   html_nodes(xpath = '/html/head/script[2]') %>% 
   html_attr(name = 'name')




### _________________________________________
VIEWSTATE <- AbrirRalSession %>% 
   read_html() %>% 
   html_node(css = '#__VIEWSTATE') %>% 
   html_attr('value')

EVENTVALIDATION <- AbrirRalSession %>% 
   read_html() %>% 
   html_node(css = '#__EVENTVALIDATION') %>% 
   html_attr('value')

VIEWSTATEGENERATOR <- AbrirRalSession %>% 
   read_html() %>% 
   html_node(css = '#__VIEWSTATEGENERATOR') %>% 
   html_attr('value')
### __________________________________________________________


#____________________________________________________________________________________________
params <- list(`__EVENTTARGET` = "",
               `__EVENTARGUMENT` = "",
               `__LASTFOCUS` = "",
               `__VIEWSTATE` = VIEWSTATE,
               `__VIEWSTATEGENERATOR` = VIEWSTATEGENERATOR,
               `__VIEWSTATEENCRYPTED` = "",
               `__EVENTVALIDATION` = EVENTVALIDATION,
               `ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$ContentPlaceHolderCorpo$txtDNPM` = "826.947/2013")



resposta <- httr::POST(PesquisarRalSession,
                       body = params,
                       encode = 'form') %>% 
   xml2::read_html()
#____________________________________________________________________________________________
