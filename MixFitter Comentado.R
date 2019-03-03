# 1. Configuración del entorno ----
library(shiny)
library(MASS)
library(mixtools)
library(goftest)
library(ggplot2)

# 2. Interfaz de usuario ----
ui <- fluidPage(
  
  # Título y presentación
  titlePanel("Mix Fitter"),
  h4("La aplicación para ajustar distribuciones de mixturas"),
  h5("Autor: Juan Sebastian Numpaque Cano. Universidad Nacional de Colombia."),
  h5(HTML("Contacto: <em>jsnumpaquec@unal.edu.co</em>")),
  
  # La interfaz se conformará de una barra lateral y un panel principal
  sidebarLayout(
    
    # 2.1 Barra lateral ----
    sidebarPanel(
      
      # 2.1.1 Panel de datos ----
      
      # Título y descripción
      h4("Panel de datos"),
      h6("Aquí podrás cargar un archivo con los datos que quieras analizar."),
      
      # Checkbox: ¿Ocultar panel de datos?
      checkboxInput("ocultarDatos", "Ocultar panel de datos", FALSE),
        
        # El panel se muestra si no está seleccionada la opción de "ocultar panel de datos"
        conditionalPanel("!input.ocultarDatos",
        
        # 2.1.1.1 Configuración de la lectura de datos ----
                         
        # Botón para cargar archivos de datos. Internamente se referirá a este como "archivoCargado"
        fileInput("archivoCargado", "Elige un archivo CSV", multiple = TRUE, 
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
        
        # Descargar una plantilla ejemplo con los datos de "faithful"
        h6("¿No sabes cómo debe ser el archivo? Descarga un ejemplo:"),
        downloadButton("DownloadFaithful",label="Plantilla de datos"),
        
        # Línea horizontal
        tags$hr(),
        
        # Checkbox: ¿Debe leerse la primera fila como los nombres de las variables?
        checkboxInput("header", "Mi tabla tiene encabezados", TRUE),
        
        # Botón de selección: ¿cuál es el separador del archivo .csv?
        radioButtons("sep", "Separador",
                     choices = c("Coma" = ",",
                                 "Punto y coma" = ";", 
                                 "Tabulación" = "\t"),
                     selected = ","),
        
        # Línea horizontal
        tags$hr(),
        
        # Ingresar número: ¿cuál de las columnas del archivo se desea analizar?
        numericInput("columna", "Columna por analizar", value = 2, step=1, min = 1, max=ncol(faithful)),
        
        # Línea horizontal
        tags$hr(),
        
        # 2.1.1.2 Resumen de datos ----
        
        # ¿Cuántos datos lee la aplicación y de qué tipo son?
        h4("Resumen de tus datos"),
        uiOutput("n.datos"),
        uiOutput("clase.datos"),
        
        # Línea horizontal
        tags$hr(),
        
        # Vista previa de los datos: ¿cómo se está leyendo el archivo?
        h4("Dale un vistazo a tus datos"),
        
        # Botón de selección: ¿deben mostrarse en la vista previa todos los datos o solo los primeros?
        radioButtons("disp", "Mostrar",
                     choices = c("Primeras filas" = "head",
                                 "Todos los datos" = "all"),
                     selected = "head"),
        
        # Tabla con la vista previa descrita
        tableOutput("contents") 
      )
    ),
    
    # 2.2 Panel principal ----
    mainPanel(
      
      # 2.2.1 Panel de visualización ----
      
      # Se muestra el módulo 1 (histograma) o el 2 (empírica), según el usuario decida
      navbarPage(
        title = "Gráfico de tus datos",
        
        # Módulo 1
        tabPanel("Histograma",
          
          # Gráfico de histogramas y funciones de densidad       
          plotOutput(outputId = "distPlot")
          
        ),
        
        # Módulo 2
        tabPanel("Empírica", 
                 
          # Gráfico de distribución empírica y funciones de distribución
          plotOutput(outputId = "plotEmpirica")
          
        )
      ),
      
      # Se divide la interfaz en cuatro secciones: derecha, centro-derecha 
      # centro-izquierda e izquierda. Las tres primeras se dejan vacías.
      fluidRow(column(9),
        
        # En la sección derecha se muestra el botón para ajustar todas las distribuciones
        column(3, actionButton("fit.all", "Ajustar todas"))
      ),
      
      # Línea horizontal
      tags$hr(),
      
      # 2.2.2 Panel de ajuste ----
      
      # Título y descripción
      h4("Panel de ajuste"),
      h5("Aquí podrás ajustar una distribución a tus datos"),
      
      # Se divide esta parte de la interfaz en tres secciones: izquierda, centro y derecha.
      fluidRow(
        
        # 2.2.2.1 Sección izquierda ----
        column(4,
               
           # Checkbox: ¿ocultar panel de ajuste?
           checkboxInput("ocultarAjuste", "Ocultar panel de ajuste", FALSE),
           
           # Se muestra esta sección si no está seleccionada la opción de "ocultar panel de ajuste"
           conditionalPanel("!input.ocultarAjuste",
             
           # La sección contiene únicamente una lista desplegable para elegir entre ajustar mixturas
           # o distribuciones de probabilidad comunes
           selectInput("TipoDist", "Tipo de distribución por ajustar",
                         choices = list("Paramétrica" = "Parametrica","Mixtura"),
                         selected = "Mixtura")
           )
        ),
        
        # 2.2.2.2 Sección central ----
        column(4,
               
           # Se muestran las siguientes opciones si no está seleccionada la opción de "ocultar panel de ajuste":
           
           # A. Cuando se elige ajustar una distribución paramétrica (no mixtura)
           conditionalPanel("input.TipoDist=='Parametrica' & !input.ocultarAjuste",
             
             # Lista desplegable: elegir tipo de distribución paramétrica
             selectInput("Dist", "Distribución",
                         list("Normal","Exponencial","Gamma",
                              "Uniforme", "Beta", "Lognormal", "Weibull"))
           ),
           
           # B. Cuando se elige ajustar una mixtura
           conditionalPanel("input.TipoDist=='Mixtura' & !input.ocultarAjuste",
             
             # Lista desplegable del primer componente de la mixtura 
             selectInput("CompMixtura1", "Componente 1",
                         list("Normal","Uniforme", "Exponencial", "Gamma")),
             
             # B.1 Cuando el primer componente es normal
             conditionalPanel("input.CompMixtura1=='Normal'",
               
               # Lista desplegable del segundo componente (normal, uniforme o exponencial)
               selectInput("CompMixtura2N", "Componente 2",
                           list("Normal","Uniforme", "Exponencial"))
             ),
             
             # B.2 Cuando el primer componente es uniforme
             conditionalPanel("input.CompMixtura1=='Uniforme'",
               
               # Lista desplegable del segundo componente (solo normal)
               selectInput("CompMixtura2U", "Componente 2",
                           list("Normal"))
             ),
             
             # B.3 Cuando el primer componente es exponencial
             conditionalPanel("input.CompMixtura1=='Exponencial'",
               
               # Lista desplegable del segundo componente (solo normal)
               selectInput("CompMixtura2E", "Componente 2",
                           list("Normal"))
             ),
             
             # B.4 Cuando el primer componente es gamma
             conditionalPanel("input.CompMixtura1=='Gamma'",
               
               # Lista desplegable del segundo componente (solo gamma)
               selectInput("CompMixtura2G", "Componente 2",
                           list("Gamma"))
             )
           )
        ),
        
        # 2.2.2.3 Sección derecha ----
        column(4,
               
           # Ingresar número: ¿con qué semilla se generarán los números aleatorios?
           # Nota: se muestra incluso si se oculta el panel de ajuste, pues afecta el de simulación
           numericInput("seed","Semilla",value=1,min=0,step = 1),
           
           # Se muestran las siguientes opciones si no está seleccionada la opción de "ocultar panel de ajuste"
           # y si se escoge ajustar una mixtura
           conditionalPanel("!input.ocultarAjuste & input.TipoDist == 'Mixtura'",
                            
              # A. Cuando se elige ajustar una mixtura de normales              
              conditionalPanel("input.CompMixtura1 == 'Normal' & input.CompMixtura2N == 'Normal'",
                
                # Lista desplegable: ¿estimar parámetros con el algoritmo EM o con k-means? 
                selectInput("AlgMixNormal", "Método",
                            list("Algoritmo EM"="EMAlg","K means"="kmeansAlg")),
                
                # A.1 Cuando se escoge utilizar el algoritmo EM para ajustar la mixtura de normales
                conditionalPanel("input.AlgMixNormal == 'EMAlg'",
                  
                  # Ingresar número: ¿cuántas iteraciones realiza como máximo el algoritmo EM?
                  numericInput("niter", "Iteraciones (máx)", 100)  
                )
              ),
              
              # B. Cuando se elige ajustar una mixtura con un componente uniforme y uno normal
              conditionalPanel(
                "(input.CompMixtura1 == 'Normal' & input.CompMixtura2N == 'Uniforme') |
                 (input.CompMixtura1 == 'Uniforme' & input.CompMixtura2U == 'Normal')",
                
                # Ingresar número: ¿cuántas iteraciones realiza como máximo el algoritmo EM?
                numericInput("NormUnifIter", "Iteraciones (máx)", 100)           
              ),
           
              # C. Cuando se elige ajustar una mixtura con un componente exponencial y uno normal
              conditionalPanel(
                "(input.CompMixtura1 == 'Normal' & input.CompMixtura2N == 'Exponencial') |
                 (input.CompMixtura1 == 'Exponencial' & input.CompMixtura2E == 'Normal')",
                 
                # Ingresar número: ¿cuántas iteraciones realiza como máximo el algoritmo EM?
                numericInput("NormExpIter", "Iteraciones (máx)", 100)       
              ),
          
              # D. Cuando se elige ajustar una mixtura con dos componentes gamma
              conditionalPanel("input.CompMixtura1 == 'Gamma'",
                
                # Ingresar número: ¿cuántas iteraciones realiza como máximo el algoritmo EM?
                numericInput("gammaiter", "Iteraciones (máx)", 10)
              )
            )
         )
      ),  
      
      # Línea horizontal
      tags$hr(),
    
      # 2.2.3 Panel de simulación ----
      
      # Título y descripción
      h4("Panel de simulación"),
      h5("Aquí podrás generar datos con la distribución ajustada"),
      
      # Se divide esta parte de la interfaz en tres secciones: izquierda, centro y derecha.
      fluidRow(
        
        # 2.2.3.1 Sección izquierda ----
        column(4,
               
          # Ingresar número: ¿cuántos datos se van a simular?
          h5(HTML("<b>Datos</b>")),
          numericInput("sims",label = NULL,value=100,min=1,step = 1)
        ),
        
        # 2.2.3.2 Sección central ----
        column(4,
              
          # Botón: Generar datos a partir de la distribución ajustada
          h5(HTML("<b>Simular</b>")),
          actionButton("simular", "Generar datos")
        ),
        
        # 2.2.3.3 Sección central ----
        column(4, 
          
          # Botón de descarga: Descargar archivo de datos generados     
          h5(HTML("<b>Descargar</b>")),
          downloadButton('downloadData', 'Descargar datos')
        )
      ),
      
      # Línea horizontal
      tags$hr(),
        
      # 2.2.4 Panel de resultados ----
      
        # 2.2.4.1 Parámetros ajustados ----
      
        # Título y descripción
        h4("Parámetros"),
        h5("Estos son los parámetros que estimamos para tu distribución"),
      
        # Tabla que muestra los parámetros ajustados
        tableOutput("parametros"),
        
        # Línea horizontal
        tags$hr(),
    
        # 2.2.4.2 Pruebas de bondad de ajuste ----
      
        # Título y descripción
        h4("Pruebas de bondad de ajuste"),
        h5("Aquí podrás saber qué tan bueno es el ajuste obtenido"),  
    
        # Tabla que muestra los resultados de las pruebas de bondad de ajuste
        tableOutput("gof"),
      
        # Línea horizontal 
        tags$hr(),
    
      # 2.2.5 Panel de ajustes adicionales ----
      
      # Título y descripción
      h4("Ajustes adicionales"),
      h5("Aquí podrás cambiar algunas opciones de visualización"),
        
      # Botón de selección: ¿cómo debe calcularse el número de intervalos del histograma?
      radioButtons("metodoBins", "Número de intervalos del histograma",
                   choices = c("Método base de R" = "rBins",
                               "Sturges" = "sBins",
                               "Knuth" = "kBins"),
                   selected = "rBins")
      )
  )
)
  


# 3. Servidor ----
server <- function(input, output, session) {

  # 3.1 Definición de valores reactivos (similares a las variables globales) ----
  
  pvalues<-reactiveValues(p=NULL)
  
  # 3.2 Funciones ----
  
  
  
  # 3.3 Observadores ----
  observeEvent(input$fit.all, {
    if(is.null(input$archivoCargado$datapath)){
      df<-faithful
    }
    else{
      df <- read.csv(input$archivoCargado$datapath,
                     header = input$header,
                     sep = input$sep)
    }
    updateNumericInput(session,"columna",max = ncol(df))
    df<-df[!is.na(df[,input$columna]),input$columna,drop=F]
    x<-as.vector(t(df))
    all.pval<-NULL
    set.seed(input$seed)
    ajustar.normal(x) #1
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.exponencial(x) #2
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.gamma(x) #3
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.uniforme(x) #4
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.beta(x) #5
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.lognormal(x) #6
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    ajustar.weibull(x) #7
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    mixtools.clustering(x, maxit=input$niter) #8
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    km.clustering(x) #9
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    normal.uniforme(x, maxit=input$NormUnifIter) #10
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    normal.exponencial(x, maxit = input$NormExpIter) #11
    all.pval<-c(all.pval,pvalues$p)
    set.seed(input$seed)
    gamma.gamma(x, maxit=input$gammaiter) #12
    all.pval<-c(all.pval,pvalues$p)
    
    switch (as.character(which(all.pval == max(all.pval))[1]),
            "1" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Normal")
            },
            "2" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Exponencial")
            },
            "3" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Gamma")
            },
            "4" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Uniforme")
            },
            "5" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Beta")
            },
            "6" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Lognormal")
            },
            "7" = {
              updateSelectInput(session, "TipoDist",  selected = "Parametrica")
              updateSelectInput(session, "Dist",  selected = "Weibull")
            },
            "8" = {
              updateSelectInput(session, "TipoDist",  selected = "Mixtura")
              updateSelectInput(session, "CompMixtura1",  selected = "Normal")
              updateSelectInput(session, "CompMixtura2N",  selected = "Normal")
              updateSelectInput(session, "AlgMixNormal",  selected = "EMAlg")
            }, 
            "9" = {
              updateSelectInput(session, "TipoDist",  selected = "Mixtura")
              updateSelectInput(session, "CompMixtura1",  selected = "Normal")
              updateSelectInput(session, "CompMixtura2N",  selected = "Normal")
              updateSelectInput(session, "AlgMixNormal",  selected = "kmeansAlg")
            },
            "10" = {
              updateSelectInput(session, "TipoDist",  selected = "Mixtura")
              updateSelectInput(session, "CompMixtura1",  selected = "Normal")
              updateSelectInput(session, "CompMixtura2N",  selected = "Uniforme")
            },
            "11" = {
              updateSelectInput(session, "TipoDist",  selected = "Mixtura")
              updateSelectInput(session, "CompMixtura1",  selected = "Normal")
              updateSelectInput(session, "CompMixtura2N",  selected = "Exponencial")
            },
            "12" = {
              updateSelectInput(session, "TipoDist",  selected = "Mixtura")
              updateSelectInput(session, "CompMixtura1",  selected = "Gamma")
              updateSelectInput(session, "CompMixtura2G",  selected = "Gamma")
            }
    )
  })
  
  plot.dat <- reactiveValues(main=NULL, layer1=NULL)

  observeEvent(input$simular,{
    
    switch (input$TipoDist,
            Parametrica = switch(input$Dist,
                                 Normal= {
                                   datos.generados<<-data.frame(Datos=rnorm(input$sims,n.mean,n.sd))
                                 },
                                 Exponencial = {
                                   datos.generados<<-data.frame(Datos=exp.min+rexp(input$sims,exp.lambda))
                                 },
                                 Gamma = {
                                   datos.generados<<-data.frame(Datos=gamma.min+rgamma(input$sims,shape = gamma.forma, scale = gamma.escala))
                                 },
                                 Uniforme = {
                                   datos.generados<<-data.frame(Datos=runif(input$sims,min = unif.min,max = unif.max))
                                 },
                                 Beta = {
                                   datos.generados<<-data.frame(Datos=b.min+rbeta(input$sims,beta.shape1,beta.shape2)*(b.max-b.min))
                                 },
                                 Lognormal = {
                                   datos.generados<<-data.frame(Datos=ln.min+rlnorm(input$sims,ln.mean,ln.sd))
                                 },
                                 Weibull = {
                                   datos.generados<<-data.frame(Datos=wei.min+rweibull(input$sims, shape = wei.forma, scale = wei.escala))
                                 }
            ),
            Mixtura = switch (input$CompMixtura1,
                              Normal = {
                                switch(input$CompMixtura2N,
                                       Normal = {
                                         switch (input$AlgMixNormal,
                                                 EMAlg = {
                                                   ID<-ifelse(runif(input$sims)<mix.lambda[1],0,1)
                                                   datos.generados<<-data.frame(Datos=
                                                      rnorm(input$sims,mix.means[1],mix.sd[1])*(1-ID)+rnorm(input$sims,mix.means[2],mix.sd[2])*ID
                                                   )
                                                 },
                                                 kmeansAlg = {
                                                   ID<-ifelse(runif(input$sims)<km.lambda[1],0,1)
                                                   datos.generados<<-data.frame(Datos=
                                                    rnorm(input$sims,km.means[1],km.sd[1])*(1-ID)+rnorm(input$sims,km.means[2],km.sd[2])*ID
                                                   )
                                                 }
                                         )
                                       },
                                       Uniforme = {
                                         ID<-ifelse(runif(input$sims)<mix.lambda[normInd],0,1)
                                         datos.generados<<-data.frame(Datos=
                                                                       rnorm(input$sims,mediaNorm,sdNorm)*(1-ID)+runif(input$sims,minUnif,maxUnif)*ID
                                                                     )
                                       },
                                       Exponencial = {
                                         ID<-ifelse(runif(input$sims)<mix.lambda[normInd],0,1)
                                         datos.generados<<-data.frame(Datos=
                                                                       rnorm(input$sims,mediaNorm,sdNorm)*(1-ID)+(exp.min+rexp(input$sims,exp.lambda))*ID
                                         )
                                       }
                                )
                              },
                              Uniforme = {
                                switch(input$CompMixtura2U,
                                       Normal = {
                                         ID<-ifelse(runif(input$sims)<mix.lambda[normInd],0,1)
                                         datos.generados<<-data.frame(Datos=
                                                                       rnorm(input$sims,mediaNorm,sdNorm)*(1-ID)+runif(input$sims,minUnif,maxUnif)*ID
                                         )
                                       }
                                )
                              },
                              Exponencial = {
                                switch(input$CompMixtura2E,
                                       Normal = {
                                         ID<-ifelse(runif(input$sims)<mix.lambda[normInd],0,1)
                                       datos.generados<<-data.frame(Datos=
                                                                     rnorm(input$sims,mediaNorm,sdNorm)*(1-ID)+(exp.min+rexp(input$sims,exp.lambda))*ID
                                       )
                                       }
                                )
                              },
                              Gamma = {
                                switch(input$CompMixtura2G,
                                       Gamma = {
                                         
                                         ID<-ifelse(runif(input$sims)<mix.lambda[1],0,1)
                                       datos.generados<<-data.frame(Datos=
                                         (gamma.min+rgamma(input$sims,shape = mix.alphas[1],scale = mix.betas[1]))*(1-ID)+
                                           (gamma.min+rgamma(input$sims,shape = mix.alphas[2],scale = mix.betas[2]))*ID
                                       )
                                       }
                                )
                              }
            )
    )
    
    plot.dat$layer1 <-
      geom_histogram(data=datos.generados, aes(x = Datos, y=..density..),
                     col="indianred2", fill= "firebrick3", alpha = .25, bins = nBins)
  })
  
  observeEvent(input$TipoDist,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$Dist,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$CompMixtura1,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$CompMixtura2N,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$CompMixtura2U,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$CompMixtura2E,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$CompMixtura2G,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$AlgMixNormal,{
    plot.dat$layer1<-NULL
  })
  
  observeEvent(input$metodoBins,{
    if(!is.null(plot.dat$layer1)){
      if(is.null(input$archivoCargado$datapath)){
        df<-faithful
      }
      else{
        df <- read.csv(input$archivoCargado$datapath,
                       header = input$header,
                       sep = input$sep)
      }
      updateNumericInput(session,"columna",max = ncol(df))
      df<-df[!is.na(df[,input$columna]),input$columna,drop=F]
      x<-as.vector(t(df))
      plot.dat$layer1 <-
        geom_histogram(data=datos.generados, aes(x = Datos, y=..density..),
                       col="indianred2", fill= "firebrick3", alpha = .25, bins = num.bins(x))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datos.generados, file=file, row.names = F)
    })
  
  empirica<-function(datos,xfit,yfit){
    ggplot(data.frame(datos), aes(datos)) +
      stat_ecdf(geom = "step", col="skyblue2", size=1)+
      geom_line(data=data.frame(x=xfit,y=yfit), aes(x=x,y=cumsum(yfit)/sum(yfit)),colour="dodgerblue4",size=1)
  }
  
  ajustar.normal <- function (datos){
    #Estimar parámetros
    n.mean<<-mean(datos)
    n.sd<<-sd(datos)
    #Calcular número de intervalos
    nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos),max(datos),length=100)
    yfit <- dnorm(xfit,n.mean,n.sd)
    #Tabla de parámetros estimados
    output$parametros <- renderTable({
      data.frame("Distribución"="Normal","Media"=n.mean, "Desviación"=n.sd)
    })
    #Pruebas de bondad de ajuste
    pruebas.gof(datos,"pnorm",mean=n.mean,sd=n.sd)
    #Gráfico
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    plot.dat$main <- 
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  ajustar.exponencial <- function (datos){
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos)<0,
           exp.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
           exp.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - exp.min
    #Estimar parámetros
    exp.lambda<<-1/mean(datos.n)
    #Calcular número de intervalos
    nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    yfit <- dexp(xfit,exp.lambda)
    xfit <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Exponencial","Lambda" = exp.lambda, "Desfase" = exp.min)
    })
    
    #Pruebas de bondad de ajuste
    pruebas.gof(datos.n,"pexp",rate=exp.lambda)
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    plot.dat$main <-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  ajustar.gamma <- function (datos){
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos)<0,
           gamma.min <- min(datos)*(length(datos)/2+1)/(length(datos)/2),
           gamma.min <- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - gamma.min
    #Estimar parámetros por momentos (puntos iniciales del MLE)
    gamma.escala<-mean(datos.n)/var(datos.n)
    gamma.forma<-mean(datos.n)^2/var(datos.n)
    #Estimar parámetros por máxima verosimilitud
    gamma.MLE<-fitdistr(x = datos.n,densfun = "gamma", start=list(scale=gamma.escala,shape=gamma.forma),lower=c(0,0))
    gamma.escala<-gamma.MLE$estimate[1]
    gamma.forma<-gamma.MLE$estimate[2]
    gamma.escala<<-gamma.escala
    gamma.forma<<-gamma.forma
    #Calcular número de intervalos
    nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    yfit <- dgamma(xfit,shape = gamma.forma, scale = gamma.escala)
    xfit <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Gamma", "Escala" = gamma.escala, "Forma" = gamma.forma, "Desfase" = gamma.min)
    })
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    #Pruebas de bondad de ajuste
    pruebas.gof(datos.n,"pgamma",shape = gamma.forma, scale = gamma.escala)
    
    plot.dat$main <-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  ajustar.uniforme <- function (datos){
    #Estimar parámetros
    unif.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2) #Uniforme (a,c), c = mediana
    unif.max <<- max(datos)*(length(datos)/2+1)/(length(datos)/2) #Uniforme (c,b), c = mediana
    #Nota: considerar estimador del máximo (b) como (N*bhat-ahat)/(N-1)
    #Calcular número de intervalos
        nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos),max(datos),length=100)
    yfit <- dunif(xfit,unif.min,unif.max)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Uniforme","Mínimo" = unif.min, "Máximo" = unif.max)
    })
    
    #Pruebas de bondad de ajuste
    pruebas.gof(datos,"punif",min=unif.min,max=unif.max)
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    plot.dat$main<-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  ajustar.beta <- function (datos){
    #Min max normalization
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos)<0,
      b.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
      b.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    ifelse(max(datos)<0,
      b.max <<- max(datos)*(length(datos)/2-1)/(length(datos)/2),
      b.max <<- max(datos)*(length(datos)/2+1)/(length(datos)/2)
    )
    datos.n <- (datos-b.min)/(b.max-b.min)
    #Estimar parámetros por momentos (puntos iniciales del MLE)
    beta.shape1 <- mean(datos.n)*(mean(datos.n)*(1-mean(datos.n))/var(datos.n)-1)
    beta.shape2 <- (1-mean(datos.n))*(mean(datos.n)*(1-mean(datos.n))/var(datos.n)-1)
    #Estimar parámetros por máxima verosimilitud
    beta.MLE <- fitdistr(x = datos.n, densfun = "beta", start = list(shape1=beta.shape1,shape2=beta.shape2),lower=c(0,0))
    beta.shape1 <<- beta.MLE$estimate[1]
    beta.shape2 <<- beta.MLE$estimate[2]
    #Calcular número de intervalos
        nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    yfit <- dbeta(xfit,beta.shape1,beta.shape2)/(b.max-b.min)
    xfit <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Beta","Mínimo" = b.min, "Máximo" = b.max,"Forma 1" = beta.shape1, "Forma 2" = beta.shape2)
    })
    
    #Pruebas de bondad de ajuste
    pruebas.gof(datos.n,"pbeta",shape1=beta.shape1,shape2=beta.shape2)

    output$plotEmpirica<-renderPlot({
      empirica(datos,datos,xfit,yfit)
    })
    
    plot.dat$main <-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  ajustar.lognormal <- function (datos){ #Tiene problema con negativos
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos)<0,
      ln.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
      ln.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - ln.min
    #Estimar parámetros
    ln.mean<<-mean(log(datos.n))
    ln.sd<<-sd(log(datos.n)) #MLE sería sd*(N-1)/N
    #Calcular número de intervalos
    nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    yfit <- dlnorm(xfit,ln.mean,ln.sd)
    xfit <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Lognormal","Media"=ln.mean, "Desviación"=ln.sd, "Desfase"=ln.min)
    })
    
    #Pruebas de bondad de ajuste
    pruebas.gof(datos.n,"plnorm", meanlog=ln.mean, sdlog=ln.sd)
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,datos,xfit,yfit)
    })
    
    plot.dat$main<-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  } 

  ajustar.weibull <- function (datos){
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos)<0,
           wei.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
           wei.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - wei.min
    #Estimar parámetros por máxima verosimilitud (inicios extraños)
    wei.MLE<-fitdistr(x = datos.n, densfun = "weibull", lower=c(0,0))
    wei.forma<<-wei.MLE$estimate[1]
    wei.escala<<-wei.MLE$estimate[2]
    #Calcular número de intervalos
    nBins<<-num.bins(datos)
    #Graficar
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    yfit <- dweibull(xfit, shape = wei.forma, scale = wei.escala)
    xfit <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Distribución"="Weibull", "Escala" = wei.escala, "Forma" = wei.forma, "Desfase" = wei.min)
    })
    
    pruebas.gof(datos.n, "pweibull",shape = wei.forma, scale = wei.escala)
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    plot.dat$main<-
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    plot.dat$main + plot.dat$layer1
  }
  
  plotMix<-function(datos,xfit,yfit,C1,C2){
    nBins<<-num.bins(datos)
    
    output$plotEmpirica<-renderPlot({
      empirica(datos,xfit,yfit)
    })
    
    ggplot(data = data.frame(datos), mapping = aes(datos)) +
      geom_histogram(aes(y=..density..),col="skyblue2", fill= "cadetblue2", alpha = .5, bins = nBins)+
      geom_line(data=data.frame(xfit,C1),aes(xfit,C1),colour="mediumseagreen",size=1)+
      geom_line(data=data.frame(xfit,C2),aes(xfit,C2),colour="darkorchid2",size=1)+
      geom_line(data=data.frame(xfit,yfit), aes(xfit,yfit),colour="dodgerblue4",size=1.5)
    
  }
  
  km.clustering <- function(datos){
    km.out <- kmeans(x = datos, centers = 2, nstart = 25)
    km.classes <- km.out$cluster
    km.means <<- km.out$centers
    km.sd <<- sqrt(km.out$withinss/(km.out$size-1))
    km.lambda <<- km.out$size/sum(km.out$size)
    #Plot de kmeans
    xfitkm <- seq(min(datos),max(datos),length=100)
    C1<-km.lambda[1]*dnorm(xfitkm,km.means[1],km.sd[1])
    C2<-km.lambda[2]*dnorm(xfitkm,km.means[2],km.sd[2])
    yfitkm <- C1+C2
    
    output$parametros <- renderTable({
      data.frame("Componente"= c("Normal (1)", "Normal (2)"), "Peso" = km.lambda, "Media" = km.means, "Desviación" = km.sd)
    })
    
    pruebas.gof(datos, pmixnorm,lambdas = km.lambda, means=km.means, sds=km.sd)
    
    plot.dat$main <-
    plotMix(datos,xfitkm,yfitkm,C1,C2)
    plot.dat$main + plot.dat$layer1
  }
  
  mixtools.clustering <- function(datos, maxit){
    mix.out <- normalmixEM(x=datos, maxit)
    mix.means <<- mix.out$mu
    mix.sd <<- mix.out$sigma
    mix.lambda <<- mix.out$lambda 
    #Plot de EM
    xfitmix <- seq(min(datos),max(datos),length=100)
    C1<-mix.lambda[1]*dnorm(xfitmix,mix.means[1],mix.sd[1])
    C2<-mix.lambda[2]*dnorm(xfitmix,mix.means[2],mix.sd[2])
    yfitmix <- C1+C2
    
    pruebas.gof(datos, pmixnorm,lambdas = mix.lambda, means=mix.means, sds=mix.sd)
    
    output$parametros <- renderTable({
      data.frame("Componente"= c("Normal (1)", "Normal (2)"), "Peso" = mix.lambda, "Media" = mix.means, "Desviación" = mix.sd)
    })
    
    plot.dat$main <- plotMix(datos,xfitmix,yfitmix,C1,C2)
    plot.dat$main + plot.dat$layer1
  }
  
  normal.uniforme <- function(datos, maxit){
    id <- c(1)
    b<-npEM(datos, mu0 = 2, id, eps = 1e-2, verb = FALSE, samebw = FALSE, maxiter= maxit)
    mix.lambda<<-b$lambdahat
    normInd<<-which.max(mix.lambda)
    unifInd<<-which.min(mix.lambda)
    clasificacion<-rep(normInd,length(datos))
    clasificacion[b$posteriors[,normInd]<=0.5]<-unifInd
    mediaNorm<<-mean(datos[clasificacion==normInd])
    sdNorm<<-sd(datos[clasificacion==normInd])
    #opción 1 unif
    mediaUnif<-mean(datos[clasificacion==unifInd])
    varUnif<-var(datos[clasificacion==unifInd])
    longUnif<-sqrt(varUnif*12)
    minUnif<<-mediaUnif - longUnif/2
    maxUnif<<-mediaUnif + longUnif/2
    #opción 2 unif
    #minUnif<-min(datos[clasificacion==unifInd])
    #maxUnif<-max(datos[clasificacion==unifInd])
    xfitmix <- seq(min(datos),max(datos),length=100)
    C1<-mix.lambda[normInd]*dnorm(xfitmix,mediaNorm,sdNorm)
    C2<-mix.lambda[unifInd]*dunif(xfitmix,minUnif,maxUnif)
    yfitmix <- C1+C2
    
    output$parametros <- renderTable({
      data.frame("Componente"= c("Normal", "Uniforme"), "Peso" = c(mix.lambda[normInd], mix.lambda[unifInd]), 
                 "Media" = c(round(mediaNorm,2),""), "Desviación" = c(round(sdNorm,2),""),
                 "Mínimo" = c("",round(minUnif,2)), "Máximo" = c("",round(maxUnif,2)))
    })
    
    pruebas.gof(datos, pmixnormunif, lambdas = c(mix.lambda[normInd], mix.lambda[unifInd]),
                n.mean=mediaNorm, n.sd=sdNorm, u.min=minUnif, u.max=maxUnif)
    
    plot.dat$main<-
    plotMix(datos,xfitmix,yfitmix,C1,C2)
    plot.dat$main + plot.dat$layer1
  }
  
  normal.exponencial <- function(datos, maxit){
    id <- c(1)
    b<-npEM(datos, mu0 = 2, id, eps = 1e-2, verb = FALSE, samebw = FALSE, maxiter = maxit)
    mix.lambda<-b$lambdahat
    #Normal
    normInd<<-which.max(mix.lambda)
    expInd<<-which.min(mix.lambda)
    clasificacion<-rep(normInd,length(datos))
    clasificacion[b$posteriors[,normInd]<=0.5]<-expInd
    mediaNorm<<-mean(datos[clasificacion==normInd])
    sdNorm<<-sd(datos[clasificacion==normInd])
    #Exponencial
    #Desplazamiento de datos al intervalo (0,Inf)
    ifelse(min(datos[clasificacion==expInd])<0,
           exp.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
           exp.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - exp.min
    exp.lambda<<-1/mean(datos.n)
    xfit <- seq(min(datos.n),max(datos.n),length=100)
    C2 <- dexp(xfit,exp.lambda)
    xfit <- seq(min(datos),max(datos),length=100)
    C1 <- dnorm(xfit,mediaNorm,sdNorm)
    yfit <- mix.lambda[normInd]*C1+mix.lambda[expInd]*C2
    C1 <- mix.lambda[normInd]*C1
    C2 <- mix.lambda[expInd]*C2
    
    output$parametros <- renderTable({
      data.frame("Componente"= c("Normal", "Exponencial"), "Peso" = c(mix.lambda[normInd], mix.lambda[expInd]), 
                 "Media" = c(round(mediaNorm,2),""), "Desviación" = c(round(sdNorm,2),""),
                 "Lambda" = c("",round(exp.lambda,2)), "Desfase" = c("",round(exp.min,2)))
    })
    
    pruebas.gof(datos, pmixnormexp, lambdas = c(mix.lambda[normInd], mix.lambda[expInd]),
                n.mean=mediaNorm, n.sd=sdNorm, desfase=exp.min, exp.lambda=exp.lambda)
    
    plot.dat$main<-
    plotMix(datos,xfit,yfit,C1,C2)
    plot.dat$main + plot.dat$layer1
  }
  
  gamma.gamma <- function(datos, maxit){
    ifelse(min(datos)<0,
           gamma.min <<- min(datos)*(length(datos)/2+1)/(length(datos)/2),
           gamma.min <<- min(datos)*(length(datos)/2-1)/(length(datos)/2)
    )
    datos.n <- datos - gamma.min
    mix.out <- gammamixEM(x=datos.n, maxit=maxit)
    mix.alphas <<- mix.out$gamma.pars[1,]
    mix.betas <<- mix.out$gamma.pars[2,]
    mix.lambda <<- mix.out$lambda 
    #Plot de EM
    xfitmix <- seq(min(datos.n),max(datos.n),length=100)
    C1<- mix.lambda[1]*dgamma(xfitmix,shape = mix.alphas[1],scale = mix.betas[1])
    C2<- mix.lambda[2]*dgamma(xfitmix,shape = mix.alphas[2],scale = mix.betas[2])
    yfitmix <- C1 + C2
    xfitmix <- seq(min(datos),max(datos),length=100)
    
    output$parametros <- renderTable({
      data.frame("Componente"= c("Gamma (1)", "Gamma (2)"), "Peso" = mix.lambda, "Forma" = mix.alphas, "Escala" = mix.betas, "Desfase" = gamma.min)
    })
    
    pruebas.gof(datos.n, pmixgamma,lambdas=mix.lambda, formas=mix.alphas, escalas=mix.betas)
    
    plot.dat$main<-
    plotMix(datos,xfitmix,yfitmix,C1,C2)
    plot.dat$main + plot.dat$layer1
  }
  
  num.bins <- function(datos){
    switch(input$metodoBins,
           rBins = length(hist(datos)$counts),
           sBins = (1+log2(length(datos))),
           kBins = maxPostProbKnuth(datos)
    )
  }
  
  postProbsKnuth <- function(M, datos) {
    M<-floor(M)
    N <- length(datos)
    h<-hist(datos, breaks = seq(min(datos),max(datos),l=(M+1)))
    n<-h$counts
    parte1<-N*log(M)+log(gamma(M/2))- N*log(gamma(1/2))
    #parte2 <- log(gamma(N+M/2))
    if(M%%2==1){ 
      m <- floor(N+M/2)
      j <- 1:m
      parte2 <- 1/2*log(pi)-m*log(2)+sum(log(2*j-1))
    }
    else{
      m <- N+M/2
      j <- 1:(m-1)
      parte2 <- sum(log(j))
    }
    #parte3 <- sum(log(gamma(n[k]+1/2)))
    parte3 <-0
    for (k in 1:M) {
      m<-n[k]
      if(n[k]>0){
        j<-1:m
        parte3 <- parte3 + 1/2*log(pi)-m*log(2)+sum(log(2*j-1))
      }
      else parte3 <- parte3 + 1/2*log(pi)
    }
    parte1-parte2+parte3
  }
  
  maxPostProbKnuth <- function(datos){
    maxProb <- -Inf
    nBinsOpt<-1
    for (i in 1:40){
      probs <- postProbsKnuth(i,datos)
      if (probs>maxProb){
        maxProb <- probs
        nBinsOpt <- i
      }
    }
    nBinsOpt
  }

  output$contents <- renderTable({

    # input$archivoCargado will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    #req(input$archivoCargado)
    
    if(is.null(input$archivoCargado$datapath)){
      df<-faithful
    }

    else{
    df <- read.csv(input$archivoCargado$datapath,
                   header = input$header,
                   sep = input$sep)
    }
    updateNumericInput(session,"columna",max = ncol(df))
    df<-df[!is.na(df[,input$columna]),input$columna,drop=F]
    
    output$n.datos<-renderUI({paste("Número de datos: ",nrow(df))})
    output$clase.datos<-renderUI({
      tipo.datos<-class(df[,1])
      switch(tipo.datos,
             "numeric"={tipo.datos<-"Numérico"},
             "integer"={tipo.datos<-"Entero"},
             "character"={tipo.datos<-"Texto"})
      paste("Tipo de datos: ",tipo.datos)
      })
    

    #Acá se pueden hacer funciones

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })
  
  output$DownloadFaithful <- downloadHandler(
    filename = "Faithful.csv",
    content = function(file) {
      write.csv(faithful, file=file, row.names = F)
    })
  
  output$distPlot <- renderPlot({
    
    if(is.null(input$archivoCargado$datapath)){
      df<-faithful
    }
    else{
      df <- read.csv(input$archivoCargado$datapath,
                   header = input$header,
                   sep = input$sep)
    }
    updateNumericInput(session,"columna",max = ncol(df))
    df<-df[!is.na(df[,input$columna]),input$columna,drop=F]
    
    x<-as.vector(t(df))
    
    set.seed(input$seed)
    
    switch (input$TipoDist,
      Parametrica = switch(input$Dist,
                            Normal= ajustar.normal(x),
                            Exponencial = ajustar.exponencial(x),
                            Gamma = ajustar.gamma(x),
                            Uniforme = ajustar.uniforme(x),
                            Beta = ajustar.beta(x),
                            Lognormal = ajustar.lognormal(x),
                            Weibull = ajustar.weibull(x)
      ),
      Mixtura = switch (input$CompMixtura1,
                        Normal = {
                          switch(input$CompMixtura2N,
                                Normal = {
                                  switch (input$AlgMixNormal,
                                    EMAlg = mixtools.clustering(x, maxit= input$niter),
                                    kmeansAlg = km.clustering(x)
                                  )
                                },
                                Uniforme = normal.uniforme(x, maxit = input$NormUnifIter),
                                Exponencial = normal.exponencial(x, maxit = input$NormExpIter)
                          )
                        },
                        Uniforme = {
                          switch(input$CompMixtura2U,
                                 Normal = normal.uniforme(x, maxit = input$NormUnifIter)
                          )
                        },
                        Exponencial = {
                          switch(input$CompMixtura2E,
                                 Normal = normal.exponencial(x, maxit = input$NormExpIter)
                          )
                        },
                        Gamma = {
                          switch(input$CompMixtura2G,
                                 Gamma = gamma.gamma(x, maxit=input$gammaiter)
                          )
                        }
      )
    )
  })
  
  pruebas.gof <- function(datos, y, ...){
    #Pruebas de bondad de ajuste
    ks <- ks.test(datos, y, ...)
    cvm <- cvm.test(datos, y, ...)
    ad <- ad.test(datos, y, ...)
    #Tabla de valores-p
    df<-data.frame("Prueba"= c("Kolmogorov-Smirnov","Cramér-von Mises", "Anderson-Darling"),
                   "ValorP"= c(sprintf("%.4f",ks$p.value), sprintf("%.4f",cvm$p.value), sprintf("%.4f",ad$p.value)),
                   "Rechaza"=c(pgof(ks$p.value), pgof(cvm$p.value), pgof(ad$p.value)))
    colnames(df)<-c("Prueba", "Valor p", "Rechaza (95%)")
    pvalues$p<-ad$p.value
    output$gof <- renderTable({
      df
    })
  }
  
  pgof <- function(p){
    ifelse(p<0.05,"Sí","No")
  }   
  
  pmixnorm<-function(q,lambdas,means,sds){
    lambdas[1]*pnorm(q,means[1],sds[1])+lambdas[2]*pnorm(q,means[2],sds[2])
  }
  
  pmixnormunif<-function(q,lambdas,n.mean,n.sd,u.min,u.max){
    lambdas[1]*pnorm(q,n.mean,n.sd)+lambdas[2]*punif(q,u.min,u.max)
  }
  
  pmixnormexp<-function(q,lambdas,n.mean,n.sd,desfase,exp.lambda){
    lambdas[1]*pnorm(q,n.mean,n.sd)+lambdas[2]*pexp(q-desfase,exp.lambda)
  }
  
  pmixgamma<-function(q,lambdas,formas,escalas){
    lambdas[1]*pgamma(q,shape=formas[1],scale=escalas[1])+lambdas[2]*pgamma(q,shape=formas[2],scale=escalas[2])
  }
  
  shinyServer(function(input, output, session){
    session$onSessionEnded(function() {
      stopApp()
    })
  })
}


# Create Shiny app ----
shinyApp(ui, server)
