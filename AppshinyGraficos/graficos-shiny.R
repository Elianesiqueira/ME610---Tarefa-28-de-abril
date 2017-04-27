library(shiny)
#library(ggplot)
library(dplyr)

setwd(choose.dir())
dados=read.table("dados.txt", header = T)
#alterando nomes das variáveis, usando uma tabela feita no excel
descricao<-read.csv("dados.csv", header = T, sep = ";")
descricao<-descricao[,1:2]
names(descricao)<-c("Original", "Nova")
names(dados)<-descricao$Nova #alterando os nomes das variáveis

#limpando os dados

#limpando os dados
dadoslimpo<-dados[,-17] #excluindo Altura_pai
dadoslimpo<-dados[,-17] #excluindo Peso_pai
dadoslimpo<-dados[,-1]
#retirando observações desconhecidas

dadoslimpo=filter(dados,Rendimento_anual!='98')
dadoslimpo=filter(dados,Cor_mae!='99')
dadoslimpo=filter(dados,Idade_mae!='99')
dadoslimpo=filter(dados,Educacao_mae!='9')
dadoslimpo=filter(dados,Altura_mae!='99')
dadoslimpo=filter(dados,Peso_mae!='999')
dadoslimpo=filter(dados,Cor_pai!='99')
dadoslimpo=filter(dados,Idade_pai!='99')
dadoslimpo=filter(dados,Educacao_pai!='9')
dadoslimpo=filter(dados,Fuma!='9')
dadoslimpo=filter(dados,Tempo_sem_fumar!='98')
dadoslimpo=filter(dados,Numero_cigarros!='98')
dadoslimpo=dadoslimpo[,-1]
#iniciando o app

ui<- fluidPage(
  titlePanel("Análise Descritiva"),
  
    sidebarLayout(
      sidebarPanel(
        selectInput('xcol', 'Variável X', names(dadoslimpo)),
        selectInput('ycol', 'Variável Y', names(dadoslimpo),
                    selected=names(dadoslimpo)[[2]]),
        selectInput('tipo_grafico', 'Tipo de gráfico', c("Boxplot", "Dispersão"))
      ),
      mainPanel(
        plotOutput('grafico')
      )  
      
  )
  
  
  
  
)

server<-function(input, output, session) {
  output$grafico <- renderPlot({
    if(input$tipo_grafico=='Boxplot'){
      eixox=names(dadoslimpo[, input$xcol])
      g<-ggplot(dadoslimpo, aes(x=dadoslimpo[,input$xcol],y=dadoslimpo[,input$ycol]))+geom_boxplot(aes(group=dadoslimpo[,input$xcol]),outlier.colour = "red")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+xlab(names(dadoslimpo[,input$xcol]))
      g<-g+labs(fill= input$xcol, x= "",y= input$ycol)
    }else{
      g<-ggplot(dadoslimpo, aes(x=dadoslimpo[,input$xcol],y=dadoslimpo[,input$ycol]))+geom_point()+theme_bw()
      g<-g+labs(fill= input$xcol, x= "",y= input$ycol)
    }
    g
  })
 
}



shinyApp(ui = ui, server = server)