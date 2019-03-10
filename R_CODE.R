#pacotes necessarios para o codigo
packs <- c("png","grid", "tibble", "dplyr", "qdap", "ggplot2", "gganimate")
lapply(packs, require, character.only = TRUE)


lista_arquivos <- list.files('CSV_FILES', full.names = TRUE)

lista_arquivos_1 <- lapply(lista_arquivos, function(x) read.csv2(x, stringsAsFactors = FALSE))

##################################################################################################################
#colocar os anos em cada data frame
id <- c(2008:2018)
filelist <-  mapply(cbind, lista_arquivos_1, "ANO" = id, SIMPLIFY = F)

#######################################################################
#criando as tabelas e armazenando suas devidas informacoes
arabica <- data.frame()
conillon <- data.frame() 
soluvel <- data.frame() 
torrado <- data.frame()

for (i in 1:length(filelist)){
  arabica <- rbind(arabica, data.frame(filelist[[i]][,c(1,2,7)]))
  conillon <- rbind(conillon, data.frame(filelist[[i]][,c(1,3,7)]))
  soluvel <- rbind(soluvel, data.frame(filelist[[i]][,c(1,4,7)]))
  torrado <- rbind(torrado, data.frame(filelist[[i]][,c(1,5,7)]))
  
}
#####################################################################
#substitui os paises  que estao duplicados e renomeando outros para facilitar a visualizacao
#agrupa por ano e ordena de forma descedente por ele, depois estrai as 15 primeiras linhas de cada um, elas
#que serao plotadas no gráfico

ordem = function(data) {
  colnames(data) <- c('PAIS_DESTINO', 'QUANTIDADE', 'ANO')
  
  substituir <- c("COREIA_DO_SUL (REPUBL.)", "COREIA DO SUL (REPUBL.)","HOLANDA (PAISES BAIXOS)", "HOLANDA_(PAISES BAIXOS)",
                  "RUSSIAN FEDERATION")
  replace <- c("COREIA DO SUL","COREIA DO SUL", "HOLANDA" ,"HOLANDA", "RUSSIA" )
  data$PAIS_DESTINO <- multigsub(substituir, replace, data$PAIS_DESTINO)
  
  data %>% 
    group_by(ANO)%>%
    do( data.frame(with(data=., .[order(-QUANTIDADE),] )) )%>%
    filter(row_number() <= 15)
}

dflist <- list(arabica, conillon, soluvel, torrado)

resultado <- lapply(dflist, ordem)

for (i in seq(resultado)){
  if (i == 1){
    assign(("arabica"), resultado[[i]])
  } else if(i == 2){
    assign(("conillon"), resultado[[i]])
  }else if (i == 3){
    assign(("soluvel"), resultado[[i]])
  }else{
    assign(("torrado"), resultado[[i]])
  }}



#####################################################################################################################
#seleciona a imagem que vai ficar por baixo dos gráficos:
image <- readPNG("background.png")

# A funcao mapa agrupa por ano, ranqueia os valores, plot o ggplot dinamico
mapa <- function(m, title){
  
  x <- m %>%
    group_by(ANO) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = min_rank(-QUANTIDADE) * 1,
           Value_rel = QUANTIDADE/QUANTIDADE[rank==1],
           Value_lbl = paste(" ",QUANTIDADE)) %>%
    
    ungroup()
  
  colourCount = length(unique(x$PAIS_DESTINO))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  p <- ggplot(x, aes(rank, group = PAIS_DESTINO, fill = as.factor(PAIS_DESTINO))) +
    scale_fill_manual(values = getPalette(colourCount))+
    geom_tile(aes(y = QUANTIDADE/2,
                  height = QUANTIDADE,
                  width = 0.9), alpha = 0.8) +
    
    geom_text(aes(y = 0, label = paste(PAIS_DESTINO, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=QUANTIDADE,label = Value_lbl, hjust=0)) +
    coord_flip(clip = "off", expand = TRUE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    
    labs(title = sprintf("TOP 15 DESTINOS CAFÉ %s BRASILEIRO ANO: {closest_state} ", title), x = "", y = "Volume Saca 60 kg",
         caption = "Fonte: Cecafé") +
    theme(plot.title = element_text(hjust = 0.5, size = 15,face = "bold"),
          axis.ticks.y = element_blank(),  
          axis.text.y  = element_blank(),  
          plot.margin = margin(1,1,1,4, "cm"),
          panel.background = element_rect(colour = NA,fill="transparent")) +
    
    transition_states(ANO, transition_length = 4, state_length = 1) +
    ease_aes('cubic-in-out')
  
  
  t <- p +annotation_custom(rasterGrob(image, width = unit(1,"npc"),  height = unit(1,"npc")), 
                            -Inf, Inf, -Inf, Inf) 
  
  
  animate(t, 8, fps = 10, duration = 30, width = 1000, height = 600)
  
}


arabica <-mapa(arabica, 'ARÁBICA')
anim_save("arabica.gif", arabica)

conillon<- mapa(conillon, 'CONILLON')
anim_save("conillon.gif", conillon)

soluvel <- mapa(soluvel, 'SOLÚVEL')
anim_save("soluvel.gif", soluvel)

torrado <- mapa(torrado, 'TORRADO')
anim_save("torrado.gif", torrado)