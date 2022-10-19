#
# This is a Shiny web application for visualizing the trait netwrok. 
# Created by Bernice Cheung

library(igraph)
library(visNetwork)
library(shiny)
library(here)

#Main Data File
data=read.csv('Personality_Categories_Fin_MShi.csv',T)


#Load in word lists
pos_words=read.table('positive_trait_list_lc2.txt',F)
neg_words=read.table('negative_trait_list_lc.txt',F)

pos_matrix=as.matrix(read.table('pos_matrix_oldnetwork.txt',F))
neg_matrix=as.matrix(read.table('neg_matrix_oldnetwork.txt',F))

data$numid=1:length(data$id)

#Edits some text fields to be consistent between trait lists and data
newpos=gsub('.','-',names(data)[39:137],fixed=T)
newneg=gsub('.','-',names(data)[138:237],fixed=T)


#Finds indices for matches between the trait words in the data and the trait word lists
pos_match=match(newpos,pos_words[,1])

neg_match=match(newneg,neg_words[,1])

#########
#Thresholds adjacency matrix to dependencies rated as existing in at least .3 of the participants
##########

pos_adj=pos_matrix>.3
neg_adj=neg_matrix>.3

#Create positive graph
pos_edges=graph.adjacency(t(pos_adj))


#Get communities from graph
pos_walk_com=membership(walktrap.community(pos_edges))

# convert the matrix into binary
pos_adj[which(pos_adj == TRUE)] <- 1


# generate graph object from the matrix(after transposing the matrix)
graph_pos <- graph_from_adjacency_matrix(t(pos_adj), mode = "directed", weighted=NULL)

# generate the edge data frame
edgelist_pos <- as_edgelist(graph_pos)
edges_pos <- data.frame(from = as.numeric(edgelist_pos[,1]), to = as.numeric(edgelist_pos[,2]))

# generate the nodes data frame
nodes_pos <- data.frame(id = 1:100,
                        label = as.character(pos_words[,1]),
                        group = as.factor(pos_walk_com))





# Define UI for application 
ui <- fluidPage(
    fluidRow(
        column(
            width = 4,
            selectInput("indistance", "In-Distance :",
                        c(0:4)),
            selectInput("outdistance", "Out-Distance :",
                        c(0:4))
        ),
        column(
            width = 8,
            visNetworkOutput("network_proxy_nodes", height = "1000px")
        )
    )
    #,mainPanel(plotOutput(outputId = "network_proxy_nodes", height = 500))
)


# Define server logic 
server <- function(input, output) {
    output$network_proxy_nodes <- renderVisNetwork({
        
        visNetwork(nodes_pos, edges_pos, height = "700px", width = "100%") %>%
            visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>%
            
            visIgraphLayout() %>% visNodes(size = 10) %>% visPhysics(stabilization = 2000) %>% visPhysics(enabled = F) 
    })
    
    
    observe({
        visNetworkProxy("network_proxy_nodes") %>%
            visOptions(selectedBy = "group",
                       highlightNearest = list(enabled = T,degree = list(from = input$indistance, to = input$outdistance),algorithm = "hierarchical", hover = T),
                       nodesIdSelection = list(enabled = T, useLabels = TRUE))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
