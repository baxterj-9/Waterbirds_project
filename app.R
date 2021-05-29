library(shiny)
library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(colorspace)
library(metR)
library(ggspatial) 
library(tmap)
library(dplyr)
library(spatstat)
library(gstat)
library(raster)
library(fields)





bird <- c("Blue-winged Teal","American Bittern", "Killdeer")
state <- c("Washington", "North Dakota")

WA_AMBI <- st_read("Washington/WA_American_Bittern.shp")

WA_BWTE <- st_read("Washington/WA_Blue_Winged_Teal.shp")

WA_Cities <- st_read("Washington/WA_Cities.shp")

WA_Highway <- st_read("Washington/WA_Highways.shp")

WA_KIDE <- st_read("Washington/WA_Killdeer.shp")

WA_Lake <- st_read("Washington/WA_Lakes.shp")

WA_Forest <- st_read("Washington/WA_Nat_Forest.shp")

WA_River <- st_read("Washington/WA_Rivers.shp")

WA_state <- st_read("Washington/Washington.shp")

ND_AMBI <- st_read("North_Dakota/ND_American_Bittern.shp")

ND_BWTE <- st_read("North_Dakota/ND_Blue_Winged_Teal.shp")

ND_Cities <- st_read("North_Dakota/ND_Cities.shp")

ND_Highway <- st_read("North_Dakota/ND_Highways.shp")

ND_KIDE <- st_read("North_Dakota/ND_Killdeer.shp")

ND_Lake <- st_read("North_Dakota/ND_Lakes.shp")

ND_Forest <- st_read("North_Dakota/ND_Nat_Grassland.shp")

ND_River <- st_read("North_Dakota/ND_Streams.shp")

ND_state <- st_read("North_Dakota/North_Dakota.shp")

WA_AMBI_MPPP <- as.ppp(WA_AMBI)
WA_AMBI.den <- density(WA_AMBI_MPPP)

WA_BWTE_MPPP <- as.ppp(WA_BWTE)
WA_BWTE.den <- density(WA_BWTE_MPPP)

WA_KIDE_MPPP <- as.ppp(WA_KIDE)
WA_KIDE.den <- density(WA_KIDE_MPPP)

ND_AMBI_MPPP <- as.ppp(ND_AMBI)
ND_AMBI.den <- density(ND_AMBI_MPPP)

ND_BWTE_MPPP <- as.ppp(ND_BWTE)
ND_BWTE.den <- density(ND_BWTE_MPPP)

ND_KIDE_MPPP <- as.ppp(ND_KIDE)
ND_KIDE.den <- density(ND_KIDE_MPPP)

WA_AMBI.den_plot <- expand_grid(x=WA_AMBI.den$xcol,y=WA_AMBI.den$yrow)
WA_AMBI.den_plot$z <- as.vector(WA_AMBI.den$v)

WA_BWTE.den_plot <- expand_grid(x=WA_BWTE.den$xcol,y=WA_BWTE.den$yrow)
WA_BWTE.den_plot$z <- as.vector(WA_BWTE.den$v)

WA_KIDE.den_plot <- expand_grid(x=WA_KIDE.den$xcol,y=WA_KIDE.den$yrow)
WA_KIDE.den_plot$z <- as.vector(WA_KIDE.den$v)

ND_KIDE.den_plot <- expand_grid(x=ND_KIDE.den$xcol,y=ND_KIDE.den$yrow)
ND_KIDE.den_plot$z <- as.vector(ND_KIDE.den$v)

ND_AMBI.den_plot <- expand_grid(x=ND_AMBI.den$xcol,y=ND_AMBI.den$yrow)
ND_AMBI.den_plot$z <- as.vector(ND_AMBI.den$v)

ND_BWTE.den_plot <- expand_grid(x=ND_BWTE.den$xcol,y=ND_BWTE.den$yrow)
ND_BWTE.den_plot$z <- as.vector(ND_BWTE.den$v)


WA_BWTE_sf <- as_Spatial(WA_BWTE)
WA_BWTE_sf <- spTransform(WA_BWTE_sf,("+init=epsg:4326"))

WA_Forest_sf <- as_Spatial(WA_Forest)
WA_Forest_sf <- spTransform(WA_Forest_sf,("+init=epsg:4326"))

dist.mat1 <- geosphere::dist2Line(p = WA_BWTE_sf, line = WA_Forest_sf)
WA_BWTE_sf.wit.dist <- cbind(WA_BWTE_sf, dist.mat1)

WA_BWTE_sf.wit.dist <- as.data.frame(WA_BWTE_sf.wit.dist)
WA_BWTE_sf.sp <- sp::SpatialPoints(coords = WA_BWTE_sf[,c("latitude","longitude")], # order matters
                                   proj4string = WA_Forest_sf@proj4string)


###
ND_BWTE_sf <- as_Spatial(ND_BWTE)
ND_BWTE_sf <- spTransform(ND_BWTE_sf,("+init=epsg:4326"))

ND_Forest_sf <- as_Spatial(ND_Forest)
ND_Forest_sf <- spTransform(ND_Forest_sf,("+init=epsg:4326"))

dist.mat2 <- geosphere::dist2Line(p = ND_BWTE_sf, line = ND_Forest_sf)
ND_BWTE_sf.wit.dist <- cbind(ND_BWTE_sf, dist.mat2)

ND_BWTE_sf.wit.dist <- as.data.frame(ND_BWTE_sf.wit.dist)
ND_BWTE_sf.sp <- sp::SpatialPoints(coords = ND_BWTE_sf[,c("latitude","longitude")], # order matters
                                   proj4string = ND_Forest_sf@proj4string)

###

WA_AMBI_sf <- as_Spatial(WA_AMBI)
WA_AMBI_sf <- spTransform(WA_AMBI_sf,("+init=epsg:4326"))


dist.mat3 <- geosphere::dist2Line(p = WA_AMBI_sf, line = WA_Forest_sf)
WA_AMBI_sf.wit.dist <- cbind(WA_AMBI_sf, dist.mat3)

WA_AMBI_sf.wit.dist <- as.data.frame(WA_AMBI_sf.wit.dist)
WA_AMBI_sf.sp <- sp::SpatialPoints(coords = WA_AMBI_sf[,c("latitude","longitude")], # order matters
                                   proj4string = WA_Forest_sf@proj4string)

###

ND_AMBI_sf <- as_Spatial(ND_AMBI)
ND_AMBI_sf <- spTransform(ND_AMBI_sf,("+init=epsg:4326"))

dist.mata <- geosphere::dist2Line(p = ND_AMBI_sf, line = ND_Forest_sf)

ND_AMBI_sf.wit.dist <- cbind(ND_AMBI_sf, dist.mata)

ND_AMBI_sf.wit.dist <- as.data.frame(ND_AMBI_sf.wit.dist)
ND_AMBI_sf.sp <- sp::SpatialPoints(coords = ND_AMBI_sf[,c("latitude","longitude")], # order matters
                                   proj4string = ND_Forest_sf@proj4string)


###

WA_KIDE_sf <- as_Spatial(WA_KIDE)
WA_KIDE_sf <- spTransform(WA_KIDE_sf,("+init=epsg:4326"))


dist.matb <- geosphere::dist2Line(p = WA_KIDE_sf, line = WA_Forest_sf)

# bind results with original points
WA_KIDE_sf.wit.dist <- cbind(WA_KIDE_sf, dist.matb)


WA_KIDE_sf.wit.dist <- as.data.frame(WA_KIDE_sf.wit.dist)
WA_KIDE_sf.sp <- sp::SpatialPoints(coords = WA_KIDE_sf[,c("latitude","longitude")], # order matters
                                   proj4string = WA_Forest_sf@proj4string)


ND_KIDE_sf <- as_Spatial(ND_KIDE)
ND_KIDE_sf <- spTransform(ND_KIDE_sf,("+init=epsg:4326"))

dist.matc <- geosphere::dist2Line(p = ND_KIDE_sf, line = ND_Forest_sf)

# bind results with original points
ND_KIDE_sf.wit.dist <- cbind(ND_KIDE_sf, dist.matc)

ND_KIDE_sf.wit.dist <- as.data.frame(ND_KIDE_sf.wit.dist)
ND_KIDE_sf.sp <- sp::SpatialPoints(coords = ND_KIDE_sf[,c("latitude","longitude")], # order matters
                                   proj4string = ND_Forest_sf@proj4string)






#Start of UI code
ui <- fluidPage(
    titlePanel("App Pitch - Bird Habitats in North Dakota and Washington"),
    fluidRow(),
    h3("Introduction"),
    textOutput("intro"),
    fluidRow(),
    h3("Select Which Data to View"),
    selectInput("bird_species", "Select a Species to View", bird),
    selectInput("state_selection", "Select a State", state),
    textOutput("selection"),
    fluidRow(),
    h3("Bird Discription"),
    textOutput("Bird_dis"),
    h3("Map"),
    fluidRow(
        #creates a plot showing injuries by age and sex
        column(12, plotOutput("state_bird"))
    ),
    textOutput("bird_map"),
    h3("Findings"),
    textOutput("fin_text"),
    h3("Bird Densities Results"),
    fluidRow(
        #creates a plot showing injuries by age and sex
        column(12, plotOutput("bird_den"))
    ),
    fluidRow(
        #creates a plot showing injuries by age and sex
        column(12, plotOutput("bird_den_L"))
    ),
    h3("Spatial Analysis of Distance between Birds and National Forests in WA, 
       and National Grasslands in ND - There will also be a map here showing only
       bird and Forest/Grassland locations"),
    fluidRow(
        #creates a plot showing injuries by age and sex
        column(12, plotOutput("bird_fors_dis"))),
    textOutput("spat_text"),
    
)



# Start of server code
server <- function(input, output, session) {
    

    
    output$intro <- renderText(paste0("In this section I will introduce the 
                                      app, what it does, and why."))
    
    output$selection <- renderText(paste0("Here, users will be able to select 
                                          which bird species to veiw, and in which state."))
    
    output$Bird_dis <- renderText(paste0("Here, I will include descriptions of the 
                                         selected bird species."))
    
    output$bird_map <- renderText(paste0("Here, users view a map of the loctions of the 
                                         bird species, as well as several habitat 
                                         areas of interest."))
    
    output$fin_text <- renderText(paste0("Here, I will include a at least a text discription
                                         of the findings, perhaps a few graphs as well, 
                                         depending on the results."))
    
    
    output$state_bird <- renderPlot({
        if (input$state_selection == "Washington" & input$bird_species == "Blue-winged Teal") {
            ggplot() +
                geom_sf(data = WA_state, fill = "lightgreen") +
                geom_sf(data = WA_Forest, fill = "darkgreen") +
                geom_sf(data = WA_Lake, fill = "blue", col = "blue") +
                geom_sf(data = WA_River, col = "blue") +
                geom_sf(data = WA_Highway, color = "grey") +
                geom_sf(data = WA_Cities, color = "grey80") +
                geom_sf(data = WA_BWTE, color = "yellow") +
                theme_classic() +
                coord_sf()
        } else if (input$state_selection == "Washington" & input$bird_species == "American Bittern") {
            ggplot() +
                geom_sf(data = WA_state, fill = "lightgreen") +
                geom_sf(data = WA_Forest, fill = "darkgreen") +
                geom_sf(data = WA_Lake, fill = "blue", col = "blue") +
                geom_sf(data = WA_River, col = "blue") +
                geom_sf(data = WA_Highway, color = "grey") +
                geom_sf(data = WA_Cities, color = "grey80") +
                geom_sf(data = WA_AMBI, color = "brown") +
                theme_classic() +
                coord_sf()
        } else if (input$state_selection == "Washington" & input$bird_species == "Killdeer") {
            ggplot() +
                geom_sf(data = WA_state, fill = "lightgreen") +
                geom_sf(data = WA_Forest, fill = "darkgreen") +
                geom_sf(data = WA_Lake, fill = "blue", col = "blue") +
                geom_sf(data = WA_River, col = "blue") +
                geom_sf(data = WA_Highway, color = "grey") +
                geom_sf(data = WA_Cities, color = "grey80") +
                geom_sf(data = WA_KIDE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
        } else if (input$state_selection == "North Dakota" & input$bird_species == "Blue-winged Teal") {
            ggplot() +
                geom_sf(data = ND_state, fill = "lightgreen") +
                geom_sf(data = ND_Forest, fill = "darkgreen") +
                geom_sf(data = ND_Lake, fill = "blue", col = "blue") +
                geom_sf(data = ND_River, col = "blue") +
                geom_sf(data = ND_Highway, color = "grey") +
                geom_sf(data = ND_Cities, color = "grey80") +
                geom_sf(data = ND_BWTE, color = "yellow") +
                theme_classic() +
                coord_sf()
        } else if (input$state_selection == "North Dakota" & input$bird_species == "American Bittern") {
            ggplot() +
                geom_sf(data = ND_state, fill = "lightgreen") +
                geom_sf(data = ND_Forest, fill = "darkgreen") +
                geom_sf(data = ND_Lake, fill = "blue", col = "blue") +
                geom_sf(data = ND_River, col = "blue") +
                geom_sf(data = ND_Highway, color = "grey") +
                geom_sf(data = ND_Cities, color = "grey80") +
                geom_sf(data = ND_AMBI, color = "brown") +
                theme_classic() +
                coord_sf()
        } else {
            ggplot() +
                geom_sf(data = ND_state, fill = "lightgreen") +
                geom_sf(data = ND_Forest, fill = "darkgreen") +
                geom_sf(data = ND_Lake, fill = "blue", col = "blue") +
                geom_sf(data = ND_River, col = "blue") +
                geom_sf(data = ND_Highway, color = "grey") +
                geom_sf(data = ND_Cities, color = "grey80") +
                geom_sf(data = ND_KIDE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
        }
    }, res = 96)
    
    
    
    
    output$bird_den <- renderPlot({
        if (input$state_selection == "Washington" & input$bird_species == "Blue-winged Teal") {
            ggplot() +
                geom_tile(data = WA_BWTE.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = WA_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = WA_BWTE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        } else if (input$state_selection == "Washington" & input$bird_species == "American Bittern") {
            ggplot() +
                geom_tile(data = WA_AMBI.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = WA_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = WA_AMBI, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        } else if (input$state_selection == "Washington" & input$bird_species == "Killdeer") {
            ggplot() +
                geom_tile(data = WA_KIDE.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = WA_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = WA_KIDE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "Blue-winged Teal") {
            ggplot() +
                geom_tile(data = ND_BWTE.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = ND_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = ND_BWTE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "American Bittern") {
            ggplot() +
                geom_tile(data = ND_AMBI.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = ND_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = ND_AMBI, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        } else {
            
            ggplot() +
                geom_tile(data = ND_KIDE.den_plot, aes(x=x,y=y,fill=z)) +
                scale_fill_continuous(type = "viridis",na.value="transparent") +
                geom_sf(data = ND_state, col = "lightgreen", alpha = 0) +
                geom_sf(data = ND_KIDE, color = "cadetblue") +
                theme_classic() +
                coord_sf()
            
        }
    }, res = 96)
    
    
    
    output$bird_den_L <- renderPlot({
        if (input$state_selection == "Washington" & input$bird_species == "Blue-winged Teal") {
            plot(envelope(WA_BWTE_MPPP, fun = Lest, verbose=FALSE), main="Washington Blue-winged Teal L")
            
        } else if (input$state_selection == "Washington" & input$bird_species == "American Bittern") {
            plot(envelope(WA_AMBI_MPPP, fun = Lest, verbose=FALSE), main="Washington American Bittern L")
            
        } else if (input$state_selection == "Washington" & input$bird_species == "Killdeer") {
            plot(envelope(WA_KIDE_MPPP, fun = Lest, verbose=FALSE), main="Washington Killdeer L")
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "Blue-winged Teal") {
            plot(envelope(ND_BWTE_MPPP, fun = Lest, verbose=FALSE), main="North Dakota Blue-winged Teal L")
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "American Bittern") {
            plot(envelope(ND_AMBI_MPPP, fun = Lest, verbose=FALSE), main="North Dakota American Bittern L")
            
        } else {
            plot(envelope(ND_KIDE_MPPP, fun = Lest, verbose=FALSE), main="North Dakota Killdeer L")
            
            
        }
    }, res = 96)
    
    
    output$bird_fors_dis <- renderPlot({
        if (input$state_selection == "Washington" & input$bird_species == "Blue-winged Teal") {
            ggplot(WA_BWTE_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "yellow") + 
                labs(x="Meters Between Blue-winged Teal and Nearest National Forest") +
                theme_minimal()
            
        } else if (input$state_selection == "Washington" & input$bird_species == "American Bittern") {
            ggplot(WA_AMBI_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "brown") + 
                labs(x="Meters Between American Bittern and Nearest National Forest") +
                theme_minimal()
            
        } else if (input$state_selection == "Washington" & input$bird_species == "Killdeer") {
            ggplot(WA_KIDE_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "cadetblue") + 
                labs(x="Meters Between American Bittern and Nearest National Forest") +
                theme_minimal()
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "Blue-winged Teal") {
            ggplot(ND_BWTE_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "yellow") + 
                labs(x="Meters Between Blue-winged Teal and Nearest National Forest") +
                theme_minimal()
            
        } else if (input$state_selection == "North Dakota" & input$bird_species == "American Bittern") {
            ggplot(ND_AMBI_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "brown") + 
                labs(x="Meters Between American Bittern and Nearest National Grasslands") +
                theme_minimal()
            
        } else {
            ggplot(ND_KIDE_sf.wit.dist) + 
                geom_histogram(aes(x=distance), fill = "cadetblue") + 
                labs(x="Meters Between American Bittern and Nearest National Grasslands") +
                theme_minimal()
            
            
        }
    }, res = 96)
    
    output$spat_text <- renderText(paste0("Spatial Analysis of 
        Distance between Birds and other points 
        of interest (rivers, highways, cities, and lakes) - also with respective maps"))
    

    
}



shinyApp(ui, server)

