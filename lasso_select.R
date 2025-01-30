library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)

# Create test data
set.seed(123)
test_data <- data.frame(
  id = 1:20,
  lat = runif(20, 51.3, 51.6),  # Latitude range for London
  lng = runif(20, -0.3, 0.1)    # Longitude range for London
)

ui <- fluidPage(
  leafletOutput("map"),
  actionButton("clear", "Clear Selection"),
  verbatimTextOutput("selected_output")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = test_data, layerId = ~id, label = ~id) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      onRender("
        function(el, x) {
          var myMap = this;
          var drawnItems = new L.FeatureGroup();
          myMap.addLayer(drawnItems);

          myMap.on('draw:created', function(e) {
            var layer = e.layer;
            drawnItems.addLayer(layer);

            var selectedMarkers = [];
            myMap.eachLayer(function(layer) {
              if (layer instanceof L.Marker) {
                if (drawnItems.getLayers()[0].getBounds().contains(layer.getLatLng())) {
                  selectedMarkers.push(layer.options.layerId);
                  layer.setIcon(L.icon({iconUrl: 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png'}));
                }
              }
            });
            Shiny.onInputChange('selected_markers', selectedMarkers);
          });
        }
      ")
  })

  observeEvent(input$clear, {
    leafletProxy("map") %>%
      clearGroup("draw") %>%
      clearMarkers() %>%
      addMarkers(data = test_data, layerId = ~id, label = ~id)
  })

  output$selected_output <- renderPrint({
    if (!is.null(input$selected_markers)) {
      cat("Selected marker IDs:\n")
      print(input$selected_markers)
    } else {
      cat("No markers selected")
    }
  })
}

shinyApp(ui, server)
