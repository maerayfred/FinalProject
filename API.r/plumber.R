
# Bringing in my best model

best<-rf_wkf|>
  finalize_workflow(rf_best_params)|>
  fit(train_data)

matrix2<-conf_mat(data |> mutate(estimate = best |> predict(data) |> pull()) ,Diabetes_binary,estimate)

my_table<-tableGrob(matrix2$table)


# Saving so we can use it in the API
save(best, file = "best.RData")
save(my_table,file="my_table.RData")

library(plumber)
library(dplyr)
library(caret)
library(gridExtra)
library(grid)
library(randomForest)


# Load the saved data
load("best.RData")
load("my_table.RData")

# Define the API endpoint
#* @apiTitle Final Project API's

#* Predict the most prevalent class of the categorical variable
#* @param Sex Female or Male
#* @param Smoker Non_Smoker or Smoker
#* @param AnyHealthcare No_Health_Coverage or Yes_Health_Coverage
#* @param GenHlth Poor, Fair, Good, Very_Good, Excellent
#* @param PhysActivity Physical_Activity or No_Physical_Activity
#* @post /pred
function(Sex=Female,Smoker=Non_Smoker,AnyHealthcare=Yes_Health_Coverage,GenHlth=Very_Good,PhysActivity=Physical_Activity) {
  
  # Call in the data 
  data_new<-data2
  
  
  # Creating Prediction
  prediction <- predict(best, data2)
  
  # Get the most prevalent class
  class_table <- table(prediction)
  prevalent_class <- names(which.max(class_table))
  
  # Return the results
  list(predicted_class = prevalent_class)
}

#* Echo back the input
#* @get /info
function(){
  list( "Name"="Maegan Frederick ",
        "url"="https://maerayfred.github.io/FinalProject/")
}

#* Plot a matrix
#* @serializer png
#* @get /confusion
function() {
  
  grid.arrange(arrangeGrob(my_table))
  
}

# Programmatically alter your API
#* @plumber
function(pr) {
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json())
}
