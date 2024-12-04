
library(plumber)
library(caret)
library(gridExtra)
library(grid)


data<-read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

data<- data|>
  mutate(HighBP=factor(HighBP, levels=c(0,1),labels=c("Normal_BP","High_BP")),
         HighChol=factor(HighChol, levels=c(0,1),labels=c("Normal_Chol","High_Chol")),
         CholCheck=as.factor(CholCheck),
         Smoker=factor(Smoker,levels=c(0,1),labels=c("Non_Smoker","Smoker")),
         Stroke=factor(Stroke, levels=c(0,1),labels=c("No_Stroke","Yes_Stroke")),
         Fruits=factor(Fruits,levels=c(0,1),labels=c("No_Fruits","Eats_Fruits")),
         Veggies=factor(Veggies,levels=c(0,1),labels=c("No_Veggies","Eats_Veggies")),
         Sex=factor(Sex, level=c(0,1), labels = c("Female","Male")),
         GenHlth=factor(GenHlth,levels=c(1,2,3,4,5), 
                        labels=c("Excellent","Very Good","Good","Fair","Poor")),
         Age=factor(Age, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                    labels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                             "60-64","65-69","70-74","75-79","80+" )),
         Education=factor(Education,levels=c(1,2,3,4,5,6),labels=c("None-K","Grades 1-8",
                                                                   "Grades 9-11","HS Diploma","Some College","Bachelor's Degree or Higher")),
         Income=factor(Income,levels=c(1,2,3,4,5,6,7,8),labels=c("<10K","<15K",
                                                                 "<20K","<25K","<35K","<50K",
                                                                 "<75K",">=75K")),
         Diabetes_binary=factor(Diabetes_binary,levels=c(0,1),labels=c("Not_Diabetic","Diabetic")),
         HeartDiseaseorAttack=factor(HeartDiseaseorAttack,levels=c(0,1),labels=c("No_Heart Disease","Heart_Disease")),
         PhysActivity=factor(PhysActivity,levels=c(0,1),labels=c("No_Physical Activity","Physical_Activity")),
         HvyAlcoholConsump=factor(HvyAlcoholConsump,levels=c(0,1),labels=c("Not_Heavy Drinker","Heavy_Drinker")),
         AnyHealthcare=factor(AnyHealthcare,levels=c(0,1),labels=c("No_Health_Coverage","Yes_Health_Coverage")),
         NoDocbcCost=factor(NoDocbcCost,levels=c(0,1),labels=c("Cost_Issue","Cost_Not_Issue")),
         DiffWalk=factor(DiffWalk,levels=c(0,1),labels=c("No_Difficulty_Walking","Difficulty_Walking"))
  )

set.seed(1234)

split_data<-initial_split(data,prop=0.1,strata = Diabetes_binary)
train_data<-training(split_data)
test_data<-testing(split_data)
cv_fold_data<-vfold_cv(train_data,v=5,strata=Diabetes_binary)


rec1<-recipe(Diabetes_binary~ Sex+Smoker+AnyHealthcare+GenHlth+PhysActivity,data=train_data)|>
  step_dummy(Sex,Smoker,GenHlth,AnyHealthcare,PhysActivity)



rf_spec<-rand_forest(mtry=tune())|>
  set_engine("ranger")|>
  set_mode("classification")

rf_wkf<-workflow()|>
  add_recipe(rec1)|>
  add_model(rf_spec)

rf_fit<-rf_wkf|>
  tune_grid(resamples=cv_fold_data,
            grid=2,
            metrics=metric_set(mn_log_loss))

rf_fit|>
  collect_metrics()|>
  filter(.metric=="mn_log_loss")|>
  arrange(mean)


rf_best_params<-select_best(rf_fit,metric = "mn_log_loss")
rf_best_params


rf_final_wkf<-rf_wkf|>
  finalize_workflow(rf_best_params)


best<-rf_wkf|>
  finalize_workflow(rf_best_params)|>
  fit(data)
best

#Confusion Matrix
rf_wkf<-workflow()|>
  add_recipe(rec1)|>
  add_model(rf_spec)

rf_final_fit<-rf_final_wkf|>
  last_fit(split_data,metrics=metric_set(mn_log_loss))|>
  collect_metrics()

rf_final_fit

matrix2<-conf_mat(data |> mutate(estimate = best |> predict(data) |> pull()) ,Diabetes_binary,estimate)

my_table<-tableGrob(matrix2$table)


library(plumber)

#* @apiTitle Final Project API
#* @apiDescription Project Information


#* Echo back the input
#* @get /info
function(){
 list( "Name"="Maegan Frederick ",
  "url"="https://github.com/maerayfred/FinalProject")
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
