# Load BIOMOD.model.out
mod <- load(paste(".\\", spname, "\\", spname, ".", folder.name, ".models.out",
                  sep = "")
)
model <- get(mod)


variables_importance(model)


## Ensemble modelling
myBiomodEM <- BIOMOD_EnsembleModeling(modeling.output = model,
                                      chosen.models = 'all',
                                      em.by = 'all',
                                      eval.metric = c('TSS'),
                                      eval.metric.quality.threshold = c(0.7),
                                      # Models.eval.meth must be one or two, because temporary raster folder can't store data of models for more than 3 evaluation metrics.
                                      # If you run this on computer with bigger storage for the folder,  it may run without the error. (In writeBin(as.vector(v[start:end, ]), x@file@con, size = x@file@dsize) :problem writing to connection)
                                      models.eval.meth = c('TSS'), #, 'ROC', 'ACCURACY'
                                      prob.mean = TRUE,
                                      prob.cv = FALSE,
                                      prob.ci = FALSE,
                                      prob.ci.alpha = 0.05,
                                      prob.median = FALSE,
                                      committee.averaging = FALSE,
                                      prob.mean.weight = TRUE,
                                      prob.mean.weight.decay = 'proportional'
)




## PLOTS THE PROJECTIONS
setwd("Y:\\BIOMOD for Grid2")
# Get folder names
folders <- list.dirs(getwd(), full.names = FALSE, recursive = F) %>% grepl(genus_name, .) %>% list.dirs(getwd(), full.names = FALSE, recursive = F)[.]

spname <- folders[1] 
folder.name = "SAI_15Jan19"
BIOMODproj.name = "SAI_15Jan19"
ensambleProj.name = "SAI_15Jan19_ensamble"

