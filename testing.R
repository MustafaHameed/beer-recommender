# Prepare Evaluation Scheme
# use cross-validation (k=5) 
evaluation_scheme <- evaluationScheme(r[1:1000], method="cross-validation", k=5, given=-1, goodRating=4) 

# vector for varying value of k for k nearest items in recommender model
k_vector <- c(1,5,10,30,seq(50,150,25))
models_to_evaluate <- lapply(k_vector, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", k_vector)

# Evaluation results for all recommender models
# k varies, n for top N list recommendation varies
evaluation_results <- evaluate(evaluation_scheme, method=models_to_evaluate, n=c(1,3,5,10,15,20))

# results can be stored in a confusion matrix
eval_results <- getConfusionMatrix(evaluation_results)[[5]]

#mar.default <- c(1.5,  .5, .25, 2)       # Right margin space for legend
# Plot results for precision/recall trade-off
plot(evaluation_results, "prec/rec", annotate=2, legend=list(x=0.016, y=0.0044))
title("Precision/Recall for different IBCF Models (k closest items varies)")

# Plot results for ROC
plot(evaluation_results, "ROC", annotate=2, legend=list(x=0.0, y=0.022))
title("ROC Curve")

## predict missing ratings
## (results in RMSE, MSE and MAE)
rmse <- evaluate(evaluation_scheme, method=models_to_evaluate, type="ratings")

k = c(1,5,10,30,50,75,100,125,150)
x <- NULL
x <- cbind(x, k)
err <- NULL
err <- rbind(err, avg(rmse$IBCF_k_1)[1])
err <- rbind(err, avg(rmse$IBCF_k_5)[1])
err <- rbind(err, avg(rmse$IBCF_k_10)[1])
err <- rbind(err, avg(rmse$IBCF_k_30)[1])
err <- rbind(err, avg(rmse$IBCF_k_50)[1])
err <- rbind(err, avg(rmse$IBCF_k_75)[1])
err <- rbind(err, avg(rmse$IBCF_k_100)[1])
err <- rbind(err, avg(rmse$IBCF_k_125)[1])
err <- rbind(err, avg(rmse$IBCF_k_150)[1])
x <- cbind(x, err)
colnames(x)[2] <- "rmse"


# Plot results for RMSE
plot(x = x, pch=16, type="b")
axis(1, at = seq(0,150,10))
title("RMSE")