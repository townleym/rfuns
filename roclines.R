roclines = function(model, truth) {
# model = mod.explode
# truth = a$match
#     threshold = 0.5
#     truth = a$match
    fitted = model$fitted.values
    
    trueposrate = function(threshold, truth, fitted) {
        pred = fitted > threshold
        
        yes = truth == T
        truepos = truth == T & pred == T
        trueposrate = sum(truepos) / sum(yes)
        
        return(trueposrate)
    }
    
    falseposrate = function(threshold, truth, fitted) {
        pred = fitted > threshold
        
        no = truth == F
        falsepos = truth == F & pred == T
        falseposrate = sum(falsepos) / sum(no)
        
        return(falseposrate)
    }
    
    thresholds = 0:100 / 100    
    
    tps = sapply(thresholds, function(x) {trueposrate(x, truth = truth, fitted = fitted)})
    fps = sapply(thresholds, function(x) {falseposrate(x, truth = truth, fitted = fitted)})
    
#     x = rev(fps)
#     y = rev(tps)

    cbind(fpr = rev(fps), tpr = rev(tps)) 
}
