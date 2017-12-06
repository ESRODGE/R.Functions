assess.prediction <- function(truth, predicted, print = TRUE) {
    predicted <- predicted[!is.na(truth)]
    truth <- truth[!is.na(truth)]
    truth <- truth[!is.na(predicted)]
    predicted <- predicted[!is.na(predicted)]
    if (print == TRUE) {
        cat("Total cases that are not NA: ", length(truth), "\n", sep = "")
        cat("Correct predictions (accuracy): ", sum(truth == predicted), "(", signif(sum(truth == predicted) * 100 / length(truth), 3), "%)\n", sep = "")
    }

    TP <- sum(truth == 1 & predicted == 1)
    TN <- sum(truth == 0 & predicted == 0)
    FP <- sum(truth == 0 & predicted == 1)
    FN <- sum(truth == 1 & predicted == 0)
    P <- TP + FN
    N <- FP + TN
    if (print == TRUE) {
        cat("TPR (sensitivity)=TP/P: ", signif(100 * TP / P, 3), "%\n", sep = "")
        cat("TNR (specificity)=TN/N: ", signif(100 * TN / N, 3), "%\n", sep = "")
        cat("PPV (precision)=TP/(TP+FP): ", signif(100 * TP / (TP + FP), 3), "%\n", sep = "")
        cat("FDR (false discovery)=1-PPV: ", signif(100 * FP / (TP + FP), 3), "%\n", sep = "")
        cat("FPR (false positive)=FP/N=1-TNR: ", signif(100 * FP / N, 3), "%\n", sep = "")
        cat("Error: ", signif(100 - (sum(truth == predicted) * 100 / length(truth)), 3), "%\n", sep = "")
    }
    error <- signif(100 - (sum(truth == predicted) * 100 / length(truth)), 3)
    sensitivity <- signif(100 * TP / P, 3)
    specificity <- signif(100 * TN / N, 3)
    return(data.frame(error = error, sensitivity = sensitivity, specificity = specificity))
}
