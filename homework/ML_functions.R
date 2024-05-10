#########################################################################################################
#  Program:     Functions for implementing machine learning methods                                     #
#  References: "Double/Debiased Machine Learning of Treatment and Causal Parameters",  AER P&P 2017     #
#              "Double Machine Learning for Treatment and Causal Parameters",  Arxiv 2016               #
#  by V.Chernozhukov, D. Chetverikov, M. Demirer, E. Duflo, C. Hansen, W. Newey                         #
#########################################################################################################


############### Part I: MC Algorithms: Rlasso, Tree, Neuralnet, Nnet, Boosting, Random Forest ###################

lassoF <- function(datause, dataout, form_x, form_y, logit = FALSE, alp = alp, arg = arg, s = s) {
    form <- as.formula(paste(form_y, "~", form_x))

    if (logit == TRUE) {
        fit <- lm(form, x = TRUE, y = TRUE, data = datause)
        lasso <- do.call(cv.glmnet, append(list(x = fit$x[, -1], y = fit$y, family = "binomial", alpha = alp), arg))
    }

    if (logit == FALSE) {
        fit <- lm(form, x = TRUE, y = TRUE, data = datause)
        lasso <- do.call(cv.glmnet, append(list(x = fit$x[, -1], y = fit$y, alpha = alp), arg))
    }

    fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
    yhatuse <- predict(lasso, newx = fit.p$x[, -1], s = s)
    if (logit == TRUE) {
        yhatuse <- predict(lasso, newx = fit.p$x[, -1], s = s, type = "response")
    }
    resuse <- fit.p$y - yhatuse
    xuse <- fit.p$x

    fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
    yhatout <- predict(lasso, newx = fit.p$x[, -1], s = s)
    if (logit == TRUE) {
        yhatout <- predict(lasso, newx = fit.p$x[, -1], s = s, type = "response")
    }
    resout <- fit.p$y - yhatout
    xout <- fit.p$x

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, xuse = xuse, xuse = xout, model = lasso, yout = fit.p$y, form = form))
}


rlassoF <- function(datause, dataout, form_x, form_y, post, logit = FALSE, arg) {
    form <- as.formula(paste(form_y, "~", form_x))

    if (logit == FALSE) {
        lasso <- do.call(rlasso, append(list(formula = form, post = post, data = datause), arg))
    }

    if (logit == TRUE) {
        lasso <- do.call(rlassologit, append(list(formula = form, post = post, data = datause), arg))
    }

    fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
    yhatuse <- predict(lasso, newdata = fit.p$x, type = "response")
    resuse <- lasso$res
    xuse <- fit.p$x

    fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
    yhatout <- predict(lasso, newdata = fit.p$x, type = "response")
    resout <- fit.p$y - predict(lasso, newdata = fit.p$x, type = "response")
    xout <- fit.p$x

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, xuse = xuse, xuse = xout, model = lasso, yout = fit.p$y))
}

tree <- function(datause, dataout, form_x, form_y, method = method, arg = arg) {
    form <- as.formula(paste(form_y, "~", form_x))
    trees <- do.call(rpart, append(list(formula = form, data = datause), arg))
    bestcp <- trees$cptable[which.min(trees$cptable[, "xerror"]), "CP"]
    ptree <- prune(trees, cp = bestcp)

    fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
    yhatuse <- predict(ptree, newdata = datause)
    resuse <- fit.p$y - yhatuse
    xuse <- fit.p$x

    fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
    yhatout <- predict(ptree, newdata = dataout)
    resout <- fit.p$y - yhatout
    xout <- fit.p$x

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, xuse = xuse, xuse = xout, model = ptree))
}

nnetF <- function(datause, dataout, form_x, form_y, clas = FALSE, arg) {
    linout <- FALSE
    if (clas == TRUE) {
        linout <- FALSE
    }
    f <- sapply(datause, is.factor)

    maxs <- apply(datause[, !f], 2, max)
    mins <- apply(datause[, !f], 2, min)

    datause[, !f] <- as.data.frame(scale(datause[, !f], center = mins, scale = maxs - mins))
    dataout[, !f] <- as.data.frame(scale(dataout[, !f], center = mins, scale = maxs - mins))

    form <- as.formula(paste(form_y, "~", form_x))
    nn <- do.call(nnet, append(list(formula = form, data = datause, linout = linout), arg))
    k <- which(colnames(dataout) == form_y)

    fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
    yhatuse <- predict(nn, datause) * (maxs[k] - mins[k]) + mins[k]
    resuse <- fit.p$y * ((maxs[k] - mins[k]) + mins[k]) - yhatuse
    xuse <- fit.p$x

    fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
    yhatout <- predict(nn, dataout) * (maxs[k] - mins[k]) + mins[k]
    resout <- fit.p$y * (maxs[k] - mins[k]) + mins[k] - yhatout
    xout <- fit.p$x

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, xuse = xuse, xuse = xout, model = nn, min = mins, max = maxs, k = k, f = f))
}

boost <- function(datause, dataout, form_x, form_y, bag.fraction = .5, interaction.depth = 2, n.trees = 1000, shrinkage = .01, distribution = "gaussian", option) {
    form <- as.formula(paste(form_y, "~", form_x))
    boostfit <- do.call(gbm, append(list(formula = form, distribution = distribution, data = datause), option))
    if (option[["cv.folds"]] > 0) {
        best <- gbm.perf(boostfit, plot.it = FALSE, method = "cv")
    } else {
        best <- gbm.perf(boostfit, plot.it = FALSE, method = "OOB")
    }

    fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
    yhatuse <- predict(boostfit, n.trees = best)
    resuse <- fit.p$y - yhatuse
    xuse <- fit.p$x

    fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
    yhatout <- predict(boostfit, n.trees = best, newdata = dataout, type = "response")
    resout <- fit.p$y - yhatout
    xout <- fit.p$x

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, xuse = xuse, xuse = xout, model = boostfit, best = best))
}

RF <- function(datause, dataout, form_x, form_y, x = NA, y = NA, xout = NA, yout = NA, nodesize, arg, reg = TRUE, tune = FALSE) {
    yhatout <- NA
    reuse <- NA
    yhatuse <- NA
    resout <- NA


    if (is.na(x)) {
        form <- as.formula(paste(form_y, "~", form_x))

        if (tune == FALSE) {
            forest <- do.call(randomForest, append(list(formula = form, nodesize = nodesize, data = datause), arg))
        }
        if (tune == TRUE) {
            fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
            forest_t <- tuneRF(x = fit.p$x, y = fit.p$y, mtryStart = floor(sqrt(ncol(fit.p$x))), stepFactor = 1.5, improve = 0.05, nodesize = 5, ntree = ntree, doBest = TRUE, plot = FALSE, trace = FALSE)
            min <- forest_t$mtry
            forest <- randomForest(form, nodesize = nodesize, mtry = min, ntree = ntree, na.action = na.omit, data = datause)
        }

        fit.p <- lm(form, x = TRUE, y = TRUE, data = datause)
        yhatuse <- as.numeric(forest$predicted)
        resuse <- as.numeric(fit.p$y) - yhatuse
        fit.p <- lm(form, x = TRUE, y = TRUE, data = dataout)
        if (reg == TRUE) {
            yhatout <- predict(forest, dataout, type = "response")
        }
        if (reg == FALSE) {
            yhatout <- predict(forest, dataout, type = "prob")[, 2]
        }

        resout <- (as.numeric(fit.p$y)) - as.numeric(yhatout)
    }

    if (!is.na(x)) {
        forest <- do.call(randomForest, append(list(x = x, y = y, nodesize = nodesize, data = datause), arg))
        yhatuse <- as.numeric(forest$predicted)
        resuse <- y - yhatuse

        if (!is.na(xout)) {
            if (reg == TRUE) {
                yhatout <- predict(forest, newdata = xout, type = "response")
            }
            if (reg == FALSE) {
                yhatout <- predict(forest, newdata = xout, type = "prob")[, 2]
            }
            resuse <- yout - as.numeric(yhatout)
        }
    }

    return(list(yhatuse = yhatuse, resuse = resuse, yhatout = yhatout, resout = resout, model = forest))
}

########################## Part II:Auxilary Functions  ####################################################;


checkBinary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L && all(sort(x[1:2]) == 0:1)
}

error <- function(yhat, y) {
    err <- sqrt(mean((yhat - y)^2))
    mis <- sum(abs(as.numeric(yhat > .5) - (as.numeric(y)))) / length(y)

    return(list(err = err, mis = mis))
}

formC <- function(form_y, form_x, data) {
    form <- as.formula(paste(form_y, "~", form_x))
    fit.p <- lm(form, x = TRUE, y = TRUE, data = data)

    return(list(x = fit.p$x, y = fit.p$y))
}


ATE <- function(y, d, my_d1x, my_d0x, md_x) {
    return(mean((d * (y - my_d1x) / md_x) - ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x))
}



SE.ATE <- function(y, d, my_d1x, my_d0x, md_x) {
    return(sd((d * (y - my_d1x) / md_x) - ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x) / sqrt(length(y)))
}

LATE <- function(y, d, z, my_z1x, my_z0x, mz_x, md_z1x, md_z0x) {
    return(mean(z * (y - my_z1x) / mz_x - ((1 - z) * (y - my_z0x) / (1 - mz_x)) + my_z1x - my_z0x) /
        mean(z * (d - md_z1x) / mz_x - ((1 - z) * (d - md_z0x) / (1 - mz_x)) + md_z1x - md_z0x))
}

SE.LATE <- function(y, d, z, my_z1x, my_z0x, mz_x, md_z1x, md_z0x) {
    return(sd((z * (y - my_z1x) / mz_x - ((1 - z) * (y - my_z0x) / (1 - mz_x)) + my_z1x - my_z0x) /
        mean(z * (d - md_z1x) / mz_x - ((1 - z) * (d - md_z0x) / (1 - mz_x)) + md_z1x - md_z0x)) / sqrt(length(y)))
}
