addNumberVisit <- function(x, cdm, id_visit, window = c(NA, NA), name = "number_visit", compute = TRUE) {
  x <- cdm[["visit_occurrence"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "visit_concept_id", "visit_start_date"
    ) %>%
    dplyr::filter(visit_concept_id %in% id_visit) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "cohort_start_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[1]) <=
        .data$visit_start_date
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[2]) >=
        .data$visit_start_date
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date
    ) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::compute() %>%
    dplyr::right_join(
      x,
      by = c("subject_id", "cohort_start_date")
    ) %>%
    dplyr::mutate(!!name := dplyr::if_else(is.na(.data[[name]]), 0, .data[[name]])) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
}

addFlagEvent_in <- function(x, cdm, eventTableName, filter = NULL, window = c(NA, NA), name = "number_event", eventAt = "cohort_start_date", eventDate = "cohort_start_date", compute = TRUE) {
  events <- cdm[[eventTableName]]
  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      events <- events %>%
        dplyr::filter_at(
          dplyr::vars(dplyr::all_of(namesFilter[k])), 
          dplyr::any_vars(. %in% !!filter[[k]])
        )
    }
  }
  if ("person_id" %in% colnames(events)) {
    events <- events %>%
      dplyr::rename("subject_id" = "person_id")
  }
  events <- events %>%
    dplyr::select("subject_id","event_date" = dplyr::all_of(eventDate)) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "event_at" = dplyr::all_of(eventAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(date_dif = !!CDMConnector::datediff("event_at", "event_date"))
  if (!is.na(window[1])) {
    events <- events %>%
      dplyr::filter(.data$date_dif >= !!window[1])
  }
  if (!is.na(window[2])) {
    events <- events %>%
      dplyr::filter(.data$date_dif <= !!window[2])
  }
  events <- events %>%
    dplyr::group_by(.data$subject_id, .data$event_at) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!eventAt := "event_at") %>%
    dplyr::right_join(
      x,
      by = c("subject_id", eventAt)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(name), 
      ~ dplyr::if_else(is.na(.x), 0, 1)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name))) 
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
}

addVisit <- function(x, cdm, eventTableName, eventId = NULL, window = c(NA, NA), name = "event", eventDate = "cohort_start_date", eventAt = "cohort_start_date", order = "first", compute = TRUE) {
  eventTable <- cdm[[eventTableName]]
  if (!is.null(eventId)) {
    eventTable <- eventTable %>% dplyr::filter(.data$visit_concept_id %in% .env$eventId)
  }
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", eventAt))))
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "all") {
    xx <- xx %>%
      dbplyr::window_order(.data$event_date) %>%
      dplyr::mutate(nam = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nam = paste0(.env$name, "_", .data$nam)) %>%
      tidyr::pivot_wider(names_from = "nam", values_from = "event_date")
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}

tryNA <- function(x){
  x <- try(x)
  if(inherits(x,'try-error')) return(NA)
  x
}

poLCA_oxf <-
  function (formula, data, nclass = 2, maxiter = 1000, graphs = FALSE,
            tol = 0.0000000001, na.rm = TRUE, probs.start = NULL, nrep = 1,
            verbose = TRUE, calc.se = TRUE,impVal=0)
  {
    nanProbs <- integer(0)
    starttime <- Sys.time()
    mframe <- model.frame(formula, data, na.action = NULL)
    mf <- model.response(mframe)
    if (any(mf < 1, na.rm = TRUE) | any(round(mf) != mf, na.rm = TRUE)) {
      cat("\n ALERT: some manifest variables contain values that are not\n    positive integers. For poLCA to run, please recode categorical\n    outcome variables to increment from 1 to the maximum number of\n    outcome categories for each variable. \n\n")
      ret <- NULL
    }
    else {
      data <- data[rowSums(is.na(model.matrix(formula, mframe))) ==
                     0, ]
      if (na.rm) {
        mframe <- model.frame(formula, data)
        y <- model.response(mframe)
      }
      else {
        mframe <- model.frame(formula, data, na.action = NULL)
        y <- model.response(mframe)
      }
      if (any(sapply(lapply(as.data.frame(y), table), length) ==
              1)) {
        y <- y[, !(sapply(apply(y, 2, table), length) ==
                     1)]
        cat("\n ALERT: at least one manifest variable contained only one\n    outcome category, and has been removed from the analysis. \n\n")
      }
      y[is.na(y)] <- 0
      
      
      x <- model.matrix(formula, mframe)
      N <- nrow(y)
      J <- ncol(y)
      K.j <- t(matrix(apply(y, 2, max)))
      R <- nclass
      S <- ncol(x)
      if (S > 1) {
        calc.se <- TRUE
      }
      eflag <- FALSE
      probs.start.ok <- TRUE
      ret <- list()
      if (R == 1) {
        ret$probs <- list()
        for (j in 1:J) {
          ret$probs[[j]] <- matrix(NA, nrow = 1, ncol = K.j[j])
          for (k in 1:K.j[j]) {
            ret$probs[[j]][k] <- sum(y[, j] == k)/sum(y[,
                                                        j] > 0)
          }
        }
        ret$probs.start <- ret$probs
        ret$P <- 1
        ret$posterior <- ret$predclass <- prior <- matrix(1,
                                                          nrow = N, ncol = 1)
        ret$llik <- sum(log(poLCA:::poLCA.ylik.C(poLCA:::poLCA.vectorize(ret$probs),
                                                 y)))
        if (calc.se) {
          se <- poLCA:::poLCA.se(y, x, ret$probs, prior, ret$posterior)
          ret$probs.se <- se$probs
          ret$P.se <- se$P
        }
        else {
          ret$probs.se <- NA
          ret$P.se <- NA
        }
        ret$numiter <- 1
        ret$probs.start.ok <- TRUE
        ret$coeff <- NA
        ret$coeff.se <- NA
        ret$coeff.V <- NA
        ret$eflag <- FALSE
        if (S > 1) {
          cat("\n ALERT: covariates not allowed when nclass=1; will be ignored. \n \n")
          S <- 1
        }
      }
      else {
        if (!is.null(probs.start)) {
          if ((length(probs.start) != J) | (!is.list(probs.start))) {
            probs.start.ok <- FALSE
          }
          else {
            if (sum(sapply(probs.start, dim)[1, ] == R) !=
                J)
              probs.start.ok <- FALSE
            if (sum(sapply(probs.start, dim)[2, ] == K.j) !=
                J)
              probs.start.ok <- FALSE
            if (sum(round(sapply(probs.start, rowSums),
                          4) == 1) != (R * J))
              probs.start.ok <- FALSE
          }
        }
        ret$llik <- -Inf
        ret$attempts <- NULL
        for (repl in 1:nrep) {
          error <- TRUE
          firstrun <- TRUE
          probs <- probs.init <- probs.start
          while (error) {
            error <- FALSE
            b <- rep(0, S * (R - 1))
            prior <- poLCA:::poLCA.updatePrior(b, x, R)
            if ((!probs.start.ok) | (is.null(probs.start)) |
                (!firstrun) | (repl > 1)) {
              probs <- list()
              for (j in 1:J) {
                probs[[j]] <- matrix(runif(R * K.j[j]),
                                     nrow = R, ncol = K.j[j])
                probs[[j]] <- probs[[j]]/rowSums(probs[[j]])
              }
              probs.init <- probs
            }
            vp <- poLCA:::poLCA.vectorize(probs)
            iter <- 1
            llik <- matrix(NA, nrow = maxiter, ncol = 1)
            llik[iter] <- -Inf
            dll <- Inf
            while ((iter <= maxiter) & (dll > tol) & (!error)) {
              iter <- iter + 1
              rgivy <- poLCA:::poLCA.postClass.C(prior, vp, y)
              vp$vecprobs <- poLCA:::poLCA.probHat.C(rgivy, y,
                                                     vp)
              if (S > 1) {
                dd <- poLCA:::poLCA.dLL2dBeta.C(rgivy, prior, x)
                b <- b + ginv(-dd$hess) %*% dd$grad
                prior <- poLCA:::poLCA.updatePrior(b, x, R)
              }
              else {
                prior <- matrix(colMeans(rgivy), nrow = N,
                                ncol = R, byrow = TRUE)
              }
              nanProbs <- which(is.nan(vp$vecprobs))
              if(length(nanProbs)){
                vp$vecprobs[nanProbs] <- impVal
                if(verbose) message(paste('Replacing NaN probs with',impVal))
              }
              llik[iter] <- tryNA(sum(log(rowSums(prior * poLCA:::poLCA.ylik.C(vp,
                                                                               y)))))
              dll <- llik[iter] - llik[iter - 1]
              if (is.na(dll)) {
                error <- TRUE
              }
              else if ((S > 1) & (dll < -0.0000001)) {
                error <- TRUE
              }
            }
            if (!error) {
              if (calc.se) {
                se <- poLCA:::poLCA.se(y, x, poLCA:::poLCA.unvectorize(vp),
                                       prior, rgivy)
              }
              else {
                se <- list(probs = NA, P = NA, b = NA,
                           var.b = NA)
              }
            }
            else {
              eflag <- TRUE
            }
            firstrun <- FALSE
          }
          ret$attempts <- c(ret$attempts, llik[iter])
          #ret$fullllik <- llik
          if (llik[iter] > ret$llik) {
            ret$llik <- llik[iter]
            ret$probs.start <- probs.init
            ret$probs <- poLCA:::poLCA.unvectorize(vp)
            ret$probs.se <- se$probs
            ret$P.se <- se$P
            ret$posterior <- rgivy
            ret$predclass <- apply(ret$posterior, 1, which.max)
            ret$P <- colMeans(ret$posterior)
            ret$numiter <- iter - 1
            ret$probs.start.ok <- probs.start.ok
            if (S > 1) {
              b <- matrix(b, nrow = S)
              rownames(b) <- colnames(x)
              rownames(se$b) <- colnames(x)
              ret$coeff <- b
              ret$coeff.se <- se$b
              ret$coeff.V <- se$var.b
            }
            else {
              ret$coeff <- NA
              ret$coeff.se <- NA
              ret$coeff.V <- NA
            }
            ret$eflag <- eflag
          }
          if (nrep > 1 & verbose) {
            cat("Model ", repl, ": llik = ", llik[iter],
                " ... best llik = ", ret$llik, "\n", sep = "")
            flush.console()
          }
        }
      }
      names(ret$probs) <- colnames(y)
      if (calc.se) {
        names(ret$probs.se) <- colnames(y)
      }
      ret$npar <- (R * sum(K.j - 1)) + (R - 1)
      if (S > 1) {
        ret$npar <- ret$npar + (S * (R - 1)) - (R - 1)
      }
      ret$aic <- (-2 * ret$llik) + (2 * ret$npar)
      ret$bic <- (-2 * ret$llik) + (log(N) * ret$npar)
      ret$Nobs <- sum(rowSums(y == 0) == 0)
      if (all(rowSums(y == 0) > 0)) {
        ret$Chisq <- NA
        ret$Gsq <- NA
        ret$predcell <- NA
      }
      else {
        compy <- poLCA:::poLCA.compress(y[(rowSums(y == 0) == 0),
        ])
        datacell <- compy$datamat
        rownames(datacell) <- NULL
        freq <- compy$freq
        if (!na.rm) {
          fit <- matrix(ret$Nobs * (poLCA:::poLCA.ylik.C(poLCA:::poLCA.vectorize(ret$probs),
                                                         datacell) %*% ret$P))
          ret$Chisq <- sum((freq - fit)^2/fit) + (ret$Nobs -
                                                    sum(fit))
        }
        else {
          fit <- matrix(N * (poLCA:::poLCA.ylik.C(poLCA:::poLCA.vectorize(ret$probs),
                                                  datacell) %*% ret$P))
          ret$Chisq <- sum((freq - fit)^2/fit) + (N - sum(fit))
        }
        ret$predcell <- data.frame(datacell, observed = freq,
                                   expected = fit)
        ret$Gsq <- 2 * sum(freq * log(freq/fit))
      }
      y[y == 0] <- NA
      ret$y <- data.frame(y)
      ret$x <- data.frame(x)
      
      if(length(nanProbs)){
        vp$vecprobs[nanProbs] <- NaN
        ret$probs <- poLCA:::poLCA.unvectorize(vp)
        names(ret$probs) <- colnames(y)
      }
      
      for (j in 1:J) {
        rownames(ret$probs[[j]]) <- paste("class ", 1:R,
                                          ": ", sep = "")
        if (is.factor(data[, match(colnames(y), colnames(data))[j]])) {
          lev <- levels(data[, match(colnames(y), colnames(data))[j]])
          colnames(ret$probs[[j]]) <- lev
          ret$y[, j] <- factor(ret$y[, j], labels = lev)
        }
        else {
          colnames(ret$probs[[j]]) <- paste("Pr(", 1:ncol(ret$probs[[j]]),
                                            ")", sep = "")
        }
      }
      ret$N <- N
      ret$maxiter <- maxiter
      ret$resid.df <- min(ret$N, (prod(K.j) - 1)) - ret$npar
      class(ret) <- "poLCA"
      if (graphs)
        plot.poLCA(ret)
      if (verbose)
        poLCA:::print.poLCA(ret)
      ret$time <- Sys.time() - starttime
      
    }
    ret$call <- match.call()
    return(ret)
  }

insertTable <- function(x,
                        cdm, 
                        name,
                        overwrite = TRUE) {
  con <- attr(cdm, "dbcon")
  writeSchema <- attr(cdm, "write_schema")
  checkTableExist <- name %in% CDMConnector::listTables(con, writeSchema)
  if (checkTableExist) {
    if (overwrite) {
      DBI::dbRemoveTable(con, CDMConnector:::inSchema(writeSchema, name))
    } else {
      stop(paste0("'", name, "' table already exists."))
    }
  }
  DBI::dbCreateTable(con, CDMConnector:::inSchema(writeSchema, name), x)
  DBI::dbAppendTable(con, CDMConnector:::inSchema(writeSchema, name), x)
  if (methods::is(con, "duckdb_connection")) {
    ref <- dplyr::tbl(con, paste(c(writeSchema, name), collapse = "."))
  } else if (length(writeSchema) == 2) {
    ref <- dplyr::tbl(con,
                      dbplyr::in_catalog(writeSchema[[1]], writeSchema[[2]], name))
  } else if (length(writeSchema) == 1) {
    ref <- dplyr::tbl(con, dbplyr::in_schema(writeSchema, name))
  } else {
    ref <- dplyr::tbl(con, name)
  }
  return(ref)
}


getCohortCount <- function(cohort) {
  cohort %>%
    group_by(cohort_definition_id) %>%
    summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    collect() %>%
    arrange(cohort_definition_id)
}
