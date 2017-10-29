vlog <- function(t) {
  ## link
  linkfun <- function(y) {
    if(t!=0) {
      odds_t <- (y/(1-y))^t
      return(2*(odds_t-1)/(t*(odds_t+1)))
    } else {
      return(log(y/(1-y)))
    }
  }
  ## inverse link
  linkinv <- function(eta) {
    if(t!=0) {
      t_eta <- t*eta
      log_odds <- log((2+t_eta)/(2-t_eta))/t
      odds <- exp(log_odds)
      y <- odds/(1+odds)
      return(y)
    } else {
      log_odds <- eta
      odds <- exp(log_odds)
      y <- odds/(1+odds)
      return(y)
    }
  }
  ## derivative of invlink wrt eta
  mu.eta <- function(eta) {
    if(t!=0) {
      t_eta <- t*eta
      partial <- 4/(4 - t_eta^2)
      log_odds <- log((2+t_eta)/(2-t_eta))/t
      odds <- exp(log_odds)
      y <- odds/(1+odds)
      full <- partial * y * (1-y)
      return(full)
    } else {
      log_odds <- eta
      odds <- exp(log_odds)
      y <- odds/(1+odds)
      return(y*(1-y))
    }
  }
  valideta <- function(eta) {
    if(t!=0) {
      if(abs(x = t*eta) < 2) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
  link <- "t-glm"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}
