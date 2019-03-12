dating_decision <- function(precise, century, century_section, sign, type) {
  res <- rep(NA, length(precise))
  for (i in 1:length(res)) {
    s <- sign_switch(sign[i], -1)
    if (!is.na(precise[i])) {
      res[i] <- s*precise[i]
    } else if ( !is.na(century[i]) ) {
      if (s == -1) {
        res[i] <- (s * century[i] * 100)
      } else if (s == 1) {
        res[i] <- (s * century[i] * 100) - 100
      }
      if (!is.na(century_section[i])) {
        res[i] <- res[i] + century_section_switch(century_section[i], type)
      }
    } 
  }
  res <- as.integer(res)
  return(res)
}

century_section_switch <- function (x, type) {
  sapply(
    century_section_switch_list(x),
    function(y) {
      if (type == "start") {
        y[1]
      } else if (type == "end") {
        y[2]
      }
    }
  )
}

century_section_switch_list <- function(x) {
  lapply(
    x,
    function(y) {
      switch(
        y,
        "1. Hälfte" = c(0, 50),
        "2. Hälfte" = c(50, 100),
        "1. Viertel" = c(0, 25),
        "2. Viertel" = c(25, 50),
        "3. Viertel" = c(50, 75),
        "4. Viertel" = c(75, 100),
        "1. Drittel" = c(0, 33),
        "2. Drittel" = c(33, 66),
        "3. Drittel" = c(66, 100),
        "Anfang/frühes" = c(0, 20),
        "Mitte" = c(40, 60),
        "Ende/spätes" = c(80, 100),
        c(0, 100)  
      )
    }
  )
}

sign_switch <- function(x, default = NA) {
  sapply(
    x, 
    function(y) {
      switch (
        y, 
        "v. Chr." = -1,
        "n. Chr." = +1,
        default
      )
    }, 
    USE.NAMES = F
  )
}
