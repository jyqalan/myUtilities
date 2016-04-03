is.Finite <-
function(x){((is.numeric(x) || is.complex(x)) & !is.na(x)) & x != Inf}
