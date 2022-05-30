# get adjustment order dependent on key
adj_order <- function(key, adj, nap){

  nap <- as.numeric(nap)
  # based on code in Distance::ds

  # this is according to p. 47 of IDS
  if(key=="UN" & adj=="CO"){
    # for Fourier...
    order <- seq(1, by=1, length.out=nap)
  }else if(adj=="PO"){
    # polynomials: even from 4
    order <- seq(4, length.out=nap, by=2)
  }else if(adj=="HE"){
    # hermite: even from 4
    order <- seq(2, by=1, length.out=nap)
    order <- 2*order
  }else if(adj=="CO"){
    # cosine: by 1 from 2
    order <- seq(2, by=1, length.out=nap)
  }else{
    return(NULL)
  }

  return(order)
}
