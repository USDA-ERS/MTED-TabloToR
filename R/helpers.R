loge = log

`%loosein%` = function(a, b) {
  return(as.logical(apply(do.call(`cbind`,as.list(Map(function(f)grepl(f,a),b))),MARGIN=1,max)))
  #return(unlist(Map(function(f)as.logical(max(grepl(f,a))),b)))
}


