
examples.set.call.list.names = function() {
  set.call.list.names(alist(print(5), x[5], x))

}

set.call.list.names = function(call.list) {
  restore.point("set.call.list.names")
  names = lapply(call.list, function(call) {
    if (length(call)>1) return(call[[1]])
    as.character(call)
  })
  names(call.list) = names
  call.list
}


call.list.to.call = function(li) {
  names(li) = NULL
  do.call("call", c("{",li))

}


#' Find function calls inside a call object but ignore subcalls listed in ignore.calls
#'
#' @param call the call to be analysed
#' @param ignore.calls a named list of quoted calls. The names must be the function names of the call. Alternatively, ignore.names can be set
#' @param ignore.names just the function names of the ignore.calls
find.funs.except = function(call, ignore.calls=NULL, ignore.names=names(ignore.calls)) {
  if (is.null(ignore.calls)) return(find.funs(call))


  if (!is.call(call)) return(NULL)
  fun.name = as.character(call[1])

  rows = ignore.names == fun.name
  ignore = any(sapply(ignore.calls[ignore.names == fun.name], identical,y=call))
  if (ignore) return(NULL)

  sub.names = lapply(call[-1], function(e1) {
    find.funs.except(e1, ignore.calls=ignore.calls, ignore.names=ignore.names)
  })
  names = unique(c(fun.name,unlist(sub.names, use.names=FALSE)))
  names
}
