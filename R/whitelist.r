# Code adapted from a blogpost by Hadley Wickham

whitelist.examples = function() {
  cat.funs(list.base.computation.funs())
  cat.pkg.funs("stats")

  cat.pkg.funs("base")
  cat.pkg.funs("dplyr")

  wl.funs = parse.whitelist.yaml("D:/libraries/WhitelistEval/WhitelistEval/lists/StratTournWhiteList.yaml")

  txt = paste0("c(",paste0('"',wl.funs,'"',collapse=","),")")
  writeClipboard(txt)

  call = quote({
    eval(1+1)
    base::print
    y <<- 5
    print("Hi")
    filter(group_by(x,y), a==5)
    ggplot()
  })
  check.whitelist(call, wl.funs = wl.funs)

}

examples.cat.pkg.funs = function() {
  cat.pkg.funs("stats")
  cat.pkg.funs("base")
  cat.pkg.funs("dplyr")
}

#' Helper function to create whitelists
#'
#' Cats all functions in a package
#' as a yaml vector
#' @param pkg the name of the package
cat.pkg.funs = function(pkg="base") {
  funs = ls(paste0("package:",pkg))
  cat.funs(funs)
}

cat.funs = function(funs) {
  txt = paste0("- '",funs,collapse="'\n")
  writeClipboard(txt)
  cat(txt)
}



examples.check.whitelist = function() {


  dir = path.package("WhitelistEval")
  wl.funs = parse.whitelist.yaml(file.path(dir,"lists/StratTournWhiteList.yaml"))
  call = quote({
    eval(1+1)
    base::print
    y <<- 5
    print("Hi")
    filter(group_by(x,y), a==5)
    ggplot()
  })
  check.whitelist(call, wl.funs = wl.funs)

}


#' Check whether a call only calls functions or uses variables that satify a whitelist of allowed symbols
#'
#' The function extracts in a nested fashion all names of called functions and used variables
#' in a call object. It can look up in whitelists whether these symbols are allowed or in
#' blacklists whether these symbols are forbidden. Whitelists and blacklists are simple
#' character vectors with the function / variable names.
#'
#' Returns a list (ok, fb.funs, fb.vars, msg) where ok is FALSE if a forbidden symbol
#' has been found and otherwise TRUE. fb.funs and fb.vars list the forbidden function calls
#' or variable names that have been found. msg is a default error message that can be shown
#' if the call did not pass the check.
#'
#' @param call the call object
#' @param wl.funs a character vector of the function names that are allowed (whitelisted). If NULL ignored.
#' @param wl.vars a character vector of the variable names that are allowed (whitelisted). If NULL ignored.
#' @param wl.calls a list of explicit calls that are allowed.
#' For example, one may not generally whitelist the function 'library'
#' (who knows what can happen if a library has functions with the
#' same name than some whitelisted function but different behavior)
#' Yet one may allow the explicit call
#' `library(dplyr)`. In this case, we could set
#'  wl.calls = alist(library(dplyr)).
#'  DOES NOT YET WORK!
#' @param bl.funs a character vector of the function names that are forbidden (blacklisted). If NULL ignored.
#' @param bl.vars a character vector of the variable names that are forbidden  (blacklisted). If NULL ignored.
#' @param funs by default set to all function calls in call.
#' @param vars by default set to all variables used in call.
#' @export
check.whitelist = function(call, wl.funs=NULL,wl.vars=NULL, wl.calls=NULL, bl.funs=NULL, bl.vars=NULL, funs = find.funs(call), vars=find.variables(call)) {
  fb.funs = NULL
  fb.vars = NULL
  msg = ""
  ok = TRUE

  if (!is.null(wl.funs)) {
    fb.funs = c(fb.funs,funs[which(! funs %in% wl.funs)])
  }
  if (!is.null(bl.funs)) {
    fb.funs = c(fb.funs,funs[which(funs %in% bl.funs)])
  }
  if (!is.null(wl.vars)) {
    fb.vars = c(fb.vars,vars[which(! funs %in% wl.vars)])
  }
  if (!is.null(bl.vars)) {
    fb.vars = c(fb.vars,vars[which(funs %in% bl.vars)])
  }

  if (length(fb.funs)>0) {
    ok = FALSE
    msg = paste0(msg,"For security reasons, it is not allowed to call the following functions:\n",paste0(fb.funs,collapse=", "),"\n")
  }
  if (length(fb.vars)>0) {
    ok = FALSE
    msg = paste0(msg,"For security reasons, it is forbidden to access the following variable(s):\n",paste0(fb.vars,collapse=", "),"\n")
  }
  return(list(ok=ok, fb.funs = fb.funs, fb.vars=fb.vars, msg=msg))
}

#' Only evaluate expr if it passes check.whitelist
#'
#' if the expression is not cleared by check.whitelist throw an error
#' @param expr an R call object that shall be evaluated
#' @param envir the environment in which expr shall be evaluated
#' @param enclos see help for `eval`
#' @param wl.funs a character vector of the function names that are allowed (whitelisted). If NULL ignored.
#' @param wl.vars a character vector of the variable names that are allowed (whitelisted). If NULL ignored.
#' @param wl.calls a list of explicit calls that are allowed.
#' For example, one may not generally whitelist the function 'library'
#' (who knows what can happen if a library has functions with the
#' same name than some whitelisted function but different behavior)
#' Yet one may allow the explicit call
#' `library(dplyr)`. In this case, we could set
#'  wl.calls = alist(library(dplyr)).
#'  DOES NOT YET WORK!
#' @param bl.funs a character vector of the function names that are forbidden (blacklisted). If NULL ignored.
#' @param bl.vars a character vector of the variable names that are forbidden  (blacklisted). If NULL ignored.
#' @return the evaluated expr if it passes check.whitelist, otherwise an error.
whitelist.eval = function(expr,
  envir = parent.frame(),
  enclos = if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(),
  wl.funs=NULL,wl.vars=NULL, wl.calls=NULL, bl.funs=NULL, bl.vars=NULL) {

  res = check.whitelist(expr, wl.funs=wl.funs,wl.vars=wl.vars, wl.calls=wl.calls, bl.funs=bl.funs, bl.vars=bl.vars)

  if (!res$ok) {
    stop(res$msg)
  }
  eval(expr,envir,enclos)
}


#' A synoym for check.whitelist with different order of the arguments
check.blacklist = function(call, bl.funs=NULL,bl.vars=NULL, wl.funs=NULL, wl.vars=NULL, funs = find.funs(call), vars=find.variables(call)) {
  check.whitelist(call=call, bl.funs=bl.funs, bl.vars=bl.vars, wl.funs=wl.funs, wl.vars0=wl.vars, funs=funs, vars=vars)
}

#' Parse a whitlist yaml file and return it as a character vector
#'
#' For a structure of the yaml file,
#' see the example whitelists in the lists folder
#' @param yaml.file the file name of the whitelist
#' @param yaml.text yaml text as a single character variable
#' @param yaml.list an already imported yaml file returned from yaml.load
#' @return a character vector with function names or variable names that are forbidden
parse.whitelist.yaml = function(yaml.file=NULL,yaml.text=NULL, yaml.list=NULL) {
  if (is.null(yaml.list)) {
    if (is.null(yaml.text)) {
      yaml.list = yaml.load_file(yaml.file)
    } else {
      yaml.list = yaml.load(yaml.text)
    }
  }
  txt = unlist(yaml.list,use.names = FALSE)
  txt
}
