#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 2016
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# author__ = "IBM SPSS, JKP"
# version__ = "1.0.0"

# History
# 31-jul-2016 Original Version


gtxt <- function(...) {
    return(gettext(...,domain="STATS_SPLINES"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_SPLINES"))
}

transformationmap = list("polyspline"="bs", "naturalspline"="ns",
                         "orthopoly"="poly")
### MAIN ROUTINE ###

dosplines = function(variables, id=NULL, dataset, transformation="polyspline",
    degree=3, df=NULL, bounds=NULL, outnamestype="varname", prefix=NULL,
    displayknots=FALSE) {
  # Calculate splines or orthogonal polynomials

  setuplocalization("STATS_SPLINES")
  
  # A warnings proc name is associated with the regular output
  # (and the same omsid), because warnings/errors may appear in
  # a separate procedure block following the regular output
  procname=gtxt("Spline Transformation")
  warningsprocname = gtxt("Spline Transformation: Warnings")
  omsid="STATSSPLINES"
  warns = Warn(procname=warningsprocname,omsid=omsid)

  tryCatch(library(splines), error=function(e){
      warns$warn(gtxtf("The R %s package is required but could not be loaded.", "splines"),dostop=TRUE)
      }
  )
  if (!is.null(spssdictionary.GetWeightVariable())) {
      warns$warn(
          gtxt("The dataset is weighted, but case weights are not used in this procedure except for screening out cases with a nonpositive weight"),
          dostop=FALSE)
  }
  if (!is.null(spssdata.GetSplitVariableNames())) {
      warns$warn(
          gtxt("Split variables are not honored by this procedure"),
          dostop=FALSE)
  }
  if (transformation != "orthopoly" && is.null(df)) {
    warns$warn(gtxt("The degrees of freedom parameter is required"), dostop=TRUE)
  }
  checkdataset(dataset, warns)
  alldata = c(variables, id)
  allargs = as.list(environment())
  dta = spssdata.GetDataFromSPSS(alldata, missingValueToNA=TRUE)
  if (is.null(id)) {
     dta['ID']=c(1:nrow(dta))
     id = "ID"
  }
  if (any(as.logical(lapply(dta,is.factor)))) {
      warns$warn(gtxt("String variables cannot be used in this procedure"),
          dostop=TRUE)
  }


  tfunc = transformationmap[[transformation]]
  results = data.frame(dta[[id]])
  names(results) = id
  
  if (transformation == "polyspline") {
    arglist = list(df=df, degree=degree)
    if (!is.null(bounds)) {
      if (length(bounds) != 2) {
        warns$warn(gtxt("Boundary must specify two values"), dostop=TRUE)
      }
      arglist["Boundary.knots"] = bounds
    }
  } else if (transformation == "naturalspline") {
    arglist = list(df=df)
    if (!is.null(bounds)) {
      arglist["Boundary.knots"] = bounds
    }
  } else {
    # orthogonal polynomials
    arglist = list(degree=degree)
  }
  # loop over variables calling appropriate transformation
  knotlist = list()
  for (i in 1:(ncol(dta)-1)) {
    arglist[['x']] = dta[[i]]
    result = tryCatch(do.call(tfunc, arglist),
      error=function(e) {warns$warn(e$message, dostop=TRUE)}
    )
    knotlist[[variables[[i]]]] = attr(result, "knots")
    result = data.frame(result)
    names(result) = makenames(results, result, 
      outnamestype, variables[[i]], prefix)
    results = data.frame(results, result)
  }
  displayresults(allargs, results, knotlist, warns)
  makedataset(dataset, results, warns)
  warns$display()
}

checkdataset = function(dataset, warns) {
  # Ensure that the active dataset is named
  # dataset is the name for a new dataset - must not already exist

  alldatasets = spssdata.GetDataSetList()
  if ("*" %in% alldatasets) {
    warns$warn(gtxt("The active dataset must have a name in order to use this procedure"),
               dostop=TRUE)
  }
  if (dataset %in% alldatasets) {
    warns$warn(gtxt("The output dataset name must not already be in use"),
               dostop=TRUE)
  }
  
}

makenames = function(results, result, 
          outnamestype, variable, prefix) {
  # make nonconflicting names according to
  # user specification.
  # results is the result data frame so far.
  # result is the data frame to append.
  # outnamestype is "fixed" or "varnames".
  # prefix is the prefix for fixed names.
  
  thenames = tolower(names(results))
  gennames = list()
  
  for (n in names(result)) {
    if (outnamestype == "varname") {
      candidate = safetrunc(paste(variable, n, sep="_"), 64)
    } else {
      candidate = safetrunc(paste(prefix, n, sep="_"), 64)
    }
    
    trial = 1
    while (tolower(candidate) %in% thenames){
      candidate = safetrunc(candidate, 59)
      candidate = paste(candidate, trial, sep="_")
      trial = trial + 1
    }
    gennames[[length(gennames) + 1]] = candidate
    thenames[[length(thenames) + 1]] = tolower(candidate)
  }
  return(gennames)
}
  
# helper function
f = function(x) {nchar(x, type="bytes")}
safetrunc = function(s, maxlen) {
  # return s truncated safely to no more than maxlen bytes
  # s is a string such as a variable name

  
  if (nchar(s, type="bytes") <= maxlen) {
    return(s)
  }
  lengths = cumsum(sapply(strsplit(s, split=""), f))
  for (index in 1:length(lengths)) {
    if (lengths[[index]] > maxlen) {
      return(substr(s, 1, index-1))
    }
  }
}

ff = function(x) {
  
}
makedataset = function(dsname, data, warns) {
  # create dataset for calculated data
  # dsname is the name of the dataset to create
  # data is the output data with the id variable first
  
  dict = list()
  nn = names(data)
  if (is.factor(data[[1]])) {
    len = max(nchar(levels(data[[1]]), type="bytes"))
    dict[[1]] = c(nn[[1]], "", len, paste("A", len, sep=""), nominal)
  } else {
    dict[[1]] = c(nn[[1]], "", 0, "F8.0", "nominal")
  }
  for (v in 2:length(nn)) {
    dict[[v]] = c(nn[[v]], "", 0, "F8.2", "scale")
  }
  
  dict = spssdictionary.CreateSPSSDictionary(dict)
  spssdictionary.SetDictionaryToSPSS(dsname, dict)
  # tryCatch(spssdata.SetDataToSPSS(dsname, data.frame(data[ncol(data)], data[-ncol(data)])),
  #          error=function(e) {warns$warn(e$message, dostop=TRUE)}
  # )
  tryCatch(spssdata.SetDataToSPSS(dsname, data.frame(data)),
           error=function(e) {warns$warn(e$message, dostop=TRUE)}
  )
  spssdictionary.EndDataStep()
  
}

displayresults = function(allargs, results, knotlist, warns) {
    # display results
    # allargs is the parameter set
    

    StartProcedure(allargs[["procname"]], allargs[["omsid"]])
    
    # summary results
    # input specifications
    # although groups can be specified (cengroup), separate results are not
    # produced.
    lbls = c(gtxt("Variables"),
             gtxt("Transformation Type"),
             gtxt("Degree"),
             gtxt("Degrees of Freedom"),
             gtxt("Number of Nonboundary Knots"),
             gtxt("Boundary Knots"),
             gtxt("Output Dataset")
    )
    vals = c(
            paste(allargs$variables, collapse=" "),
            allargs$transformation,
            allargs$degree,
            ifelse(is.null(allargs$df), gtxt("NA"), allargs$df),
            paste(sapply(knotlist, length), collapse=", "),
            ifelse(is.null(allargs$bounds), "NA", paste(allargs$bounds, collapse=", ")),
            allargs$dataset
    )
    spsspivottable.Display(data.frame(cbind(vals), row.names=lbls), title = gtxt("Transformation Summary"),
        collabels=c(gtxt("Summary")), templateName="SPLINESSUMMARY", outline=gtxt("Transformation Summary"),
        caption = gtxtf("Computations done by R package splines, version: %s", packageVersion("splines"))
    )

    if (allargs$displayknots && length(knotlist) > 0) {
      z = list()
      for (i in 1:length(knotlist)) {
        if (length(knotlist[[i]]) > 0) {
          z[[i]] = paste(knotlist[[i]], collapse=", ")
      } else {
          z[[i]] = gtxt("None")
        }
      }
      ddf = cbind(z)
      spsspivottable.Display(data.frame(ddf),
        title=gtxt("Nonboundary Knots"), rowlabels= allargs$variables,
        collabels=gtxt("Knots"), 
        templateName="KNOTS", outline=gtxt("Nonboundary Knots"))
    }
    spsspkg.EndProcedure()
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = mylist2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                    },
                    error = function(e) {
                        FALSE
                    }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                    gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

mylist2env = function(alist) {
    env = new.env()
    lnames = names(alist)
    for (i in 1:length(alist)) {
        assign(lnames[[i]],value = alist[[i]], envir=env)
    }
    return(env)
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}



Run = function(args) {
    #Execute the STATS COMPRISK command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("VARIABLES", subc="", ktype="existingvarlist", var="variables", islist=TRUE),
        spsspkg.Template("ID", subc="", ktype="existingvarlist", var="id"),
        spsspkg.Template("DATASET", subc="", ktype="varname", var="dataset"),
        
        spsspkg.Template("TYPE", subc="TRANSFORMATION", ktype="str", var="transformation",
            vallist=list("polyspline", "naturalspline", "orthopoly")),
        spsspkg.Template("DEGREE", subc="TRANSFORMATION", ktype="int", var="degree"),
        spsspkg.Template("DF", subc="TRANSFORMATION", ktype="int", var="df"),
        spsspkg.Template("BOUNDS", subc="TRANSFORMATION", ktype="float", var="bounds", islist=TRUE),
        
        spsspkg.Template("OUTNAMES", subc="OPTIONS", ktype="str", var="outnamestype",
            vallist=list("fixed", "varname")),
        spsspkg.Template("PREFIX", subc="OPTIONS", ktype="varname", var="prefix"),
        spsspkg.Template("DISPLAYKNOTS", subc="OPTIONS", ktype="bool", var="displayknots")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dosplines")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
