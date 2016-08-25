* Encoding: UTF-8.
get file="c:/spss23/samples/english/employee data.sav".
compute sÜlary = salary.
dataset name emp.

stats splines variables = salary jobtime dataset=splines/transformation df=5 type=polyspline.

stats splines variables = salary jobtime dataset=splines/transformation df=5 type=polyspline
/options displayknots=yes.

stats splines variables = sÜlary jobtime  dataset=splines/transformation df=3.
stats splines variables = sÜlary jobtime  dataset=splines/transformation df=6 degree=3.

stats splines variables = sÜlary sÜlary   dataset=splines/transformation df=3.

stats splines variables=salary jobtime prevexp dataset=ortho/transformation degree=4 type=orthopoly.
stats splines variables=salary jobtime prevexp dataset=ortho/transformation degree=4 type=orthopoly
/options displayknots=yes.
stats splines variables=salary jobtime prevexp dataset=natural id=id
 /transformation degree=4 df = 5 type=naturalspline

stats splines variables=jobtime dataset=jobtime /transformation degree=1 df=10.

begin program r.
dta = spssdata.GetDataFromSPSS()
browser()
end program.

missing values bdate to sÜlary(99999).

* merge back results into main dataset.
STATS SPLINES VARIABLES=bdate educ jobtime prevexp salary salbegin sÜlary ID=id DATASET=x
/TRANSFORMATION TYPE=ORTHOPOLY DEGREE=4  
/OPTIONS OUTNAMES=VARNAME DISPLAYKNOTS=YES.
DATASET ACTIVATE emp.
match files /file=* /file=x by id.
