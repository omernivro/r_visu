get_start_day_not_sund=function(date,weeks_pas){
  d=as.Date(date)
  if(as.numeric(strftime((d-weeks_pas*7)+1,format = "%u"))==7){
    start_date=(d-weeks_pas*7)+2
  }else{
    start_date=(d-weeks_pas*7)+1
  }
  return(start_date)
}