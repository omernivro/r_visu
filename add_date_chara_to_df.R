add_date_chara_to_df=function(df,date){
  df$date=as.Date(date)
  df$year=as.numeric(strftime(date,format = "%Y"))
  df$cw=as.numeric(strftime(date,format="%V"))
  df$mon_no=as.numeric(strftime(date,format="%m"))
  df$day_name=strftime(date,format="%a")
  df$day_no=as.numeric(strftime(date,format="%d"))  
  return(df)
}