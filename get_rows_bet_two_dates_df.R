# fits df where later dates come on top of earlier dates
get_rows_bet_two_dates_df=function(df,df_dat_col,ear_dat,lat_dat,na){
  ear_dat=as.Date(ear_dat)
  lat_dat=as.Date(lat_dat)
  rownames(df) <- 1:nrow(df)
  
    while(as.numeric(strftime(as.Date(lat_dat),format = "%u"))==7 | !(any(df[,df_dat_col]==as.Date(lat_dat))))
    {
      lat_dat=lat_dat-1
    }
  fir_r=as.numeric(rownames(df[df[,df_dat_col]==as.Date(lat_dat),][1,]))
  rownames(df) <- 1:nrow(df)
    while(!(any(df[,df_dat_col]==as.Date(ear_dat))))
    {
      ear_dat=ear_dat+1
    }
  las_r=as.numeric(rownames(tail(df[df[,df_dat_col]==ear_dat,],n=1)))
  rownames(df) <- 1:nrow(df)
  df=df[fir_r:las_r,]
  rownames(df) <- 1:nrow(df)
    if(na==T){
    df=df[complete.cases(df),]
    }
  return(df)
}