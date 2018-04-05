get_fir_n_las_date_df=function(df,d_col){
  init_da_file=tail(df,n=1)[,d_col]-1
  las_da_file=df[1,d_col]
  init_las_d=c(init_da_file,las_da_file)
  return(init_las_d)
}