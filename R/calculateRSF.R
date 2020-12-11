calculateRSF <- function(caribouModelsRSF, burnedLowlands10y, burnedUplandsNonTreed10y, burnedUplandConifer10y, 
                         burnedUplandBroadleaf10y, burnedLowlands20y, burnedUplandsNonTreed20y, 
                         burnedUplandConifer20y, burnedUplandBroadleaf20y, burnedLowlands30y, 
                         burnedUplandsNonTreed30y, burnedUplandConifer30y, burnedUplandBroadleaf30y, 
                         burnedLowlands40y, burnedUplandsNonTreed40y, burnedUplandConifer40y, 
                         burnedUplandBroadleaf40y, burnedLowlands60y, burnedUplandsNonTreed60y, 
                         burnedUplandConifer60y, burnedUplandBroadleaf60y, bryoids, 
                         t_shrub, s_shrub, t_wet, s_wet, h_wet, herb, water, 
                         con_open, con_sp, broad_dens, broad_open, mix_dens, 
                         mix_open, nonveg, p_broad, p_consparse, lden1000_2015, 
                         exp_maj_rds, exp_poly, exp_sett){
  
  if (is.na(lden1000_2015)) return(list(meanResponse = NA, sdResponse = NA))
    resp <- eval(parse(text = caribouModelsRSF))
    return(list(meanResponse = mean(resp), sdResponse = sd(resp)))
}
