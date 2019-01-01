censo_load <- drake_plan(
  censo =
    fread(
      file_in("DATADIR__/inei/censo/2017/censo_full.csv")),
censo_desag = (function(ll, ul){
  x <- censo[ll<=edad&edad<=ul]
  bind_rows(
    # GLOBAL
    x[,.(desag = "NACIONAL", pob = sum(poblacion))],
    # GLOBAL-SEXUAL
    x[,.(pob = sum(poblacion)),.(desag = sexo)],
    # REGIONAL
    x[region!="LIMA", .(pob = sum(poblacion)), .(desag = region)],
    x[region=="LIMA"&provincia=="LIMA",
      .(desag = "LIMA METROPOLITANA", pob = sum(poblacion))],
    x[region=="LIMA"&provincia!="LIMA",
      .(desag = "LIMA REGION", pob = sum(poblacion))],
    # REGIONAL-SEXUAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, sexo, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA METROPOLITANA", sexo, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", sexo, sep = "_"))],
    # DISTRITAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, provincia, distrito, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA METROPOLITANA", provincia, distrito, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", provincia, distrito, sep = "_"))],
    # DISTRITAL-SEXUAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, provincia, distrito, sexo, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA METROPOLITANA", provincia, distrito, sexo, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", provincia, distrito, sexo, sep = "_"))]
  )
})(11, 17)
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )