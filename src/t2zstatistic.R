t2zstatistic <- function(tstats, DF, zcrit = 4){
  Zx = qnorm(pt(tstats, DF));
  infIndices = is.infinite(Zx);
  Zx[infIndices] = -1*qnorm(pt(tstats[infIndices], DF[infIndices], lower.tail = FALSE));
  
  inflationFactor = tstats / Zx;
  sdInflationMax = qt(pnorm(zcrit), DF)/zcrit;
  sdInflationMin = sqrt((DF) / (DF - 2));
  inflationFactor = pmax(inflationFactor, sdInflationMin);
  inflationFactor = pmin(inflationFactor, sdInflationMax);
  return(list("Zeq" = Zx, "sd.inflationfactor" = inflationFactor))
}


