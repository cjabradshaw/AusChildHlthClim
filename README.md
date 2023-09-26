# Child health <em>versus</em> climate in Australia
Australia SA3-level analysis of relationship between child mortality/morbidity and climate conditions
<img align="right" src="www/ausminTcMoTransp.png" alt="minimum temperature of the coldest month" width="400" style="margin-top: 20px">
<br>
<br>
Prof <a href="https://globalecologyflinders.com/people/#DIRECTOR">Corey J. A. Bradshaw</a> <br>
<a href="http://globalecologyflinders.com" target="_blank">Global Ecology</a> | <em><a href="https://globalecologyflinders.com/partuyarta-ngadluku-wardli-kuu/" target="_blank">Partuyarta Ngadluku Wardli Kuu</a></em>, <a href="http://flinders.edu.au" target="_blank">Flinders University</a>, Adelaide, Australia <br>
June 2023<br>
<a href=mailto:corey.bradshaw@flinders.edu.au>e-mail</a> <br>
<br>

## <a href="https://github.com/cjabradshaw/AusChildHlthClim/tree/main/scripts">Scripts</a>
- <code>SA3climateChildHealth.R</code>: R code to reproduce the resampled boosted regression tree analysis for determining the relationships between child-health metrics and climate.

## <a href="https://github.com/cjabradshaw/AusChildHlthClim/tree/main/data/brtdata">Data</a>
- <em>healthclim.csv</em>
- <em>fire20132022relIndex.csv</em>
- <em>NO2</em>: NO<sub>2</sub> pollution data (2010-2019)
- <em>PM25</em>: PM<sub>2.5</sub> pollution data (2012-2021)

## Required R libraries
- <code>dismo</code>
- <code>gbm</code>
- <code>mice</code>
- <code>boot</code>
- <code>truncnorm</code>

<p><a href="https://www.flinders.edu.au"><img align="bottom-left" src="www/Flinders_University_Logo_Horizontal_RGB_Master.png" alt="Flinders University" width="150" style="margin-top: 20px"></a> &nbsp; <a href="https://globalecologyflinders.com"><img align="bottom-left" src="www/GEL Logo Kaurna New Transp.png" alt="GEL" width="85" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="https://www.uwa.edu.au/"><img align="bottom-left" src="www/uwa2.png" alt="UWA" width="100" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="https://www.telethonkids.org.au"><img align="bottom-left" src="www/tkilogo.png" alt="TKI" width="90" style="margin-top: 20px"></a>
