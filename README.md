# GeoTrace

## install package
```{r}
library(devtools)
install_github("gsnrguo/GeoTrace")
```
## example
```{r}
cases <- house_death[house_death$death_dum==1,]
BS_cholera <- geotrace(cases, pumps, house_death)
BS_cholera
plot(BS_cholera)
```
