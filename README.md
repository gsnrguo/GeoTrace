# GeoTrace: Analysis potential origins of infectious diseases based on spatial distance

## Install package
```{r}
# install.packages("devtools")
library(devtools)
install_github("gsnrguo/GeoTrace")
```
## Example

### 1854 Broad Street cholera outbreak
```{r}
library(GeoTrace)
pumps$name
sim_names <-c("Broad St","Great Mal.", "Ramilies Place","Rupert St","Brewer St","Warwick St")
cases <- house_death[house_death$death_dum==1,]
BS_cholera <- geotrace(cases, pumps, house_death, orig_names = sim_names)
BS_cholera
plot(BS_cholera)
```
