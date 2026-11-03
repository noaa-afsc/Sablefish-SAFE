library(here)
library(SPoRC)

faa <- readRDS(here("Mods", "___2026_Final_Mods_Nov", "26.2_FAA", "26.2b_v1.2_model_results_matt.RDS"))
sgl <- readRDS(here("Mods", "___2026_Final_Mods_Nov", "26.1_1Reg", "26.1_1reg_v1.2_model_results_matt.RDS"))

# look at average recruitment
mean(faa$rep$Rec[,,which(1960:2026 >= 1979 & 1960:2026 <= 2024)])
mean(sgl$rep$Rec[,,which(1960:2026 >= 1979 & 1960:2026 <= 2024)])

# look at average recruitment after 2013
mean(faa$rep$Rec[,,which(1960:2026 >= 2013 & 1960:2026 <= 2024)])
mean(sgl$rep$Rec[,,which(1960:2026 >= 2013 & 1960:2026 <= 2024)])

# look at average recruitment after 2020
mean(faa$rep$Rec[,,which(1960:2026 >= 2020 & 1960:2026 <= 2024)])
mean(sgl$rep$Rec[,,which(1960:2026 >= 2020 & 1960:2026 <= 2024)])

par(mfrow = c(1,2))

# look at total biomass
plot(1960:2026, as.vector(faa$rep$Total_Biom), type = 'l', ylab = 'Total Biomass (kt)', ylim = c(0,800))
lines(1960:2026, as.vector(sgl$rep$Total_Biom), type = 'l', col = 'blue')
legend(
  x = 1970, y = 800,
  legend = c('26.2_FAA', "26.1_1Reg"), 
  fill = c("black", "blue")
)

# look at total biomass standardized
plot(1960:2026, (as.vector(faa$rep$Total_Biom) - mean(as.vector(faa$rep$Total_Biom))) / sd(as.vector(faa$rep$Total_Biom)), type = 'l', xlab = 'Year', ylab = 'Total Biomass (standardized)')
lines(1960:2026, (as.vector(sgl$rep$Total_Biom) - mean(as.vector(sgl$rep$Total_Biom))) / sd(as.vector(sgl$rep$Total_Biom)), type = 'l', col = 'blue')
legend(
  x = 1970, y = 2,
  legend = c('26.2_FAA', "26.1_1Reg"), 
  fill = c("black", "blue")
)

dev.off()

# look at recruitment
plot(1960:2026, as.vector(faa$rep$Rec), type = 'l', xlab = 'Year', ylab = 'Age-2 Recruitment', ylim = c(0,130))
lines(1960:2026, as.vector(sgl$rep$Rec), type = 'l', col = 'blue')
legend(
  x = 1965, y = 120,
  legend = c('26.2_FAA', "26.1_1Reg"), 
  fill = c("black", "blue")
)

as.vector(faa$rep$Rec)[which(1960:2026 == 2019)]
as.vector(sgl$rep$Rec)[which(1960:2026 == 2018)]

par(mfrow = c(1,2))
plot(
  diag(faa$rep$NAA[1,1,which(1960:2026 %in% 2019:2026),1,,1] * faa$data$WAA[1,1,67,1,1:8, 1]) +
    diag(faa$rep$NAA[1,1,which(1960:2026 %in% 2019:2026),1,,2] * faa$data$WAA[1,1,67,1,1:8, 2]),
  type = 'l', ylim = c(0, 180), xlab = "2017 Cohort Index", ylab = 'FAA Region Model 2017 Cohort Biomass'
)

plot(
  diag(sgl$rep$NAA[1,1,which(1960:2026 %in% 2018:2026),1,,1] * faa$data$WAA[1,1,67,1,1:9, 1]) +
    diag(sgl$rep$NAA[1,1,which(1960:2026 %in% 2018:2026),1,,2] * faa$data$WAA[1,1,67,1,1:9, 2]),
  type = 'l', ylim = c(0, 180), xlab = "2016 Cohort Index", ylab = 'Single Region Model 2016 Cohort Biomass'
)
