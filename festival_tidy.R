#####ajustes adicionais
###tidyando os dados

festival_do_rio %>% 
    separate(dia, c("dia","data"), sep = ", ") -> festival_do_rio


#salvando csv
write.csv(festival_do_rio, "festival_do_rio.csv")

