variables <- read_delim("../../data/variables.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                        trim_ws = TRUE)

kbl(variables,
    booktabs = T) %>%
  row_spec(0, bold=T) %>%
  kable_classic() %>%
  column_spec(1, width = "1cm") %>%
  column_spec(2, width = "5cm") %>%
  column_spec(3, width = "20cm") %>%
  column_spec(4, width = "40cm") %>%
  column_spec(5, width = "5cm") %>%
  footnote(general = "Variables PRSL and PRRK are originally on a scale from 0 to 7, but was transposed to lie between 1 and 8 to conform to the other variables.") %>%
  save_kable("images/tbl_variables.jpg",
             zoom=3)
