library(ggplot2)


# devtools::install_github("lchiffon/wordcloud2")

library(wordcloud2)

palabras_semana |> 
  filter(semana == max(semana)) |> 
  group_by(palabra) |> 
  summarize(n = sum(n)) |> 
  select(palabra, n) |> 
  slice_max(n, n = 200) |> 
  arrange(desc(n)) |> 
  rename(word = palabra, freq = n) |> 
  print() |> 
  wordcloud2(color = c(color_texto),
             backgroundColor = color_fondo,
             fontWeight = "normal",
             fontFamily = "Lato",
             rotateRatio = 0.1,
             gridSize = 10,
             size = 0.4,
             minSize = 12
             )

lista_semanas <- palabras_semana |> 
  distinct(fecha, semana) |> 
  arrange(desc(fecha)) |> 
  mutate(fecha_t = redactar_fecha(fecha),
         fecha_t = paste("Semana del", fecha_t)) |> 
  select(fecha_t, fecha) |> 
  tibble::deframe()
