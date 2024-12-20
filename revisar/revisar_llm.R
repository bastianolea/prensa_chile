library(ggplot2)

# por palabras y tiempo de procesamiento
resumenes |> 
  list_rbind() |> 
  mutate(orden = row_number()) |> 
  ggplot() +
  aes(tiempo, n_palabras) +
  geom_point()

# por palabras y orden de ejecución
resumenes |> 
  list_rbind() |> 
  mutate(orden = row_number()) |> 
  ggplot() +
  aes(orden, n_palabras) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# por tiempo de ejecución y orden de ejecución
resumenes |> 
  list_rbind() |> 
  mutate(orden = row_number()) |> 
  ggplot() +
  aes(orden, tiempo) +
  geom_point() +
  geom_smooth(method = "lm", se = F)
