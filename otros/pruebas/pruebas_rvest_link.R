url <- "https://www.elmostrador.cl/dia/"


sesion <- url |> 
  session()

sesion_2 <- sesion |> 
  session_follow_link(css = "#mas-noticias > footer > a")

sesion_2 |> 
  html_elements("h4") |>
  html_elements("a") |>
  html_attr("href")
  
sesion_3 <- sesion_2 |> 
    session_follow_link(xpath = '//*[@id="claves"]/div/div[12]/a[3]')

sesion_3 |> 
  html_elements("h4") |>
  html_elements("a") |>
  html_attr("href")

sesion_4 <- sesion_3 |> 
  session_follow_link(xpath = '//*[@id="claves"]/div/div[12]/a[3]')

sesion_4 |> 
  html_elements("h4") |>
  html_elements("a") |>
  html_attr("href")

sesion_5 <- sesion_4 |> 
  session_follow_link(xpath = '//*[@id="claves"]/div/div[12]/a[3]')

sesion_5 |> 
  html_elements("h4") |>
  html_elements("a") |>
  html_attr("href")


session_6 <- sesion_5 |> 
  session_jump_to("https://www.elmostrador.cl/categoria/dia/page/40/")


session_6 |> 
  html_elements("h4") |>
  html_elements("a") |>
  html_attr("href")
