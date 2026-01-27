# =========================
# MAPA STRATEGICZNA VIKOR
# =========================

#' Mapa Strategiczna VIKOR
#'
#' @param x Obiekt klasy `rozmyty_vikor_wynik`
#' @param ... Dodatkowe argumenty
#'
#' @export
plot.rozmyty_vikor_wynik <- function(x, ...) {

  df <- x$wyniki

  df$Rozmiar <- (df$Wynik)^3

  # Środki podziału (dynamicznie liczone)
  srodek_perf <- mean(df$Wydajnosc)
  srodek_ryzyko <- mean(df$Def_R)

  ggplot2::ggplot(df, ggplot2::aes(x = Wydajnosc, y = Def_R)) +

    # Linie podziału
    ggplot2::geom_vline(
      xintercept = srodek_perf,
      linetype = "dashed",
      color = "grey50"
    ) +

    ggplot2::geom_hline(
      yintercept = srodek_ryzyko,
      linetype = "dashed",
      color = "grey50"
    ) +

    # Strefy decyzyjne
    ggplot2::annotate(
      "text",
      x = max(df$Wydajnosc),
      y = min(df$Def_R),
      label = "STABILNY LIDER\n(Wysoka Efektywność,\nNiskie Ryzyko)",
      hjust = 1,
      vjust = 0,
      size = 3,
      fontface = "bold.italic",
      color = "darkgreen"
    ) +

    ggplot2::annotate(
      "text",
      x = min(df$Wydajnosc),
      y = max(df$Def_R),
      label = "UNIKAĆ\n(Niska Efektywność,\nWysokie Ryzyko)",
      hjust = 0,
      vjust = 1,
      size = 3,
      fontface = "italic",
      color = "#B71C1C"
    ) +

    # Punkty (bąble)
    ggplot2::geom_point(
      ggplot2::aes(size = Rozmiar, fill = Wynik),
      shape = 21,
      color = "black",
      alpha = 0.85
    ) +

    ggrepel::geom_text_repel(
      ggplot2::aes(label = paste0("Alt ", Alternatywa)),
      box.padding = 0.5
    ) +

    ggplot2::labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Zielona strefa wskazuje najlepszy kompromis decyzyjny",
      x = "Indeks Wydajności (S)",
      y = "Indeks Ryzyka / Żalu (R)",
      size = "Znaczenie",
      fill = "Wynik Q"
    ) +

    .motyw_mcda()
}
