przygotuj_dane_mcda <- function(dane, skladnia, kolumna_alternatyw = NULL, funkcja_agregacji = mean) {
  if (!is.data.frame(dane)) stop("Argument 'dane' musi być ramką danych (data frame).")

  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)

  # Tymczasowy df z ocenami przed agregacją
  tymczasowe_wyniki <- data.frame(matrix(nrow = nrow(dane), ncol = 0))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]
    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) stop(paste("Brakuje zmiennych w danych:", paste(brakujace, collapse=", ")))

    # agregacja jeśli więcej niż jedna zmienna w kryterium
    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_wynik <- dane[[zmienne]]
    }

    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)
  }

  # Dodanie kolumny alternatyw
  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) stop("Nie znaleziono kolumny alternatyw w danych.")
    tymczasowe_wyniki$ID_Alternatywy <- dane[[kolumna_alternatyw]]

    # Agregacja po alternatywach
    dane_zagregowane <- aggregate(. ~ ID_Alternatywy, data = tymczasowe_wyniki, FUN = funkcja_agregacji)
    dane_zagregowane <- dane_zagregowane[order(dane_zagregowane$ID_Alternatywy), ]

    nazwy_wierszy <- dane_zagregowane$ID_Alternatywy
    macierz_wynikow <- as.matrix(dane_zagregowane[, nazwy_kryteriow])
  } else {
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow])
    nazwy_wierszy <- 1:nrow(macierz_wynikow)
  }

  # Rozmycie każdej kolumny (TFN)
  lista_decyzyjna <- lapply(seq_along(nazwy_kryteriow), function(i) {
    .rozmyj_wektor(macierz_wynikow[, i])
  })
  names(lista_decyzyjna) <- nazwy_kryteriow

  finalna_macierz <- do.call(cbind, lista_decyzyjna)
  rownames(finalna_macierz) <- nazwy_wierszy
  attr(finalna_macierz, "nazwy_kryteriow") <- nazwy_kryteriow

  return(finalna_macierz)
}
