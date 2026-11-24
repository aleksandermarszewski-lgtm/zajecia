# ZADANIA GGPLOT2
# Stwórz wykres punktowy zależności między zmiennymi CL i FL ze zbioru crabs.

ggplot(data = crabs) +
  geom_point(mapping = aes(x = CL, y = FL))

# Dodaj do wykresu mapowanie kolorów na podstawie gatunku.

ggplot(data = crabs) +
  geom_point(mapping = aes(x = CL, y = FL, color = sp))

# Utwórz histogram długości pancerza (CL) dla różnych gatunków.

ggplot(crabs, aes(x = CL, fill = sp)) +
  geom_histogram(binwidth = 0.5,
                 colour = 'black')

# Stwórz wykres pudełkowy długości pancerza dla różnych gatunków.

ggplot(crabs, aes(x = sp, y = CL, fill = sp)) +
  geom_boxplot()

# Stwórz złożony wykres, który pokaże relacje między wszystkimi zmiennymi numerycznymi w zbiorze crabs.

crabs.long <- crabs |> 
  pivot_longer(
    cols = -c(sp, sex, index),
    names_to = "Cecha",
    values_to = "Rozmiar"
  )

ggplot(crabs.long, aes(x = Cecha, y = Rozmiar, fill = sp)) +
  geom_boxplot()

# Na podstawie danych birthwt z pakietu MASS utwórz wykres punktowy opisujący zależność pomiędzy wagą urodzeniową
# dziecka a wagą matki sprzed ciąży. Co możesz powiedzieć o tej zależności?

ggplot(birthwt, aes(x = lwt, y = bwt)) + 
  geom_point()

# Korzystając z danych hla.long, policz reguły statystyki opisowej dla każdego markera:
# liczbę pomiarów, średnią i odchylenie standardowe ekspresji. Następnie funkcją pivot_wider()
# przedstaw same średnie w tabeli, w której wiersze odpowiadają markerom, a kolumny grupom (0 i 1).
# W którym markerze różnica między grupami jest największa?

hla.long <- hla %>%
  pivot_longer(
    cols      = -c(id, y),
    names_to  = "marker",
    values_to = "ekspresja"
  ) %>%
  mutate(y = as.factor(y)) %>%
  rename(grupa = y)

hla_summary <- hla.long |> 
  group_by(marker, grupa) |> 
  summarise(liczba = n(),
            srednia_ekspresji = mean(ekspresja),
            odchylenie_ekspresja = sd(ekspresja),
            .groups = "drop")

tabela = hla_summary %>%
  dplyr::select(marker, grupa, srednia_ekspresji) |> 
  pivot_wider(names_from = grupa, values_from = srednia_ekspresji)

# Na podstawie danych z pliku Protein.txt (do pobrania poniżej)
# stwórz zbiór danych w formacie long i nazwij go protein.long.

Protein.long <- Protein |> 
  pivot_longer(cols = -Country,
               names_to = "produkt",
               values_to = "białko")

# ESTETYKI

# Stwórz wykres punktowy zależności między CL i FL, gdzie kolor zależy od gatunku,
# przezroczystość jest ustawiona na 0,5, a kształt zależy od płci.

ggplot(crabs, aes(x = CL, y = FL, colour = sp, alpha = 0.5, shape = sex)) + 
  geom_point()

# Na podstawie danych hla.long wykonaj wykres punktowy zależności między markerami a ekspresją.
# Dodaj różne wielkości punktów w zależności od id. Zastanów się nad czytelnością wykresu i zmień go tak,
# aby był czytelny.

ggplot(hla.long, aes(x = marker, y = ekspresja, size = id)) +
  geom_point()

ggplot(hla.long, aes(x = marker, y = ekspresja, size = 5, colour = id)) +
  geom_point()

# Dla wykresu z zadania 7 z poprzedniego rozdziału (zależność wagi urodzeniowej dziecka
# od wagi matki sprzed ciąży) uwzględnij zmienne race i ht, korzystając z wybranych przez Ciebie estetyk.

birthwt$race <- factor(birthwt$race, labels = c("white", "black", "other"))
birthwt$ht <- factor(birthwt$ht, labels = c("no", "yes"))
ggplot(birthwt, aes(x = lwt, y = bwt, colour = race, shape = ht, size = 0.0001)) + 
  geom_point()

# Pobierz poniższy plik BodyTemperature.txt i załaduj go. Wykonaj wykres punktowy dla zmiennych HeartRate
# i Temperature z uwzględnieniem płci (zmienna Gender).


ggplot(BodyTemperature, aes(HeartRate, Temperature, colour = Gender)) +
  geom_point()


# ZADANIA PANELE

# Wykonaj wykres punktowy zależności między CL i FL. Podziel go na panele według
# gatunku i dodaj kolor zależny od płci.

ggplot(crabs) +
  geom_point(aes(x = CL, y = FL, colour = sex)) +
  facet_wrap(~sp)

# Na podstawie danych birthwt stwórz wykres zależności wagi matki sprzed
# ciąży i wagi urodzeniowej dziecka. Podziel go na panele według
# rasy i nadciśnienia oraz uwzględnij informacje o paleniu.

birthwt$smoke = factor(birthwt$smoke, labels = c("no", "yes"))
ggplot(birthwt, aes(x = lwt, y = bwt, colour = smoke)) + 
  geom_point() +
  facet_grid(race ~ ht)

# Na podstawie danych hla.long wykonaj wykres punktowy, który będzie pokazywał
# zależność pomiędzy markerami a ekspresją dla każdej grupy oddzielnie. Z jakiej funkcji skorzystasz?

ggplot(hla.long) + 
  geom_point(aes(x = marker, y = ekspresja)) +
  facet_wrap(~grupa)

# Na podstawie danych BodyTemperature.txt wykonaj wykres punktowy dla
# zmiennych HeartRate i Temperature na dwóch oddzielnych wykresach zależnych od płci (zmienna Gender).

ggplot(BodyTemperature) +
  geom_point(aes(x = HeartRate, y = Temperature)) +
  facet_wrap(~Gender)

# ZADANIA PINGWINY

# Wykonaj wykres punktowy zależności długości płetwy (flipper_length_mm) od masy ciała (body_mass_g).
# Jaki widzisz związek między tymi cechami?

ggplot(penguins) +
  geom_point(aes(x = flipper_length_mm, body_mass_g))

# Narysuj ten sam wykres, zastępując geom_point() funkcją geom_line().
# Dlaczego taki wykres jest nieczytelny i do jakich danych lepiej nadaje się wykres liniowy?

ggplot(penguins) +
  geom_line(aes(x = flipper_length_mm, y = body_mass_g))

# Przy rysowaniu wykresów R wypisuje ostrzeżenie o usuniętych wierszach.
# Sprawdź, ile braków danych (NA) mają zmienne body_mass_g oraz sex, i wyjaśnij, skąd bierze się to ostrzeżenie.



# Zrób histogram masy ciała. Dobierz szerokość przedziału argumentem binwidth, a następnie
# ustaw kolor wypełnienia słupków i kolor ich obramowania.
# Porównaj wyniki dla binwidth = 50, 200 i 1000 — która wersja najlepiej pokazuje kształt rozkładu?

ggplot(penguins) +
  geom_histogram(aes(body_mass_g),
                 binwidth = 50,
                 colour = 'black',
                 fill = 'cyan')

ggplot(penguins) +
  geom_histogram(aes(body_mass_g),
                 binwidth = 200,
                 colour = 'black',
                 fill = 'cyan')

ggplot(penguins) +
  geom_histogram(aes(body_mass_g),
                 binwidth = 1000,
                 colour = 'black',
                 fill = 'cyan')

# Zrób wykres słupkowy pokazujący liczbę pingwinów każdego gatunku.
# Czym różni się ten wykres od histogramu z poprzedniego zadania?

ggplot(penguins) +
  geom_histogram(aes(species),
                 binwidth = 50,
                 colour = 'black',
                 fill = 'cyan')

# Wróć do wykresu punktowego z zadania 2 i zmapuj kolor punktów na gatunek.
# Co nowego widać po dodaniu koloru?

ggplot(penguins) +
  geom_line(aes(x = flipper_length_mm, y = body_mass_g, colour = species))

# Na tym samym wykresie zmapuj na gatunek jednocześnie kolor, rozmiar i kształt punktów.
# Oceń czytelność wyniku: czy kodowanie jednej zmiennej trzema estetykami naraz coś wnosi?

ggplot(penguins) +
  geom_line(aes(x = flipper_length_mm, y = body_mass_g, colour = species,
                size = species, shape = species))

# Narysuj wykres punktowy, w którym wszystkie punkty są niebieskie i półprzezroczyste.
# Uwaga: to zadanie wymaga ustawienia koloru poza funkcją aes() — zastanów się dlaczego.

ggplot(penguins) +
  geom_point(aes(x = species, y = body_mass_g),
             colour = 'blue', alpha = 0.5)

# Podziel wykres punktowy z zadania 7 na panele według wyspy. 
# Czy na którejkolwiek wyspie występują wszystkie trzy gatunki? Porównaj wykres z tabelą z zadania 1.

ggplot(penguins) +
  geom_line(aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  facet_wrap(~island)

# Wykonaj wykres punktowy długości dzioba (bill_length_mm) względem jego wysokości (bill_depth_mm)
# z kolorem według gatunku i panelami według płci. Co zwraca uwagę w panelu z brakującą płcią?

ggplot(penguins) +
  geom_point(aes(bill_length_mm, bill_depth_mm, colour = species)) +
  facet_wrap(~sex)

# Korzystając z poznanej funkcji pivot_longer(), przekształć cztery kolumny z pomiarami
# (bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) do formatu długiego,
# a następnie narysuj wykresy pudełkowe wartości w podziale na gatunki,
# z osobnym panelem dla każdej mierzonej cechy. Wskazówka: żeby każdy panel miał własną skalę
# osi pionowej, użyj argumentu scales = "free_y" w funkcji facet_wrap().








