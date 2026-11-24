# Korzystając ze zbioru danych BodyTemperature.txt (do pobrania poniżej),
# utwórz wykres punktowy zależności między temperaturą ciała a tętnem. Dodaj linię regresji loess.

ggplot(BodyTemperature, aes(x = Temperature, y = HeartRate)) +
  geom_point(shape = 1) +
  geom_smooth(method = "loess")

# Korzystając ze zbioru danych birthwt z pakietu MASS, utwórz wykres punktowy
# zależności między masą urodzeniową dziecka a masą matki przed ciążą.
# Dla każdej grupy dzieci (niska i prawidłowa masa urodzeniowa) dodaj
# linie regresji lm. Co możesz powiedzieć o tych zależnościach?

birthwt$low = factor(birthwt$low, levels = c(0,1), labels = c("normal", "low"))

ggplot(birthwt, aes(x = bwt, y = lwt, color = low)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm)

#
# WYKRESY SŁUPKOWE
#

# Na podstawie danych birthwt:
# utwórz wykres słupkowy liczebności poszczególnych kategorii dla zmiennej race.
# Każda kategoria niech będzie wypełniona innym kolorem, a zarys słupków niech będzie niebieski;

birthwt$race = factor(birthwt$race, labels = c("white", "black", "other"))
ggplot(birthwt, aes(x = race, fill = race)) +
  geom_bar(color = "blue")

# utwórz wykres słupkowy liczebności poszczególnych kategorii dla zmiennej race w zależności od
# informacji na temat palenia;

birthwt$smoke = factor(birthwt$smoke, labels = c("non-smoker", "smoker"))
ggplot(birthwt, aes(x = race, fill = smoke)) +
  geom_bar()

# utwórz wykres słupkowy mówiący o procentowym udziale poszczególnych kategorii dla
# zmiennej race w zależności od informacji na temat palenia;

ggplot(birthwt, aes(x = race, fill = smoke)) +
  geom_bar(position = "fill")

# utwórz wykres słupkowy, który pozwoli odpowiedzieć na pytanie: dla której rasy jest
# najwięcej kobiet niepalących w trakcie ciąży?

ggplot(birthwt, aes(x = race, fill = smoke)) +
  geom_bar(position = "dodge")

# zagreguj dane tak, aby móc wykonać wykres liniowy, który będzie podawał liczbę matek
# niepalących i palących w zależności od rasy. Dodaj punkty o różnych kształtach w zależności
# od informacji o paleniu.

birthwt.agr <- birthwt %>%
  group_by(smoke, race) %>%
  summarize(l.dzieci = n(), .groups = "drop")

ggplot(data = birthwt.agr, aes(x = race, y = l.dzieci, group = smoke, shape = smoke)) +
  geom_point(size = 3) +
  geom_line()

# Na podstawie danych BodyTemperature.txt:
# wyznacz przedziały wiekowe 20−30, 30−40 i 40−50. Dla każdego pacjenta podaj informację,
# do jakiej grupy wiekowej należy;

BodyTemp <- BodyTemperature |> 
  mutate(
    Age_Group = cut(
      Age, 
      breaks = c(20, 30, 40, 50), 
      right = FALSE, 
      labels = c("20-30", "30-40", "40-50")
    )
  )

# wyznacz średni puls w każdej grupie wiekowej oraz dla obu płci;

summary_BT <- BodyTemp %>%
  filter(!is.na(Age_Group)) %>%
  group_by(Gender, Age_Group) %>%
  summarise(
    Mean_HeartRate = mean(HeartRate, na.rm = TRUE),
    .groups = "drop"
  )

summary_BT

# utwórz wykres liniowy zależności między płcią a średnim pulsem z uwzględnieniem grup wiekowych;

# dodaj dla każdej grupy wiekowej inny kolor;
# niech linie będą przerywane, a ich grubość niech wynosi 0.7;
# dołącz punkty danych o wielkości 2 i różnych kształtach zależnych od grupy.

ggplot(summary_BT, aes(x = factor(Gender), y = Mean_HeartRate, color = Age_Group, group = Age_Group)) +
  geom_line(linetype = "dashed", linewidth = 0.7) +
  geom_point(aes(shape = Age_Group), size = 2)

#
# WYKRESY ROZKŁADÓW
#

# Na podstawie danych birthwt utwórz histogram częstości zmiennej lwt. Co możesz powiedzieć o tym wykresie?

ggplot(birthwt, aes(x = lwt)) +
  geom_histogram()

# Na podstawie danych z pliku Protein.txt (do pobrania poniżej) wykonaj
# histogram gęstości razem z wykresem gęstości zmiennej WhiteMeat
# (wskazówka: wysokość słupków wyrażamy jako gęstość zapisem 
# aes(y = after_stat(density)) — tak jak na zajęciach 4). Co możesz powiedzieć o tym wykresie?

ggplot(Protein, aes(x = WhiteMeat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 6, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1)

# Na podstawie danych BodyTemperature.txt utwórz wykres pudełkowy temperatury ciała 
# oddzielnie dla mężczyzn i kobiet.
# Która grupa ma wyższe wartości? Która grupa ma większy rozrzut?

ggplot(BodyTemperature, aes(x = Gender, y = Temperature, fill = Gender)) +
  geom_boxplot()

# Na podstawie danych penguins utwórz wykres gęstości długości płetwy (flipper_length_mm)
# oddzielnie dla każdego gatunku (species), mapując gatunek na wypełnienie.
# Ustaw przezroczystość tak, aby wszystkie krzywe były widoczne. Czy rozkłady gatunków się pokrywają?

ggplot(penguins, aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.5)

# Porównaj różne sposoby przedstawienia rozkładu długości dzioba (bill_length_mm)
# w zależności od gatunku pingwina:
  
# wykonaj histogram z podziałem na panele według gatunku (facet_wrap()), z panelami ułożonymi jeden pod drugim;

ggplot(penguins, aes(x = bill_length_mm, fill = species)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  facet_wrap(~ species, ncol = 1)

# wykonaj wykres pudełkowy;

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot()

# wykonaj wykres skrzypcowy;

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_violin(alpha = 0.5)

# na wykres skrzypcowy nałóż wąski wykres pudełkowy (width = 0.1) oraz odwróć osie za pomocą coord_flip().

ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.8, outlier.shape = NA) +
  coord_flip()
# Który z wykresów najlepiej pokazuje różnice między gatunkami, a który najwięcej mówi o kształcie rozkładu?

# Na podstawie danych gapminder z pakietu gapminder wybierz obserwacje z roku 2007
# i utwórz wykres pudełkowy oczekiwanej długości życia (lifeExp) dla poszczególnych kontynentów
# (continent). Pudełka ułóż poziomo i uporządkuj kontynenty według mediany długości życia
# (wskazówka: x = reorder(continent, lifeExp, median)). 
# Na którym kontynencie zróżnicowanie długości życia jest największe?

gap_2007 <- gapminder %>% filter(year == 2007)

ggplot(gap_2007, aes(x = reorder(continent, lifeExp, median), y = lifeExp, fill = continent)) +
  geom_boxplot() +
  coord_flip()

# Na podstawie danych mtcars utwórz wykres punktowy zależności zużycia paliwa (mpg)
# od masy samochodu (wt). Kolor punktów uzależnij od liczby cylindrów (cyl, pamiętaj o zamianie na czynnik),
# dodaj linie regresji lm dla każdej grupy cylindrów, a całość podziel na panele według rodzaju
# skrzyni biegów (am). Czy zależność wygląda podobnie w obu panelach?

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatyczna", "Manualna"))

ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ am)

# Na podstawie danych gapminder:
# wybierz dane dla Polski, Niemiec, Czech i Węgier i utwórz wykres liniowy z punktami
# pokazujący zmiany oczekiwanej długości życia w latach 1952–2007 (każdy kraj innym kolorem);

kraje <- c("Poland", "Germany", "Czech Republic", "Hungary")
gap_kraje <- gapminder %>% filter(country %in% kraje)

ggplot(gap_kraje, aes(x = year, y = lifeExp, color = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2)

# dla wszystkich krajów wylicz średnią oczekiwaną długość życia w kolejnych latach dla każdego
# kontynentu (group_by() i summarise()), a następnie przedstaw wynik na wykresie liniowym z punktami;

gap_kontynenty <- gapminder %>%
  group_by(continent, year) %>%
  summarise(mean_lifeExp = mean(lifeExp), .groups = "drop")

ggplot(gap_kontynenty, aes(x = year, y = mean_lifeExp, color = continent)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2)

# utwórz wykres słupkowy liczby krajów na każdym kontynencie w roku 2007.

gap_2007_count <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(liczba_krajow = n_distinct(country))

ggplot(gap_2007_count, aes(x = reorder(continent, -liczba_krajow), y = liczba_krajow, fill = continent)) +
  geom_col()

