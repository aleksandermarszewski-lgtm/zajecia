#
data("crabs")
?crabs

crabs |> 
  filter(FL > 9, sp == 'B') |> 
  dplyr::select(RW)

# Cwiczenia Crabs
# Wybierz tylko samice krabów.

crabs |> 
  filter(sex == 'F')

# Wybierz kraby gatunku „O” z FL < 15.

crabs |> 
  filter(FL < 15, sp == 'O')

# Posortuj kraby według: gatunek → płeć → CW (rosnąco).

crabs |> 
  arrange(sp, sex, CW)

# Utwórz kolumnę CW_squared (kwadrat CW).

crabs |> 
  mutate(CW_squared = CW ^ 2)

# Dla każdej płci oblicz: liczbę, średnią CW, medianę CW, odchylenie standardowe CW.

crabs |> 
  group_by(sex) |> 
  summarise(
    CW_mean = mean(CW),
    CW_meadian = median(CW),
    CW_sd = sd(CW)
  )

# Policz kraby według gatunku i płci, posortuj malejąco.

crabs |> 
  count(sp, sex, sort = TRUE)
  

# Utwórz potok, który:
  # filtruje kraby z FL > 15,
  # tworzy nową kolumnę size_category („Duża”, jeśli CW > 30, „Mała” w przeciwnym razie),
  # wybiera gatunek, płeć, CW i size_category,
  # sortuje według gatunku, a następnie CW.

crabs |> 
  filter(FL > 15) |> 
  mutate(size_category = ifelse(CW > 30, "Duży", "Mały")) |> 
  dplyr::select(sp, sex, CW, size_category) |> 
  arrange(sp, CW)

# Cwiczenia flights
# Wybierz loty z JFK obsługiwane przez United Airlines z opóźnieniem > 60 min.

data("flights")
flights |> 
  filter(dep_delay > 60 | arr_delay > 60)

#Utwórz pipeline, który:
  # filtruje tylko loty ze stycznia,
  # wybiera kolumny: carrier, flight, dep_delay, arr_delay,
  # filtruje, gdy dep_delay jest dodatnie (tylko opóźnione wyloty),
  # sortuje według dep_delay w kolejności malejącej,
  # wyświetla tylko 20 pierwszych lotów.

flights |> 
  filter(month == 1) |> 
  dplyr::select(carrier, flight, dep_delay, arr_delay) |> 
  filter(dep_delay > 0) |> 
  arrange(dep_delay) |> 
  head(20)

# Oblicz średnie opóźnienie odlotu dla każdego przewoźnika.

flights |> 
  group_by(carrier) |> 
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = T)
  )

# Cwiczenia - Grupowanie

set.seed(123)
df1 <- data.frame(
  group = sample(c("A", "B", "C"), 100, replace = TRUE),
  value1 = runif(100, min = 5, max = 20),
  value2 = runif(100, min = 1, max = 100)
)

# Użyj funkcji filter(), aby wybrać tylko te wiersze, w których value1 jest większe niż 10.
# Następnie użyj funkcji group_by() i summarise(), aby obliczyć średnią value2 dla każdej grupy group.

df1 |> 
  filter(value1 > 10) |> 
  group_by(group) |> 
  summarise(
    mean_v2 = mean(value2, na.rm = T)
  )

# 

set.seed(123)
df3 <- data.frame(
  kategoria = sample(c("Elektronika", "Gospodarstwo Domowe", "Odzież"),
                     100, replace = TRUE),
  podkategoria = sample(c("Telefony", "Sprzęt AGD", "Koszulki"),
                        100, replace = TRUE),
  cena = runif(100, min = 50, max = 5000),
  jakość = sample(1:20, 100, replace = TRUE)
)

# Użyj funkcji group_by(), aby pogrupować dane według zmiennych kategoria i podkategoria.
# Następnie użyj funkcji summarise() w połączeniu z across(),
# aby obliczyć średnią wartość zmiennych numerycznych (cena i jakość).

df3 |> 
  group_by(kategoria, podkategoria) |> 
  summarise(across(cena:jakość, mean, .names = "mean_{.col}"))


# Cwiczenia Titanic

data('TitanicSurvival')

# Średni wiek ….. wraz z klasą – pasażerowie …….. klasy byli starsi.

TitanicSurvival |> 
  group_by(passengerClass) |> 
  summarise( mean_age = mean(age, na.rm = T) ) |> 
  arrange(desc(mean_age))

# Kobiety miały znacznie …… wskaźnik przeżycia niż mężczyźni.

TitanicSurvival |> 
  count(sex, survived) |> 
  filter(survived == "yes") |> 
  mutate(surv_chance = n / sum(n)) |> 
  select(sex, surv_chance) |> 
  arrange(desc(surv_chance))

TitanicSurvival |> 
  group_by(sex) |> 
  summarise(mean(survived == "yes"))

# Pasażerowie …. klasy mieli znacznie większą szansę na przeżycie niż ci z ….. klasy.

TitanicSurvival |> 
  count(passengerClass, survived) |> 
  filter(survived == "yes") |> 
  mutate(surv_chance = n / sum(n)) |> 
  select(passengerClass, surv_chance) |> 
  arrange(desc(surv_chance))

# Najbardziej narażeni byli ….. z …… klasy w wieku dorosłym.

TitanicSurvival |>
  filter(age >= 18) |> 
  group_by(sex) |> 
  count(passengerClass, survived) |> 
  filter(survived == "yes") |> 
  mutate(surv_chance = n / sum(n)) |> 
  select(sex, passengerClass, surv_chance) |> 
  arrange(surv_chance)

# Największą przeżywalność odnotowano wśród ….. z ……. klasy w grupie dorosłych.

TitanicSurvival |>
  filter(age >= 18) |> 
  group_by(sex) |> 
  count(passengerClass, survived) |> 
  filter(survived == "yes") |> 
  mutate(surv_chance = n / sum(n)) |> 
  select(sex, passengerClass, surv_chance) |> 
  arrange(desc(surv_chance))

# Cwiczenia mtcars

data("mtcars")

# Wczytanie i podstawowa eksploracja danych:
  # wczytaj dane mtcars,
  # wyświetl pierwsze 6 wierszy,
  # sprawdź strukturę danych,
  # sprawdź, ile jest wierszy w zbiorze danych,
  # wyświetl podsumowanie statystyczne.

mtcars |> 
  head(6)

str(mtcars)

nrow(mtcars)

summary(mtcars)

#Funkcje bazowe R:
  # policz, ile samochodów ma daną liczbę cylindrów,
  # stwórz podzbiór danych zawierający tylko samochody z silnikiem V8 (8 cylindrów),
  # sprawdź, ile jest samochodów z silnikiem V8.

mtcars |> 
  group_by(cyl) |> 
  arrange(cyl)

subset(mtcars, cyl == 8)

nrow(subset(mtcars, cyl == 8))

# Analiza z pakietem dplyr:
  # pogrupuj dane ze względu na liczbę cylindrów oraz stwórz nowe zmienne: średnie zużycie paliwa, średnia moc i liczba aut,
  # przefiltruj dane, wyświetlając tylko samochody z mocą silnika > 200, i uporządkuj je malejąco według zużycia paliwa.

mtcars |> 
  group_by(cyl) |> 
  summarise(across(mpg:hp, mean, .names = "mean_{.col}")) |> 
  mutate(car_number = n()) |> 
  filter(hp > 200) |> 
  arrange(desc(mpg))

# Jaka jest średnia moc samochodu dla każdej liczby cylindrów?
# Które samochody mają najlepsze, a które najgorsze zużycie paliwa?

mtcars |> 
  group_by(cyl) |> 
  mutate(mean_hp = mean(hp, na.rm = T))

mtcars |> 
  arrange(mpg)

mtcars |> 
  arrange(desc(mpg))

# Cwiczenia Tidyr
# Użyj funkcji spread() tak, aby każdy wiersz odpowiadał uczniowi, a kolumny reprezentowały przedmioty.
# Kolumny przedmiotów powinny zawierać wyniki.
# Teraz użyj funkcji gather(), aby przywrócić oryginalny format danych:
# kolumny uczeń, przedmiot, wynik. Co się zmieniło?

oceny <- tibble(
  uczeń = c("Anna", "Bartek", "Celina", "Darek"),
  przedmiot = c("Matematyka", "Matematyka", "Nauka", "Nauka"),
  wynik = c(80, 90, 70, 85)
)

oceny_szerokie <- oceny |> 
  spread(key = przedmiot, value = wynik)

oceny_szerokie |> 
  gather(Matematyka, Nauka, key = "Przedmiot", value = "wynik", na.rm = T)

sprzedaz <- data.frame(
  rok = c(2020, 2020, 2021, 2021),
  produkt = c("A", "B", "A", "B"),
  przychod = c(1000, 1500, 2000, 2500)
)

sprzedaz |> 
  spread(key = rok, value = przychod)
