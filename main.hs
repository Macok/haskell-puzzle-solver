import Data.Bits
import Data.List


-- Funkcje służące do wczytywania wymagań z pliku

czytajLinie i = do
    content <- readFile "data.txt"
    let linesOfFile = lines content
    let line = linesOfFile !! i
    return line

czytajWymaganiaWiersze = do
    linia <- czytajLinie 0
    return (read linia::[[Int]])

czytajWymaganiaKolumny = do
    linia <- czytajLinie 1
    return (read linia::[[Int]])


-- Funkcja rozpoczyna działanie programu
main =
    do
    wymaganiaWiersze <-czytajWymaganiaWiersze
    wymaganiaKolumny <-czytajWymaganiaKolumny
    putStrLn ("Wymiary planszy: "++show (length wymaganiaWiersze)++"x"++show (length wymaganiaKolumny))
    let rozwiazania = generujRozwiazania wymaganiaWiersze wymaganiaKolumny
    if length rozwiazania == 0 
	    then putStrLn "Nie znaleziono rozwiazania"    
	else
        do	
        putStrLn ("Liczba rozwiazan ukladu: "++show (length rozwiazania))
        mapM_ (drukujRozwiazanie . rozwinRozwiazanie) rozwiazania


{-
Za pomocą funkcji generujPermutacje generuje wszystkie możliwe permutacje zapełnionych pól dla każdego wiersza,
a następnie tworzy kombinacje tych permutacji.
Najpierw generowane są kombinacje 2 pierwszych wierszy, potem 3 itd.
Na bieżąco wykonywane jest sprawdzanie, czy dana kombinacja wierszy ma w przyszłości szansę spełnić wymagania dla kolumn.
-}
generujRozwiazania wymaganiaWiersze wymaganiaKolumny = generujRozwiazania' wymaganiaKolumny tablicaPermutacji []
    where tablicaPermutacji = generujPermutacje wymaganiaWiersze wymaganiaKolumny
generujRozwiazania' wymaganiaKolumny [] mozliwosci = mozliwosci
generujRozwiazania' wymaganiaKolumny (t:tablicaPermutacji) mozliwosci = 
    generujRozwiazania' wymaganiaKolumny tablicaPermutacji (filter (sprawdzKolumny wymaganiaKolumny) (cartProd mozliwosci t))
	
{-
Sprawdza, czy wygenerowana kombinacja wierszy ma szansę spełnić wymagania dotyczące kolumn.
Przykładowo, jezeli wygenerowano kombinację 2 wierszy i w n-tej kolumnie są już 2 zapełnione pola,
a wymagania dla tej kolumny mają postać [1,...], to kombinację można odrzucić.
-}
sprawdzKolumny wymaganiaKolumny wiersze = sprawdzKolumny' wymaganiaKolumny (transpose (rozwinRozwiazanie wiersze))
sprawdzKolumny' [] [] = True
sprawdzKolumny' (w:wymaganiaKolumny) (kolumna:kolumny)
	| length kolumnaJakoWymagania > length w = False
	| length kolumnaJakoWymagania == 0 = sprawdzKolumny' wymaganiaKolumny kolumny
    | (isPrefixOf (init kolumnaJakoWymagania) w) && (kolumnaJakoWymagania!!n <= w!!n)  = sprawdzKolumny' wymaganiaKolumny kolumny
	| otherwise = False
	where 
        n = (length kolumnaJakoWymagania)-1
        kolumnaJakoWymagania = wierszDoWymagan kolumna

{-
Funkcja dla każdego wiersza wymagań generuje wszystkie mozliwe permutacje pól spełniające te wymagania.
Przykładowo dla wiersza wymagań [2,1] i długości wiersza 5, wygenerowane zostaną następujące permutacje:
[2,0,1,0], [2,0,0,1], [0,2,0,1]
-}
generujPermutacje [] _ = []
generujPermutacje (w:wymaganiaWiersze) wymaganiaKolumny = 
    filter  (czyZgodnaZWymaganiami w) (filter czyZgodnaZZalozeniami permutacjeWiersza) 
        : generujPermutacje wymaganiaWiersze wymaganiaKolumny
	where
    permutacjeWiersza = uniquePerms elementy
    elementy = w ++ replicate (lKolumn - (sum w)) 0
    lKolumn = length wymaganiaKolumny
	

{-
Permutacja jest zgodna z wymaganiami, jezeli po usunieciu z niej elementów 0 otrzymujemy wymagania dla danego wiersza
Przykladowo dla wymagan [2,1] permutacja [2,0,1,0] jest zgodna z wymaganiami, a [0,1,0,2] nie
-}
czyZgodnaZWymaganiami wymagania permutacja = 
    filter (\el->el/=0) permutacja == wymagania	

{- 
Permutacja jest niezgodna z zalozeniami, jeśli ciągi zapełnionych elementów występują bezpośrednio po sobie
Np. permutacja [2,1,0] jest błędna, bo ciągi o długości 2 i 1 tworzą łącznie ciąg o długości 3
Permutacja [2,0,1] jest natomiast poprawna
-}
czyZgodnaZZalozeniami [_]=True
czyZgodnaZZalozeniami (a:b:permutacja)
    | a>0 && b>0 = False
    | otherwise = czyZgodnaZZalozeniami (b:permutacja)


{-
Iloczyn kartezjański wygenerowanych poprzednio kombinacji permutacji wierszy z permutacjami następnego wiersza
-}		
cartProd [] ys = [[y] | y<-ys]
cartProd xss ys = [xs++[y] | xs <- xss, y<-ys]

	
{- 
Rozwija rozwiazanie z postaci:
    [1,0,1]
	[2,0]
Do postaci:
    [True,False,True]
	[True,True,False]
-}
rozwinRozwiazanie = map rozwinWiersz

rozwinWiersz [] = []
rozwinWiersz (el:wiersz)
    | el > 0 = (replicate el True)++rozwinWiersz wiersz
    | otherwise = False:rozwinWiersz wiersz

	
{-
Zamienia wiersz (kolumnę) w postaci:
    [True,True,False,True]
Na odpowiadające mu wymagania, w tym wypadku:
	[2,1]
-}
wierszDoWymagan w = wierszDoWymagan' w 0
wierszDoWymagan' [] biezacaWartosc = 
    if biezacaWartosc==0 
    then [] 
    else [biezacaWartosc]
wierszDoWymagan' (x:w) biezacaWartosc
    | x == True = wierszDoWymagan' w (biezacaWartosc+1)
    | biezacaWartosc == 0 = (wierszDoWymagan' w 0)
    | otherwise = biezacaWartosc : (wierszDoWymagan' w 0)


-- Funkcje wykorzystywane przy drukowaniu znalezionego rozwiązania
drukujRozwiazanie r = 
    do
    putStrLn (' ':(replicate n '-'))
    mapM_ (putStrLn . wierszDoStr) r
    putStrLn (' ':(replicate n '-'))
    putStrLn ""
	where n=length (r!!0)

wierszDoStr w= "|"++(map (\el -> if el then 'X' else ' ') w)++"|"





{-
Poniższe funkcje służą do generowania unikalnych permutacji
Źródło: http://codemirror.net/mode/haskell/
-}

-- | Find all unique permutations of a list where there might be duplicates.
uniquePerms :: (Eq a) => [a] -> [[a]]
uniquePerms = permBag . makeBag

-- | An unordered collection where duplicate values are allowed,
-- but represented with a single value and a count.
type Bag a = [(a, Int)]

makeBag :: (Eq a) => [a] -> Bag a
makeBag [] = []
makeBag (a:as) = mix a $ makeBag as
  where
    mix a []                        = [(a,1)]
    mix a (bn@(b,n):bs) | a == b    = (b,n+1):bs
                        | otherwise = bn : mix a bs

permBag :: Bag a -> [[a]]
permBag [] = [[]]
permBag bs = concatMap (\(f,cs) -> map (f:) $ permBag cs) . oneOfEach $ bs
  where
    oneOfEach [] = []
    oneOfEach (an@(a,n):bs) =
        let bs' = if n == 1 then bs else (a,n-1):bs
        in (a,bs') : mapSnd (an:) (oneOfEach bs)
    
    apSnd f (a,b) = (a, f b)
    mapSnd = map . apSnd
