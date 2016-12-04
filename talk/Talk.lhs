\documentclass[a4paper]{article}

\usepackage[utf8]{inputenc}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include forall.fmt

\begin{document}

\title{A Short Cut to Deforestation}
\author{Christoph Mende}
\date{02.12.2016}
\maketitle

\section{Einleitung}

Funktionale Programmiersprachen benutzen oft Zwischenlisten, um verschiedene
Funktionen miteinander zu verketten. Diese Zwischenlisten werden allerdings nur
einmal benutzt und danach verworfen, wodurch unnötig Rechenzeit und Speicher
verschwendet wird.

Die Arbeit ``A Short Cut to Deforestation'' von Andrew Gill, John Launchbury und
Simon L Peyton Jones beschreibt eine Lösung für dieses Problem, indem sie eine
Technik entwickeln, die es dem Compiler erlaubt, das Programm so zu optimieren,
dass die Zwischenlisten nicht erzeugt werden.

\section{Das Problem}

Um dieses Problem zu verdeutlichen gucken wir uns folgende Funktion an, die
eine Liste von Zahlen nimmt, die ungeraden Zahlen herausfiltert und anschließend
die Zahlen in der Ergebnisliste verdoppelt:

\begin{code}
doubleOdds :: [Int] -> [Int]
doubleOdds xs = map (*2) (filter odd xs)
\end{code}

Hier wird deutlich, dass |filter| die Eingabeliste von |doubleOdds| konsumiert
und eine neue Liste erzeugt. Diese neue Liste wird an |map| weitergegeben,
welches diese konsumiert und wieder eine neue Liste erzeugt. Diese neue Liste
wird anschließend von |doubleOdds| zurückgegeben.

Man könnte dieses Problem jetzt umgehen, indem man |doubleOdds| wie folgt
umschreibt:

\begin{code}
doubleOdds' :: [Int] -> [Int]
doubleOdds' = h
  where
    h []     = []
    h (x:xs) = if odd x then (x * 2) : h xs else h xs
\end{code}

Diese Umformung kann jedoch nicht automatisch erfolgen, da der Compiler eine
unendliche Liste an Regeln bräuchte, um diese Muster zu erkennen. Wir brauchen
also eine Lösung, die generell anwendbar ist.

\section{Die Lösung}

Um diese Lösung zu finden, werden in der Arbeit zunächst die Funktionen
verallgemeinert. |filter| und |map| lassen sich beispielsweise auf einfache
|foldr|-Aufrufe vereinfachen:

\begin{code}
filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\ x xs -> if f x then x : xs else xs) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\ x xs -> f x : xs) []
\end{code}

|foldr| stellt dabei eine Funktion dar, die Listen konsumiert. Um ein Muster
zu erkennen müssen wir also auch die Funktionen verallgemeinern, die Listen
produzieren. Dazu wird eine neue Funktion namens |build| eingeführt:

% ((cons) -> nil -> List a)
\begin{code}
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []
\end{code}

Diese Funktion nimmt eine Funktion von einem Typen 2. Ranges. Das heißt, dass
die Funktion, die übergeben wird, ebenfalls Polymorph sein muss. Konkret heißt
das, dass eine Funktion vom Typen |forall b. (a -> b -> b) -> b -> b| übergeben
werden muss. Das |forall b.| (geschrieben als `forall b.') ist bei
Haskell--Funktionen impliziert, sodass die Funktion üblicherweise einfach als
|(a -> b -> b) -> b -> b| angegeben wird.

Für dieses |build| werden der übergebenen Funktion der Listenkonstruktor |(:)|
(also vom Typ |a -> [a] -> [a]|) sowie die leere Liste |[]| (also vom Typ |[a]|)
übergeben. Die Typvariable |b| wird also an den Typen |[a]| gebunden, weshalb
die übergeben Funktion |g| sowie das |build| einen Wert vom Typen |[a]|
zurückgeben.

\subsection*{Exkurs Rank--N--Types}

Mit Typen n. Ranges lassen sich Funktionssignaturen für Funktionen, die
polymorphe Funktionen als Parameter nehmen, beschreiben. Zur Verdeutlichung
dazu vergleiche folgende Funktionen:

\begin{code}
f1 :: (a -> a) -> Int
f2 :: forall a. (a -> a) -> Int

g :: (forall a. a -> a) -> Int
\end{code}

|f1| und |f2| beschreiben hierbei jeweils den gleichen Typen. Der Unterschied
ist lediglich, dass bei |f2| das implizite |forall a.| angibt, während |f1| es
weglässt. Dies ändert nichts am Typen der Funktion.

Die Funktion |g| hingegen hat das |forall a.| innerhalb der Klammern des ersten
Parameters. Hierdurch wird vorgeschrieben, dass die übergeben Funktion ebenfalls
Polymorph hinsichtlich |a| sein muss, während |f1| und |f2| auch Funktionen
vom Typen |Int -> Int| oder |String -> String| übergeben werden kann.

Ein gültiger Aufruf von |f1| könnte also beispielsweise |f1 (+1)| sein. Das
partiell angewendete |(+)| hat jetzt noch den Typen |Int -> Int| und erfüllt
damit die Bedingung |forall a. (a -> a)|. Für |g| wäre dies kein gültiger
Parameter, da |(+1)| nicht die Signatur |forall a. a -> a| hat. Ein valider
Parameter für |g| wäre zum Beispiel |id|, da diese Funktion tatsächlich den
Typen |forall a. a -> a| hat.

Um diese Funktionalität nutzen zu können, muss das |RankNTypes|-Flag gesetzt
werden. Dies geschieht mittels |{-# LANGUAGE RankNTypes #-}|.

% TODO
% (list consumer) foldr k z xs = replace cons with k, replace nil with z in xs
% (list producer) build g = g (:) []
% => foldr k z (build g)
% = foldr k z (g (:) [])
% = g k z (see foldr definition)

% replace : and [] with build
\begin{code}
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xxs = build
  (\ c n -> foldr (\ x xs -> if f x then c x xs else xs) n xxs)
\end{code}

\begin{code}
map' :: (a -> b) -> [a] -> [b]
map' f xxs = build
  (\ c n -> foldr (\ x xs -> c (f x) xs) n xxs)
\end{code}

% replace map and filter
\begin{code}
doubleOdds'' :: [Int] -> [Int]
doubleOdds'' xxs = build
  (\ c0 n0 -> foldr (\ x xs -> c0 (x * 2) xs) n0 (build
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)))
\end{code}

% foldr k z (build g) = g k z
\begin{code}
doubleOdds''' :: [Int] -> [Int]
doubleOdds''' xxs = build
  (\ c0 n0 ->
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> c0 (x * 2) xs) n0)
\end{code}

% inline build
\begin{code}
doubleOdds'''' :: [Int] -> [Int]
doubleOdds'''' xxs =
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> (x * 2) : xs) []
\end{code}

\begin{code}
doubleOdds''''' :: [Int] -> [Int]
doubleOdds''''' =
  foldr (\ x xs -> if odd x then (x * 2) : xs else xs) []
\end{code}

\end{document}
