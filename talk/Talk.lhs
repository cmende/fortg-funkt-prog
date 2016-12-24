\documentclass[a4paper]{article}

\usepackage[utf8]{inputenc}
\usepackage{framed}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include forall.fmt
%include greek.fmt

\begin{document}

%if false
\begin{code}
{-# LANGUAGE RankNTypes #-}
module Talk where
import Prelude hiding (map,filter)
\end{code}
%endif

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
filter :: (alpha -> Bool) -> [alpha] -> [alpha]
filter f = foldr (\ x xs -> if f x then x : xs else xs) []

map :: (alpha -> beta) -> [alpha] -> [beta]
map f = foldr (\ x xs -> f x : xs) []
\end{code}

|foldr| stellt dabei eine Funktion dar, die Listen konsumiert. Um ein Muster
zu erkennen müssen wir also auch die Funktionen verallgemeinern, die Listen
produzieren. Dazu wird eine neue Funktion namens |build| eingeführt:

\begin{code}
build :: (forall beta. (alpha -> beta -> beta) -> beta -> beta) -> [alpha]
build g = g (:) []
\end{code}

Diese Funktion nimmt eine Funktion von einem Typen 2. Ranges. Das heißt, dass
die Funktion, die übergeben wird, ebenfalls Polymorph sein muss. Konkret heißt
das, dass eine Funktion vom Typen |forall beta. (alpha -> beta -> beta) -> beta -> beta|
übergeben werden muss. Das |forall beta.| (geschrieben als @forall b.@) ist bei
Haskell--Funktionen impliziert, sodass die Funktion üblicherweise einfach als
|(alpha -> beta -> beta) -> beta -> beta| angegeben wird.

Für dieses |build| werden der übergebenen Funktion der Listenkonstruktor |(:)|
(also vom Typ |alpha -> [alpha] -> [alpha]|) sowie die leere Liste |[]|
(also vom Typ |[alpha]|) übergeben. Die Typvariable |beta| wird also an den
Typen |[alpha]| gebunden, weshalb die übergeben Funktion |g| sowie das |build|
einen Wert vom Typen |[alpha]| zurückgeben.

\begin{framed}
\subsection*{Exkurs Rank--N--Types}

Mit Typen n. Ranges lassen sich Funktionssignaturen für Funktionen, die
polymorphe Funktionen als Parameter nehmen, beschreiben. Zur Verdeutlichung
dazu vergleiche folgende Funktionen:

\begin{code}
f1 :: (alpha -> alpha) -> Int
f2 :: forall alpha. (alpha -> alpha) -> Int

g :: (forall alpha. alpha -> alpha) -> Int
\end{code}

%if false
\begin{code}
f1 _ = 0
f2 _ = 0
g _ = 0
\end{code}
%endif

|f1| und |f2| beschreiben hierbei jeweils den gleichen Typen. Der Unterschied
ist lediglich, dass bei |f2| das implizite |forall alpha.| angibt, während |f1|
es weglässt. Dies ändert nichts am Typen der Funktion.

Die Funktion |g| hingegen hat das |forall alpha.| innerhalb der Klammern des
ersten Parameters. Hierdurch wird vorgeschrieben, dass die übergeben Funktion
ebenfalls Polymorph hinsichtlich |a| sein muss, während |f1| und |f2| auch
Funktionen vom Typen |Int -> Int| oder |String -> String| übergeben werden kann.

Ein gültiger Aufruf von |f1| könnte also beispielsweise |f1 (+1)| sein. Das
partiell angewendete |(+)| hat jetzt noch den Typen |Int -> Int| und erfüllt
damit die Bedingung |forall alpha. (alpha -> alpha)|. Für |g| wäre dies kein
gültiger Parameter, da |(+1)| nicht die Signatur |forall alpha. alpha -> alpha|
hat. Ein valider Parameter für |g| wäre zum Beispiel |id|, da diese Funktion
tatsächlich den Typen |forall alpha. alpha -> alpha| hat.

Um diese Funktionalität nutzen zu können, muss das |RankNTypes|-Flag gesetzt
werden. Dies geschieht mittels |{-# LANGUAGE RankNTypes #-}|.
\end{framed}

Nun haben wir eine Funktion, die Listen konsumiert und eine, die Listen erzeugt.
Wenn man sich das Vorgehen von |foldr| einmal ansieht, können wir eine Regel
ableiten, mit der sich diese beiden Funktionen aufheben.

Das Ziel von einem Aufruf |foldr k z xs| ist es, alle Listenkonstruktoren
in |xs| mit einem Aufruf von |k| zu ersetzen und die leere Liste mit |z|.
Wenn wir nun unsere Liste |xs| mit |build| erzeugen, bekommen wir den Aufruf
|foldr k z (build g)|, oder wenn wir |build| inline schreiben
|foldr k z (g (:) [])|. Dieses abstrakte |foldr| kann man nun evaluieren und
die oben genannte Transformation durchführen, wir ersetzen also im Aufruf
von |g| das |(:)| durch |k| und |[]| durch |z|. Das Ergebnis ist |g k z|.
Bei dieser Transformation konnten sowohl |foldr| als auch |build| entfernt
werden, es werden |k| und |z| jetzt direkt auf |g| angewendet, wodurch die
Zwischenliste, die |build| vorher erzeugt hat, entfällt.

Um diese Transformation durchführen zu können, schreiben wir zunächst |filter|
und |map| mit Hilfe von |build|. Dazu müssen wir nur unsere alte Definition
kopieren, als Lambda (das |g|) an Build übergeben und die Listenkonstruktoren
und leeren Listen ersetzen.

\begin{code}
filter' :: (alpha -> Bool) -> [alpha] -> [alpha]
filter' f xxs = build
  (\ c n -> foldr (\ x xs -> if f x then c x xs else xs) n xxs)

map' :: (alpha -> beta) -> [alpha] -> [beta]
map' f xxs = build
  (\ c n -> foldr (\ x xs -> c (f x) xs) n xxs)
\end{code}

Diese Definitionen können wir jetzt in einer neuen Version von |doubleOdds|
verwenden. Hierzu schreiben wir auch diese wieder inline, um später die
Transformation durchführen zu können. Auch hier brauchen wir wieder nur
unsere Definitionen von |filter'| und |map'| in das neue |doubleOdds''| zu
kopieren und ersetzen dieses mal den Parameter |f| durch eine konkrete Funktion.
Im Fall vom |map'|, das wir inlinen wollen, ist das der Lambda--Ausdruck
|(\ x xs -> c0 (x * 2) xs)|, wobei |c0| den von |build| übergebenen Konstruktor
darstellt. Und an Filter übergeben wir |(\ x xs -> if odd x then c1 x xs else xs)|:

\begin{code}
doubleOdds'' :: [Int] -> [Int]
doubleOdds'' xxs = build
  (\ c0 n0 -> foldr (\ x xs -> c0 (x * 2) xs) n0 (build
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)))
\end{code}

Auf diese Funktion können wir nun die Regel |foldr k z (build g) = g k z|
anwenden. Dazu wird das Lambda der inneren |build|-Funktion als |g| benutzt
und die Argumente des |foldr| im äußeren |build| stellen die Parameter |k| und
|z|.

\begin{code}
doubleOdds''' :: [Int] -> [Int]
doubleOdds''' xxs = build
  (\ c0 n0 ->
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> c0 (x * 2) xs) n0)
\end{code}

An dieser Stelle könnte man aufhören, da das Ziel bereits erreicht ist und
keine Zwischenliste mehr erzeugt wird. Aus Gründen der Lesbarkeit kann man
jedoch auch das äußere |build| noch inline schreiben und anschließend den
Lambda--Ausdruck auflösen.

\begin{code}
doubleOdds'''' :: [Int] -> [Int]
doubleOdds'''' xxs =
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> (x * 2) : xs) []

doubleOdds''''' :: [Int] -> [Int]
doubleOdds''''' = foldr (\ x xs -> if odd x then (x * 2) : xs else xs) []
\end{code}

\end{document}
