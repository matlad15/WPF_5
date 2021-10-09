# WPF_5

Sortowanie topologiczne polega na rozszerzeniu grafu skierowanego bez cykli (DAG-u) do porządku liniowego.

Mówiąc prościej, mając dany DAG należy przypisać wierzchołkom takie różne liczby naturalne (nadające kolejność tym wierzchołkom), żeby dla każdej krawędzi grafu jej źródło miało niższy numer niż jej cel.

Mówiąc jeszcze prościej, mając daną częściową informację o zależności np. czynności od siebie (np. buty wkładamy po skarpetkach, krawat po koszuli itp. ale kolejność wkładania skarpetek i koszuli może być dowolna) mamy wygenerować ścisłą kolejność wykonywania czynności (np. koszula, skarpetki, buty, krawat).
