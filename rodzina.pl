kobieta(ewa).
kobieta(elzbieta).
kobieta(iwona).
kobieta(ilona).
kobieta(anna).
kobieta(marta).
kobieta(iza).
kobieta(ola).
kobieta(magda).

mezczyzna(piotr).
mezczyzna(adam).
mezczyzna(pawel).
mezczyzna(dariusz).
mezczyzna(jan).
mezczyzna(norbert).
mezczyzna(marek).
mezczyzna(krzysztof).
mezczyzna(maciej).

rodzic(ewa,marek).
rodzic(ewa,marta).
rodzic(piotr,marek).
rodzic(piotr,marta).
rodzic(elzbieta,magda).
rodzic(norbert,magda).
rodzic(maciej,anna).
rodzic(ola,anna).
rodzic(marek,ola).
rodzic(magda,ola).
rodzic(pawel,krzysztof).
rodzic(pawel,iza).
rodzic(iza,alicja).
rodzic(krzysztof,adam).
rodzic(marta,adam).
rodzic(iwona,iza).
rodzic(iwona,krzysztof).
rodzic(ilona,dariusz).
rodzic(adam,dariusz).
rodzic(ola,marta).
rodzic(maciej,marta).

wiek(elzbieta,78).
wiek(norbert,80).
wiek(ewa,84).
wiek(piotr,85).
wiek(iwona,85).
wiek(pawel,86).
wiek(krzysztof,65).
wiek(iza,60).
wiek(marta,55).
wiek(marek,58).
wiek(magda,55).
wiek(maciej,32).
wiek(ola,30).
wiek(adam,37).
wiek(ilona,33).
wiek(dariusz,13).
wiek(anna,10).

ojciec(X,Y):-mezczyzna(X),rodzic(X,Y).
rodzice(M,O,D):-matka(M,D),ojciec(O,D).



