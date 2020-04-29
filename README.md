#### Logický projekt: Rubikova kostka 
##### FLP, 2019/2020
##### Vladislav Halva
##### xhalva04

Odevzdaný adresář kromě tohoto dokumentu obsahuje soubor **rubics.pl** obsahující zdrojový kód 
programu **Makefile** a několik souborů s testovacími vstupy **testN**,
kde N vždy označuje minimální počet kroků, které je nutné vykonat pro vyřešení Rubikovy kostky. 
Přibližné doby běhu pro tyto vstupy jsou následující: 

    test2   <1s
    test3   <1s
    test4    2s
    test5   38s

Řešení Rubikovy kostky je implementováno pomocí prohledávání do šířky (BFS) se seznamem closed.
Fronta open i seznam closed jsou řešeny pomocí dynamických predikátů open a closed. 
Implementováno je 12 základních transformací - kroků, tj. otočení vrchní, přední, zadní, levé, 
pravé a zadní stěny kostky vždy po a proti směru hodinových ručiček.

