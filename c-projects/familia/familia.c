#include <stdio.h>
#include <intrin.h>
#include <string.h>
#include <ctype.h>

typedef struct Lista {
    char imie[20];
    char nazwisko[20];
    char dataUrodzenia[20];
    char dataSmierci[20];
    struct Lista *nastepny;
} Lista;

typedef struct Wezel{
    char imie[20];
    char nazwisko[20];
    char dataUrodzenia[20];
    char dataSmierci[20];
    char imieMalzonka[20];
    char nazwiskoMalzonka[20];
    char dataUrodzeniaMalzonka[20];
    char dataSmierciMalzonka[20];
    int statusSpolecznyTabow;
    int zalatwiony;
    struct Wezel *dziecko[5];
    struct Wezel *rodzice;
}Wezel;

//void zamiana(Lista ***korzen, int pozycja1, int pozycja2){}

void sortowanie(Lista **korzen, int iloscOsobListy) {
    Lista *itowy = *korzen;
    Lista *jtowy = *korzen;
    jtowy = jtowy->nastepny;
    struct Lista *wskaznikNajmniejszego;
    int j;
    for (int i = 0; i < iloscOsobListy; i++) {
        wskaznikNajmniejszego = itowy;
        for (j = i+1; j < iloscOsobListy; j++) {
            if (wskaznikNajmniejszego->imie < jtowy->imie) {
                wskaznikNajmniejszego = jtowy;
            }
            jtowy = jtowy->nastepny;
        }
        //zamiana(&korzen, i, j);
    }

}
void wypiszListe(Lista *glowaLisciana) {
    if (glowaLisciana == NULL)
        printf("lista jest pusta");

    else {
        Lista *obecnaOsoba = glowaLisciana;

        while (obecnaOsoba != NULL) {
            printf("%s %s %s %s\n", obecnaOsoba->imie, obecnaOsoba->nazwisko, obecnaOsoba->dataUrodzenia,
                   obecnaOsoba->dataSmierci);
            obecnaOsoba = obecnaOsoba->nastepny;
        }
    }
}

void wypiszRodzinke(Wezel *korzen, int iloscOsob){
    Wezel *osobaWypisywana = korzen;

    int kolejnoscDziecka = 0;
    int aktualnaIloscOsob = 0;

    for (int i = 0; aktualnaIloscOsob <iloscOsob; ++i) {
        while(osobaWypisywana->zalatwiony!=1){
            for (int j = 0; j < osobaWypisywana->statusSpolecznyTabow; ++j) {
                printf("    ");
            }
            printf("%s %s %s %s %s %s %s %s\n", osobaWypisywana->imie, osobaWypisywana->nazwisko, osobaWypisywana->dataUrodzenia,
                   osobaWypisywana->dataSmierci, osobaWypisywana->imieMalzonka, osobaWypisywana->nazwiskoMalzonka, osobaWypisywana->dataUrodzeniaMalzonka,
                   osobaWypisywana->dataSmierciMalzonka);
            aktualnaIloscOsob++;
            osobaWypisywana->zalatwiony=1;

            if(osobaWypisywana->dziecko[kolejnoscDziecka]==NULL)
                break;
            osobaWypisywana = osobaWypisywana->dziecko[kolejnoscDziecka];
        }
        osobaWypisywana=osobaWypisywana->rodzice;

        for (int j = 0; osobaWypisywana->dziecko[kolejnoscDziecka]!=NULL && osobaWypisywana->dziecko[kolejnoscDziecka]->zalatwiony==1; ++j) {
            kolejnoscDziecka++;
        }

        if(osobaWypisywana->dziecko[kolejnoscDziecka]!=NULL)
        osobaWypisywana = osobaWypisywana->dziecko[kolejnoscDziecka];
    }
}

void dodajDoListy(Lista ***glowaLisciana, char *imie, char *nazwisko, char *dataUrodzenia, char *dataSmierci ){
    Lista *nowaOsoba = malloc(sizeof(Lista));
    strcpy(nowaOsoba->imie, imie);
    strcpy(nowaOsoba->nazwisko, nazwisko);
    strcpy(nowaOsoba->dataUrodzenia, dataUrodzenia);
    strcpy(nowaOsoba->dataSmierci, dataSmierci);
    nowaOsoba->nastepny=NULL;

    if (**glowaLisciana == NULL) {
        **glowaLisciana = nowaOsoba;
    }
    else {
        Lista *staryOstatniElement = **glowaLisciana;

        while (staryOstatniElement->nastepny != NULL)
            staryOstatniElement = staryOstatniElement->nastepny;
        staryOstatniElement->nastepny = nowaOsoba;
    }
}

void dodajWezel(Wezel ***korzen, char *imie, char *nazwisko, char *dataUrodzenia, char *dataSmierci,
                char *imieMalzonka, char *nazwiskoMalzonka, char *dataUrodzeniaMalzonka, char *dataSmierciMalzonka,
                int statusDziecka) {
    Wezel *nowyPotomek = malloc(sizeof(Wezel));
    strcpy(nowyPotomek->imie, imie);
    strcpy(nowyPotomek->nazwisko, nazwisko);
    strcpy(nowyPotomek->dataUrodzenia, dataUrodzenia);
    strcpy(nowyPotomek->dataSmierci, dataSmierci);
    strcpy(nowyPotomek->imieMalzonka, imieMalzonka);
    strcpy(nowyPotomek->nazwiskoMalzonka, nazwiskoMalzonka);
    strcpy(nowyPotomek->dataUrodzeniaMalzonka, dataUrodzeniaMalzonka);
    strcpy(nowyPotomek->dataSmierciMalzonka, dataSmierciMalzonka);
    for (int i = 0; i < 5; ++i) {
        nowyPotomek->dziecko[i] = NULL;
    }

    if (**korzen == NULL) {
        **korzen = nowyPotomek;
        nowyPotomek->rodzice = NULL;
    }
    else {
        Wezel *staraOstatniaOsoba = **korzen;
        int kolejnoscDziecka = 0;
        int aktualnyPoziom = 0;
        while(aktualnyPoziom!=statusDziecka){

            int alternatywka = kolejnoscDziecka+1;
            while ((staraOstatniaOsoba->dziecko[kolejnoscDziecka] != NULL && staraOstatniaOsoba->dziecko[alternatywka] != NULL) ||
                    (staraOstatniaOsoba->dziecko[kolejnoscDziecka] != NULL && statusDziecka==aktualnyPoziom+1)) {
                kolejnoscDziecka++;
            }
            if(staraOstatniaOsoba->dziecko[kolejnoscDziecka]!=NULL)
                staraOstatniaOsoba = staraOstatniaOsoba->dziecko[kolejnoscDziecka];
            aktualnyPoziom++;

            if(aktualnyPoziom==statusDziecka) {
                nowyPotomek->rodzice = staraOstatniaOsoba;
                staraOstatniaOsoba->dziecko[kolejnoscDziecka] = nowyPotomek;
                break;
            }
        }
        nowyPotomek->rodzice = staraOstatniaOsoba;
        staraOstatniaOsoba->dziecko[kolejnoscDziecka] = nowyPotomek;
        while(staraOstatniaOsoba->dziecko[kolejnoscDziecka] != NULL){
            kolejnoscDziecka++;
        }
    }
    nowyPotomek->statusSpolecznyTabow = statusDziecka;
    nowyPotomek->zalatwiony=0;
}

void pobierzDane(FILE *plikWejsciowy, Wezel **korzen, Lista **glowaLisciana, int *iloscOsob, int *iloscOsobListy){
    char znak;
    char a[20], b[20], c[20], d[20], e[20], f[20], g[20], h[20];
    int statusDziecka = 0;
    while(fscanf(plikWejsciowy,"%c", &znak)!=EOF){
        while(!strstr(&znak,"(")){
            if(strstr(&znak,"[")) {
                statusDziecka++;
            }
            if(strstr(&znak,"]"))
                statusDziecka--;
            if(fscanf(plikWejsciowy,"%c", &znak)==EOF)
                return;
        }

        fscanf(plikWejsciowy,"%[^,], %[^,], %[^,],", &a, &b, &c);
        char czyPrzeszkadzacz;
        fscanf(plikWejsciowy, "%c", &czyPrzeszkadzacz);
        if(!isspace(czyPrzeszkadzacz)){
            memset(d,0,20*sizeof (char));
        }
        else{
            fscanf(plikWejsciowy,"%c", &czyPrzeszkadzacz);
            if(strstr(&czyPrzeszkadzacz, ",")) {
                memset(d, 0, 20 * sizeof(char));
            }
            else{
                fseek(plikWejsciowy, -1, SEEK_CUR);
                fscanf(plikWejsciowy,"%[^,],", &d);
            }
        }
        fscanf(plikWejsciowy, "%c", &czyPrzeszkadzacz);
        if(!isspace(czyPrzeszkadzacz)){
            memset(e,0,20*sizeof (char));
            memset(f,0,20*sizeof (char));
            memset(g,0,20*sizeof (char));
            memset(h,0,20*sizeof (char));
        } else {
            fscanf(plikWejsciowy, "%c", &czyPrzeszkadzacz);
            if (strstr(&czyPrzeszkadzacz, ",")) {
                memset(e, 0, 20 * sizeof(char));
                memset(f, 0, 20 * sizeof(char));
                memset(g, 0, 20 * sizeof(char));
                memset(h, 0, 20 * sizeof(char));
            } else {
                fseek(plikWejsciowy, -1, SEEK_CUR);
                fscanf(plikWejsciowy, "%[^,], %[^,], %[^,],", &e, &f, &g);

                fscanf(plikWejsciowy, "%c", &czyPrzeszkadzacz);
                if (strstr(&czyPrzeszkadzacz, ",")) {
                    memset(h, 0, 20 * sizeof(char));
                } else {
                    fscanf(plikWejsciowy, "%c", &czyPrzeszkadzacz);
                    if (strstr(&czyPrzeszkadzacz, ",")) {
                        memset(d, 0, 20 * sizeof(char));
                    } else {
                        fseek(plikWejsciowy, -1, SEEK_CUR);
                        fscanf(plikWejsciowy, "%[^,],", &h);
                    }
                }
            }
        }
        dodajWezel(&korzen, a, b, c, d, e, f, g, h, statusDziecka);
        dodajDoListy(&glowaLisciana, a, b, c, d);
        iloscOsobListy++;

        if(e[0]!=0) {
            dodajDoListy(&glowaLisciana, e, f, g, h);
            iloscOsobListy++;
        }

        (*iloscOsob)++;
        memset(a,0,20*sizeof (char));
        memset(b,0,20*sizeof (char));
        memset(c,0,20*sizeof (char));
        memset(d,0,20*sizeof (char));
        memset(e,0,20*sizeof (char));
        memset(f,0,20*sizeof (char));
        memset(g,0,20*sizeof (char));
        memset(h,0,20*sizeof (char));
    }
}
int main(int argc, char *argv[]) {
    FILE *plikWejsciowy = fopen(argv[1], "r");

    if (plikWejsciowy == NULL) {
        printf("pliku nie znaleziono");
        return 1;
    }

    if(argc!=2){
        printf("zla liczba argumentow");
        return 2;
    }
    Wezel *korzen = NULL;
    Lista *glowaLisciana = NULL;

    int iloscOsob = 0;
    int iloscOsobListy = 0;

    pobierzDane(plikWejsciowy, &korzen, &glowaLisciana, &iloscOsob, &iloscOsobListy);
    wypiszListe(glowaLisciana);
    wypiszRodzinke(korzen, iloscOsob);
    return 0;
}