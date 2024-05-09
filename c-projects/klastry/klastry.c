#include <stdio.h>
#include <malloc.h>

typedef struct Klaster{
    int wiersz;
    int kolumna;
    int kolor;
    struct Klaster *nastepny;
}Klaster;

typedef  struct Zbior{
    int kolor;
    int rozmiar;
    struct Zbior *nastepny;
}Zbior;

int dodajKwadratDoKlastra(Klaster **glowa, int wiersz, int kolumna, int kolor, int *rozmiarKlastra) {
    Klaster *staryOstatniKwadrat = *glowa;
    if(*glowa!=NULL) {
        while (staryOstatniKwadrat != NULL) {
            if (staryOstatniKwadrat->wiersz == wiersz && staryOstatniKwadrat->kolumna == kolumna){
               return 1;
            }
            staryOstatniKwadrat = staryOstatniKwadrat->nastepny;
        }
    }
    staryOstatniKwadrat = *glowa;
    Klaster *nowyKwadratDoKlastra = malloc(sizeof(Klaster));
    nowyKwadratDoKlastra->wiersz = wiersz;
    nowyKwadratDoKlastra->kolumna = kolumna;
    nowyKwadratDoKlastra->kolor = kolor;
    nowyKwadratDoKlastra->nastepny = NULL;
    (*rozmiarKlastra)++;

    if (*glowa == NULL)
       *glowa = nowyKwadratDoKlastra;
    else {
        if (staryOstatniKwadrat->wiersz == nowyKwadratDoKlastra->wiersz && staryOstatniKwadrat->kolumna == nowyKwadratDoKlastra->kolumna)
            (*rozmiarKlastra)--;
        while (staryOstatniKwadrat->nastepny != NULL)
            staryOstatniKwadrat = staryOstatniKwadrat->nastepny;

        staryOstatniKwadrat->nastepny = nowyKwadratDoKlastra;
    }
    return 0;
}

void wyczyscKlaster(Klaster **glowa){
    Klaster *kwadratUsuwany = *glowa;
    Klaster *kwadratnastepny = kwadratUsuwany->nastepny;
    *glowa = NULL;

    while(kwadratUsuwany != NULL){
        free(kwadratUsuwany);
        kwadratUsuwany = kwadratnastepny;
        if(kwadratnastepny != NULL)
            kwadratnastepny = kwadratnastepny->nastepny;
    }
}

void dodajKlasterDoZbioru(Zbior **glowa, int kolor, int rozmiar) {
    Zbior *nowyKlasterDoZbioru = malloc(sizeof(Zbior));
    nowyKlasterDoZbioru->rozmiar = rozmiar;
    nowyKlasterDoZbioru->kolor = kolor;
    Zbior *klasterPoprzedni = *glowa;

    if (*glowa == NULL){
        *glowa = nowyKlasterDoZbioru;
        nowyKlasterDoZbioru->nastepny=NULL;
    }
    else if (klasterPoprzedni->kolor > kolor || (klasterPoprzedni->kolor==kolor && klasterPoprzedni->rozmiar > rozmiar)) {
        nowyKlasterDoZbioru->nastepny = klasterPoprzedni;
        *glowa = nowyKlasterDoZbioru;
    }
    else {
        while (klasterPoprzedni->kolor < kolor){
            if(klasterPoprzedni->nastepny == NULL || klasterPoprzedni->nastepny->kolor>=kolor)
                break;
            klasterPoprzedni = klasterPoprzedni->nastepny;
        }
        if(klasterPoprzedni->nastepny!=NULL) {
            if (klasterPoprzedni->nastepny->kolor == kolor) {

                while (rozmiar > klasterPoprzedni->nastepny->rozmiar) {
                    if (klasterPoprzedni->nastepny->kolor != kolor)
                        break;
                    klasterPoprzedni = klasterPoprzedni->nastepny;

                    if(klasterPoprzedni->nastepny==NULL)
                        break;
                }
            }
        }
        if (klasterPoprzedni->nastepny == NULL)
            nowyKlasterDoZbioru->nastepny = NULL;
        else
            nowyKlasterDoZbioru->nastepny = klasterPoprzedni->nastepny;

        klasterPoprzedni->nastepny = nowyKlasterDoZbioru;
    }
}

void wypiszZbior(Zbior *glowa) {
    if (glowa == NULL)
        printf("lista jest pusta");

    else {
        Zbior *poprzedniKlaster = glowa;
        Zbior *obecnyKlaster = glowa->nastepny;
        printf("\n");
        printf("Kolor : rozmiar\n%5d : %d ", poprzedniKlaster->kolor, poprzedniKlaster->rozmiar);

        while (obecnyKlaster != NULL) {
            if(poprzedniKlaster== glowa && poprzedniKlaster->kolor!=obecnyKlaster->kolor)
                printf("\n");
            if(obecnyKlaster->kolor==poprzedniKlaster->kolor)
                printf("%d ", obecnyKlaster->rozmiar);
            else
                printf("%5d : %d ", obecnyKlaster->kolor, obecnyKlaster->rozmiar);
            if(obecnyKlaster->nastepny!=NULL) {
                if (obecnyKlaster->nastepny->kolor != obecnyKlaster->kolor)
                    printf("\n");
            }
            poprzedniKlaster = obecnyKlaster;
            obecnyKlaster = obecnyKlaster->nastepny;
        }
    }
}

int main(int argc, char *argv[]) {
    FILE *plikWejsciowy = fopen(argv[1], "r");

    if (plikWejsciowy == NULL) {
        printf("pliku nie znaleziono");
        return 1;
    }

    int iloscLinii = 1;
    int znak;
    int iloscKolumn;
    int iloscWierszy;
    int **tab;

    fgetc(plikWejsciowy);
    int typP = fgetc(plikWejsciowy);
    if(typP==52) {
        while (iloscLinii < 3) {
            znak = fgetc(plikWejsciowy);
            if (znak == 10)
                iloscLinii++;
        }

        fscanf(plikWejsciowy, "%d", &iloscKolumn);
        fscanf(plikWejsciowy, "%d\n", &iloscWierszy);

        tab = malloc(iloscWierszy * sizeof(int*));
        for (int i = 0; i < iloscWierszy; i++)
            tab[i] = malloc(iloscKolumn * sizeof(int));


        for (int i = 0; i < iloscWierszy; i++) {
            for (int j = 0; j < iloscKolumn; j+=8) {
                unsigned char bajt = fgetc(plikWejsciowy);
                for (int k = 7; k >= 0; --k)
                    tab[i][j+7-k] = bajt >> k & 1;
                }
        }
        fclose(plikWejsciowy);
    }
    else if(typP==53) {

        while (iloscLinii < 3) {
            znak = fgetc(plikWejsciowy);
            if (znak == 10)
                iloscLinii++;
        }

        fscanf(plikWejsciowy, "%d", &iloscKolumn);
        fscanf(plikWejsciowy, "%d", &iloscWierszy);

        while (iloscLinii < 5) {
            znak = fgetc(plikWejsciowy);
            if (znak == 10)
                iloscLinii++;
        }

        tab = malloc(iloscWierszy * sizeof(int*));
        for (int i = 0; i < iloscWierszy; i++)
            tab[i] = malloc(iloscKolumn * sizeof(int));

        for (int i = 0; i < iloscWierszy; i++) {
            for (int j = 0; j < iloscKolumn; j++)
                tab[i][j] = fgetc(plikWejsciowy);
        }

        fclose(plikWejsciowy);
    }
    else{
        printf("obsluga pliku tego typu niemozliwa");
        return 2;
    }

    Klaster *glowaKwadratowa = NULL;
    Zbior *glowaKlastrowa = NULL;

    for(int n = 0; n < iloscWierszy; n++) {
        for (int m = 0; m < iloscKolumn; m++) {
            if (tab[n][m] != 999) {
                int i = n;
                int j = m;
                int rozmiarKlastra = 0;

                dodajKwadratDoKlastra(&glowaKwadratowa, i, j, tab[i][j], &rozmiarKlastra);
                Klaster *naszKwadrat = glowaKwadratowa;
                while (1) {
                    if (i > 0) {
                        if (tab[i][j] == tab[i - 1][j])
                            dodajKwadratDoKlastra(&glowaKwadratowa, i - 1, j, tab[i - 1][j], &rozmiarKlastra);
                    }
                    if (j < iloscKolumn - 1) {
                        if (tab[i][j] == tab[i][j + 1])
                            dodajKwadratDoKlastra(&glowaKwadratowa, i, j + 1, tab[i][j + 1], &rozmiarKlastra);
                    }
                    if (i < iloscWierszy - 1) {
                        if (tab[i][j] == tab[i + 1][j])
                            dodajKwadratDoKlastra(&glowaKwadratowa, i + 1, j, tab[i + 1][j], &rozmiarKlastra);
                    }
                    if (j > 0) {
                        if (tab[i][j] == tab[i][j - 1])
                            dodajKwadratDoKlastra(&glowaKwadratowa, i, j - 1, tab[i][j - 1], &rozmiarKlastra);
                    }
                    tab[i][j] = 999;
                    if (naszKwadrat->nastepny == NULL)
                        break;

                    i = naszKwadrat->nastepny->wiersz;
                    j = naszKwadrat->nastepny->kolumna;
                    tab[i][j] = naszKwadrat->nastepny->kolor;
                    naszKwadrat = naszKwadrat->nastepny;
                }

                dodajKlasterDoZbioru(&glowaKlastrowa, naszKwadrat->kolor, rozmiarKlastra);
                wyczyscKlaster(&glowaKwadratowa);
            }
        }
    }
    wypiszZbior(glowaKlastrowa);

    return 0;
}
