package mediathek.tool.table;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenFilm;
import mediathek.tool.models.TModelFilm;

public class MVFilmTable extends ASelectableMVTable {
    private static final long serialVersionUID = -5362792359176783146L;

    @Override
    protected void setupTableType() {
        maxSpalten = DatenFilm.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(Daten.spaltenAnzeigenFilme, DatenFilm.MAX_ELEM);
        indexSpalte = DatenFilm.FILM_NR;
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME;
        iconAnzeigenStr = MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN;
        iconKleinStr = MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN;

        setModel(new TModelFilm());
    }

    private void resetFilmeTab(int i) {
        reihe[i] = i;
        breite[i] = 200;
        switch (i) {
            case DatenFilm.FILM_NR:
                breite[i] = 75;
                break;
            case DatenFilm.FILM_TITEL:
                breite[i] = 300;
                break;
            case DatenFilm.FILM_DATUM:
            case DatenFilm.FILM_ZEIT:
            case DatenFilm.FILM_SENDER:
            case DatenFilm.FILM_GROESSE:
            case DatenFilm.FILM_DAUER:
            case DatenFilm.FILM_GEO:
                breite[i] = 100;
                break;
            case DatenFilm.FILM_URL:
                breite[i] = 500;
                break;
            case DatenFilm.FILM_ABSPIELEN:
            case DatenFilm.FILM_AUFZEICHNEN:
            case DatenFilm.FILM_HD:
            case DatenFilm.FILM_UT:
                breite[i] = 50;
                break;
        }
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetFilmeTab(i);
        }

        super.resetTabelle();
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            switch (i) {
                case DatenFilm.FILM_NEU:
                case DatenFilm.FILM_URL_HD:
                case DatenFilm.FILM_URL_KLEIN:
                case DatenFilm.FILM_URL_HISTORY:
                case DatenFilm.FILM_URL_SUBTITLE:
                case DatenFilm.FILM_REF:
                    breite[i] = 0;
                    break;
            }
        }
    }
}
