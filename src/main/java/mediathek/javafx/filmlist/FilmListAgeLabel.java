package mediathek.javafx.filmlist;

import mSearch.daten.ListeFilme;
import mediathek.config.Daten;
import mediathek.javafx.tool.ComputedLabel;

/**
 * Label which will compute the age of the filmlist when updated.
 */
class FilmListAgeLabel extends ComputedLabel {
    private final Daten daten;

    FilmListAgeLabel(Daten daten) {
        super();
        this.daten = daten;
    }

    public void computeAge() {
        final ListeFilme listeFilme = daten.getListeFilme();

        String strText = "Alter: ";

        final int sekunden = listeFilme.getAge();
        if (sekunden != 0) {
            final int minuten = sekunden / 60;
            String strSekunde = String.valueOf(sekunden % 60);
            if (strSekunde.length() < 2) {
                strSekunde = '0' + strSekunde;
            }

            String strMinute = String.valueOf(minuten % 60);
            if (strMinute.length() < 2) {
                strMinute = '0' + strMinute;
            }

            String strStunde = String.valueOf(minuten / 60);
            if (strStunde.length() < 2) {
                strStunde = '0' + strStunde;
            }

            strText += strStunde + ':' + strMinute + ':' + strSekunde;
        }

        setComputedText(strText);
    }
}
