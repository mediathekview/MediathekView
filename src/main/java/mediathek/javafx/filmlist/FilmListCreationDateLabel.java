package mediathek.javafx.filmlist;

import mediathek.config.Daten;
import mediathek.javafx.tool.ComputedLabel;

/**
 * Computed label which will display the creation date of the current film list.
 */
class FilmListCreationDateLabel extends ComputedLabel {
    private final Daten daten;

    FilmListCreationDateLabel(Daten daten) {
        super();
        this.daten = daten;
    }

    /**
     * Computes and displays the age of the list.
     */
    public void computeCreationDate() {
        final String genDate = daten.getListeFilme().genDate();
        //update text
        String strText = "Filmliste erstellt: ";
        strText += genDate;
        strText += " Uhr";

        setComputedText(strText);
    }
}
