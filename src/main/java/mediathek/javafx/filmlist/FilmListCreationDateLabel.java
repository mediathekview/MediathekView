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
        //update text
        setComputedText(String.format("Filmliste erstellt: %s Uhr", daten.getListeFilme().metaData().getGenerationDateTimeAsString()));
    }
}
