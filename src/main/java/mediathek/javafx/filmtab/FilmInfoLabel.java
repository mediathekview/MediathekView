package mediathek.javafx.filmtab;

import javafx.scene.control.Label;
import mediathek.config.Daten;
import mediathek.gui.tabs.tab_film.GuiFilme;

public class FilmInfoLabel extends Label {
    private final Daten daten;
    private final GuiFilme tabFilme;

    public FilmInfoLabel(Daten daten, GuiFilme tabFilme) {
        super();
        this.daten = daten;
        this.tabFilme = tabFilme;
    }

    private String createFilmLabel(final int rowCount) {
        String textLinks;
        if (rowCount == 1)
            textLinks = "1 Film";
        else
            textLinks = rowCount + " Filme";

        return textLinks;
    }

    private int oldGesamt = 0;
    private int oldRowCount = 0;

    public void updateValues() {
        String textLinks;
        final int gesamt = daten.getListeFilme().size();
        final int rowCount = tabFilme.getTableRowCount();

        if (gesamt == oldGesamt && rowCount == oldRowCount)
            return;

        // Anzahl der Filme
        if (gesamt == rowCount) {
            textLinks = createFilmLabel(rowCount);
        } else {
            textLinks = createFilmLabel(rowCount);
            textLinks += " (Insgesamt: " + gesamt + ")";
        }

        setText(textLinks);

        oldGesamt = gesamt;
        oldRowCount = rowCount;
    }
}
