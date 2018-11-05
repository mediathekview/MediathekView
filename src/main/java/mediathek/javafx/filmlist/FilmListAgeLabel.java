package mediathek.javafx.filmlist;

import mSearch.daten.ListeFilme;
import mediathek.config.Daten;
import mediathek.javafx.tool.ComputedLabel;
import org.apache.commons.lang3.time.DurationFormatUtils;

import java.time.Duration;

/**
 * Label which will compute the age of the filmlist when updated.
 */
class FilmListAgeLabel extends ComputedLabel {
    private final Daten daten;

    FilmListAgeLabel(Daten daten) {
        super();
        this.daten = daten;
    }

    public void setAgeToLabel() {
      final ListeFilme listeFilme = daten.getListeFilme();

      setComputedText(computeAge(listeFilme.getAge()));
    }

    public String computeAge(long seconds) {
      Duration duration = Duration.ofSeconds(seconds);
      return String.format("Alter: %s", DurationFormatUtils.formatDuration(duration.toMillis(), "HH:mm:ss"));
    }
}
