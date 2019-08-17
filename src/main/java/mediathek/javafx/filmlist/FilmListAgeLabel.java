package mediathek.javafx.filmlist;

import javafx.application.Platform;
import mediathek.config.Daten;
import mediathek.daten.ListeFilme;
import mediathek.gui.messages.TimerEvent;
import mediathek.javafx.tool.ComputedLabel;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.time.DurationFormatUtils;

import java.time.Duration;

/**
 * Label which will compute the age of the filmlist when updated.
 * Update cycle one second.
 */
class FilmListAgeLabel extends ComputedLabel {
    private final Daten daten;

    FilmListAgeLabel(Daten daten) {
        super();
        this.daten = daten;

        daten.getMessageBus().subscribe(this);
    }

    private void setAgeToLabel() {
      final ListeFilme listeFilme = daten.getListeFilme();

      setComputedText(computeAge(listeFilme.getAge()));
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::setAgeToLabel);
    }

    private String computeAge(long seconds) {
      Duration duration = Duration.ofSeconds(seconds);
      return String.format("Alter: %s", DurationFormatUtils.formatDuration(duration.toMillis(), "HH:mm:ss"));
    }
}
