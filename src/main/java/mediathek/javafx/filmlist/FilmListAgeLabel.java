package mediathek.javafx.filmlist;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.javafx.tool.ComputedLabel;
import org.apache.commons.lang3.time.DurationFormatUtils;


/**
 * Label which will compute the age of the filmlist when updated.
 * Update cycle one second.
 */
public class FilmListAgeLabel extends ComputedLabel {
    private Timeline timeline;

    FilmListAgeLabel() {
        setupTimer();
    }

    public void enableTimer() {
        timeline.play();
    }

    public void disableTimer() {
        timeline.pause();
    }

    private void setupTimer() {
        timeline = new Timeline(new KeyFrame(Duration.millis(1000d), ae -> setAgeToLabel()));
        timeline.setCycleCount(Animation.INDEFINITE);
        timeline.play();
    }

    private void setAgeToLabel() {
      setComputedText(computeAge(Daten.getInstance().getListeFilme().metaData().getAge()));
    }

    private String computeAge(long seconds) {
      var duration = java.time.Duration.ofSeconds(seconds);
      return String.format("Alter: %s", DurationFormatUtils.formatDuration(duration.toMillis(), "HH:mm:ss"));
    }
}
