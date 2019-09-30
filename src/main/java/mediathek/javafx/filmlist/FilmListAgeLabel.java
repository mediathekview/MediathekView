package mediathek.javafx.filmlist;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.util.Duration;
import mediathek.config.Daten;
import mediathek.javafx.tool.ComputedLabel;
import mediathek.mainwindow.MediathekGui;
import org.apache.commons.lang3.time.DurationFormatUtils;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;


/**
 * Label which will compute the age of the filmlist when updated.
 * Update cycle one second.
 */
class FilmListAgeLabel extends ComputedLabel {
    private Timeline timeline;

    FilmListAgeLabel() {
        setupTimer();
        installWindowListener();
    }

    private void installWindowListener() {
        MediathekGui.ui().addWindowListener(new WindowAdapter() {
            @Override
            public void windowActivated(WindowEvent e) {
                timeline.play();
            }

            @Override
            public void windowDeactivated(WindowEvent e) {
                timeline.pause();
            }
        });
    }

    private void setupTimer() {
        timeline = new Timeline(new KeyFrame(Duration.millis(1000d), ae -> setAgeToLabel()));
        timeline.setCycleCount(Animation.INDEFINITE);
        timeline.play();
    }

    private void setAgeToLabel() {
      setComputedText(computeAge(Daten.getInstance().getListeFilme().getAge()));
    }

    private String computeAge(long seconds) {
      var duration = java.time.Duration.ofSeconds(seconds);
      return String.format("Alter: %s", DurationFormatUtils.formatDuration(duration.toMillis(), "HH:mm:ss"));
    }
}
