package mediathek.mainwindow;

import mediathek.config.Daten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.time.Duration;
import java.util.IllegalFormatException;

public class FilmAgeLabel extends JLabel implements ActionListener {
    record FilmListAge(long hours, long minutes) {
    }

    private FilmListAge calculateFilmListAge() {
        var duration = Duration.ofSeconds(Daten.getInstance().getListeFilme().getMetaData().getAgeInSeconds());
        var minutes = duration.toMinutes();
        var hours = minutes / 60;
        minutes -= hours * 60;
        return new FilmListAge(hours, minutes);
    }

    private final Timer timer;
    private FilmListAge oldAge = new FilmListAge(0, 0);

    public FilmAgeLabel() {
        setToolTipText("Alter der Filmliste");

        setAgeToLabel();

        timer = new Timer(1000, this);
        timer.setRepeats(true);
        timer.start();
    }

    private String computeAgeString(@NotNull FilmListAge age) throws IllegalFormatException {
        if (age.hours == 0) {
            return String.format("Alter: %dm", age.minutes);
        } else {
            return String.format("Alter: %dh %dm", age.hours, age.minutes);
        }
    }

    private void setAgeToLabel() {
        var curAge = calculateFilmListAge();
        if (!curAge.equals(oldAge)) {
            var result = computeAgeString(curAge);
            setText(result);
            oldAge = curAge;
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        setAgeToLabel();
    }
}
