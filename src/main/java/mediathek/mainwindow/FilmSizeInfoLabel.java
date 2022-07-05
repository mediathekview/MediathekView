package mediathek.mainwindow;

import mediathek.config.Daten;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class FilmSizeInfoLabel extends JLabel implements ActionListener {
    private int oldGesamt;
    private int oldRowCount;
    private final MediathekGui mediathekGui;

    private final Timer timer;

    public FilmSizeInfoLabel(@NotNull MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;

        timer = new Timer(1000, this);
        timer.setRepeats(true);
        timer.start();

        MessageBus.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        SwingUtilities.invokeLater(this::updateValues);
    }

    private void updateValues() {
        String textLinks;
        final int gesamt = Daten.getInstance().getListeFilme().size();
        final int rowCount = mediathekGui.tabFilme.getTableRowCount();

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

    private String createFilmLabel(final int rowCount) {
        String textLinks;
        if (rowCount == 1)
            textLinks = "1 Film";
        else
            textLinks = rowCount + " Filme";

        return textLinks;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        updateValues();
    }
}
