package mediathek.gui.actions;

import mediathek.gui.statistics.FilmStatisticsDialog;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ShowFilmStatisticsAction extends AbstractAction {
    private final Frame owner;

    public ShowFilmStatisticsAction(@NotNull Frame owner) {
        this.owner = owner;
        putValue(Action.NAME, "Filmlisten-Statistik anzeigen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var dlg = new FilmStatisticsDialog(owner, this);
        dlg.setVisible(true);
    }
}
