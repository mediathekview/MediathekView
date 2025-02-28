package mediathek.gui.actions;

import mediathek.gui.duplicates.statistics.DuplicateStatisticsDialog;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ShowDuplicateStatisticsAction extends AbstractAction {
    private final Frame owner;

    public ShowDuplicateStatisticsAction(@NotNull Frame owner) {
        this.owner = owner;
        putValue(Action.NAME, "Film-Statistik anzeigen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var dlg = new DuplicateStatisticsDialog(owner, this);
        dlg.setVisible(true);
    }
}
