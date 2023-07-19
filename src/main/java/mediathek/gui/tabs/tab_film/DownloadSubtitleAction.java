package mediathek.gui.tabs.tab_film;

import mediathek.config.Konstanten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;
import mediathek.tool.MVSubtitle;
import mediathek.tool.SwingErrorDialog;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class DownloadSubtitleAction extends AbstractAction {
    private final GuiFilme guiFilme;

    public DownloadSubtitleAction(GuiFilme guiFilme) {
        this.guiFilme = guiFilme;
        putValue(Action.NAME, "Untertitel-Datei sofort laden...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiFilme.getCurrentlySelectedFilm().ifPresent(film -> {
            var selectedFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Untertitel speichern", "");
            if (selectedFile != null) {
                try {
                    MVSubtitle subtitleFile = new MVSubtitle();
                    subtitleFile.writeSubtitle(film.getSubtitleUrl(), selectedFile);
                    JOptionPane.showMessageDialog(MediathekGui.ui(), "Untertitel wurde erfolgreich geladen.",
                            Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
                } catch (Exception ex) {
                    SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                            "Untertitel konnte nicht geladen werden.", ex);
                }
            }
            else
                JOptionPane.showMessageDialog(MediathekGui.ui(), "Vorgang wurde abgebrochen.",
                        Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
        });
    }
}
