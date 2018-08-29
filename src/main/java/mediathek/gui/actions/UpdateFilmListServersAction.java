package mediathek.gui.actions;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import mSearch.filmlisten.FilmlistenSuchen;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.filmlisten.FilmeLaden;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * "Force" update the list of filmlist servers.
 */
public class UpdateFilmListServersAction extends AbstractAction {
    public UpdateFilmListServersAction() {
        super();
        putValue(NAME, "Update-Server aktualisieren");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final FilmeLaden filmeLaden = Daten.getInstance().getFilmeLaden();
        final FilmlistenSuchen list = filmeLaden.getFilmlistenSuchen();

        filmeLaden.getDownloadUrlsFilmlisten_akt().clear();
        filmeLaden.getDownloadUrlsFilmlisten_diff().clear();

        list.updateURLsFilmlisten(true);
        list.updateURLsFilmlisten(false);

        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText("Update-Server aktualisieren");
            alert.setContentText("Aktualisierung wurde durchgeführt.");
            alert.showAndWait();
        });
    }
}
