package mediathek.gui.tabs.tab_film;

import javafx.embed.swing.SwingFXUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;

import javax.swing.*;
import java.net.URL;
import java.util.ResourceBundle;

public class SaveDownloadController implements Initializable {
    private final JDialog dialog;
    @FXML
    public Button saveBtn;
    @FXML
    public Button cancelBtn;
    @FXML
    public Label lblThema;
    @FXML
    public Label lblTitle;
    @FXML
    ImageView ivSender;

    private boolean success;
    private final DatenFilm film;
    private final DatenPset pSet;

    public SaveDownloadController(JDialog dialog, DatenFilm film, DatenPset pSet) {
        this.dialog = dialog;
        this.film = film;
        this.pSet = pSet;
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        saveBtn.setOnAction(e -> {
            success = true;
            dialog.dispose();
        });

        cancelBtn.setOnAction(e -> {
            success = false;
            dialog.dispose();
        });

        lblThema.setText(film.getThema());
        lblTitle.setText(film.getTitle());
        var icn = MVSenderIconCache.get(film.getSender(), false);
        icn.ifPresentOrElse(icon -> {
            var image = SwingFXUtils.toFXImage(JavaFxUtils.toBufferedImage(icon),null);
            ivSender.setImage(image);
        }, () -> ivSender.setImage(null));
    }

    public boolean success() {
        return success;
    }
}
