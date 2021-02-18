package mediathek.gui.tabs.tab_film;

import javafx.embed.swing.SwingFXUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.image.ImageView;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.net.URL;
import java.util.ResourceBundle;

public class SaveDownloadController implements Initializable {
    private final JDialog dialog;
    @FXML
    private Button saveBtn;
    @FXML
    private Button cancelBtn;
    @FXML
    private Label lblThema;
    @FXML
    private Label lblTitle;
    @FXML
    ImageView ivSender;
    @FXML
    ComboBox<String> cBxPSet;

    private boolean success;
    private final DatenFilm film;
    private final DatenPset pSet;

    public SaveDownloadController(@NotNull JDialog dialog, @NotNull DatenFilm film, @Nullable DatenPset pSet) {
        this.dialog = dialog;
        this.film = film;
        this.pSet = pSet;
    }

    private void setupButtonBarActions() {
        saveBtn.setOnAction(e -> {
            success = true;
            dialog.dispose();
        });

        cancelBtn.setOnAction(e -> {
            success = false;
            dialog.dispose();
        });
    }

    private void setupSenderLogo() {
        var icn = MVSenderIconCache.get(film.getSender(), false);
        icn.ifPresentOrElse(icon -> {
            var image = SwingFXUtils.toFXImage(JavaFxUtils.toBufferedImage(icon),null);
            ivSender.setImage(image);
        }, () -> ivSender.setImage(null));
    }

    private void setupProgramSetComboBox() {
        var saveOnlyPSets = Daten.listePset.getListeSpeichern();
        for (var pSet : saveOnlyPSets) {
            cBxPSet.getItems().add(pSet.arr[DatenPset.PROGRAMMSET_NAME]);
        }

        if (pSet != null) {
            cBxPSet.getSelectionModel().select(pSet.arr[DatenPset.PROGRAMMSET_NAME]);
        }
        else
            cBxPSet.getSelectionModel().selectFirst();
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        setupButtonBarActions();
        setupSenderLogo();

        lblThema.setText(film.getThema());
        lblTitle.setText(film.getTitle());

        setupProgramSetComboBox();
    }

    public boolean success() {
        return success;
    }
}
