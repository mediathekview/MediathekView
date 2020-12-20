package mediathek.javafx.buttonsPanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.TilePane;
import mediathek.config.Daten;
import mediathek.daten.DatenPset;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.javafx.tool.JavaFxUtils;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class ButtonsPanelController implements Initializable {
    private static final Logger logger = LogManager.getLogger();
    @FXML
    private Tab buttonsTab;
    @FXML
    private TilePane tilePane;

    public void setGuiFilme(GuiFilme guiFilme) {
        this.guiFilme = guiFilme;
    }

    private GuiFilme guiFilme;

    public static ButtonsPanelController install(JFXPanel parent, GuiFilme guiFilme) throws IOException {
        logger.trace("install");

        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(ButtonsPanelController.class.getResource("/mediathek/res/programm/fxml/pset_buttons.fxml"));

        TabPane buttonsPane = loader.load();
        final ButtonsPanelController buttonsController = loader.getController();
        buttonsController.setGuiFilme(guiFilme);
        parent.setScene(new Scene(buttonsPane));

        return buttonsController;
    }

    public void setOnCloseRequest(EventHandler<Event> e) {
        buttonsTab.setOnCloseRequest(e);
    }

    public void setupButtonLayout() {
        logger.trace("setupButtonLayout called");

        tilePane.getChildren().clear();
        var listeButton = Daten.listePset.getListeButton();
        final var children = tilePane.getChildren();
        for (var pset : listeButton) {
            final var psetName = pset.arr[DatenPset.PROGRAMMSET_NAME];
            final var psetColor = pset.getFarbe();
            if (!pset.isFreeLine()) {
                if (pset.isLabel()) {
                    var l = new Label(psetName);
                    if (psetColor != null)
                        l.setTextFill(JavaFxUtils.toFXColor(psetColor));
                    children.add(l);
                } else {
                    var b = new Button(psetName);
                    if (psetColor != null)
                        b.setBackground(new Background(new BackgroundFill(JavaFxUtils.toFXColor(psetColor),null,null)));
                    b.setOnAction(e -> {
                        System.out.println("EXECUTING PSET BUTTON");
                        SwingUtilities.invokeLater(() -> guiFilme.playerStarten(pset));
                    });
                    children.add(b);
                }
            }
            else {
                children.add(new Label(""));
            }
        }
    }

    @Handler
    private void handleProgramSetChangedEvent(ProgramSetChangedEvent e) {
        Platform.runLater(this::setupButtonLayout);
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        Daten.getInstance().getMessageBus().subscribe(this);
    }
}
