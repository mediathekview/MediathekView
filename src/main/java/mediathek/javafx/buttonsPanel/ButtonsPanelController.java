package mediathek.javafx.buttonsPanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.TilePane;
import mediathek.config.Daten;
import mediathek.daten.DatenPset;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ButtonsPanelController {
    private final GuiFilme guiFilme;
    private final TilePane tilePane = new TilePane();
    private final Tab buttonsTab = new Tab("Buttons");

    public ButtonsPanelController(@NotNull GuiFilme guiFilme, @NotNull JFXPanel parent) {
        this.guiFilme = guiFilme;

        tilePane.setPadding(new Insets(5, 5, 5, 5));
        tilePane.setVgap(5d);
        tilePane.setHgap(5d);

        ScrollPane sp = new ScrollPane();
        sp.setFitToHeight(true);
        sp.setFitToWidth(true);
        sp.setContent(tilePane);
        buttonsTab.setContent(sp);

        TabPane tabPane = new TabPane();
        tabPane.setPrefHeight(110d);
        tabPane.getTabs().add(buttonsTab);

        parent.setScene(new Scene(tabPane));
        MessageBus.getMessageBus().subscribe(this);
    }

    public void setOnCloseRequest(EventHandler<Event> e) {
        buttonsTab.setOnCloseRequest(e);
    }

    public void setupButtonLayout() {
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
                        b.setBackground(new Background(new BackgroundFill(JavaFxUtils.toFXColor(psetColor), null, null)));
                    b.setOnAction(e -> {
                        System.out.println("EXECUTING PSET BUTTON");
                        SwingUtilities.invokeLater(() -> guiFilme.playerStarten(pset));
                    });
                    children.add(b);
                }
            } else {
                children.add(new Label(""));
            }
        }
    }

    @Handler
    private void handleProgramSetChangedEvent(ProgramSetChangedEvent e) {
        Platform.runLater(this::setupButtonLayout);
    }
}
