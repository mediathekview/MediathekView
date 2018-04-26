package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;

import javax.swing.*;

public class BlacklistButton extends Button {
    private final Image offImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-aus.png"));
    private final ImageView offImageView = new ImageView(offImage);
    private final Image onImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-ein.png"));
    private final ImageView onImageView = new ImageView(onImage);
    private final BooleanProperty activeProperty = new SimpleBooleanProperty(false);
    private final Tooltip tooltipOn = new Tooltip("Blacklist ausschalten");
    private final Tooltip tooltipOff = new Tooltip("Blacklist einschalten");
    private final Daten daten;


    public BlacklistButton(Daten daten) {
        super("");
        this.daten = daten;

        final boolean isOn = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
        if (isOn)
            setupOn();
        else
            setupOff();

        //set initial state
        activeProperty.addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                setupOn();
            } else {
                setupOff();
            }
        });

        activeProperty.setValue(isOn);
        activeProperty.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(() -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(newValue));
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, BlacklistButton.class.getSimpleName());
        }));

        setOnAction(value -> activeProperty.setValue(!activeProperty.getValue()));

        Listener.addListener(new Listener(Listener.EREIGNIS_BLACKLIST_GEAENDERT, BlacklistButton.class.getSimpleName()) {
            @Override
            public void ping() {
                //config was changed outside
                final boolean on = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
                Platform.runLater(() -> {
                    activeProperty.setValue(on);
                });
            }
        });

    }

    private void setupOn() {
        setGraphic(onImageView);
        setTooltip(tooltipOn);
    }

    private void setupOff() {
        setGraphic(offImageView);
        setTooltip(tooltipOff);
    }
}
