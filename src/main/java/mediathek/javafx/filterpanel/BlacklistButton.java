package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.BlacklistChangedEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;

public class BlacklistButton extends Button {
    private final Image offImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-aus.png"),0,18,true,true);
    private final ImageView offImageView = new ImageView(offImage);
    private final Image onImage = new Image(getClass().getResourceAsStream("/mediathek/res/programm/button-blacklist-ein.png"),0,18,true,true);
    private final ImageView onImageView = new ImageView(onImage);
    private final BooleanProperty activeProperty = new SimpleBooleanProperty(false);
    private final Tooltip tooltipOn = new Tooltip("Blacklist ausschalten");
    private final Tooltip tooltipOff = new Tooltip("Blacklist einschalten");

    public BlacklistButton() {
        super();

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

        final var daten = Daten.getInstance();
        final var messageBus = daten.getMessageBus();
        messageBus.subscribe(this);

        activeProperty.setValue(isOn);
        activeProperty.addListener((observable, oldValue, newValue) -> SwingUtilities.invokeLater(() -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, Boolean.toString(newValue));
            daten.getListeBlacklist().filterListe();
            messageBus.publishAsync(new BlacklistChangedEvent());
        }));

        setOnAction(value -> activeProperty.setValue(!activeProperty.getValue()));
    }

    @Handler
    private void handleBlacklistChangedEvent(BlacklistChangedEvent e) {
        //config was changed outside
        final boolean on = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
        Platform.runLater(() -> activeProperty.setValue(on));
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
