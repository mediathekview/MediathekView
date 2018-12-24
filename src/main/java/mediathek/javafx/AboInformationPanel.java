package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import net.engio.mbassy.listener.Handler;


public class AboInformationPanel extends HBox {
    private final Daten daten;
    private final Label gesamtLabel = new Label();
    private final Label onOffLabel = new Label();

    public AboInformationPanel(Daten daten) {
        super();
        this.daten = daten;

        getChildren().addAll(gesamtLabel,
                new VerticalSeparator(),
                onOffLabel);

        gesamtLabel.setTooltip(new Tooltip("Anzahl aller vorhandenen Abos"));
        daten.getMessageBus().subscribe(this);
    }

    private void setInfoAbo() {
        final var listeAbo = daten.getListeAbo();

        final int gesamt = listeAbo.size();
        final String text;
        if (gesamt == 1) {
            text = "1 Abo";
        } else {
            text = gesamt + " Abos";
        }
        gesamtLabel.setText(text);

        onOffLabel.setText(listeAbo.activeAbos() + " eingeschaltet, " + listeAbo.inactiveAbos() + " ausgeschaltet");
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::setInfoAbo);
    }
}
