package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Label;
import mediathek.config.Daten;
import mediathek.daten.DatenAbo;
import mediathek.gui.messages.TimerEvent;
import net.engio.mbassy.listener.Handler;

public class AboTabInformationLabel extends Label {
    private static final String TRENNER = "  ||  ";
    private final Daten daten;

    public AboTabInformationLabel(Daten daten) {
        super();
        this.daten = daten;

        daten.getMessageBus().subscribe(this);
        setText("0 Abos");
    }

    private void setInfoAbo() {
        String textLinks;
        int ein = 0;
        int aus = 0;

        final int gesamt = daten.getListeAbo().size();

        for (DatenAbo abo : daten.getListeAbo()) {
            if (abo.aboIstEingeschaltet()) {
                ++ein;
            } else {
                ++aus;
            }
        }
        if (gesamt == 1) {
            textLinks = "1 Abo";
        } else {
            textLinks = gesamt + " Abos";
        }
        textLinks += TRENNER + ein + " eingeschaltet, " + aus + " ausgeschaltet";

        setText(textLinks);
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::setInfoAbo);
    }
}
