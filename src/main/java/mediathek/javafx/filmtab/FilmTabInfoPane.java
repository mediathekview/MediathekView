package mediathek.javafx.filmtab;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.GuiFilme;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;
import net.engio.mbassy.listener.Handler;

public class FilmTabInfoPane extends HBox {
    private final FilmTabInformationLabel infoLabel;
    private final FilmInfoLabel filmLabel;

    public FilmTabInfoPane(Daten daten, GuiFilme tabFilme) {
        super();
        infoLabel = new FilmTabInformationLabel(daten);
        filmLabel = new FilmInfoLabel(daten,tabFilme);

        getChildren().addAll(new CenteredBorderPane(filmLabel),
                new VerticalSeparator(),
                new CenteredBorderPane(infoLabel),
                new VerticalSeparator());

        if (isVisible())
            daten.getMessageBus().subscribe(this);

        visibleProperty().addListener(new ChangeListener<>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                if (newValue) {
                    daten.getMessageBus().subscribe(this);
                } else {
                    daten.getMessageBus().unsubscribe(this);
                }
            }
        });
    }

    private void updateLayout() {
        infoLabel.setInfoFilme();
        filmLabel.updateValues();
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        Platform.runLater(this::updateLayout);
    }


    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::updateLayout);
    }
}
