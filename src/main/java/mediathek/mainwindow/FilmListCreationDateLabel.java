package mediathek.mainwindow;

import mediathek.config.Daten;
import mediathek.daten.FilmListMetaData;
import mediathek.gui.messages.FilmListReadStopEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class FilmListCreationDateLabel extends JLabel implements PropertyChangeListener {
    public FilmListCreationDateLabel() {
        var blacklist = Daten.getInstance().getListeFilmeNachBlackList();
        setText(blacklist.getMetaData());
        //works only on blacklist!
        blacklist.addMetaDataChangeListener(this);

        MessageBus.getMessageBus().subscribe(this);
    }

    private void setText(@NotNull FilmListMetaData metaData) {
        var text = String.format("Filmliste erstellt: %s Uhr", metaData.getGenerationDateTimeAsString());
        SwingUtilities.invokeLater(() -> setText(text));
    }

    @Handler
    private void handleFilmListStop(@NotNull FilmListReadStopEvent e) {
        setText(Daten.getInstance().getListeFilmeNachBlackList().getMetaData());
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        var metaData = (FilmListMetaData) evt.getNewValue();
        setText(metaData);
    }
}
