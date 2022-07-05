package mediathek.mainwindow;

import mediathek.config.Daten;
import mediathek.daten.FilmListMetaData;

import javax.swing.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class FilmListCreationDateLabel extends JLabel implements PropertyChangeListener {
    public FilmListCreationDateLabel() {
        //works only on blacklist!
        Daten.getInstance().getListeFilmeNachBlackList().addMetaDataChangeListener(this);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        var metaData = (FilmListMetaData) evt.getNewValue();
        var text = String.format("Filmliste erstellt: %s Uhr", metaData.getGenerationDateTimeAsString());
        SwingUtilities.invokeLater(() -> setText(text));
    }
}
