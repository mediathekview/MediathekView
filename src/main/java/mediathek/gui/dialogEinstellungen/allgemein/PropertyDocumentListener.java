package mediathek.gui.dialogEinstellungen.allgemein;

import javafx.animation.PauseTransition;
import javafx.util.Duration;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * This will fire a transition action after 1 second delay, even if several calls were made.
 */
public class PropertyDocumentListener implements DocumentListener {
    protected final PauseTransition savePropertyTransition = new PauseTransition(Duration.millis(1000));

    @Override
    public void insertUpdate(DocumentEvent e) {
        savePropertyTransition.playFromStart();
    }

    @Override
    public void removeUpdate(DocumentEvent e) {
        savePropertyTransition.playFromStart();
    }

    @Override
    public void changedUpdate(DocumentEvent e) {
        savePropertyTransition.playFromStart();
    }
}
