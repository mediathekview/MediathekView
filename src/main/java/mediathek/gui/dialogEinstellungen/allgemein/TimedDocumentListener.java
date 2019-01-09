package mediathek.gui.dialogEinstellungen.allgemein;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.event.ActionListener;

/**
 * Listens to changes in a Document and finally fires the assigned Action.
 * All operations will be performed on Swing EDT.
 */
public final class TimedDocumentListener implements DocumentListener {
    private final Timer timer;

    public TimedDocumentListener(ActionListener taskPerformer) {
        timer = new Timer(1_000, taskPerformer);
        timer.setRepeats(false);
    }

    private void fireTimer() {
        if (timer.isRunning())
            timer.restart();
        else
            timer.start();
    }

    @Override
    public void insertUpdate(DocumentEvent e) {
        fireTimer();
    }

    @Override
    public void removeUpdate(DocumentEvent e) {
        fireTimer();
    }

    @Override
    public void changedUpdate(DocumentEvent e) {
        fireTimer();
    }
}
