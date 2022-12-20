package mediathek.javafx;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;

/**
 * This will display a JPanel with a indefinite progress indicator and some status
 * messages used as a glass pane overlay during app termination.
 */
public class AppTerminationIndefiniteProgress extends JPanel {
    private final JLabel lblMessage = new JLabel("Warte auf Abschluss der Downloads...");

    public AppTerminationIndefiniteProgress(boolean willbeShutDown) {
        super();

        setLayout(new MigLayout(new LC().hideMode(3),
                new AC().fill().fill(),new AC()));

        var busyLabel = new JXBusyLabel();
        add(busyLabel, new CC().cell(0,0).span(1,3));
        busyLabel.setBusy(true);

        add(lblMessage, new CC().cell(1,0));
        if (willbeShutDown) {
            add(new JLabel("Der Rechner wird danach heruntergefahren."), new CC().cell(1,1));
        }
        add(new JLabel("Sie können den Vorgang mit Escape abbrechen."), new CC().cell(1,2));
    }

    public void setMessage(String text) {
        SwingUtilities.invokeLater(() -> lblMessage.setText(text));
    }
}
