/*
 * Created by JFormDesigner on Thu Mar 28 11:17:20 CET 2019
 */

package mediathek.gui.dialogEinstellungen;

import mediathek.gui.messages.NotificationCenterChangeEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

public class PanelNotifications extends JPanel {
    public PanelNotifications() {
        initComponents();

        final var config = ApplicationConfiguration.getConfiguration();
        final boolean showNotification = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,true);
        cbShowNotifications.setSelected(showNotification);
        cbShowNotifications.addActionListener(e -> {
            config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,cbShowNotifications.isSelected());
            MessageBus.getMessageBus().publishAsync(new NotificationCenterChangeEvent());
        });
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        cbShowNotifications = new JCheckBox();

        //======== this ========
        setLayout(new MigLayout(
            new LC().hideMode(3),
            // columns
            new AC()
                .fill().gap()
                .fill(),
            // rows
            new AC()
                ));

        //---- cbShowNotifications ----
        cbShowNotifications.setText("Benachrichtigungen anzeigen"); //NON-NLS
        cbShowNotifications.setToolTipText("Zeige Programminformationen als Benachrichtigungen in einem Popup-Fenster an."); //NON-NLS
        add(cbShowNotifications, new CC().cell(0, 0));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbShowNotifications;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
