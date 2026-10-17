/*
 * Created by JFormDesigner on Thu Mar 28 11:17:20 CET 2019
 */

package mediathek.gui.dialogEinstellungen;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

/** Settings controls using the selected interface-language bundle. */
public class PanelNotificationsBase extends JPanel {
    public PanelNotificationsBase() {
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
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
        cbShowNotifications.setText(bundle.getString("settings.PanelNotifications.cbShowNotifications.text")); //NON-NLS
        cbShowNotifications.setToolTipText(bundle.getString("settings.PanelNotifications.cbShowNotifications.toolTipText")); //NON-NLS
        add(cbShowNotifications, new CC().cell(0, 0));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JCheckBox cbShowNotifications;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
