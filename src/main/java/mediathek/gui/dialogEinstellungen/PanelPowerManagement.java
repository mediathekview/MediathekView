/*
 * Created by JFormDesigner on Tue Dec 03 21:03:54 CET 2019
 */

package mediathek.gui.dialogEinstellungen;

import mediathek.tool.ApplicationConfiguration;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

public class PanelPowerManagement extends JPanel {
    public PanelPowerManagement() {
        initComponents();

        var config = ApplicationConfiguration.getConfiguration();
        final boolean enablePowerManagement = config.getBoolean(ApplicationConfiguration.UI_FILMLIST_LABEL_ENABLE_POWERMANAGEMENT, false);
        cbUpdateFilmlistAge.setSelected(enablePowerManagement);
        cbUpdateFilmlistAge.addActionListener(l -> config.setProperty(ApplicationConfiguration.UI_FILMLIST_LABEL_ENABLE_POWERMANAGEMENT, cbUpdateFilmlistAge.isSelected()));
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        var panel1 = new JPanel();
        cbUpdateFilmlistAge = new JCheckBox();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("0").hideMode(3), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                ));

        //======== panel1 ========
        {
            panel1.setBorder(new TitledBorder("Power Management (erfordert Neustart!)")); //NON-NLS
            panel1.setLayout(new BorderLayout());

            //---- cbUpdateFilmlistAge ----
            cbUpdateFilmlistAge.setText("Aktualisierung Alter Filmliste bei inaktivem Programmfenster deaktivieren"); //NON-NLS
            panel1.add(cbUpdateFilmlistAge, BorderLayout.CENTER);
        }
        add(panel1, new CC().cell(0, 0));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbUpdateFilmlistAge;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
