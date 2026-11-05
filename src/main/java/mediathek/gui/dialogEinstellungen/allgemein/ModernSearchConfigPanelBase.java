/*
 * Created by JFormDesigner on Fri Aug 05 11:18:25 CEST 2022
 */

package mediathek.gui.dialogEinstellungen.allgemein;

import com.jidesoft.swing.MultilineLabel;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

/**
 * JFormDesigner layout for the localized modern-search controls.
 *
 * @author Christian Franzke
 */
public class ModernSearchConfigPanelBase extends JPanel {
    /** Creates the localized controls using the current startup language. */
    public ModernSearchConfigPanelBase() {
        initComponents();
    }

    /** Initializes the controls and their resource-bundle labels. */
    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
        // Generated using JFormDesigner non-commercial license
        var multilineLabel1 = new MultilineLabel();
        cbActivateModernSearch = new JCheckBox();

        //======== this ========
        setLayout(new MigLayout(
            "insets 5,hidemode 3",
            // columns
            "[grow,fill]",
            // rows
            "[]" +
            "[]"));

        //---- multilineLabel1 ----
        multilineLabel1.setText(bundle.getString("settings.search.multilineLabel1.text"));
        add(multilineLabel1, "cell 0 0,grow");

        //---- cbActivateModernSearch ----
        cbActivateModernSearch.setText(bundle.getString("settings.search.cbActivateModernSearch.text"));
        add(cbActivateModernSearch, "cell 0 1");
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JCheckBox cbActivateModernSearch;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
