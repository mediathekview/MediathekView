/*
 * Created by JFormDesigner on Fri Aug 05 11:18:25 CEST 2022
 */

package mediathek.gui.dialogEinstellungen.allgemein;

import com.jidesoft.swing.MultilineLabel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

/**
 * @author Christian Franzke
 */
public class ModernSearchConfigPanel extends JPanel {
    public ModernSearchConfigPanel() {
        initComponents();
    }

    public JCheckBox getCbActivateModernSearch() {
        return cbActivateModernSearch;
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        var multilineLabel1 = new MultilineLabel();
        cbActivateModernSearch = new JCheckBox();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                ));

        //---- multilineLabel1 ----
        multilineLabel1.setText("Mit der modernen Suche k\u00f6nnen komplexe Abfragen inkl. UND/ODER/NICHT Anteilen \u00fcber diverse Felder der Filmliste durchgef\u00fchrt werden. \nDie Bedienung ist komplett anders und deutlich komplexer als die bisher verwendete Suche und richtet sich daher nur an erfahrene Nutzer, die sich in die Materie einarbeiten wollen. Eine Suche im Internet zur Gestaltung der Abfragen ist unausweichlich.\nDie Lade-/Verarbeitungszeit der Filmliste erh\u00f6ht sich je nach verwendetem Rechner um ca. 10-30 Sekunden.\nEs ist nicht m\u00f6glich, die neue Suche und die alte parallel zu verwenden."); //NON-NLS
        add(multilineLabel1, new CC().cell(0, 0).grow());

        //---- cbActivateModernSearch ----
        cbActivateModernSearch.setText("<html>Moderne Suchfunktion aktivieren <b>(erfordert Neustart!)</b></html>"); //NON-NLS
        add(cbActivateModernSearch, new CC().cell(0, 1));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox cbActivateModernSearch;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
