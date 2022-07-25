/*
 * Created by JFormDesigner on Mon Jul 25 15:12:10 CEST 2022
 */

package mediathek.gui;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;
import java.awt.*;

/**
 * @author Christian Franzke
 */
public class AppShutdownWindow extends JWindow {
    public AppShutdownWindow(Window owner) {
        super(owner);
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        label1 = new JXBusyLabel();
        label2 = new JLabel();
        progressBar1 = new JProgressBar();

        //======== this ========
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().insets("20").hideMode(3), //NON-NLS
            // columns
            new AC()
                .fill().gap()
                .fill(),
            // rows
            new AC()
                .gap()
                ));

        //---- label1 ----
        label1.setVerticalAlignment(SwingConstants.TOP);
        contentPane.add(label1, new CC().cell(0, 0, 1, 2).alignY("top").growY(0)); //NON-NLS

        //---- label2 ----
        label2.setText("Progress text here..."); //NON-NLS
        contentPane.add(label2, new CC().cell(1, 0));
        contentPane.add(progressBar1, new CC().cell(1, 1).alignY("center").grow(100, 0).width("350:350:350")); //NON-NLS
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    public JXBusyLabel label1;
    public JLabel label2;
    public JProgressBar progressBar1;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
