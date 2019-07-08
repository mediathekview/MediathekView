package mediathek.update;

import javafx.embed.swing.JFXPanel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class UpdateNotificationPanel extends JPanel {
    public UpdateNotificationPanel() {
        initComponents();
    }

    public JLabel getReleaseInfoLabel() {
        return lblReleaseInfo;
    }

    public JFXPanel getFxPanel() {
        return fxPanel;
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        var label1 = new JLabel();
        var lblAppIcon = new JLabel();
        lblReleaseInfo = new JLabel();
        var label4 = new JLabel();
        fxPanel = new JFXPanel();

        //======== this ========
        setLayout(new MigLayout(
            new LC().hideMode(3),
            // columns
            new AC()
                .fill().gap()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .grow()));

        //---- label1 ----
        label1.setText("Eine neue Version von MediathekView ist verf\u00fcgbar!"); //NON-NLS
        label1.setFont(label1.getFont().deriveFont(label1.getFont().getStyle() | Font.BOLD, label1.getFont().getSize() + 5f));
        add(label1, new CC().cell(1, 0).alignY("bottom").growY(0)); //NON-NLS

        //---- lblAppIcon ----
        lblAppIcon.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView_k.png"))); //NON-NLS
        add(lblAppIcon, new CC().cell(0, 0, 1, 2));

        //---- lblReleaseInfo ----
        lblReleaseInfo.setText("text"); //NON-NLS
        lblReleaseInfo.setFont(lblReleaseInfo.getFont().deriveFont(lblReleaseInfo.getFont().getSize() - 1f));
        add(lblReleaseInfo, new CC().cell(1, 1).alignY("top").growY(0)); //NON-NLS

        //---- label4 ----
        label4.setText("Release Notes:"); //NON-NLS
        label4.setFont(label4.getFont().deriveFont(label4.getFont().getStyle() | Font.BOLD, label4.getFont().getSize() - 2f));
        add(label4, new CC().cell(1, 2));

        //---- fxPanel ----
        fxPanel.setPreferredSize(new Dimension(480, 240));
        add(fxPanel, new CC().cell(1, 3).grow());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel lblReleaseInfo;
    private JFXPanel fxPanel;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
