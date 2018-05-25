package mediathek.update;

import javafx.embed.swing.JFXPanel;
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
        JLabel label1 = new JLabel();
        JLabel lblAppIcon = new JLabel();
        lblReleaseInfo = new JLabel();
        JLabel label4 = new JLabel();
        fxPanel = new JFXPanel();

        //======== this ========
        setLayout(new MigLayout(
                "hidemode 3",
                // columns
                "[fill]" +
                        "[grow,fill]",
                // rows
                "[]" +
                        "[]" +
                        "[]" +
                        "[grow]"));

        //---- label1 ----
        label1.setText("Eine neue Version von MediathekView ist verf\u00fcgbar!");
        label1.setFont(label1.getFont().deriveFont(label1.getFont().getStyle() | Font.BOLD, label1.getFont().getSize() + 5f));
        add(label1, "cell 1 0,aligny bottom,growy 0");

        //---- lblAppIcon ----
        lblAppIcon.setIcon(new ImageIcon("/Users/christianfranzke/development/IntelliJ Projekte/MediathekView/src/main/resources/mediathek/res/MediathekView_k.png"));
        add(lblAppIcon, "cell 0 0 1 2");

        //---- lblReleaseInfo ----
        lblReleaseInfo.setText("text");
        lblReleaseInfo.setFont(lblReleaseInfo.getFont().deriveFont(lblReleaseInfo.getFont().getSize() - 1f));
        add(lblReleaseInfo, "cell 1 1,aligny top,growy 0");

        //---- label4 ----
        label4.setText("Release Notes:");
        label4.setFont(label4.getFont().deriveFont(label4.getFont().getStyle() | Font.BOLD, label4.getFont().getSize() - 2f));
        add(label4, "cell 1 2");

        //---- fxPanel ----
        fxPanel.setPreferredSize(new Dimension(480, 240));
        add(fxPanel, "cell 1 3,grow");
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel lblReleaseInfo;
    private JFXPanel fxPanel;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
