package mediathek.update;

import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("DataFlowIssue")
public class UpdateNotificationPanelBase extends JPanel {
    protected void createUIComponents() {
        webView = new JEditorPane();
    }

    protected void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        createUIComponents();

        var label1 = new JLabel();
        var lblAppIcon = new JLabel();
        lblReleaseInfo = new JLabel();
        var label4 = new JLabel();
        var scrollPane1 = new JScrollPane();

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
        lblAppIcon.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView_k.png")));
        add(lblAppIcon, "cell 0 0 1 2");

        //---- lblReleaseInfo ----
        lblReleaseInfo.setText("text");
        lblReleaseInfo.setFont(lblReleaseInfo.getFont().deriveFont(lblReleaseInfo.getFont().getSize() - 1f));
        add(lblReleaseInfo, "cell 1 1,aligny top,growy 0");

        //---- label4 ----
        label4.setText("Release Notes:");
        label4.setFont(label4.getFont().deriveFont(label4.getFont().getStyle() | Font.BOLD));
        add(label4, "cell 1 2");

        //======== scrollPane1 ========
        {

            //---- webView ----
            webView.setPreferredSize(new Dimension(480, 240));
            webView.setEditable(false);
            webView.setContentType("text/html");
            scrollPane1.setViewportView(webView);
        }
        add(scrollPane1, "cell 1 3,grow");
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JLabel lblReleaseInfo;
    protected JEditorPane webView;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
