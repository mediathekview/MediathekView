package mediathek.update;

import mediathek.config.Konstanten;
import net.miginfocom.swing.MigLayout;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.util.Objects;

public class UpdateNotificationPanel extends JPanel {
    private static final Logger logger = LogManager.getLogger();

    public UpdateNotificationPanel() {
        initComponents();
    }

    public JLabel getReleaseInfoLabel() {
        return lblReleaseInfo;
    }

    /**
     * custom initialization for JEditorPane and GUI designer.
     */
    private void createUIComponents() {
        try {
            webView = new JEditorPane(Objects.requireNonNull(Konstanten.WEBSITE_BASE_URL.resolve("changelogs")).toString());
        }
        catch (IOException e) {
            logger.error("Failed to load changelog from web");
            webView = new JEditorPane();
            webView.setText("<html><body>Load failed!</body></html>");
        }
    }

    private void initComponents() {
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
    private JLabel lblReleaseInfo;
    private JEditorPane webView;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
