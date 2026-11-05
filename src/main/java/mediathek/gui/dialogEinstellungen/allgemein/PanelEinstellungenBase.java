package mediathek.gui.dialogEinstellungen.allgemein;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTitledPanel;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/** JFormDesigner layout whose labels use the interface language selected at startup. */
public class PanelEinstellungenBase extends JPanel {
    /** Creates the localized controls using the current startup language. */
    public PanelEinstellungenBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    /** Initializes the controls and their resource-bundle labels. */
    private void initComponents() {
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
        var jPanel5 = new JPanel();
        jCheckBoxTabsTop = new JCheckBox();
        jCheckBoxTabIcon = new JCheckBox();
        cbAutomaticMenuTabSwitching = new JCheckBox();
        cbRestoreSelectedTab = new JCheckBox();
        var jPanel3 = new JPanel();
        var jLabel3 = new JLabel();
        jtfUserAgent = new JTextField();
        var jPanel4 = new JPanel();
        var jLabel4 = new JLabel();
        jtfProxyHost = new JTextField();
        var jLabel5 = new JLabel();
        jtfProxyPort = new JTextField();
        var jLabel7 = new JLabel();
        jtfProxyUser = new JTextField();
        var jLabel8 = new JLabel();
        jpfProxyPassword = new JPasswordField();
        jButtonApplyProxySettings = new JButton();
        var panel1 = new JPanel();
        jCheckBoxTray = new JCheckBox();
        cbShowBlacklistIconWithText = new JCheckBox();
        cbUseWikipediaSenderLogos = new JCheckBox();
        cbAutomaticUpdateChecks = new JCheckBox();
        cbDrawListIconsRight = new JCheckBox();
        cbUseSystemDarkMode = new JCheckBox();
        cbTabFilmeTimeUseLongFormat = new JCheckBox();
        var languageLabel = new JLabel();
        languageComboBox = new JComboBox<>();
        var languageRestartLabel = new JLabel();
        modernSearchTitlePanel = new JXTitledPanel();

        //======== this ========
        setMaximumSize(new Dimension(10, 10));

        //======== jPanel5 ========
        {
            jPanel5.setBorder(new TitledBorder(bundle.getString("settings.general.jPanel5.borderTitle")));
            jPanel5.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .gap()
                    .fill()));

            //---- jCheckBoxTabsTop ----
            jCheckBoxTabsTop.setText(bundle.getString("settings.general.jCheckBoxTabsTop.text"));
            jPanel5.add(jCheckBoxTabsTop, new CC().cell(0, 0));

            //---- jCheckBoxTabIcon ----
            jCheckBoxTabIcon.setText(bundle.getString("settings.general.jCheckBoxTabIcon.text"));
            jCheckBoxTabIcon.setToolTipText(bundle.getString("settings.general.jCheckBoxTabIcon.toolTipText"));
            jPanel5.add(jCheckBoxTabIcon, new CC().cell(1, 0));

            //---- cbAutomaticMenuTabSwitching ----
            cbAutomaticMenuTabSwitching.setText(bundle.getString("settings.general.cbAutomaticMenuTabSwitching.text"));
            jPanel5.add(cbAutomaticMenuTabSwitching, new CC().cell(2, 0));

            //---- cbRestoreSelectedTab ----
            cbRestoreSelectedTab.setText(bundle.getString("settings.general.cbRestoreSelectedTab.text"));
            cbRestoreSelectedTab.setToolTipText(bundle.getString("settings.general.cbRestoreSelectedTab.toolTipText"));
            jPanel5.add(cbRestoreSelectedTab, new CC().cell(0, 1, 3, 1));
        }

        //======== jPanel3 ========
        {
            jPanel3.setBorder(new TitledBorder(bundle.getString("settings.general.jPanel3.borderTitle")));

            //---- jLabel3 ----
            jLabel3.setText(bundle.getString("settings.general.jLabel3.text"));

            //---- jtfUserAgent ----
            jtfUserAgent.setMinimumSize(new Dimension(200, 26));
            jtfUserAgent.setPreferredSize(new Dimension(520, 26));

            GroupLayout jPanel3Layout = new GroupLayout(jPanel3);
            jPanel3.setLayout(jPanel3Layout);
            jPanel3Layout.setHorizontalGroup(
                jPanel3Layout.createParallelGroup()
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jLabel3)
                        .addGap(5, 5, 5)
                        .addComponent(jtfUserAgent, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
            jPanel3Layout.setVerticalGroup(
                jPanel3Layout.createParallelGroup()
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addComponent(jLabel3))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jtfUserAgent, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
            );
        }

        //======== jPanel4 ========
        {
            jPanel4.setBorder(new TitledBorder(bundle.getString("settings.general.jPanel4.borderTitle")));
            jPanel4.setToolTipText("");

            //---- jLabel4 ----
            jLabel4.setText(bundle.getString("settings.general.jLabel4.text"));

            //---- jLabel5 ----
            jLabel5.setText(bundle.getString("settings.general.jLabel5.text"));

            //---- jLabel7 ----
            jLabel7.setText(bundle.getString("settings.general.jLabel7.text"));

            //---- jLabel8 ----
            jLabel8.setText(bundle.getString("settings.general.jLabel8.text"));

            //---- jButtonApplyProxySettings ----
            jButtonApplyProxySettings.setText(bundle.getString("settings.general.jButtonApplyProxySettings.text"));

            GroupLayout jPanel4Layout = new GroupLayout(jPanel4);
            jPanel4.setLayout(jPanel4Layout);
            jPanel4Layout.setHorizontalGroup(
                jPanel4Layout.createParallelGroup()
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jLabel4)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jtfProxyHost, GroupLayout.PREFERRED_SIZE, 250, GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jLabel7)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jtfProxyUser)))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabel8)
                            .addComponent(jLabel5))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup()
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jtfProxyPort, GroupLayout.PREFERRED_SIZE, 72, GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 187, Short.MAX_VALUE))
                            .addComponent(jpfProxyPassword))
                        .addContainerGap())
                    .addGroup(GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonApplyProxySettings)
                        .addContainerGap())
            );
            jPanel4Layout.setVerticalGroup(
                jPanel4Layout.createParallelGroup()
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel4)
                            .addComponent(jtfProxyHost, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel5)
                            .addComponent(jtfProxyPort, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel7)
                            .addComponent(jtfProxyUser, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel8)
                            .addComponent(jpfProxyPassword, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonApplyProxySettings)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("5", "0"),
                // columns
                new AC()
                    .grow().align("left").gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .gap()
                    .fill().gap()
                    .gap()
                    .gap()
                    ));

            //---- jCheckBoxTray ----
            jCheckBoxTray.setText(bundle.getString("settings.general.jCheckBoxTray.text"));
            panel1.add(jCheckBoxTray, new CC().cell(0, 0));

            //---- cbShowBlacklistIconWithText ----
            cbShowBlacklistIconWithText.setText(bundle.getString("settings.general.cbShowBlacklistIconWithText.text"));
            cbShowBlacklistIconWithText.setToolTipText(bundle.getString("settings.general.cbShowBlacklistIconWithText.toolTipText"));
            panel1.add(cbShowBlacklistIconWithText, new CC().cell(1, 0));

            //---- cbUseWikipediaSenderLogos ----
            cbUseWikipediaSenderLogos.setText(bundle.getString("settings.general.cbUseWikipediaSenderLogos.text"));
            panel1.add(cbUseWikipediaSenderLogos, new CC().cell(0, 1));

            //---- cbAutomaticUpdateChecks ----
            cbAutomaticUpdateChecks.setText(bundle.getString("settings.general.cbAutomaticUpdateChecks.text"));
            panel1.add(cbAutomaticUpdateChecks, new CC().cell(0, 2));

            //---- cbDrawListIconsRight ----
            cbDrawListIconsRight.setText(bundle.getString("settings.general.cbDrawListIconsRight.text"));
            panel1.add(cbDrawListIconsRight, new CC().cell(0, 3));

            //---- cbUseSystemDarkMode ----
            cbUseSystemDarkMode.setText(bundle.getString("settings.general.checkBox1.text"));
            cbUseSystemDarkMode.setToolTipText(bundle.getString("settings.general.checkBox1.toolTipText"));
            panel1.add(cbUseSystemDarkMode, new CC().cell(0, 4, 2, 1));

            //---- cbTabFilmeTimeUseLongFormat ----
            cbTabFilmeTimeUseLongFormat.setText(bundle.getString("settings.general.cbTabFilmeTimeUseLongFormat.text"));
            panel1.add(cbTabFilmeTimeUseLongFormat, new CC().cell(0, 5));

            //---- languageLabel ----
            languageLabel.setText(bundle.getString("settings.general.languageLabel.text"));
            languageLabel.setLabelFor(languageComboBox);
            panel1.add(languageLabel, new CC().cell(0, 6).split(2));
            panel1.add(languageComboBox, new CC().cell(0, 6));

            //---- languageRestartLabel ----
            languageRestartLabel.setText(bundle.getString("settings.general.languageRestartLabel.text"));
            panel1.add(languageRestartLabel, new CC().cell(0, 7, 2, 1));
        }

        //---- modernSearchTitlePanel ----
        modernSearchTitlePanel.setTitle(bundle.getString("settings.general.modernSearchTitlePanel.title"));

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jPanel3, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanel5, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup()
                                .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addComponent(modernSearchTitlePanel, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel5, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel3, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(modernSearchTitlePanel, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JCheckBox jCheckBoxTabsTop;
    protected JCheckBox jCheckBoxTabIcon;
    protected JCheckBox cbAutomaticMenuTabSwitching;
    protected JCheckBox cbRestoreSelectedTab;
    protected JTextField jtfUserAgent;
    protected JTextField jtfProxyHost;
    protected JTextField jtfProxyPort;
    protected JTextField jtfProxyUser;
    protected JPasswordField jpfProxyPassword;
    protected JButton jButtonApplyProxySettings;
    protected JCheckBox jCheckBoxTray;
    protected JCheckBox cbShowBlacklistIconWithText;
    protected JCheckBox cbUseWikipediaSenderLogos;
    protected JCheckBox cbAutomaticUpdateChecks;
    protected JCheckBox cbDrawListIconsRight;
    protected JCheckBox cbUseSystemDarkMode;
    protected JCheckBox cbTabFilmeTimeUseLongFormat;
    protected JComboBox<mediathek.i18n.UiLanguage> languageComboBox;
    protected JXTitledPanel modernSearchTitlePanel;
    // End of variables declaration//GEN-END:variables
}
