package mediathek.gui.dialogEinstellungen;

import com.jidesoft.swing.CheckBoxList;
import mediathek.swing.MultilineLabel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/**
 * Base class for UI Designer.
 * Subclasses contain the hand-written panel behavior.
 */
/** Settings controls using the selected interface-language bundle. */
public class PanelFilmlisteLadenBase extends JPanel {
    public PanelFilmlisteLadenBase() {
        super();
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
        var jPanelAuto = new JPanel();
        var multilineLabel1 = new MultilineLabel();
        var jPanelManuel = new JPanel();
        var multilineLabel2 = new MultilineLabel();
        lblUrl = new JLabel();
        jTextFieldUrl = new JTextField();
        jButtonDateiAuswaehlen = new JButton();
        jCheckBoxUpdate = new JCheckBox();
        jRadioButtonAuto = new JRadioButton();
        jRadioButtonManuell = new JRadioButton();
        var separator1 = new JSeparator();
        var panel3 = new JPanel();
        var panel4 = new JPanel();
        cbEvaluateDuplicates = new JCheckBox();
        var panel2 = new JPanel();
        var label1 = new JLabel();
        var jSpinnerDays = new DaysSpinner();
        var label2 = new JLabel();
        btnReloadFilmlist = new JButton();
        var jPanel1 = new JPanel();
        cbSign = new JCheckBox();
        cbTrailer = new JCheckBox();
        cbAudio = new JCheckBox();
        cbLivestreams = new JCheckBox();
        panel1 = new JPanel();
        var scrollPane1 = new JScrollPane();
        senderCheckBoxList = new CheckBoxList();

        //======== this ========
        setPreferredSize(new Dimension(740, 506));
        setLayout(new MigLayout(
            new LC().fillX().insets("5").hideMode(3).gridGap("5", "5"),
            // columns
            new AC()
                .align("label").gap()
                .size("640").grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .gap()
                ));

        //======== jPanelAuto ========
        {
            jPanelAuto.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.jPanelAuto.borderTitle")));
            jPanelAuto.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .fill()));

            //---- multilineLabel1 ----
            multilineLabel1.setText(bundle.getString("settings.PanelFilmlisteLaden.multilineLabel1.text"));
            multilineLabel1.setFont(multilineLabel1.getFont().deriveFont(multilineLabel1.getFont().getSize() - 1f));
            jPanelAuto.add(multilineLabel1, new CC().cell(0, 0, 2, 1));
        }
        add(jPanelAuto, new CC().cell(1, 0));

        //======== jPanelManuel ========
        {
            jPanelManuel.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.jPanelManuel.borderTitle")));
            jPanelManuel.setMaximumSize(new Dimension(750, 2147483647));
            jPanelManuel.setLayout(new MigLayout(
                new LC().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- multilineLabel2 ----
            multilineLabel2.setText(bundle.getString("settings.PanelFilmlisteLaden.multilineLabel2.text"));
            multilineLabel2.setFont(multilineLabel2.getFont().deriveFont(multilineLabel2.getFont().getSize() - 1f));
            jPanelManuel.add(multilineLabel2, new CC().cell(0, 0, 4, 1));

            //---- lblUrl ----
            lblUrl.setText(bundle.getString("settings.PanelFilmlisteLaden.jLabel1.text"));
            jPanelManuel.add(lblUrl, new CC().cell(0, 1));
            jPanelManuel.add(jTextFieldUrl, new CC().cell(1, 1, 2, 1));

            //---- jButtonDateiAuswaehlen ----
            jButtonDateiAuswaehlen.setToolTipText(bundle.getString("settings.PanelFilmlisteLaden.jButtonDateiAuswaehlen.toolTipText"));
            jPanelManuel.add(jButtonDateiAuswaehlen, new CC().cell(3, 1).alignX("left").growX(0).width("32:32:32").height("32:32:32"));

            //---- jCheckBoxUpdate ----
            jCheckBoxUpdate.setText(bundle.getString("settings.PanelFilmlisteLaden.jCheckBoxUpdate.text"));
            jPanelManuel.add(jCheckBoxUpdate, new CC().cell(0, 2, 2, 1));
        }
        add(jPanelManuel, new CC().cell(1, 1));
        add(jRadioButtonAuto, new CC().cell(0, 0).alignY("top").growY(0));
        add(jRadioButtonManuell, new CC().cell(0, 1).alignY("top").growY(0));
        add(separator1, new CC().cell(0, 2, 2, 1).growX());

        //======== panel3 ========
        {
            panel3.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.panel3.borderTitle")));
            panel3.setLayout(new VerticalLayout());

            //======== panel4 ========
            {
                panel4.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.panel4.borderTitle")));
                panel4.setLayout(new VerticalLayout());

                //---- cbEvaluateDuplicates ----
                cbEvaluateDuplicates.setText(bundle.getString("settings.PanelFilmlisteLaden.cbEvaluateDuplicates.text"));
                panel4.add(cbEvaluateDuplicates);
            }
            panel3.add(panel4);

            //======== panel2 ========
            {
                panel2.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3),
                    // columns
                    new AC()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .align("left"),
                    // rows
                    new AC()
                        .fill()));

                //---- label1 ----
                label1.setText(bundle.getString("settings.PanelFilmlisteLaden.label1.text"));
                panel2.add(label1, new CC().cell(0, 0).alignX("center").growX(0));
                panel2.add(jSpinnerDays, new CC().cell(1, 0));

                //---- label2 ----
                label2.setText(bundle.getString("settings.PanelFilmlisteLaden.label2.text"));
                panel2.add(label2, new CC().cell(2, 0).alignX("center").growX(0));

                //---- btnReloadFilmlist ----
                btnReloadFilmlist.setToolTipText(bundle.getString("settings.PanelFilmlisteLaden.btnReloadFilmlist.toolTipText"));
                panel2.add(btnReloadFilmlist, new CC().cell(3, 0));
            }
            panel3.add(panel2);

            //======== jPanel1 ========
            {
                jPanel1.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.jPanel1.borderTitle")));
                jPanel1.setToolTipText(bundle.getString("settings.PanelFilmlisteLaden.jPanel1.toolTipText"));
                jPanel1.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3).gridGap("5", "5"),
                    // columns
                    new AC()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        .fill()));

                //---- cbSign ----
                cbSign.setText(bundle.getString("settings.PanelFilmlisteLaden.cbSign.text"));
                jPanel1.add(cbSign, new CC().cell(2, 0));

                //---- cbTrailer ----
                cbTrailer.setText(bundle.getString("settings.PanelFilmlisteLaden.cbTrailer.text"));
                jPanel1.add(cbTrailer, new CC().cell(0, 0));

                //---- cbAudio ----
                cbAudio.setText(bundle.getString("settings.PanelFilmlisteLaden.cbAudio.text"));
                jPanel1.add(cbAudio, new CC().cell(1, 0));

                //---- cbLivestreams ----
                cbLivestreams.setText(bundle.getString("settings.PanelFilmlisteLaden.cbLivestreams.text"));
                jPanel1.add(cbLivestreams, new CC().cell(3, 0));
            }
            panel3.add(jPanel1);

            //======== panel1 ========
            {
                panel1.setBorder(new TitledBorder(bundle.getString("settings.PanelFilmlisteLaden.panel1.borderTitle")));
                panel1.setToolTipText(bundle.getString("settings.PanelFilmlisteLaden.panel1.toolTipText"));
                panel1.setLayout(new MigLayout(
                    new LC().insets("5").hideMode(3).alignX("left").gridGapX("10"),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        ));

                //======== scrollPane1 ========
                {

                    //---- senderCheckBoxList ----
                    senderCheckBoxList.setMaximumSize(null);
                    senderCheckBoxList.setMinimumSize(new Dimension(100, 50));
                    senderCheckBoxList.setPreferredSize(null);
                    senderCheckBoxList.setLayoutOrientation(JList.VERTICAL_WRAP);
                    senderCheckBoxList.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
                    scrollPane1.setViewportView(senderCheckBoxList);
                }
                panel1.add(scrollPane1, new CC().cell(0, 0));
            }
            panel3.add(panel1);
        }
        add(panel3, new CC().cell(0, 3, 2, 1).growX());

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAuto);
        buttonGroup1.add(jRadioButtonManuell);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JLabel lblUrl;
    protected JTextField jTextFieldUrl;
    protected JButton jButtonDateiAuswaehlen;
    protected JCheckBox jCheckBoxUpdate;
    protected JRadioButton jRadioButtonAuto;
    protected JRadioButton jRadioButtonManuell;
    protected JCheckBox cbEvaluateDuplicates;
    protected JButton btnReloadFilmlist;
    protected JCheckBox cbSign;
    protected JCheckBox cbTrailer;
    protected JCheckBox cbAudio;
    protected JCheckBox cbLivestreams;
    protected JPanel panel1;
    protected CheckBoxList senderCheckBoxList;
    // End of variables declaration//GEN-END:variables
}
