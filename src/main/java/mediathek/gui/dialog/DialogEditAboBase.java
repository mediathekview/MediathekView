package mediathek.gui.dialog;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;

public class DialogEditAboBase extends JDialog {
    protected DialogEditAboBase(JFrame parent) {
        super(parent, true);
        initComponents();
    }

    protected JScrollPane getJScrollPane1() {
        return jScrollPane1;
    }

    protected JLabel getLabelMultiEditHeader() {
        return labelMultiEditHeader;
    }
    protected JCheckBox getCheckBoxEingeschaltet() {
        return checkBoxEingeschaltet;
    }

    protected JCheckBox getCheckBoxMultiEditEingeschaltet() {
        return checkBoxMultiEditEingeschaltet;
    }

    protected JCheckBox getCheckBoxDoNotStartAutomatically() {
        return checkBoxDoNotStartAutomatically;
    }

    protected JCheckBox getCheckBoxMultiEditDoNotStartAutomatically() {
        return checkBoxMultiEditDoNotStartAutomatically;
    }

    protected JTextField getTextFieldName() {
        return textFieldName;
    }

    protected JLabel getLabelSender() {
        return labelSender;
    }

    @SuppressWarnings("unchecked")
    protected JComboBox<String> getComboboxSender() {
        return comboboxSender;
    }

    protected JLabel getLabelThema() {
        return labelThema;
    }

    protected JTextField getTextFieldThema() {
        return textFieldThema;
    }

    protected JLabel getLabelTitel() {
        return labelTitel;
    }

    protected JTextField getTextFieldTitel() {
        return textFieldTitel;
    }

    protected JLabel getLabelThemaTitel() {
        return labelThemaTitel;
    }

    protected JTextField getTextFieldThemaTitel() {
        return textFieldThemaTitel;
    }

    protected JLabel getLabelIrgendwo() {
        return labelIrgendwo;
    }

    protected JTextField getTextFieldIrgendwo() {
        return textFieldIrgendwo;
    }

    protected JSlider getSliderDauer() {
        return sliderDauer;
    }

    protected JLabel getLabelDauer() {
        return labelDauer;
    }

    protected JCheckBox getCheckBoxMultiEditMindestdauer() {
        return checkBoxMultiEditMindestdauer;
    }

    protected JRadioButton getRbMin() {
        return rbMin;
    }

    protected JRadioButton getRbMax() {
        return rbMax;
    }

    protected JCheckBox getCheckBoxMultiEditMin() {
        return checkBoxMultiEditMin;
    }

    @SuppressWarnings("unchecked")
    protected JComboBox<String> getComboboxPfad() {
        return comboboxPfad;
    }

    protected JCheckBox getCheckBoxMultiEditZielpfad() {
        return checkBoxMultiEditZielpfad;
    }

    protected JLabel getLabelDownDatumValue() {
        return labelDownDatumValue;
    }

    @SuppressWarnings("unchecked")
    protected JComboBox<String> getComboboxPSet() {
        return comboboxPSet;
    }

    protected JCheckBox getCheckBoxMultiEditPSet() {
        return checkBoxMultiEditPSet;
    }

    protected JButton getJButtonBeenden() {
        return jButtonBeenden;
    }

    protected JButton getJButtonAbbrechen() {
        return jButtonAbbrechen;
    }

    protected JButton getJButtonHelp() {
        return jButtonHelp;
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jScrollPane1 = new JScrollPane();
        jPanelExtra = new JPanel();
        labelMultiEditHeader = new JLabel();
        var labelEingeschaltet = new JLabel();
        checkBoxEingeschaltet = new JCheckBox();
        checkBoxMultiEditEingeschaltet = new JCheckBox();
        var labelDoNotStartAutomatically = new JLabel();
        checkBoxDoNotStartAutomatically = new JCheckBox();
        checkBoxMultiEditDoNotStartAutomatically = new JCheckBox();
        var labelName = new JLabel();
        textFieldName = new JTextField();
        labelSender = new JLabel();
        comboboxSender = new JComboBox();
        labelThema = new JLabel();
        textFieldThema = new JTextField();
        labelTitel = new JLabel();
        textFieldTitel = new JTextField();
        labelThemaTitel = new JLabel();
        textFieldThemaTitel = new JTextField();
        labelIrgendwo = new JLabel();
        textFieldIrgendwo = new JTextField();
        var labelMindestdauer = new JLabel();
        panelDauer = new JPanel();
        sliderDauer = new JSlider();
        labelDauer = new JLabel();
        checkBoxMultiEditMindestdauer = new JCheckBox();
        var labelMin = new JLabel();
        panelMinMax = new JPanel();
        rbMin = new JRadioButton();
        rbMax = new JRadioButton();
        checkBoxMultiEditMin = new JCheckBox();
        var labelZielpfad = new JLabel();
        comboboxPfad = new JComboBox();
        checkBoxMultiEditZielpfad = new JCheckBox();
        var labelDownDatum = new JLabel();
        labelDownDatumValue = new JLabel();
        var labelPSet = new JLabel();
        comboboxPSet = new JComboBox();
        checkBoxMultiEditPSet = new JCheckBox();
        var buttonPanel = new JPanel();
        jButtonBeenden = new JButton();
        jButtonAbbrechen = new JButton();
        jButtonHelp = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Neues Abo anlegen");
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().insets("dialog").hideMode(3).gridGap("rel", "rel"),
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jScrollPane1 ========
        {

            //======== jPanelExtra ========
            {
                jPanelExtra.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).gridGap("10", "5"),
                    // columns
                    new AC()
                        .fill().gap()
                        .grow().fill().gap()
                        .fill(),
                    // rows
                    new AC()
                        ));

                //---- labelMultiEditHeader ----
                labelMultiEditHeader.setText("<html><style type=\"text/css\"> p { text-align: center; }</style><p>bei allen<br />\u00e4ndern</p></html>");
                jPanelExtra.add(labelMultiEditHeader, new CC().cell(2, 0));
                //---- labelEingeschaltet ----
                labelEingeschaltet.setText("Aktiv:");
                jPanelExtra.add(labelEingeschaltet, new CC().cell(0, 1));
                jPanelExtra.add(checkBoxEingeschaltet, new CC().cell(1, 1));
                jPanelExtra.add(checkBoxMultiEditEingeschaltet, new CC().cell(2, 1).alignX("center").growX(0));

                //---- labelDoNotStartAutomatically ----
                labelDoNotStartAutomatically.setText("Nicht automatisch starten:");
                jPanelExtra.add(labelDoNotStartAutomatically, new CC().cell(0, 2));

                //---- checkBoxDoNotStartAutomatically ----
                checkBoxDoNotStartAutomatically.setToolTipText("<html>Wenn aktiviert werden die aus diesem Abo generierten Downloads <b>nicht automatisch</b><br/> gestartet, auch wenn <i>Downloads aus Abos sofort starten</i> in den Einstellungen aktiviert ist.");
                jPanelExtra.add(checkBoxDoNotStartAutomatically, new CC().cell(1, 2));
                jPanelExtra.add(checkBoxMultiEditDoNotStartAutomatically, new CC().cell(2, 2).alignX("center").growX(0));

                //---- labelName ----
                labelName.setText("Name:");
                jPanelExtra.add(labelName, new CC().cell(0, 3));
                jPanelExtra.add(textFieldName, new CC().cell(1, 3).growX());

                //---- labelSender ----
                labelSender.setText("Sender:");
                jPanelExtra.add(labelSender, new CC().cell(0, 4));
                jPanelExtra.add(comboboxSender, new CC().cell(1, 4).growX());

                //---- labelThema ----
                labelThema.setText("Thema:");
                jPanelExtra.add(labelThema, new CC().cell(0, 5));
                jPanelExtra.add(textFieldThema, new CC().cell(1, 5).growX());

                //---- labelTitel ----
                labelTitel.setText("Titel:");
                jPanelExtra.add(labelTitel, new CC().cell(0, 6));
                jPanelExtra.add(textFieldTitel, new CC().cell(1, 6).growX());

                //---- labelThemaTitel ----
                labelThemaTitel.setText("Thema-Titel:");
                jPanelExtra.add(labelThemaTitel, new CC().cell(0, 7));
                jPanelExtra.add(textFieldThemaTitel, new CC().cell(1, 7).growX());

                //---- labelIrgendwo ----
                labelIrgendwo.setText("Irgendwo:");
                jPanelExtra.add(labelIrgendwo, new CC().cell(0, 8));
                jPanelExtra.add(textFieldIrgendwo, new CC().cell(1, 8).growX());

                //---- labelMindestdauer ----
                labelMindestdauer.setText("Dauer [Min]: ");
                jPanelExtra.add(labelMindestdauer, new CC().cell(0, 9));

                //======== panelDauer ========
                {
                    panelDauer.setLayout(new BorderLayout());

                    //---- sliderDauer ----
                    sliderDauer.setValue(0);
                    panelDauer.add(sliderDauer, BorderLayout.CENTER);

                    //---- labelDauer ----
                    labelDauer.setText("0");
                    panelDauer.add(labelDauer, BorderLayout.EAST);
                }
                jPanelExtra.add(panelDauer, new CC().cell(1, 9).growX());
                jPanelExtra.add(checkBoxMultiEditMindestdauer, new CC().cell(2, 9).alignX("center").growX(0));

                //---- labelMin ----
                labelMin.setText("Min/Max:");
                jPanelExtra.add(labelMin, new CC().cell(0, 10));

                //======== panelMinMax ========
                {
                    panelMinMax.setLayout(new BorderLayout());

                    //---- rbMin ----
                    rbMin.setText("Mindestdauer");
                    panelMinMax.add(rbMin, BorderLayout.NORTH);

                    //---- rbMax ----
                    rbMax.setText("Maximaldauer");
                    panelMinMax.add(rbMax, BorderLayout.CENTER);
                }
                jPanelExtra.add(panelMinMax, new CC().cell(1, 10).growX());
                jPanelExtra.add(checkBoxMultiEditMin, new CC().cell(2, 10).alignX("center").growX(0));

                //---- labelZielpfad ----
                labelZielpfad.setText("Zielpfad:");
                jPanelExtra.add(labelZielpfad, new CC().cell(0, 11));
                jPanelExtra.add(comboboxPfad, new CC().cell(1, 11).growX());
                jPanelExtra.add(checkBoxMultiEditZielpfad, new CC().cell(2, 11).alignX("center").growX(0));

                //---- labelDownDatum ----
                labelDownDatum.setText("Letztes Abo:");
                jPanelExtra.add(labelDownDatum, new CC().cell(0, 12));
                jPanelExtra.add(labelDownDatumValue, new CC().cell(1, 12));

                //---- labelPSet ----
                labelPSet.setText("Programmset:");
                jPanelExtra.add(labelPSet, new CC().cell(0, 13));
                jPanelExtra.add(comboboxPSet, new CC().cell(1, 13).growX());
                jPanelExtra.add(checkBoxMultiEditPSet, new CC().cell(2, 13).alignX("center").growX(0));
            }
            jScrollPane1.setViewportView(jPanelExtra);
        }
        contentPane.add(jScrollPane1, new CC().cell(0, 0).push().grow());

        //======== buttonPanel ========
        {
            buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

            //---- jButtonBeenden ----
            jButtonBeenden.setText("Ok");
            buttonPanel.add(jButtonBeenden);

            //---- jButtonAbbrechen ----
            jButtonAbbrechen.setText("Abbrechen");
            buttonPanel.add(jButtonAbbrechen);

            //---- jButtonHelp ----
            jButtonHelp.setIcon(new FlatSVGIcon("icons/fontawesome/circle-question.svg", 16, 16));
            jButtonHelp.setToolTipText("Hilfe anzeigen");
            buttonPanel.add(jButtonHelp);
        }
        contentPane.add(buttonPanel, new CC().cell(0, 1).alignX("right").growX(0));
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JScrollPane jScrollPane1;
    private JPanel jPanelExtra;
    private JLabel labelMultiEditHeader;
    private JCheckBox checkBoxEingeschaltet;
    private JCheckBox checkBoxMultiEditEingeschaltet;
    private JCheckBox checkBoxDoNotStartAutomatically;
    private JCheckBox checkBoxMultiEditDoNotStartAutomatically;
    private JTextField textFieldName;
    private JLabel labelSender;
    private JComboBox comboboxSender;
    private JLabel labelThema;
    private JTextField textFieldThema;
    private JLabel labelTitel;
    private JTextField textFieldTitel;
    private JLabel labelThemaTitel;
    private JTextField textFieldThemaTitel;
    private JLabel labelIrgendwo;
    private JTextField textFieldIrgendwo;
    private JPanel panelDauer;
    private JSlider sliderDauer;
    private JLabel labelDauer;
    private JCheckBox checkBoxMultiEditMindestdauer;
    private JPanel panelMinMax;
    private JRadioButton rbMin;
    private JRadioButton rbMax;
    private JCheckBox checkBoxMultiEditMin;
    private JComboBox comboboxPfad;
    private JCheckBox checkBoxMultiEditZielpfad;
    private JLabel labelDownDatumValue;
    private JComboBox comboboxPSet;
    private JCheckBox checkBoxMultiEditPSet;
    private JButton jButtonBeenden;
    private JButton jButtonAbbrechen;
    private JButton jButtonHelp;
    // End of variables declaration//GEN-END:variables

}
