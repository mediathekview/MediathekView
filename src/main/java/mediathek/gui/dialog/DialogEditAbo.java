package mediathek.gui.dialog;

import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.daten.abo.AboTags;
import mediathek.daten.abo.DatenAbo;
import mediathek.daten.abo.FilmLengthState;
import mediathek.file.GetFile;
import mediathek.tool.*;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.Objects;

public class DialogEditAbo extends JDialog {
    private final DatenAbo aktAbo;
    private final EnumMap<AboTags, JTextField> textFieldMap = new EnumMap<>(AboTags.class);
    private final JComboBox<String> comboboxPSet = new JComboBox<>();
    private final JComboBox<String> comboboxSender = new JComboBox<>();
    private final JComboBox<String> comboboxPfad = new JComboBox<>();
    private final JCheckBox checkBoxEingeschaltet = new JCheckBox();
    private final JRadioButton rbMin = new JRadioButton("Mindestdauer");
    private final JRadioButton rbMax = new JRadioButton("Maximaldauer");
    private final JSlider sliderDauer = new JSlider(0, 100, 0);
    private final JLabel labelDauer = new JLabel("0");
    private final boolean isMultiEditMode;
    /**
     * This determines in multi edit mode, which fields should be applied to all selected abos...
     */
    public boolean[] multiEditCbIndices = new boolean[AboTags.values().length];
    /**
     * Determines whether the whole edit operation was "ok" -> successful or not.
     */
    private boolean ok;

    /**
     * Edit dialog for abos.
     * @param parent dialog parent.
     * @param aktA the active abo.
     * @param isMultiEditMode show checkbox for each field which shall be changed in multi edit mode
     */
    public DialogEditAbo(final JFrame parent, DatenAbo aktA, boolean isMultiEditMode) {
        super(parent, true);
        initComponents();

        Daten daten = Daten.getInstance();
        this.isMultiEditMode = isMultiEditMode;
        aktAbo = aktA;

        ButtonGroup gr = new ButtonGroup();
        gr.add(rbMin);
        gr.add(rbMax);

        jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        comboboxPSet.setModel(new DefaultComboBoxModel<>(Daten.listePset.getListeAbo().getObjectDataCombo()));
        comboboxSender.setModel(new SenderListComboBoxModel());

        // Zielpfad ========================
        ArrayList<String> pfade = daten.getListeAbo().getPfade();
        if (!pfade.contains(aktAbo.getZielpfad())) {
            pfade.add(0, aktAbo.getZielpfad());
        }
        comboboxPfad.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[0])));
        comboboxPfad.setEditable(true);
        checkPfad();

        final var editorComp = ((JTextComponent) comboboxPfad.getEditor().getEditorComponent());
        editorComp.setOpaque(true);
        editorComp.getDocument().addDocumentListener(new CheckPathDocListener());

        // =====================
        jButtonBeenden.addActionListener(e -> {
            if (check()) {
                dispose();
            } else {
                MVMessageDialog.showMessageDialog(parent, "Filter angeben!", "Leeres Abo", JOptionPane.ERROR_MESSAGE);
            }
        });
        jButtonAbbrechen.addActionListener(e -> dispose());
        getRootPane().setDefaultButton(jButtonBeenden);

        EscapeKeyHandler.installHandler(this, this::dispose);

        jButtonHelp.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jButtonHelp.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_DIALOG_ADD_ABO)).setVisible(true));

        if (comboboxPSet.getModel().getSize() == 0) {
            // dann gibts kein Set zum Aufzeichnen
            new DialogAboNoSet(parent).setVisible(true);
        } else {
            setExtra();
            this.pack();
        }

        GuiFunktionen.centerOnScreen(this, false);
    }

    public boolean successful() {
        return ok;
    }

    @Override
    public void setVisible(boolean vis) {
        if (comboboxPSet.getModel().getSize() == 0) {
            // dann gibts kein Set zum Aufzeichnen
            dispose();
        } else {
            super.setVisible(vis);
        }
    }

    private void checkPfad() {
        String s = ((JTextComponent) comboboxPfad.getEditor().getEditorComponent()).getText();
        if (!s.equals(FilenameUtils.checkDateiname(s, false /*pfad*/))) {
            comboboxPfad.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
        } else {
            comboboxPfad.getEditor().getEditorComponent().setBackground(Color.WHITE);
        }
    }

    private void setExtra() {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 10, 10, 5);
        jPanelExtra.setLayout(gridbag);

        int zeile = 1;
        if (isMultiEditMode) {
            c.gridx = 2;
            c.weightx = 1;
            var label = new JLabel("<html><style type=\"text/css\"> p { text-align: center; }</style><p>bei allen<br />ändern</p></html>");
            var border = new CompoundBorder(BorderFactory.createLineBorder(new Color(204, 204, 255), 4, true),
                    emptyBorder);
            label.setBorder(border);
            gridbag.setConstraints(label, c);
            jPanelExtra.add(label);
            c.gridy = 1;
        } else {
            zeile = 0;
            c.gridy = 0;
        }

        for (var tag : AboTags.values()) {
            c.fill = GridBagConstraints.HORIZONTAL;
            c.anchor = GridBagConstraints.WEST;
            addExtraFeld(tag, gridbag, c, jPanelExtra);
            ++zeile;
            c.gridy = zeile;
        }
    }

    private JTextField createTextField(@NotNull String displayText,GridBagLayout gridbag, GridBagConstraints c) {
        var tf = new JTextField();
        tf.setText(displayText);
        gridbag.setConstraints(tf, c);

        return tf;
    }

    private JLabel createLabel(@NotNull AboTags tag, GridBagLayout gridbag, GridBagConstraints c) {
        JLabel label;

        switch(tag) {
            case SENDER, THEMA, TITEL, THEMA_TITEL, IRGENDWO -> {
                label = new JLabel(DatenAbo.COLUMN_NAMES[tag.getIndex()] + ":");
                label.setForeground(Color.BLUE);
            }
            case MINDESTDAUER -> label = new JLabel("Dauer [min]: ");
            default -> label = new JLabel(DatenAbo.COLUMN_NAMES[tag.getIndex()] + ":");
        }
        gridbag.setConstraints(label, c);

        return label;
    }

    private void addExtraFeld(AboTags index, GridBagLayout gridbag, GridBagConstraints c, JPanel panel) {
        //Labels
        c.gridx = 0;
        c.weightx = 0;
        panel.add(createLabel(index, gridbag, c));

        //Textfelder
        c.gridx = 1;
        c.weightx = 10;
        switch (index) {
            case NR -> {
                var lbl = new JLabel();
                final var nr = aktAbo.getNr();
                if (nr > 0)
                    lbl.setText(Integer.toString(aktAbo.getNr()));
                else
                    lbl.setText("noch nicht vergeben");
                gridbag.setConstraints(lbl, c);
                panel.add(lbl);
            }

            case EINGESCHALTET -> {
                checkBoxEingeschaltet.setSelected(aktAbo.isActive());
                gridbag.setConstraints(checkBoxEingeschaltet, c);
                panel.add(checkBoxEingeschaltet);
            }

            case NAME -> {
                var tf = createTextField(aktAbo.getName(), gridbag, c);
                tf.getDocument().addDocumentListener(new EmptyTextDocListener(tf));
                textFieldMap.put(index, tf);
                panel.add(tf);
            }

            case SENDER -> {
                comboboxSender.setSelectedItem(aktAbo.getSender());
                gridbag.setConstraints(comboboxSender, c);
                panel.add(comboboxSender);
            }

            case THEMA -> {
                var tf = createTextField(aktAbo.getThema(), gridbag, c);
                textFieldMap.put(index, tf);
                panel.add(tf);
            }

            case TITEL -> {
                var tf = createTextField(aktAbo.getTitle(), gridbag, c);
                textFieldMap.put(index, tf);
                panel.add(tf);
            }

            case THEMA_TITEL -> {
                var tf = createTextField(aktAbo.getThemaTitel(), gridbag, c);
                textFieldMap.put(index, tf);
                panel.add(tf);
            }

            case IRGENDWO -> {
                var tf = createTextField(aktAbo.getIrgendwo(), gridbag, c);
                textFieldMap.put(index, tf);
                panel.add(tf);
            }

            case MINDESTDAUER -> {
                final int minDauer = aktAbo.getMindestDauerMinuten();
                sliderDauer.setValue(minDauer);
                labelDauer.setText(String.valueOf(minDauer == 0 ? " alles " : minDauer));
                sliderDauer.addChangeListener(e -> labelDauer.setText("  " + (sliderDauer.getValue() == 0 ? "alles" : Integer.toString(sliderDauer.getValue()))));
                var p = new JPanel(new BorderLayout());
                p.add(sliderDauer, BorderLayout.CENTER);
                p.add(labelDauer, BorderLayout.EAST);
                gridbag.setConstraints(p, c);
                panel.add(p);
            }

            case MIN -> {
                final boolean isMin = aktAbo.getFilmLengthState() == FilmLengthState.MINIMUM;
                rbMin.setSelected(isMin);
                rbMax.setSelected(!isMin);
                var p = new JPanel(new BorderLayout());
                p.add(rbMin, BorderLayout.NORTH);
                p.add(rbMax, BorderLayout.CENTER);
                gridbag.setConstraints(p, c);
                panel.add(p);
            }

            case ZIELPFAD -> {
                comboboxPfad.setSelectedItem(aktAbo.getZielpfad());
                gridbag.setConstraints(comboboxPfad, c);
                panel.add(comboboxPfad);
            }

            case DOWN_DATUM -> {
                var p = new JLabel(aktAbo.getDownDatum());
                gridbag.setConstraints(p, c);
                panel.add(p);
            }

            case PSET -> {
                comboboxPSet.setSelectedItem(aktAbo.getPsetName());
                //falls das Feld leer war, wird es jetzt auf den ersten Eintrag gesetzt
                aktAbo.setPsetName(Objects.requireNonNull(comboboxPSet.getSelectedItem()).toString());// damit immer eine Set eingetragen ist!
                gridbag.setConstraints(comboboxPSet, c);
                panel.add(comboboxPSet);
            }
        }

        if (isMultiEditMode) {
            //Checkbox
            c.gridx = 2;
            c.weightx = 0;
            switch(index) {
                case EINGESCHALTET, MIN, MINDESTDAUER, PSET, ZIELPFAD -> {
                    c.fill = GridBagConstraints.NONE;
                    c.anchor = GridBagConstraints.CENTER;
                    var jcb = new JCheckBox();
                    jcb.setBorder(emptyBorder);
                    jcb.setHorizontalTextPosition(JCheckBox.CENTER);
                    jcb.addActionListener(l -> multiEditCbIndices[index.getIndex()] = jcb.isSelected());
                    gridbag.setConstraints(jcb, c);
                    panel.add(jcb);
                }
            }
        }
    }

    private final EmptyBorder emptyBorder = new EmptyBorder(5, 5, 5, 5);

    private boolean check() {
        DatenAbo test = aktAbo.getCopy();
        get(test);
        if (test.isInvalid()) {
            ok = false;
        } else {
            get(aktAbo);
            ok = true;
        }
        return ok;
    }

    private void get(DatenAbo abo) {
        //no ABO_NR
        abo.setActive(checkBoxEingeschaltet.isSelected());
        abo.setName(textFieldMap.get(AboTags.NAME).getText().trim());
        abo.setSender(Objects.requireNonNull(comboboxSender.getSelectedItem()).toString());
        abo.setThema(textFieldMap.get(AboTags.THEMA).getText().trim());
        abo.setTitle(textFieldMap.get(AboTags.TITEL).getText().trim());
        abo.setThemaTitel(textFieldMap.get(AboTags.THEMA_TITEL).getText().trim());
        abo.setIrgendwo(textFieldMap.get(AboTags.IRGENDWO).getText().trim());
        abo.setMindestDauerMinuten(sliderDauer.getValue());
        if (rbMin.isSelected())
            abo.setFilmLengthState(FilmLengthState.MINIMUM);
        else
            abo.setFilmLengthState(FilmLengthState.MAXIMUM);
        abo.setZielpfad(Objects.requireNonNull(comboboxPfad.getSelectedItem()).toString());
        //no ABO_DOWN_DATUM
        abo.setPsetName(Objects.requireNonNull(comboboxPSet.getSelectedItem()).toString());
    }

    private class CheckPathDocListener implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            checkPfad();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            checkPfad();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            checkPfad();
        }

    }

    private class EmptyTextDocListener implements DocumentListener {
        private final JTextField tf;

        public EmptyTextDocListener(JTextField tf) {
            this.tf = tf;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            final boolean isEmpty = tf.getText().isBlank();
            tf.setBackground(isEmpty ? Color.red : Color.white);
            jButtonBeenden.setEnabled(!isEmpty);
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            final boolean isEmpty = tf.getText().isBlank();
            tf.setBackground(isEmpty ? Color.red : Color.white);
            jButtonBeenden.setEnabled(!isEmpty);
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            final boolean isEmpty = tf.getText().isBlank();
            tf.setBackground(isEmpty ? Color.red : Color.white);
            jButtonBeenden.setEnabled(!isEmpty);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jButtonAbbrechen = new javax.swing.JButton();
        jButtonBeenden = new javax.swing.JButton();
        jButtonHelp = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );

        jScrollPane1.setViewportView(jPanelExtra);

        jButtonAbbrechen.setText("Abbrechen");

        jButtonBeenden.setText("Ok");

        jButtonHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHelp.setToolTipText("Hilfe anzeigen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane1)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonBeenden, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAbbrechen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHelp)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonBeenden});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonBeenden)
                    .addComponent(jButtonAbbrechen)
                    .addComponent(jButtonHelp))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JButton jButtonHelp;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

}
