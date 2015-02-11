/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialog;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.tool.EscBeenden;
import mediathek.tool.FilenameUtils;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVColor;

public class DialogEditAbo extends javax.swing.JDialog {

    private final DatenAbo aktAbo;
    private JTextField[] textfeldListe;
    private final JComboBox<String> comboboxPSet = new JComboBox<>();
    private final JComboBox<String> comboboxSender = new JComboBox<>();
    private final JComboBox<String> comboboxPfad = new JComboBox<>();
    private final JCheckBox checkBoxEingeschaltet = new JCheckBox();
    private final JSlider sliderDauer = new JSlider(0, 100, 0);
    private final JLabel labelDauer = new JLabel("0");
    public boolean ok = false;

    public DialogEditAbo(java.awt.Frame parent, boolean modal, Daten d, DatenAbo aktA) {
        super(parent, modal);
        initComponents();
        aktAbo = aktA;
        jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        comboboxPSet.setModel(new javax.swing.DefaultComboBoxModel<>(Daten.listePset.getListeAbo().getObjectDataCombo()));
        comboboxSender.setModel(new javax.swing.DefaultComboBoxModel<>(GuiFunktionen.addLeerListe(Daten.filmeLaden.getSenderNamen())));
        // Zeilpfad ========================
        ArrayList<String> pfade = Daten.listeAbo.getPfade();
        if (!pfade.contains(aktAbo.arr[DatenAbo.ABO_ZIELPFAD_NR])) {
            pfade.add(0, aktAbo.arr[DatenAbo.ABO_ZIELPFAD_NR]);
        }
        comboboxPfad.setModel(new javax.swing.DefaultComboBoxModel<>(pfade.toArray(new String[pfade.size()])));
        comboboxPfad.setEditable(true);
        checkPfad();
        ((JTextComponent) comboboxPfad.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) comboboxPfad.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {

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

        });
        // =====================
        jButtonBeenden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                check();
                beenden();
            }
        });
        jButtonAbbrechen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
            }
        });
        getRootPane().setDefaultButton(jButtonBeenden);
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };
        setExtra();
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
        textfeldListe = new JTextField[DatenAbo.MAX_ELEM];
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 10, 10, 5);
        jPanelExtra.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < DatenAbo.MAX_ELEM; ++i) {
            addExtraFeld(i, gridbag, c, jPanelExtra);
            ++zeile;
            c.gridy = zeile;
        }
    }

    private void addExtraFeld(int i, GridBagLayout gridbag, GridBagConstraints c,
            JPanel panel) {
        //Label
        c.gridx = 0;
        c.weightx = 0;
        JLabel label;
        if (i == DatenAbo.ABO_SENDER_NR || i == DatenAbo.ABO_THEMA_NR || i == DatenAbo.ABO_TITEL_NR || i == DatenAbo.ABO_THEMA_TITEL_NR || i == DatenAbo.ABO_IRGENDWO_NR) {
            label = new JLabel("  " + DatenAbo.COLUMN_NAMES[i] + ": ");
            label.setForeground(Color.BLUE);
        } else {
            label = new JLabel(DatenAbo.COLUMN_NAMES[i] + ": ");
        }
        gridbag.setConstraints(label, c);
        panel.add(label);
        //Textfeld
        c.gridx = 1;
        c.weightx = 10;
        if (i == DatenAbo.ABO_PSET_NR) {
            comboboxPSet.setSelectedItem(aktAbo.arr[i]);
            //falls das Feld leer war, wird es jetzt auf den ersten Eintrag gesetzt
            aktAbo.arr[DatenAbo.ABO_PSET_NR] = comboboxPSet.getSelectedItem().toString(); // damit immer eine Set eingetragen ist!
            gridbag.setConstraints(comboboxPSet, c);
            panel.add(comboboxPSet);
        } else if (i == DatenAbo.ABO_SENDER_NR) {
            comboboxSender.setSelectedItem(aktAbo.arr[i]);
            gridbag.setConstraints(comboboxSender, c);
            panel.add(comboboxSender);
        } else if (i == DatenAbo.ABO_ZIELPFAD_NR) {
            comboboxPfad.setSelectedItem(aktAbo.arr[i]);
            gridbag.setConstraints(comboboxPfad, c);
            panel.add(comboboxPfad);
        } else if (i == DatenAbo.ABO_MINDESTDAUER_NR) {
            sliderDauer.setValue(aktAbo.mindestdauerMinuten);
            labelDauer.setText(String.valueOf(aktAbo.mindestdauerMinuten));
            sliderDauer.addChangeListener(new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    labelDauer.setText("  " + sliderDauer.getValue() + " ");
                }
            });
            JPanel p = new JPanel(new BorderLayout());
            p.add(sliderDauer, BorderLayout.CENTER);
            p.add(labelDauer, BorderLayout.EAST);
            gridbag.setConstraints(p, c);
            panel.add(p);
        } else if (i == DatenAbo.ABO_EINGESCHALTET_NR) {
            checkBoxEingeschaltet.setSelected(Boolean.parseBoolean(aktAbo.arr[i]));
            gridbag.setConstraints(checkBoxEingeschaltet, c);
            panel.add(checkBoxEingeschaltet);
        } else {
            JTextField textfeld = new JTextField();
            textfeldListe[i] = textfeld;
            if (i == DatenAbo.ABO_NR_NR
                    || i == DatenAbo.ABO_DOWN_DATUM_NR) {
                textfeld.setEditable(false);
            }
            textfeld.setText(aktAbo.arr[i]);
            gridbag.setConstraints(textfeld, c);
            panel.add(textfeld);
        }
    }

    private void check() {
        for (int i = 0; i < DatenAbo.MAX_ELEM; ++i) {
            switch (i) {
                case (DatenAbo.ABO_ZIELPFAD_NR):
                    aktAbo.arr[DatenAbo.ABO_ZIELPFAD_NR] = comboboxPfad.getSelectedItem().toString();
                    break;
                case (DatenAbo.ABO_PSET_NR):
                    aktAbo.arr[DatenAbo.ABO_PSET_NR] = comboboxPSet.getSelectedItem().toString();
                    break;
                case (DatenAbo.ABO_SENDER_NR):
                    aktAbo.arr[DatenAbo.ABO_SENDER_NR] = comboboxSender.getSelectedItem().toString();
                    break;
                case (DatenAbo.ABO_EINGESCHALTET_NR):
                    aktAbo.arr[DatenAbo.ABO_EINGESCHALTET_NR] = Boolean.toString(checkBoxEingeschaltet.isSelected());
                    break;
                case (DatenAbo.ABO_MINDESTDAUER_NR):
                    aktAbo.setMindestDauerMinuten(sliderDauer.getValue());
                    break;
                case (DatenAbo.ABO_NR_NR):
                case (DatenAbo.ABO_DOWN_DATUM_NR):
                    break;
                default:
                    aktAbo.arr[i] = textfeldListe[i].getText().trim();
                    break;
            }
        }
        ok = true;
    }

    private void beenden() {
        this.dispose();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jButtonAbbrechen = new javax.swing.JButton();
        jButtonBeenden = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 513, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 502, Short.MAX_VALUE)
        );

        jScrollPane1.setViewportView(jPanelExtra);

        jButtonAbbrechen.setText("Abbrechen");

        jButtonBeenden.setText("Ok");

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
                        .addComponent(jButtonAbbrechen)))
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
                    .addComponent(jButtonAbbrechen))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

}
