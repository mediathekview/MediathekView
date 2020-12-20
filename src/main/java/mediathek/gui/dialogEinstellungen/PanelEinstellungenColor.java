package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.CellRendererColor;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVC;
import mediathek.tool.models.TModelColor;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import static mediathek.config.MVColor.*;

public class PanelEinstellungenColor extends JPanel {
    public PanelEinstellungenColor() {
        initComponents();
        init();
    }

    private void init() {
        jTable1.addMouseListener(new BeobMausTabelle());
        jTable1.setDefaultRenderer(MVC.class, new CellRendererColor());
        jTable1.setModel(getModel());
        jButtonReset.addActionListener(e -> {
            Daten.mVColor.reset();
            GuiFunktionen.updateGui(MediathekGui.ui());
            Daten.mVColor.save();
        });
    }

    private void getColor(MVC mvc) {
        var selectedColor = JColorChooser.showDialog(this,"Farbe auswählen", mvc.color);
        if (selectedColor != null) {
            if (!selectedColor.equals(mvc.color)) {
                mvc.set(selectedColor);
                jTable1.setModel(getModel());
                GuiFunktionen.updateGui(MediathekGui.ui());
                Daten.mVColor.save();
            }
        }
    }

    private TableModel getModel() {
        Object[] object;
        TModelColor tModel = new TModelColor(new Object[][]{});
        tModel.setRowCount(0);
        for (MVC mvc : Daten.mVColor.liste) {
            object = new Object[MVC_MAX];
            object[MVC_TEXT] = mvc.text;
            object[MVC_COLOR] = mvc;
            tModel.addRow(object);
        }
        return tModel;
    }

    public class BeobMausTabelle extends MouseAdapter {

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    Point p = arg0.getPoint();
                    int row = jTable1.rowAtPoint(p);
                    int column = jTable1.columnAtPoint(p);
                    if (row >= 0) {
                        MVC mvc = (MVC) jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(row), MVColor.MVC_COLOR);
                        if (jTable1.convertColumnIndexToModel(column) == MVColor.MVC_COLOR) {
                            getColor(mvc);
                        }
                    }
                }
            }
        }

    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jScrollPane1 = new JScrollPane();
        jTable1 = new JTable();
        var hSpacer1 = new JPanel(null);
        jButtonReset = new JButton();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("0").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .fill().gap()
                .grow().fill().gap()
                .fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jScrollPane1 ========
        {
            jScrollPane1.setViewportView(jTable1);
        }
        add(jScrollPane1, new CC().cell(1, 0, 2, 1));
        add(hSpacer1, new CC().cell(1, 1));

        //---- jButtonReset ----
        jButtonReset.setText("Farben zur\u00fccksetzen"); //NON-NLS
        add(jButtonReset, new CC().cell(2, 1));
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JTable jTable1;
    private JButton jButtonReset;
    // End of variables declaration//GEN-END:variables
}
