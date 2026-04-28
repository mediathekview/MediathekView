package mediathek.gui.dialogEinstellungen;

import mediathek.config.MVColor;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MVC;
import mediathek.tool.cellrenderer.CellRendererColor;
import mediathek.tool.models.TModelColor;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class PanelEinstellungenColor extends JPanel {
    private final TModelColor lightColorTableModel = new TModelColor(false);
    private final TModelColor darkColorTableModel = new TModelColor(true);

    public PanelEinstellungenColor() {
        initComponents();
        init();
    }

    /**
     * Force update of the user interface.
     */
    public void updateGui() {
        try {
            MediathekGui.ui().setupAlternatingRowColors();
            SwingUtilities.updateComponentTreeUI(MediathekGui.ui());
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
        }
    }

    private void init() {
        configureTable(jTableLight, lightColorTableModel);
        configureTable(jTableDark, darkColorTableModel);
        jTabbedPane1.addChangeListener(_ -> updateResetButtonText());
        jButtonReset.addActionListener(_ -> resetSelectedThemeColors());
        updateResetButtonText();
    }

    private void configureTable(JTable table, TModelColor model) {
        table.addMouseListener(new BeobMausTabelle(table, model));
        table.setDefaultRenderer(MVC.class, new CellRendererColor());
        table.setModel(model);
    }

    private void getColor(MVC mvc, boolean darkMode) {
        var selectedColor = JColorChooser.showDialog(this,"Farbe auswählen", mvc.getColor(darkMode));
        if (selectedColor != null) {
            if (!selectedColor.equals(mvc.getColor(darkMode))) {
                mvc.setColor(darkMode, selectedColor);
                refreshColorTables();
                updateGui();
                MVColor.save();
            }
        }
    }

    private void refreshColorTables() {
        lightColorTableModel.fireTableDataChanged();
        darkColorTableModel.fireTableDataChanged();
    }

    private void resetSelectedThemeColors() {
        boolean darkMode = jTabbedPane1.getSelectedIndex() == 1;
        for (MVC mvc : MVColor.getColors()) {
            mvc.reset(darkMode);
        }
        refreshColorTables();
        updateGui();
        MVColor.save();
    }

    private void updateResetButtonText() {
        jButtonReset.setText(jTabbedPane1.getSelectedIndex() == 1
                ? "Dunkle Farben zurücksetzen"
                : "Helle Farben zurücksetzen");
    }

    public class BeobMausTabelle extends MouseAdapter {
        private final JTable table;
        private final TModelColor model;

        public BeobMausTabelle(JTable table, TModelColor model) {
            this.table = table;
            this.model = model;
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    Point p = arg0.getPoint();
                    int row = table.rowAtPoint(p);
                    int column = table.columnAtPoint(p);
                    if (row >= 0) {
                        MVC mvc = model.getEntry(table.convertRowIndexToModel(row));
                        if (table.convertColumnIndexToModel(column) == MVColor.MVC_COLOR) {
                            getColor(mvc, model.isDarkMode());
                        }
                    }
                }
            }
        }

    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jTabbedPane1 = new JTabbedPane();
        var panelLight = new JPanel();
        var scrollPaneLight = new JScrollPane();
        jTableLight = new JTable();
        var panelDark = new JPanel();
        var scrollPaneDark = new JScrollPane();
        jTableDark = new JTable();
        var hSpacer1 = new JPanel(null);
        jButtonReset = new JButton();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .fill().gap()
                .grow().fill().gap()
                .fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jTabbedPane1 ========
        {
            //======== panelLight ========
            {
                panelLight.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .grow().fill()));

                //======== scrollPaneLight ========
                {
                    scrollPaneLight.setViewportView(jTableLight);
                }
                panelLight.add(scrollPaneLight, new CC().cell(0, 0));
            }
            jTabbedPane1.addTab("Hell", panelLight);

            //======== panelDark ========
            {
                panelDark.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .grow().fill()));

                //======== scrollPaneDark ========
                {
                    scrollPaneDark.setViewportView(jTableDark);
                }
                panelDark.add(scrollPaneDark, new CC().cell(0, 0));
            }
            jTabbedPane1.addTab("Dunkel", panelDark);
        }
        add(jTabbedPane1, new CC().cell(1, 0, 2, 1));
        add(hSpacer1, new CC().cell(1, 1));

        //---- jButtonReset ----
        jButtonReset.setText("Helle Farben zur\u00fccksetzen"); //NON-NLS
        add(jButtonReset, new CC().cell(2, 1));
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JTabbedPane jTabbedPane1;
    private JTable jTableLight;
    private JTable jTableDark;
    private JButton jButtonReset;
    // End of variables declaration//GEN-END:variables
}
