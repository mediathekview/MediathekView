package mediathek.tool.listener;

import mediathek.config.MVConfig;
import mediathek.tool.table.MVTable;
import org.jetbrains.annotations.NotNull;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class BeobTableHeaderBookmark extends MouseAdapter {
    protected final MVTable tabelle;
    private final String[] columns;
    private final boolean[] spaltenAnzeigen;
    /**
     * Column indices which should NOT be displayed
     */
    private final int[] hiddenColumns;
    /**
     * Column indices which are supposed to be buttons.
     */
    private final int[] button;
    private final MVConfig.Configs configKey;
    private JCheckBoxMenuItem[] box;
    private JMenuItem miShowColumns;
    private JMenuItem miHideColumns;
    public BeobTableHeaderBookmark(@NotNull MVTable tabelle, boolean[] spalten, int[] hiddenColumns, int[] bbutton, MVConfig.Configs configKey) {
        this.tabelle = tabelle;
        spaltenAnzeigen = spalten;
        this.hiddenColumns = hiddenColumns;
        this.configKey = configKey;
        button = bbutton;

        //dynamically query column names from table
        final var colModel = tabelle.getTableHeader().getColumnModel();
        final int colCount = colModel.getColumnCount();
        columns = new String[colCount];
        for (int index = 0; index < colCount; index++) {
            columns[index] = (String) colModel.getColumn(index).getHeaderValue();
        }

        createStaticMenuEntries();
    }

    private void createStaticMenuEntries() {
        miShowColumns = new JMenuItem("Alle auswählen");
        miShowColumns.addActionListener(e -> selectAllColumns());
        miHideColumns = new JMenuItem("Alle abwählen");
        miHideColumns.addActionListener(e -> deselectAllColumns());

    }

    @Override
    public void mousePressed(MouseEvent arg0) {
        if (arg0.isPopupTrigger()) {
            showMenu(arg0);
        }
    }

    @Override
    public void mouseReleased(MouseEvent arg0) {
        if (arg0.isPopupTrigger()) {
            showMenu(arg0);
        }
    }

    private boolean immer(int i) {
        for (int ii : hiddenColumns) {
            if (i == ii) {
                return true;
            }
        }
        return false;
    }

    protected void toggleButtonVisibility(boolean isSelected) {
        for (int i : button) {
            setSpalten(i, isSelected);
        }
    }

    protected JPopupMenu prepareMenu() {
        JPopupMenu jPopupMenu = new JPopupMenu();
        // Spalten ein-ausschalten
        box = new JCheckBoxMenuItem[this.columns.length];
        for (int i = 0; i < columns.length; ++i) {
            if (immer(i)) {
                continue;
            }
            box[i] = new JCheckBoxMenuItem(columns[i]);
            box[i].setSelected(anzeigen(i));
            box[i].addActionListener(e -> setSpalten());
            jPopupMenu.add(box[i]);
        }
        // jetzt evtl. noch die Button
        if (button.length > 0) {
            jPopupMenu.addSeparator();

            final JCheckBoxMenuItem item2 = new JCheckBoxMenuItem("Buttons anzeigen");
            item2.setSelected(anzeigen(button[0])); //entweder alle oder keiner!
            item2.addActionListener(e -> toggleButtonVisibility(item2.isSelected()));
            jPopupMenu.add(item2);
        }

        return jPopupMenu;
    }

    private void showMenu(MouseEvent evt) {
        var popupMenu = prepareMenu();
        popupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
    }

    private boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    private void setSpalten() {
        for (int i = 0; i < box.length; ++i) {
            if (box[i] != null) {
                spaltenAnzeigen[i] = box[i].isSelected();
            }
        }
        tabelle.spaltenEinAus();
        tabelle.calculateRowHeight();
    }

    protected void setSpalten(int k, boolean anz) {
        spaltenAnzeigen[k] = anz;
        tabelle.spaltenEinAus();
    }

    private void selectAllColumns() {
        TableColumnModel columnModel = tabelle.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            column.setMinWidth(50);
            column.setMaxWidth(Integer.MAX_VALUE);
            column.setWidth(100);
            column.setPreferredWidth(100);
        }
    }

    private void deselectAllColumns() {
        TableColumnModel columnModel = tabelle.getColumnModel();
        for (int i = 0; i < columnModel.getColumnCount(); i++) {
            TableColumn column = columnModel.getColumn(i);
            column.setMinWidth(0);
            column.setMaxWidth(0);
            column.setWidth(0);
            column.setPreferredWidth(0);
        }
    }

}
