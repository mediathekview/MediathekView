package mediathek.tool.listener;

import mediathek.config.MVConfig;
import mediathek.tool.table.MVTable;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Rechte Maustaste in der Tabelle (Kontextmenü)
 */
public class BeobTableHeader extends MouseAdapter {
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
    private final boolean displaySenderIconMenus;
    /**
     * Determines whether the menu should include options for selecting/deselecting all columns.
     */
    private final boolean showColumnSelectionOptions;
    private final MVConfig.Configs configKey;
    private boolean displayAll = true;
    private boolean hideAll = true;
    private JCheckBoxMenuItem[] box;
    private JCheckBoxMenuItem cbmiShowAllColumns;
    private JCheckBoxMenuItem cbmiHideAllColumns;
    private JMenuItem miResetColumns;

    /**
     * Context Menu for manipulation of table visual appearance from table header.
     *
     * @param tabelle Attach mouse context handler to this table.
     * @param spalten Which columns shall be displayed.
     * @param hiddenColumns Column indices which should NOT be displayed.
     * @param bbutton Column indices which are supposed to be buttons.
     * @param displaySenderIconMenus Let user manipulate the display of sender icons.
     * @param showColumnSelectionOptions If true, adds menu items for selecting/deselecting all columns.
     * @param configKey If not NULL, store config setting for LINEBREAK in this key.
     *                  If NULL, do not store/restore values and do not show LINEBREAK context menu entries.
     */
    public BeobTableHeader(@NotNull MVTable tabelle, boolean[] spalten, int[] hiddenColumns, int[] bbutton, boolean displaySenderIconMenus, boolean showColumnSelectionOptions, MVConfig.Configs configKey) {
        this.tabelle = tabelle;
        this.displaySenderIconMenus = displaySenderIconMenus;
        spaltenAnzeigen = spalten;
        this.hiddenColumns = hiddenColumns;
        this.configKey = configKey;
        button = bbutton;
        this.showColumnSelectionOptions = showColumnSelectionOptions;

        // Dynamically query column names from table
        final var colModel = tabelle.getTableHeader().getColumnModel();
        final int colCount = colModel.getColumnCount();
        columns = new String[colCount];
        for (int index = 0; index < colCount; index++) {
            columns[index] = (String) colModel.getColumn(index).getHeaderValue();
        }

        createStaticMenuEntries();
    }

    public BeobTableHeader(@NotNull MVTable tabelle, boolean[] spalten, int[] hiddenColumns, int[] bbutton, boolean displaySenderIconMenus, MVConfig.Configs configKey) {
        this(tabelle, spalten, hiddenColumns, bbutton, displaySenderIconMenus, false, configKey);
    }

    private void createStaticMenuEntries() {
        miResetColumns = new JMenuItem("Spalten zurücksetzen");
        miResetColumns.addActionListener(e -> tabelle.resetTabelle());
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

    protected void toggleSenderIconDisplay(boolean isSelected) {
        tabelle.setShowIcon(isSelected);
        setSpalten();
    }

    protected JPopupMenu prepareMenu() {
        JPopupMenu jPopupMenu = new JPopupMenu();
        if (showColumnSelectionOptions) {
            cbmiShowAllColumns = new JCheckBoxMenuItem("Alle auswählen");
            cbmiShowAllColumns.setSelected(alleAnzeigen());
            jPopupMenu.add(cbmiShowAllColumns);
            cbmiHideAllColumns = new JCheckBoxMenuItem("Alle abwählen");
            cbmiHideAllColumns.setSelected(alleVerstecken());
            jPopupMenu.add(cbmiHideAllColumns);
            jPopupMenu.addSeparator();
        }

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

        jPopupMenu.addSeparator();
        jPopupMenu.add(miResetColumns);

        return jPopupMenu;
    }

    private boolean alleAnzeigen() {
        return displayAll;
    }

    private boolean alleVerstecken() {
        return hideAll;
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
                if (showColumnSelectionOptions) {
                    if (!box[i].isSelected()) {
                        displayAll = false;
                        cbmiShowAllColumns.setSelected(false);
                    }
                    if (box[i].isSelected()) {
                        hideAll = false;
                        cbmiHideAllColumns.setSelected(false);
                    }
                }
            }
        }
        tabelle.spaltenEinAus();
        tabelle.calculateRowHeight();
    }

    protected void setSpalten(int k, boolean anz) {
        spaltenAnzeigen[k] = anz;
        tabelle.spaltenEinAus();
    }
}
