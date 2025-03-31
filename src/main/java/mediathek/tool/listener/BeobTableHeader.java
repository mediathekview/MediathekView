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
    private final MVConfig.Configs configKey;
    private JCheckBoxMenuItem[] box;

    /**
     * Context Menu for manipulation of table visual appearance from table header.
     *
     * @param tabelle Attach mouse context handler to this table.
     * @param spalten Which columns shall be displayed.
     * @param hiddenColumns Column indices which should NOT be displayed.
     * @param bbutton Column indices which are supposed to be buttons.
     * @param displaySenderIconMenus Let user manipulate the display of sender icons.
     * @param configKey If not NULL, store config setting for LINEBREAK in this key.
     *                  If NULL, do not store/restore values and do not show LINEBREAK context menu entries.
     */
    public BeobTableHeader(@NotNull MVTable tabelle, boolean[] spalten, int[] hiddenColumns, int[] bbutton, boolean displaySenderIconMenus, MVConfig.Configs configKey) {
        this.tabelle = tabelle;
        this.displaySenderIconMenus = displaySenderIconMenus;
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
        if (displaySenderIconMenus) {
            jPopupMenu.addSeparator();

            final JCheckBoxMenuItem item3 = new JCheckBoxMenuItem("Sendericons anzeigen");
            item3.setSelected(tabelle.showSenderIcons());
            item3.addActionListener(e -> toggleSenderIconDisplay(item3.isSelected()));
            jPopupMenu.add(item3);

            final JCheckBoxMenuItem item2 = new JCheckBoxMenuItem("kleine Sendericons anzeigen");
            item2.setSelected(tabelle.useSmallSenderIcons);
            if (!tabelle.showSenderIcons()) {
                item2.setEnabled(false);
            } else {
                item2.addActionListener(e -> {
                    tabelle.useSmallSenderIcons = item2.isSelected();
                    setSpalten();
                });
            }
            jPopupMenu.add(item2);
        }

        jPopupMenu.addSeparator();
        if (configKey != null) {
            // Tabellenspalten umbrechen
            JCheckBoxMenuItem itemBr = new JCheckBoxMenuItem("Zeilen umbrechen");
            itemBr.setSelected(tabelle.isLineBreak());
            itemBr.addActionListener(e -> {
                tabelle.setLineBreak(itemBr.isSelected());
                MVConfig.add(configKey, Boolean.toString(itemBr.isSelected()));
                setSpalten();
            });
            jPopupMenu.add(itemBr);

            jPopupMenu.addSeparator();
        }

        // Tabellenspalten zurücksetzen
        var miResetColumns = new JMenuItem("Spalten zurücksetzen");
        miResetColumns.addActionListener(e -> tabelle.resetTabelle());
        jPopupMenu.add(miResetColumns);

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

}
