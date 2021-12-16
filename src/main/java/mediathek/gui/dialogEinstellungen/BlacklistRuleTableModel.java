package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.daten.blacklist.ListeBlacklist;

import javax.swing.table.AbstractTableModel;
import java.util.List;

public class BlacklistRuleTableModel extends AbstractTableModel {
    private final ListeBlacklist blacklist;

    public BlacklistRuleTableModel() {
        this.blacklist = Daten.getInstance().getListeBlacklist();
    }

    @Override
    public int getRowCount() {
        return blacklist.size();
    }

    @Override
    public int getColumnCount() {
        return 5;
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        var rule = blacklist.get(rowIndex);
        return switch (columnIndex) {
            case BlacklistRule.BLACKLIST_NR -> rule.arr[BlacklistRule.BLACKLIST_NR];
            case BlacklistRule.BLACKLIST_SENDER -> rule.arr[BlacklistRule.BLACKLIST_SENDER];
            case BlacklistRule.BLACKLIST_THEMA -> rule.arr[BlacklistRule.BLACKLIST_THEMA];
            case BlacklistRule.BLACKLIST_TITEL -> rule.arr[BlacklistRule.BLACKLIST_TITEL];
            case BlacklistRule.BLACKLIST_THEMA_TITEL -> rule.arr[BlacklistRule.BLACKLIST_THEMA_TITEL];
            case 5 -> rule;
            default -> throw new IllegalStateException("Unexpected value: " + columnIndex);
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case BlacklistRule.BLACKLIST_NR -> "Nr";
            case BlacklistRule.BLACKLIST_SENDER -> "Sender";
            case BlacklistRule.BLACKLIST_THEMA -> "Thema";
            case BlacklistRule.BLACKLIST_TITEL -> "Titel";
            case BlacklistRule.BLACKLIST_THEMA_TITEL -> "Thema-Titel";
            default -> throw new IllegalStateException("Unexpected value: " + column);
        };
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    /**
     * Remove a BlacklistRule from model
     * @param modelIndex index from blacklist to delete
     */
    public void removeRow(int modelIndex) {
        blacklist.remove(modelIndex);
        fireTableRowsDeleted(modelIndex,modelIndex);
    }

    /**
     * Remove a collection of rules.
     * Fire update after all rules have been removed.
     * @param list of objects to be deleted
     */
    public void removeRows(List<BlacklistRule> list) {
        blacklist.remove(list);
        fireTableDataChanged();
    }
}
