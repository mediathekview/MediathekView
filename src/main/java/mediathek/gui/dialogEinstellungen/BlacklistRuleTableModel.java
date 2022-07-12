package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.daten.blacklist.ListeBlacklist;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.AbstractTableModel;
import java.util.List;

public class BlacklistRuleTableModel extends AbstractTableModel {
    private static final int BLACKLIST_SENDER = 0;
    private static final int BLACKLIST_THEMA = 1;
    private static final int BLACKLIST_TITEL = 2;
    private static final int BLACKLIST_THEMA_TITEL = 3;
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
        return 4;
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        var rule = blacklist.get(rowIndex);
        return switch (columnIndex) {
            case BLACKLIST_SENDER -> rule.getSender();
            case BLACKLIST_THEMA -> rule.getThema();
            case BLACKLIST_TITEL -> rule.getTitel();
            case BLACKLIST_THEMA_TITEL -> rule.getThema_titel();
            default -> throw new IllegalStateException("Unexpected value: " + columnIndex);
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case BLACKLIST_SENDER -> "Sender";
            case BLACKLIST_THEMA -> "Thema";
            case BLACKLIST_TITEL -> "Titel";
            case BLACKLIST_THEMA_TITEL -> "Thema-Titel";
            default -> throw new IllegalStateException("Unexpected value: " + column);
        };
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    /**
     * Remove a BlacklistRule from model
     *
     * @param modelIndex index from blacklist to delete
     */
    public void removeRow(int modelIndex) {
        blacklist.remove(modelIndex);
        fireTableRowsDeleted(modelIndex, modelIndex);
    }

    /**
     * Remove a collection of rules.
     * Fire update after all rules have been removed.
     *
     * @param list of objects to be deleted
     */
    public void removeRules(@NotNull List<BlacklistRule> list) {
        blacklist.remove(list);
        fireTableDataChanged();
    }

    /**
     * Remove all blacklist rules.
     */
    public void removeAll() {
        blacklist.clear();
        fireTableDataChanged();
    }

    /**
     * Add a rule to the blacklist store.
     *
     * @param rule to be added.
     */
    public void addRule(@NotNull BlacklistRule rule) {
        blacklist.add(rule);
        fireTableDataChanged();
    }

    public boolean contains(@NotNull BlacklistRule rule) {
        return blacklist.contains(rule);
    }

    /**
     * Get a blacklist rule based on model index.
     *
     * @param fromModelIndex the index.
     * @return the rule.
     */
    public BlacklistRule get(int fromModelIndex) {
        return blacklist.get(fromModelIndex);
    }
}
