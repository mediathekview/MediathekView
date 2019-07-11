package mediathek.tool;

import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.daten.DatenMediaDB;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererMediaDB extends DefaultTableCellRenderer {
    private static final ImageIcon ja_16 = Icons.ICON_TABELLE_EIN;
    private static final ImageIcon nein_12 = Icons.ICON_TABELLE_AUS;

    public CellRendererMediaDB() {
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setIcon(null);
        setHorizontalAlignment(SwingConstants.LEADING);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int c = table.convertColumnIndexToModel(column);
            if (c == DatenMediaDB.MEDIA_DB_SIZE) {
                setHorizontalAlignment(SwingConstants.CENTER);
            } else if (c == DatenMediaDB.MEDIA_DB_EXTERN) {
                setHorizontalAlignment(CENTER);
                if (getText().equals(Boolean.TRUE.toString())) {
                    setIcon(ja_16);
                } else {
                    setIcon(nein_12);
                }
                setText("");
            }
        } catch (Exception ex) {
            Log.errorLog(461203547, ex);
        }
        return this;
    }
}
