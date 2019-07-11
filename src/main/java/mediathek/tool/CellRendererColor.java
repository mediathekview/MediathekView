package mediathek.tool;

import mediathek.config.Daten;
import mediathek.config.MVColor;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererColor extends DefaultTableCellRenderer {
    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setBackground(null);
        setHorizontalAlignment(SwingConstants.LEADING);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            MVC color = Daten.mVColor.liste.get(r);
            if (c == MVColor.MVC_COLOR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setBackground(color.color);
                setText("");
            }
        } catch (Exception ex) {
            Log.errorLog(630365892, ex);
        }
        return this;
    }
}
