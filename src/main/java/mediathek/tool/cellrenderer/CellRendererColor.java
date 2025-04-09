package mediathek.tool.cellrenderer;

import mediathek.config.Daten;
import mediathek.tool.MVC;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

public class CellRendererColor extends DefaultTableCellRenderer {
    private static final Logger logger = LogManager.getLogger();

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
                                                   boolean hasFocus, int row, int column) {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        final int r = table.convertRowIndexToModel(row);

        try {
            MVC color = Daten.mVColor.liste.get(r);
            setHorizontalAlignment(SwingConstants.CENTER);
            setBackground(color.color);
            setText("");
        } catch (IndexOutOfBoundsException ex) {
            logger.error("unable to get color", ex);
        }
        return this;
    }
}
