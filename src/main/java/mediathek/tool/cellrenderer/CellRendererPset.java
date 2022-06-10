package mediathek.tool.cellrenderer;

import mediathek.daten.DatenPset;
import mediathek.tool.SVGIconUtilities;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

public class CellRendererPset extends DefaultTableCellRenderer {
    private static final ImageIcon ja_16 = SVGIconUtilities.createSVGIcon("icons/fontawesome/check.svg");
    private static final Logger logger = LogManager.getLogger();

    public CellRendererPset() {
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setBackground(null);
        setForeground(null);
        setFont(null);
        setIcon(null);
        setHorizontalAlignment(SwingConstants.LEADING);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenPset datenPset = new DatenPset();
            for (int i = 0; i < DatenPset.MAX_ELEM; ++i) {
                datenPset.arr[i] = table.getModel().getValueAt(r, i).toString();
            }
            if (c == DatenPset.PROGRAMMSET_NAME) {
                setForeground(datenPset.getFarbe());
            }
            if (c == DatenPset.PROGRAMMSET_IST_ABSPIELEN) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istAbspielen()) {
                    setIcon(ja_16);
                } else {
                    setIcon(null);
                }
            }
            if (c == DatenPset.PROGRAMMSET_IST_SPEICHERN) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istSpeichern()) {
                    setIcon(ja_16);
                } else {
                    setIcon(null);
                }
            }
        } catch (Exception ex) {
            logger.error("getTableCellRendererComponent", ex);
        }
        return this;
    }
}
