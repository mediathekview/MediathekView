package mediathek.tool.cellrenderer;

import mediathek.daten.abo.AboTags;
import mediathek.daten.abo.DatenAbo;
import mediathek.daten.abo.FilmLengthState;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;

public class CellRendererAbo extends CellRendererBase {
    private static final Logger logger = LogManager.getLogger(CellRendererAbo.class);

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
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        try {
            var abo = (DatenAbo) table.getModel().getValueAt(table.convertRowIndexToModel(row), DatenAbo.ABO_REF);
            AboTags.fromIndex(table.convertColumnIndexToModel(column)).ifPresent(col -> {
                switch (col) {
                    case NR:
                        setHorizontalAlignment(SwingConstants.CENTER);
                        setText(Integer.toString(abo.getNr()));
                        break;
                    case NAME:
                        setText(abo.getName());
                        break;
                    case THEMA:
                        setText(abo.getThema());
                        break;
                    case TITEL:
                        setText(abo.getTitle());
                        break;
                    case MINDESTDAUER:
                        setHorizontalAlignment(SwingConstants.CENTER);
                        setText(Integer.toString(abo.getMindestDauerMinuten()));
                        break;
                    case MIN:
                        setHorizontalAlignment(SwingConstants.CENTER);
                        if (abo.getFilmLengthState() == FilmLengthState.MINIMUM)
                            setText("min");
                        else
                            setText("max");
                        break;
                    case DOWN_DATUM:
                        setHorizontalAlignment(SwingConstants.CENTER);
                        setText(abo.getDownDatum());
                        break;

                    case SENDER:
                        if (((MVTable) table).showSenderIcons()) {
                            final int columnModelIndex = table.convertColumnIndexToModel(column);
                            Dimension targetDim = getSenderCellDimension(table, row,columnModelIndex);
                            setSenderIcon(abo.getSender(), targetDim);
                        }
                        break;

                    case THEMA_TITEL:
                        setText(abo.getThemaTitel());
                        break;

                    case IRGENDWO:
                        setText(abo.getIrgendwo());
                        break;

                    case ZIELPFAD:
                        setText(abo.getZielpfad());
                        break;

                    case PSET:
                        setText(abo.getPsetName());
                        break;
                }
            });

            if (!abo.isActive())
                setFont(getFont().deriveFont(Font.ITALIC));
        } catch (Exception ex) {
            logger.error("Fehler 630365892", ex);
        }
        return this;
    }
}
