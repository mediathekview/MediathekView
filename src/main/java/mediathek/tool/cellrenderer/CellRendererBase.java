package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Base class for all cell renderer.
 */
public class CellRendererBase extends DefaultTableCellRenderer {
    /**
     * Stores the pre-scaled icon for a specific sender and a specific cell dimension.
     * Will get evicted automatically in order to not store too many useless objects.
     */
    private final SelfEvictingSenderIconCache senderCellIconCache = new SelfEvictingSenderIconCache();

    /**
     * Draws the sender icon in the sender model column.
     *
     * @param sender Name of the sender.
     */
    protected void setSenderIcon(@NotNull String sender, @NotNull Dimension targetDim) {
        var key = new SenderCacheKey(sender, targetDim);
        final AtomicReference<Icon> cachedIcon = new AtomicReference<>();
        cachedIcon.set(senderCellIconCache.getOrDefault(key, null));
        if (cachedIcon.get() == null) {
            MVSenderIconCache.get(sender).ifPresentOrElse(icon -> {
                var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                var destDim = GuiFunktionen.calculateFittedDimension(imageDim, targetDim);
                cachedIcon.set(new ScaledImageIcon(icon, destDim.width, destDim.height));
                senderCellIconCache.put(key, cachedIcon.get());
            }, () -> cachedIcon.set(null));
        }

        if (cachedIcon.get() != null) {
            setHorizontalAlignment(SwingConstants.CENTER);
            setText("");
            setIcon(cachedIcon.get());
        }
    }

    /**
     * Calculate the dimensions of a table cell for the sender icon.
     *
     * @param table            where it will be displayed.
     * @param row              the used row index.
     * @param columnModelIndex the used column index.
     * @return the calculated dimension of the available table cell.
     */
    protected Dimension getSenderCellDimension(@NotNull JTable table, int row, int columnModelIndex) {
        Dimension targetDim = new Dimension();
        targetDim.height = table.getRowHeight(row);
        targetDim.width = table.getColumnModel().getColumn(columnModelIndex).getWidth();
        targetDim.height -= 4;
        targetDim.width -= 4;
        return targetDim;
    }
}
