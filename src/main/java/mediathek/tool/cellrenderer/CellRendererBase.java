package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import org.apache.commons.lang3.SystemUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

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
        var cachedIcon = senderCellIconCache.getOrDefault(key, null);
        if (cachedIcon == null) {
            //creeate icon and store in cache
            var origIcon = MVSenderIconCache.get(sender);
            if (origIcon.isPresent()) {
                var icon = origIcon.get();
                Dimension iconDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                var scaleDim = getScaledDimension(iconDim, targetDim);
                Icon imgIcon;
                //Of course Windows is again the only OS which sucks at automatic scaling...
                if (SystemUtils.IS_OS_WINDOWS) {
                    imgIcon = new ScaledImageIcon(new ImageIcon(icon.getImage()), scaleDim.width, scaleDim.height);
                } else {
                    Image newimg = icon.getImage().getScaledInstance(scaleDim.width, scaleDim.height, Image.SCALE_SMOOTH);
                    imgIcon = new ImageIcon(newimg);
                }

                cachedIcon = imgIcon;
                senderCellIconCache.put(key, cachedIcon);
            }
        }

        if (cachedIcon != null) {
            setHorizontalAlignment(SwingConstants.CENTER);
            setText("");
            setIcon(cachedIcon);
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

    /**
     * Calculate the target dimensions based on image size and a boundary.
     *
     * @param imageSize the size of the original image.
     * @param boundary  the boundary size.
     * @return the target boundary while maintaining aspect ratio.
     */
    protected Dimension getScaledDimension(@NotNull Dimension imageSize, @NotNull Dimension boundary) {

        double widthRatio = boundary.getWidth() / imageSize.getWidth();
        double heightRatio = boundary.getHeight() / imageSize.getHeight();
        double ratio = Math.min(widthRatio, heightRatio);

        return new Dimension((int) (imageSize.width * ratio),
                (int) (imageSize.height * ratio));
    }


}
