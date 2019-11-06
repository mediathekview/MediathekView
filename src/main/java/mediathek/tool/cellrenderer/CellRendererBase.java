package mediathek.tool.cellrenderer;

import mediathek.tool.MVSenderIconCache;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;
import java.util.Optional;

/**
 * Base class for all cell renderer.
 */
public class CellRendererBase extends DefaultTableCellRenderer {
    private static final long serialVersionUID = 4187677730323830219L;
    private final MVSenderIconCache senderIconCache;

    public CellRendererBase(MVSenderIconCache cache) {
        super();
        senderIconCache = cache;
    }

    /**
     * Draws the sender icon in the sender model column.
     *
     * @param sender Name of the sender.
     */
    protected void setSenderIcon(String sender, boolean small) {
        setHorizontalAlignment(SwingConstants.CENTER);
        final Optional<ImageIcon> optIcon = senderIconCache.get(sender, small);
        optIcon.ifPresent(icon -> {
            setText("");
            setIcon(icon);
        });
    }

    /**
     * Set the font for highlighting a selection based on operating system.
     * Disabled for macOS as it violates HIG...
     *
     * @param c          component where font needs to be changed.
     * @param isSelected is the component selected
     */
    protected void setSelectionFont(final Component c, final boolean isSelected) {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            final Font font;
            if (isSelected)
                font = c.getFont().deriveFont(Font.BOLD);
            else
                font = c.getFont().deriveFont(Font.PLAIN);

            c.setFont(font);
        }
    }
}
