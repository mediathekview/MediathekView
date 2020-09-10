package mediathek.tool.cellrenderer;

import mediathek.tool.MVSenderIconCache;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
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
}
