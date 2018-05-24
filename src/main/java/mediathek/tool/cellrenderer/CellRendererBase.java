package mediathek.tool.cellrenderer;

import com.jidesoft.utils.SystemInfo;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.tool.MVFont;
import mediathek.tool.MVSenderIconCache;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

/**
 * Base class for all cell renderer.
 */
public class CellRendererBase extends DefaultTableCellRenderer {
    private static final long serialVersionUID = 4187677730323830219L;
    private final MVSenderIconCache senderIconCache;
    private final Icon checkedIcon;
    private final Icon uncheckedIcon;

    public CellRendererBase(MVSenderIconCache cache) {
        super();
        senderIconCache = cache;

        checkedIcon = IconFontSwing.buildIcon(FontAwesome.CHECK, 12);
        uncheckedIcon = IconFontSwing.buildIcon(FontAwesome.MINUS, 12);
    }

    /**
     * Draws the sender icon in the sender model column.
     *
     * @param sender Name of the sender.
     */
    protected void setSenderIcon(String sender, boolean small) {
        setHorizontalAlignment(SwingConstants.CENTER);
        final ImageIcon icon = senderIconCache.get(sender, small);
        if (icon != null) {
            setText("");
            setIcon(icon);
        }
    }

    /**
     * Set the font for highlighting a selection based on operating system.
     * Disabled for OS X as it violates HIG...
     *
     * @param c          component where font needs to be changed.
     * @param isSelected is the component selected
     */
    protected void setSelectionFont(final Component c, final boolean isSelected) {
        if (!SystemInfo.isMacOSX()) {
            final Font font;
            if (isSelected)
                font = new Font("Dialog", Font.BOLD, MVFont.fontSize);
            else
                font = new Font("Dialog", Font.PLAIN, MVFont.fontSize);

            c.setFont(font);
        }
    }

    /**
     * Set icon either to yes or no based on condition
     *
     * @param condition yes if true, no if false
     */
    protected void setCheckedOrUncheckedIcon(final boolean condition) {
        final Icon icon;
        if (condition)
            icon = checkedIcon;
        else
            icon = uncheckedIcon;

        setIcon(icon);
    }
}
