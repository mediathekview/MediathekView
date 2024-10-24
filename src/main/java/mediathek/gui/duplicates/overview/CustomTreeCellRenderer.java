package mediathek.gui.duplicates.overview;

import mediathek.daten.DatenFilm;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import java.awt.*;

class CustomTreeCellRenderer
        extends DefaultTreeCellRenderer {
    public Component getTreeCellRendererComponent(
            JTree tree,
            Object value,
            boolean selected,
            boolean expanded,
            boolean leaf,
            int row,
            boolean hasFocus) {
        // Allow the original renderer to set up the label
        Component c = super.getTreeCellRendererComponent(
                tree, value, selected,
                expanded, leaf, row,
                hasFocus);

        var node = (DefaultMutableTreeNode) value;
        if (node.isRoot())
            return c;

        switch (node.getUserObject()) {
            case DatenFilm f -> {
                setText(f.getTitle());
                setToolTipText(prepareTooltipText(f));
            }
            case String s -> setText(String.format("%s (%d)", s, node.getChildCount()));
            default -> setText(value.toString());
        }

        return c;
    }

    private String prepareTooltipText(DatenFilm f) {
        var s = """
                <html>
                <b>Thema:</b> %s<br>
                <b>Titel:</b> %s<br>
                <b>gesendet:</b> %s %s<br>
                </html>
                """;
        return String.format(s, f.getThema(), f.getTitle(),
                f.getSendeDatum(), f.getSendeZeit());
    }
}
