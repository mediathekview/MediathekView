package com.explodingpixels.macwidgets;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.UIManager;

public class MacFontUtils {

    public static final Font ITUNES_FONT =
            UIManager.getFont("Table.font").deriveFont(11.0f);

    public static final Font ITUNES_TABLE_HEADER_FONT =
            UIManager.getFont("Table.font").deriveFont(Font.BOLD, 11.0f);

    public static final Font TOOLBAR_LABEL_FONT =
            UIManager.getFont("Label.font").deriveFont(11.0f);

    public static void enableAntialiasing(Graphics2D g2) {
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    }
}
