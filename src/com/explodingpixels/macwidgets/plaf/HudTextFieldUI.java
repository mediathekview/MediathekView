package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.plaf.basic.BasicTextFieldUI;
import javax.swing.text.JTextComponent;

import com.explodingpixels.macwidgets.WidgetBaseColors;

/**
 * Creates a Heads Up Display (HUD) style text field, similar to that seen in various iApps (e.g.
 * iPhoto).
 * <br>
 * <img src="../../../../../graphics/HUDTextFieldUI.png">
 */
public class HudTextFieldUI extends BasicTextFieldUI {

    private boolean isDarkColorScheme = true;
    private Color fontColor = WidgetBaseColors.DARK_FONT_COLOR;

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);

    	if (isDarkColorScheme)
    		fontColor = WidgetBaseColors.DARK_FONT_COLOR;
    	else
    		fontColor = WidgetBaseColors.LIGHT_FONT_COLOR;

        JTextComponent textComponent = (JTextComponent) c;

        textComponent.setOpaque(false);
        textComponent.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(HudPaintingUtils.BORDER_COLOR),
                BorderFactory.createEmptyBorder(1, 2, 1, 2)));
        textComponent.setBackground(new Color(0, 0, 0, 0));
        textComponent.setForeground(fontColor);
        textComponent.setFont(HudPaintingUtils.getHudFont());
        textComponent.setSelectedTextColor(Color.BLACK);
        textComponent.setSelectionColor(fontColor);
        textComponent.setCaretColor(fontColor);
    }

    @Override
    protected void paintSafely(Graphics graphics) {
        ((Graphics2D) graphics).setRenderingHint(
                RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        super.paintSafely(graphics);
    }
}
