package com.explodingpixels.macwidgets.plaf;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ButtonModel;
import javax.swing.JComponent;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import com.explodingpixels.macwidgets.MacColorUtils;
import com.explodingpixels.macwidgets.MacFontUtils;

public class UnifiedToolbarButtonUI extends BasicButtonUI {

    private static final Color PRESSED_BUTTON_MASK_COLOR = new Color(0, 0, 0, 128);

    private static final Color DISABLED_BUTTON_MASK_COLOR = new Color(255, 255, 255, 128);

    @Override
    protected void installDefaults(AbstractButton b) {
        super.installDefaults(b);

        // TODO save original values.

        b.setHorizontalTextPosition(AbstractButton.CENTER);
        b.setVerticalTextPosition(AbstractButton.BOTTOM);
        b.setIconTextGap(0);
        b.setBorder(BorderFactory.createEmptyBorder(0,0,0,0));
        b.setOpaque(false);
        b.setFocusable(false);
        // TODO make the font derivation more robust.
        b.setFont(UIManager.getFont("Button.font").deriveFont(11.0f));
    }

    @Override
    protected void uninstallDefaults(AbstractButton b) {
        super.uninstallDefaults(b);
        // TODO implement.
    }

    @Override
    protected void paintIcon(Graphics g, JComponent c, Rectangle iconRect) {

        AbstractButton b = (AbstractButton) c;
        ButtonModel model = b.getModel();

        // create a buffered image to draw the icon and mask into.
        BufferedImage image = new BufferedImage(iconRect.width, iconRect.height,
                BufferedImage.TYPE_INT_ARGB);
        // create a graphics context from the buffered image.
        Graphics2D graphics = (Graphics2D) image.getGraphics();
        // paint the icon into the buffered image.
        b.getIcon().paintIcon(c, graphics, 0, 0);

        // set the composite on the graphics context to SrcAtop which blends the
        // source with the destination, and thus transparent pixels in the
        // destination, remain transparent.
        graphics.setComposite(AlphaComposite.SrcAtop);

        // set the mask color based on the button models state.
        if (!model.isEnabled()) {
            graphics.setColor(DISABLED_BUTTON_MASK_COLOR);
        } else if (model.isArmed()) {
            graphics.setColor(PRESSED_BUTTON_MASK_COLOR);
        } else {
            graphics.setColor(new Color(0, 0, 0, 0));
        }

        // fill a rectangle with the mask color.
        graphics.fillRect(0, 0, iconRect.width, iconRect.height);

        graphics.dispose();
        g.drawImage(image, iconRect.x, iconRect.y, null);
    }

    @Override
    protected void paintText(Graphics g, JComponent c, Rectangle textRect, String text) {
        MacFontUtils.enableAntialiasing((Graphics2D) g);
        Graphics2D graphics = (Graphics2D) g.create();

        AbstractButton b = (AbstractButton) c;
        ButtonModel model = b.getModel();
        FontMetrics fm = c.getFontMetrics(c.getFont());

        // 1) Draw the emphasis text.
        graphics.setColor(model.isArmed()
                ? MacColorUtils.EMPTY_COLOR
                : EmphasizedLabelUI.DEFAULT_EMPHASIS_COLOR);
        BasicGraphicsUtils.drawStringUnderlineCharAt(graphics, text, -1,
                textRect.x, textRect.y + 1 + fm.getAscent());

        // 2) Draw the text.
        graphics.setColor(model.isEnabled()
                ? EmphasizedLabelUI.DEFAULT_FOCUSED_FONT_COLOR
                : EmphasizedLabelUI.DEFAULT_DISABLED_FONT_COLOR);
        BasicGraphicsUtils.drawStringUnderlineCharAt(graphics, text, -1,
                textRect.x, textRect.y + fm.getAscent());

        graphics.dispose();
    }
}
