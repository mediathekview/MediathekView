package com.explodingpixels.macwidgets.plaf;

import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import com.explodingpixels.macwidgets.WidgetBaseColors;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils.Roundedness;

/**
 * Creates a Heads Up Display (HUD) style button, similar to that seen in various iApps (e.g.
 * iPhoto).
 * <br>
 * <img src="../../../../../graphics/HUDButtonUI.png">
 */
public class HudButtonUI extends BasicButtonUI {

    private final HudPaintingUtils.Roundedness fRoundedness;

    private static final int TOP_AND_BOTTOM_MARGIN = 2;

    private static final int LEFT_AND_RIGHT_MARGIN = 16;
    
    private boolean isDarkColorScheme = true;
    
    HudComboBoxUI hudComboBoxUI = null;

    /**
     * Creates a HUD style {@link javax.swing.plaf.ButtonUI} with full rounded edges.
     */
    public HudButtonUI() {
        this(HudPaintingUtils.Roundedness.ROUNDED_BUTTON);
    }

    /**
     * Creates a HUD style {@link javax.swing.plaf.ButtonUI} with full rounded edges.
     */
    public HudButtonUI(boolean isDarkColorScheme) {
        this(HudPaintingUtils.Roundedness.ROUNDED_BUTTON);
        
        this.isDarkColorScheme = isDarkColorScheme;
    }
    
    /**
     * Creates a HUD style {@link javax.swing.plaf.ButtonUI} with the given edge rounded ness.
     *
     * @param roundedness the rounded style to use for the button edges.
     */
    public HudButtonUI(HudPaintingUtils.Roundedness roundedness) {
        fRoundedness = roundedness;
    }

    /**
     * Creates a HUD style {@link javax.swing.plaf.ButtonUI} with the given edge rounded ness.
     *
     * @param roundedness the rounded style to use for the button edges.
     */
    public HudButtonUI(HudPaintingUtils.Roundedness roundedness, boolean isDarkColorScheme) {
        fRoundedness = roundedness;
        this.isDarkColorScheme = isDarkColorScheme;
    }
    
    protected HudButtonUI(Roundedness roundness, HudComboBoxUI hudComboBoxUI, boolean isDarkColorScheme)
	{
        this(roundness, isDarkColorScheme);

        this.hudComboBoxUI = hudComboBoxUI;
	}

    protected HudButtonUI(Roundedness roundness, HudComboBoxUI hudComboBoxUI)
	{
		this(roundness);
        this.hudComboBoxUI = hudComboBoxUI;
	}
    
    @Override
    protected void installDefaults(AbstractButton b) {
        super.installDefaults(b);

        // TODO save original values.

        HudPaintingUtils.initHudComponent(b, isDarkColorScheme);
        b.setHorizontalTextPosition(AbstractButton.CENTER);
        int bottomMargin = TOP_AND_BOTTOM_MARGIN + HudPaintingUtils.getHudControlShadowSize(b);
        b.setBorder(BorderFactory.createEmptyBorder(TOP_AND_BOTTOM_MARGIN, LEFT_AND_RIGHT_MARGIN,
                bottomMargin, LEFT_AND_RIGHT_MARGIN));        	
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AbstractButton button = (AbstractButton) c;
        Graphics2D graphics = (Graphics2D) g;
        HudPaintingUtils.updateGraphicsToPaintDisabledControlIfNecessary(graphics, button);

        int buttonHeight = button.getHeight() - HudPaintingUtils.getHudControlShadowSize(button);
        HudPaintingUtils.paintHudControlBackground(graphics, button, button.getWidth(),
                buttonHeight, fRoundedness, isDarkColorScheme);

        if (hudComboBoxUI != null)
        	hudComboBoxUI.paint(g, c);

        super.paint(g, c);
    }

    @Override
    protected void paintText(Graphics g, AbstractButton button, Rectangle textRect, String text) {
        FontMetrics fontMetrics = g.getFontMetrics(button.getFont());
        int mnemonicIndex = button.getDisplayedMnemonicIndex();

        g.setColor(button.getForeground());
         	
        BasicGraphicsUtils.drawStringUnderlineCharAt(g, text, mnemonicIndex,
                textRect.x + getTextShiftOffset(),
                textRect.y + fontMetrics.getAscent() + getTextShiftOffset());
    }

}
