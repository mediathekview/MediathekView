package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.plaf.basic.BasicGraphicsUtils;
import javax.swing.plaf.basic.BasicLabelUI;

import com.explodingpixels.macwidgets.MacFontUtils;
import com.explodingpixels.widgets.WindowUtils;

/**
 * <p>
 * A {@link BasicLabelUI} that paints a shadow under the text using the given shadow color, which
 * helps emphasize the text. The UI delegate also provides a facility for drawing a different shadow
 * color when the corresponding label's containing {@link java.awt.Window} is unfocused.
 * </p>
 * While this UI delegate can be directly installed on existing {@code JLabel}s, it is
 * recommended that you use the
 * {@link com.explodingpixels.macwidgets.MacWidgetFactory#createEmphasizedLabel(String)} or
 * {@link com.explodingpixels.macwidgets.MacWidgetFactory#makeEmphasizedLabel(JLabel, Color, Color, Color)}
 * factory methods.
 * <p>
 * Here's a close-up of an emphasized label:
 * <br>
 * <img src="../../../../../graphics/EmphasizedLabelUI.png">
 * </p>
 */
public class EmphasizedLabelUI extends BasicLabelUI {

    private Color fShadowColor;
    private Color fFocusedTextColor;
    private Color fUnfocusedTextColor;

    public static final Color DEFAULT_EMPHASIS_COLOR = new Color(255, 255, 255, 110);
    public static final Color DEFAULT_FOCUSED_FONT_COLOR = new Color(0x000000);
    public static final Color DEFAULT_UNFOCUSED_FONT_COLOR = new Color(0x3f3f3f);
    public static final Color DEFAULT_DISABLED_FONT_COLOR = new Color(0x3f3f3f);

    /**
     * Creates an {@code EmphasizedLabelUI} using the default colors.
     */
    public EmphasizedLabelUI() {
        this(DEFAULT_FOCUSED_FONT_COLOR, DEFAULT_UNFOCUSED_FONT_COLOR,
                DEFAULT_EMPHASIS_COLOR);
    }

    /**
     * Creates an {@code EmphasizedLabelUI} using the given colors.
     *
     * @param focusedTextColor   the color to draw the text with when the parent
     *                           {@link java.awt.Window} has focus.
     * @param unfocusedTextColor the color to draw the text with when the parent
     *                           {@link java.awt.Window} does not have focus.
     * @param emphasisColor      the color to draw the emphasis text with.
     */
    public EmphasizedLabelUI(Color focusedTextColor, Color unfocusedTextColor, Color emphasisColor) {
        fFocusedTextColor = focusedTextColor;
        fUnfocusedTextColor = unfocusedTextColor;
        fShadowColor = emphasisColor;

    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        c.setOpaque(false);
    }

    @Override
    public void uninstallUI(JComponent c) {
        super.uninstallUI(c);
        // TODO add uninstallation.
    }

    @Override
    protected void paintEnabledText(JLabel label, Graphics g, String s,
                                    int textX, int textY) {
        MacFontUtils.enableAntialiasing((Graphics2D) g);
        g.setColor(fShadowColor);
        g.setFont(label.getFont());
        BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY + 1);
        g.setColor(WindowUtils.isParentWindowFocused(label)
                ? fFocusedTextColor : fUnfocusedTextColor);
        BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY);
    }

    @Override
    protected void paintDisabledText(JLabel label, Graphics g, String s, int textX, int textY) {
        // TODO do use a diabled color here.
        MacFontUtils.enableAntialiasing((Graphics2D) g);
        g.setColor(fShadowColor);
        g.setFont(label.getFont());
        BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY + 1);
        g.setColor(DEFAULT_DISABLED_FONT_COLOR);
        BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY);
    }
}
