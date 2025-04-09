package raven.toast.ui;

import com.formdev.flatlaf.FlatPropertiesLaf;
import com.formdev.flatlaf.ui.FlatStylingSupport.Styleable;
import com.formdev.flatlaf.ui.FlatUIUtils;
import com.formdev.flatlaf.util.UIScale;
import raven.toast.util.ShadowRenderer;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * @author Raven
 */
public class DropShadowBorder extends EmptyBorder {

    @Styleable
    protected Color shadowColor;
    @Styleable
    protected Insets shadowInsets;
    @Styleable
    protected float shadowOpacity;

    private Image shadowImage;
    private final int shadowSize;
    private Color lastShadowColor;
    private float lastShadowOpacity;
    private int lastShadowSize;
    private int lastArc;
    private int lastWidth;
    private int lastHeight;

    public DropShadowBorder() {
        this(new Color(0, 0, 0), new Insets(0, 0, 6, 6), 0.1f);
    }

    public DropShadowBorder(Color shadowColor, Insets shadowInsets, float shadowOpacity) {
        super(nonNegativeInsets(shadowInsets));

        this.shadowColor = shadowColor;
        this.shadowInsets = shadowInsets;
        this.shadowOpacity = shadowOpacity;

        this.shadowSize = maxInset(shadowInsets);
    }

    private static Insets nonNegativeInsets(Insets shadowInsets) {
        return new Insets(Math.max(shadowInsets.top, 0), Math.max(shadowInsets.left, 0), Math.max(shadowInsets.bottom, 0), Math.max(shadowInsets.right, 0));
    }

    private int maxInset(Insets shadowInsets) {
        return Math.max(Math.max(shadowInsets.left, shadowInsets.right), Math.max(shadowInsets.top, shadowInsets.bottom));
    }

    @Override
    public Insets getBorderInsets() {
        return UIScale.scale(super.getBorderInsets());
    }

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        JComponent com = (JComponent) c;
        int arc = FlatPropertiesLaf.getStyleableValue(com, "arc");
        boolean useEffect = FlatPropertiesLaf.getStyleableValue(com, "useEffect");
        if (shadowImage == null || !shadowColor.equals(lastShadowColor) || width != lastWidth || height != lastHeight || shadowSize != lastShadowSize || shadowOpacity != lastShadowOpacity || arc != lastArc) {
            shadowImage = createShadowImage(width, height, arc);

            lastShadowColor = shadowColor;
            lastWidth = width;
            lastHeight = height;
            lastShadowSize = shadowSize;
            lastShadowOpacity = shadowOpacity;
            lastArc = arc;
        }
        g.drawImage(shadowImage, 0, 0, null);
        Insets insets = getBorderInsets();
        int lx = insets.left;
        int ly = insets.top;
        int lw = width - (insets.left + insets.right);
        int lh = height - (insets.top + insets.bottom);
        Graphics2D g2 = (Graphics2D) g.create();
        if (arc > 0) {
            FlatUIUtils.setRenderingHints(g2);
            g2.setColor(c.getBackground());
            FlatUIUtils.paintComponentBackground(g2, lx, ly, lw, lh, 0, UIScale.scale(arc));
        } else {
            g2.setColor(c.getBackground());
            g2.fillRect(lx, ly, lw, lh);
        }
        if (useEffect) {
            createEffect(com, g2, lx, ly, lw, lh, arc);
        }
        int outlineWidth = FlatPropertiesLaf.getStyleableValue(com, "outlineWidth");
        if (outlineWidth > 0) {
            Color outlineColor = FlatPropertiesLaf.getStyleableValue(com, "outlineColor");
            g2.setColor(outlineColor);
            FlatUIUtils.paintOutline(g2, lx, ly, lw, lh, UIScale.scale(outlineWidth), UIScale.scale(arc));
        }
        g2.dispose();
    }

    private void createEffect(JComponent c, Graphics2D g2, int x, int y, int width, int height, int arc) {
        Color effectColor = FlatPropertiesLaf.getStyleableValue(c, "effectColor");
        float effectWidth = FlatPropertiesLaf.getStyleableValue(c, "effectWidth");
        float effectOpacity = FlatPropertiesLaf.getStyleableValue(c, "effectOpacity");
        boolean effectRight = FlatPropertiesLaf.getStyleableValue(c, "effectAlignment").equals("right");
        if (!effectRight) {
            g2.setPaint(new GradientPaint(x, 0, effectColor, x + (width * effectWidth), 0, c.getBackground()));
        } else {
            g2.setPaint(new GradientPaint(x + width, 0, effectColor, x + width - (width * effectWidth), 0, c.getBackground()));
        }
        g2.setComposite(AlphaComposite.SrcOver.derive(effectOpacity));
        if (arc > 0) {
            FlatUIUtils.paintComponentBackground(g2, x, y, width, height, 0, UIScale.scale(arc));
        } else {
            g2.fillRect(x, y, width, height);
        }
        g2.setComposite(AlphaComposite.SrcOver);
    }

    private BufferedImage createShadowImage(int width, int height, int arc) {
        int size = UIScale.scale(shadowSize);
        float round = UIScale.scale(arc * 0.7f);
        int shadowWidth = width - size * 2;
        int shadowHeight = height - size * 2;
        Shape shape = FlatUIUtils.createRoundRectanglePath(0, 0, shadowWidth, shadowHeight, round, round, round, round);
        return new ShadowRenderer(size, shadowOpacity, shadowColor).createShadow(shape);
    }
}

