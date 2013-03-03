package com.explodingpixels.widgets;

import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.plaf.basic.BasicButtonUI;

/**
 * A button backed by an image. Additionally, a click mask can be provided. Any fully
 * non-transparent pixels in the mask will not be clickable.
 */
public class ImageButton extends JButton {

    // create a static index for the alpha channel of a raster image. i'm not exactly sure where
    // it's specified that red = channel 0, green = channel 1, blue = channel 2, and
    // alpha = channel 3, but this have been the values i've observed.
    private static final int ALPHA_BAND = 3;

    // a buffered image representing the mask for this button.
    private final BufferedImage fMask;

    private Icon fInactiveIcon;

    /**
     * Creates an image based button.
     *
     * @param icon the icon to use for the button.
     */
    public ImageButton(Icon icon) {
        this(icon, icon);
    }

    /**
     * Creates an image based button with the given click mask.
     *
     * @param icon the icon to use for the button.
     * @param mask the click mask to use for the button.
     * @throws IllegalArgumentException if the given icon is null, the given mask is null or
     *                                  the given mask's bounds do not match the given icons bounds.
     */
    public ImageButton(Icon icon, Icon mask) {
        super(icon);

        if (icon == null) {
            throw new IllegalArgumentException("The icon cannot be null.");
        }

        if (mask == null) {
            throw new IllegalArgumentException("The mask cannot be null.");
        }

        checkIconMatchesMaskBounds(icon, mask);

        // remove the margins from this button, request that the content area not be filled, and
        // indicate that the border not be painted.
        setMargin(new Insets(0, 0, 0, 0));
        setBorder(BorderFactory.createEmptyBorder());
        setContentAreaFilled(false);

        // create the mask from the supplied icon.
        fMask = createMask(mask);

        // repaint this button when the parent window's focus state changes so
        // that we can correctly show the active or inactive icon.
        WindowUtils.installJComponentRepainterOnWindowFocusChanged(this);
    }

    private BufferedImage createMask(Icon mask) {
        // create a BufferedImage to paint the mask into so that we can later retrieve pixel data
        // out of the image.
        BufferedImage image = new BufferedImage(
                mask.getIconWidth(), mask.getIconHeight(), BufferedImage.TYPE_INT_ARGB);

        Graphics graphics = image.getGraphics();
        mask.paintIcon(null, graphics, 0, 0);
        graphics.dispose();

        return image;
    }

    public Icon getIcon() {
        return WindowUtils.isParentWindowFocused(this) || fInactiveIcon == null
                ? super.getIcon() : fInactiveIcon;
    }

    @Override
    public void setIcon(Icon defaultIcon) {
        super.setIcon(defaultIcon);
        // if this class has already been initialized, ensure that the new icon matches the bounds
        // of the current mask.
        if (fMask != null) {
            checkIconMatchesMaskBounds(defaultIcon, new ImageIcon(fMask));
        }
    }

    public void setInactiveIcon(Icon inactiveIcon) {
        checkIconMatchesMaskBounds(inactiveIcon, new ImageIcon(fMask));
        fInactiveIcon = inactiveIcon;
    }

    @Override
    public void updateUI() {
        // install the custom ui delegate to track the icon rectangle and answer the contains
        // method.
        setUI(new CustomButtonUI());
    }

    private static void checkIconMatchesMaskBounds(Icon icon, Icon mask) {
        if (mask.getIconWidth() != icon.getIconWidth()
                || mask.getIconHeight() != icon.getIconHeight()) {
            throw new IllegalArgumentException("The mask must be the same size as the icon.");
        }
    }

    // CustomButtonUI implementation so that we can maintain the icon rectangle. //////////////////

    private class CustomButtonUI extends BasicButtonUI {

        private Rectangle fIconRect;

        private boolean maskContains(int x, int y) {
            return fIconRect != null && fIconRect.contains(x, y)
                    && fMask.getRaster().getSample(x - fIconRect.x, y - fIconRect.y, ALPHA_BAND) > 0;
        }

        @Override
        public boolean contains(JComponent c, int x, int y) {
            return maskContains(x, y);
        }

        @Override
        protected void paintIcon(Graphics g, JComponent c, Rectangle iconRect) {
            super.paintIcon(g, c, iconRect);
            // capture where the icon is being painted within the bounds of this button so we can
            // later use this information in the contains calculation.
            if (fIconRect == null || !fIconRect.equals(iconRect)) {
                // create a copy of the icon rectangle, as the given iconRect is a static variable
                // in BasicButtonUI that will be updated for each button painted.
                fIconRect = new Rectangle(iconRect);
            }
        }
    }

}
