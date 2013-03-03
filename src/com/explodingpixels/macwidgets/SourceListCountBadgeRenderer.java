package com.explodingpixels.macwidgets;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.image.BufferedImage;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;

import com.explodingpixels.widgets.WindowUtils;

/**
 * Renders a rounded rectangle (i.e. a badge) with a given number in the center of the rectangle.
 */
public class SourceListCountBadgeRenderer {

    private CustomJLabel fLabel = new CustomJLabel();

    private boolean fSelected = false;

    private static Font BADGE_FONT = new Font("Helvetica", Font.BOLD, 11);

    private final Color fSelectedColor;
    private final Color fActiveUnselectedColor;
    private final Color fInactiveUnselectedColor;
    private final Color fTextColor;

    /**
     * Creates a badge renderer.
     */
    public SourceListCountBadgeRenderer(Color selectedColor, Color activeUnselectedColor,
                                        Color inactiveUnselectedColor, Color textColor) {
        fLabel.setFont(BADGE_FONT);
        fLabel.setBorder(BorderFactory.createEmptyBorder(0, 6, 0, 6));

        fSelectedColor = selectedColor;
        fActiveUnselectedColor = activeUnselectedColor;
        fInactiveUnselectedColor = inactiveUnselectedColor;
        fTextColor = textColor;
    }

    /**
     * Sets the state to use when drawing the badge.
     *
     * @param count    the count value to draw in the center of the badge.
     * @param selected {@code} true if the badge should be rendered in a selected state.
     */
    public void setState(int count, boolean selected) {
        fLabel.setText(String.valueOf(count));
        fSelected = selected;
    }

    /**
     * Gets the user interface component to representing this {@code SourceListCountBadgeRenderer}.
     * The returned {@link JComponent} should be added to a container that will be displayed.
     *
     * @return the user interface component representing this {@code SourceListCountBadgeRenderer}.
     */
    public JComponent getComponent() {
        return fLabel;
    }

    // Custom JLabel implementation. //////////////////////////////////////////////////////////////

    private class CustomJLabel extends JLabel {

        private Color getSelectedBadgeColor() {
            return fSelectedColor;
        }

        private Color getUnselectedBadgeColor(boolean parentWindowHasFocus) {
            return parentWindowHasFocus ? fActiveUnselectedColor : fInactiveUnselectedColor;
        }

        @Override
        protected void paintComponent(Graphics g) {
            // create a buffered image to draw the component into. this lets us
            // draw "out" an area, making it transparent.
            BufferedImage image = new BufferedImage(getWidth(), getHeight(),
                    BufferedImage.TYPE_INT_ARGB);

            // create the graphics and set its initial state.
            Graphics2D g2d = image.createGraphics();
            g2d.setFont(getFont());
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setColor(fSelected
                    ? getSelectedBadgeColor()
                    : getUnselectedBadgeColor(WindowUtils.isParentWindowFocused(this)));

            // draw the badge.
            g2d.fillRoundRect(0, 0, getWidth(), getHeight(), getHeight(), getHeight());

            // set the color to use for the text - note this color is always
            // the same, though it won't always show because of the composite
            // set below.
            g2d.setColor(fTextColor);
            // if the badge is selected, punch out the text so that the
            //    underlying color shows through as the font color.
            // else use use a standard alpha composite to simply draw on top of
            //    whatever is currently there.
            g2d.setComposite(fSelected
                    ? AlphaComposite.DstOut : AlphaComposite.SrcOver);
            // calculate the bottom left point to draw the text at.
            Font font = g2d.getFont();
            FontRenderContext renderContext = g2d.getFontRenderContext();
            GlyphVector glyphVector = font.createGlyphVector(renderContext, getText());
            Rectangle visualBounds = glyphVector.getVisualBounds().getBounds();
            int x = getWidth() / 2 - g2d.getFontMetrics().stringWidth(getText()) / 2;
            int y = getHeight() / 2 - visualBounds.height / 2 - visualBounds.y;

            // draw the badge text.
            g2d.drawString(getText(), x, y);

            // draw the image into this component.
            g.drawImage(image, 0, 0, null);

            // dispose of the buffered image graphics.
            g2d.dispose();
        }
    }

}
