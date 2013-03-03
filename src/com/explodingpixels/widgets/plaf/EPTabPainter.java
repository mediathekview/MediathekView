package com.explodingpixels.widgets.plaf;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.GeneralPath;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicGraphicsUtils;

public class EPTabPainter {

    private CloseButtonLocation fCloseButtonLocation = CloseButtonLocation.LEFT;
    private CloseButtonIcon fCloseButtonIcon = new DefaultCloseButtonIcon();

    private static final int CONTENT_DISTANCE_FROM_EDGE = 5;
    private static final int CLOSE_BUTTON_DISTANCE_FROM_EDGE = 5;
    private static final int CLOSE_BUTTON_DISTANCE_FROM_CONTENT = 3;

    // create single variable to reuse when layout the tab. this reuse of rectangle is also seen in BasicLabelUI and
    // was used there for performance gains -- we're following that example, though it's unclear whether or not there
    // are still performance gains to be had by doing this.
    private static final Rectangle ADJUSTED_TAB_BOUNDS = new Rectangle();
    private static final Rectangle TEXT_BOUNDS = new Rectangle();
    private static final Rectangle ICON_BOUNDS = new Rectangle();

    // the colors used to draw the tab backround and border.
    static final Color SELECTED_BORDER_COLOR = new Color(0x666666);
    private static final Color UNSELECTED_BORDER_COLOR = new Color(0x888888);
    private static final Color SELECTED_BACKGROUND_COLOR = Color.WHITE;
    private static final Color UNSELECTED_BACKGROUND_COLOR = new Color(0xcccccc);

    private static final int CORNER_ARC_DIAMETER = 6;

    public void setCloseButtonLocation(CloseButtonLocation closeButtonLocation) {
        fCloseButtonLocation = closeButtonLocation;
    }

    public void paintTab(Graphics2D graphics, JTabbedPane tabPane, Rectangle tabBounds,
                         String tabText, Icon tabIcon, boolean isSelected,
                         boolean isMouseOverCloseButton, boolean isMousePressedOverCloseButton) {

        paintTabBackgroundAndBorder(graphics, tabBounds, isSelected);
        paintCloseButton(graphics, tabBounds, isSelected, isMouseOverCloseButton,
                isMousePressedOverCloseButton);

        FontMetrics fontMetrics = graphics.getFontMetrics();
        int closeButtonWidth = fCloseButtonIcon.getWidth();

        int textWidth = fontMetrics.stringWidth(tabText);

        int widthRequiredForCloseButton =
                fCloseButtonLocation.calculateWidthRequiredForCloseButton(closeButtonWidth);
        boolean tooWide = textWidth > tabBounds.width - widthRequiredForCloseButton - CONTENT_DISTANCE_FROM_EDGE;

        Rectangle adjustedTabRect = new Rectangle();
        adjustedTabRect.x = tooWide
                ? fCloseButtonLocation.calculateContentX(tabBounds, closeButtonWidth) : tabBounds.x;
        adjustedTabRect.y = tabBounds.y;
        adjustedTabRect.width = tooWide
                ? tabBounds.width - widthRequiredForCloseButton - CONTENT_DISTANCE_FROM_EDGE
                : tabBounds.width;
        adjustedTabRect.height = tabBounds.height;

        ICON_BOUNDS.x = 0;
        ICON_BOUNDS.y = 0;

        String clippedText = SwingUtilities.layoutCompoundLabel(tabPane, fontMetrics, tabText, tabIcon,
                SwingUtilities.CENTER, SwingUtilities.CENTER, SwingUtilities.CENTER, SwingUtilities.TRAILING,
                adjustedTabRect, ICON_BOUNDS, TEXT_BOUNDS, 4);

        int textX = fCloseButtonLocation.adjustXToPreventEncroachment(tabBounds, closeButtonWidth, TEXT_BOUNDS);
        int textY = TEXT_BOUNDS.y + fontMetrics.getAscent();
        graphics.setColor(tabPane.getForeground());
        BasicGraphicsUtils.drawString(graphics, clippedText, -1, textX, textY);
    }

    private void paintTabBackgroundAndBorder(Graphics2D graphics, Rectangle tabBounds, boolean isSelected) {
        int extendedHeight = tabBounds.height + CORNER_ARC_DIAMETER / 2;
        // paint the background.
        graphics.setColor(isSelected ? SELECTED_BACKGROUND_COLOR : UNSELECTED_BACKGROUND_COLOR);
        graphics.fillRoundRect(tabBounds.x, tabBounds.y, tabBounds.width, extendedHeight,
                CORNER_ARC_DIAMETER, CORNER_ARC_DIAMETER);

        // paint the border.
        graphics.setColor(isSelected ? SELECTED_BORDER_COLOR : UNSELECTED_BORDER_COLOR);
        graphics.drawRoundRect(tabBounds.x, tabBounds.y, tabBounds.width, extendedHeight,
                CORNER_ARC_DIAMETER, CORNER_ARC_DIAMETER);
    }

    private void paintCloseButton(Graphics2D graphics, Rectangle tabBounds, boolean isSelected,
                                  boolean isMouseOverCloseButton, boolean isMousePressedOverCloseButton) {
        int x = fCloseButtonLocation.calculateCloseButtonX(tabBounds, fCloseButtonIcon.getWidth());
        int y = fCloseButtonLocation.calculateCloseButtonY(tabBounds, fCloseButtonIcon.getHeight());
        ImageIcon closeImageIcon =
                fCloseButtonIcon.getImageIcon(isSelected, isMouseOverCloseButton, isMousePressedOverCloseButton);
        graphics.drawImage(closeImageIcon.getImage(), x, y, null);
    }


    public boolean isPointOverCloseButton(Rectangle tabBounds, Point point) {
        int closeButtonWidth = fCloseButtonIcon.getWidth();
        int closeButtonHeight = fCloseButtonIcon.getHeight();
        int closeButtonX = fCloseButtonLocation.calculateCloseButtonX(tabBounds, closeButtonWidth);
        int closeButtonY = fCloseButtonLocation.calculateCloseButtonY(tabBounds, closeButtonHeight);
        boolean overHorizontally = closeButtonX <= point.x && point.x <= closeButtonX + closeButtonWidth;
        boolean overVertically = closeButtonY <= point.y && point.y <= closeButtonY + closeButtonHeight;
        return overHorizontally && overVertically;
    }

    private Shape createTabShape(Rectangle tabBounds) {
        int topIndent = 3;
        int topCurveRadius = 3;
        int bottomCurveRadius = 4;
        int topLeftX = tabBounds.x + topIndent;
        int topRightX = tabBounds.x + tabBounds.width - topIndent;
        int bottomLeftX = tabBounds.x;
        int bottomRightX = tabBounds.x + tabBounds.width;

        GeneralPath path = new GeneralPath();
        path.moveTo(topLeftX + topCurveRadius, tabBounds.y);
        path.quadTo(topLeftX, tabBounds.y, topLeftX, tabBounds.y + topCurveRadius);
//        path.lineTo(topLeftX,  tabBounds.y + tabBounds.height - bottomCurveRadius);
        path.quadTo(topLeftX, tabBounds.y + tabBounds.height, bottomLeftX, tabBounds.y + tabBounds.height);
//        path.moveTo(tabBounds.x + curveRadius, tabBounds.y);
//        path.quadTo(topLeftX, tabBounds.y + tabBounds.height, bottomLeftX, tabBounds.y + tabBounds.height);
//        path.lineTo(tabBounds.x,  tabBounds.y + tabBounds.height);
//        path.lineTo(tabBounds.x + tabBounds.width, tabBounds.y + tabBounds.height);
//        path.lineTo(tabBounds.x + tabBounds.width, tabBounds.y + topCurveRadius);
//        path.quadTo(tabBounds.x + tabBounds.width, tabBounds.y, tabBounds.x + tabBounds.width - topCurveRadius, tabBounds.y);
        path.lineTo(bottomRightX, tabBounds.y + tabBounds.height);
        path.quadTo(topRightX, tabBounds.y + tabBounds.height, topRightX, tabBounds.y + topCurveRadius);
        path.quadTo(topRightX, tabBounds.y, topRightX - topCurveRadius, tabBounds.y);
        path.closePath();
        return path;
    }

    // CloseButton enumeration implementation. ////////////////////////////////////////////////////////////////////////

    public enum CloseButtonLocation {

        LEFT {
            int calculateCloseButtonX(Rectangle tabBounds, int closeButtonWidth) {
                return tabBounds.x + CLOSE_BUTTON_DISTANCE_FROM_EDGE;
            }
            int calculateContentX(Rectangle tabBounds, int closeButtonWidth) {
                return tabBounds.x + calculateWidthRequiredForCloseButton(closeButtonWidth);
            }
            int adjustXToPreventEncroachment(Rectangle tabBounds, int closeButtonWidth, Rectangle contentBounds) {
                int closeButtonX = calculateCloseButtonX(tabBounds, closeButtonWidth);
                int closeButtonXWithPad = closeButtonX + closeButtonWidth + CLOSE_BUTTON_DISTANCE_FROM_CONTENT;
                return Math.max(closeButtonXWithPad, contentBounds.x);
            }
        },
        RIGHT {
            int calculateCloseButtonX(Rectangle tabBounds, int closeButtonWidth) {
                return tabBounds.x + tabBounds.width - CLOSE_BUTTON_DISTANCE_FROM_CONTENT - closeButtonWidth;
            }
            int calculateContentX(Rectangle tabBounds, int closeButtonWidth) {
                return tabBounds.x + CONTENT_DISTANCE_FROM_EDGE;
            }
            int adjustXToPreventEncroachment(Rectangle tabBounds, int closeButtonWidth, Rectangle contentBounds) {
                int closeButtonX = calculateCloseButtonX(tabBounds, closeButtonWidth);
                int closeButtonXWithPad = closeButtonX - CLOSE_BUTTON_DISTANCE_FROM_CONTENT;
                int shiftAmount = (contentBounds.x + contentBounds.width) - closeButtonXWithPad;
                return shiftAmount > 0 ? contentBounds.x - shiftAmount : contentBounds.x;
            }
        };

        abstract int calculateCloseButtonX(Rectangle tabBounds, int closeButtonWidth);

        abstract int calculateContentX(Rectangle tabBounds, int closeButtonWidth);

        abstract int adjustXToPreventEncroachment(Rectangle tabBounds, int closeButtonWidth, Rectangle contentBounds);

        private int calculateCloseButtonY(Rectangle tabBounds, int closeButtonHeight) {
            return tabBounds.y + tabBounds.height / 2 - closeButtonHeight / 2;
        }

        int calculateWidthRequiredForCloseButton(int closeButtonWidth) {
            return closeButtonWidth + CLOSE_BUTTON_DISTANCE_FROM_EDGE + CLOSE_BUTTON_DISTANCE_FROM_CONTENT;
        }

    }

    // CloseButton interface and implementations. /////////////////////////////////////////////////////////////////////

    private interface CloseButtonIcon {
        int getWidth();

        int getHeight();

        ImageIcon getImageIcon(boolean isSelected, boolean isOver, boolean isPressed);
    }

    private static class DefaultCloseButtonIcon implements CloseButtonIcon {

        private ImageIcon fSelected = createImageIcon("close.png");
        private ImageIcon fUnselected = createImageIcon("close_unselected.png");
        private ImageIcon fOver = createImageIcon("close_over.png");
        private ImageIcon fPressed = createImageIcon("close_pressed.png");

        public int getWidth() {
            return fSelected.getIconWidth();
        }

        public int getHeight() {
            return fSelected.getIconHeight();
        }

        public ImageIcon getImageIcon(boolean isSelected, boolean isOver, boolean isPressed) {
            ImageIcon closeImageIcon;

            if (isOver && isPressed) {
                closeImageIcon = fPressed;
            } else if (isOver) {
                closeImageIcon = fOver;
            } else if (isSelected) {
                closeImageIcon = fSelected;
            } else {
                closeImageIcon = fUnselected;
            }

            return closeImageIcon;
        }

        private static ImageIcon createImageIcon(String fileName) {
            return new ImageIcon(EPTabbedPaneUI.class.getResource("/com/explodingpixels/widgets/images/" + fileName));
        }
    }
}
