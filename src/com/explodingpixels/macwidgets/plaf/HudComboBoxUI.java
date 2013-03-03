package com.explodingpixels.macwidgets.plaf;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.GeneralPath;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ButtonModel;
import javax.swing.DefaultButtonModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.WidgetBaseColors;
import com.explodingpixels.widgets.plaf.EPComboPopup;

/**
 * Creates a Heads Up Display (HUD) style combo box, similar to that seen in various iApps (e.g.
 * iPhoto).
 * <br>
 * <img src="../../../../../graphics/HUDComboBoxUI.png">
 */
public class HudComboBoxUI extends BasicComboBoxUI {

    private HudButtonUI fArrowButtonUI;
    private ActionListener fSelectedItemChangedActionListener = createSelectedItemChangedActionListener();
    private PopupMenuListener fPopupMenuListener = createPopupMenuListener();

    private static final int LEFT_MARGIN = 7;
    private static final int RIGHT_MARGIN = 19;
    private static final int DEFAULT_WIDTH = 100;
    
    private boolean isDarkColorScheme = true;

    /**
     * Creates a HUD style {@link javax.swing.plaf.ComboBoxUI}.
     */
    public HudComboBoxUI() {
        fArrowButtonUI = new HudButtonUI(HudPaintingUtils.Roundedness.COMBO_BUTTON, this);
    }

    /**
     * Creates a HUD style {@link javax.swing.plaf.ComboBoxUI}.
     */
    public HudComboBoxUI(boolean isDarkColorScheme) {
        
        this.isDarkColorScheme = isDarkColorScheme;
        fArrowButtonUI = new HudButtonUI(HudPaintingUtils.Roundedness.COMBO_BUTTON, this, this.isDarkColorScheme);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        HudPaintingUtils.initHudComponent(comboBox, isDarkColorScheme);

    }

    @Override
    protected void installListeners() {
        super.installListeners();
        comboBox.addActionListener(createComboBoxListener());
        comboBox.addActionListener(fSelectedItemChangedActionListener);
        comboBox.addPopupMenuListener(fPopupMenuListener);
    }

    @Override
    protected void uninstallListeners() {
        comboBox.removeActionListener(fSelectedItemChangedActionListener);
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        // TODO implement this.
    }

    @Override
    protected void installComponents() {
        super.installComponents();
        updateDisplayedItem();
    }

    /**
     * Creates an {@link ActionListener} that udpates the displayed item in the {@code arrowButton}.
     * NOTE: This listener doesn't seem to be required on Mac OS X, but Windows does. The selected
     * item is not correctly reflected in the UI without this listener.
     */
    private ActionListener createSelectedItemChangedActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateDisplayedItem();
            }
        };
    }

    /**
     * Updates the value displayed to match that of {@link JComboBox#getSelectedItem()}.
     */
    private void updateDisplayedItem() {
        // TODO make the calculation of the display string more robust
        // TODO (i.e. use TextProvider interface).
        String displayValue = comboBox.getSelectedItem() == null
                ? " " : comboBox.getSelectedItem().toString();
        arrowButton.setText(displayValue);
        comboBox.invalidate();
    }

    /**
     * Creates a {@link EPComboPopup.ComboBoxVerticalCenterProvider} that returns a vertical
     * center value that takes HudComboBoxUI's drop shadow. The visual center is calculated as if
     * the drop shadow did not exist.
     */
    private EPComboPopup.ComboBoxVerticalCenterProvider createComboBoxVerticalCenterProvider() {
        return new EPComboPopup.ComboBoxVerticalCenterProvider() {
            public int provideCenter(JComboBox comboBox) {
                return calculateArrowButtonVisualVerticalCenter();
            }
        };
    }

    /**
     * Creates an {@link ActionListener} that updates the displayed item when the
     * {@link JComboBox}'s currently selected item changes.
     */
    private ActionListener createComboBoxListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateDisplayedItem();
            }
        };
    }

    /**
     * Creates a {@link PopupMenuListener} that forces the associated combo box button to be pressed when the combo
     * popup is shown and to be non-pressed when the combo popup is hidden.
     */
    private PopupMenuListener createPopupMenuListener() {
        return new PopupMenuListener() {
            public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                arrowButton.getModel().setPressed(true);
            }

            public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                arrowButton.getModel().setPressed(false);
            }

            public void popupMenuCanceled(PopupMenuEvent e) {
                // no implementation.
            }
        };
    }

    @Override
    protected JButton createArrowButton() {
        JButton arrowButton = new JButton("");
        arrowButton.setModel(createButtonModel());
        arrowButton.setUI(fArrowButtonUI);
        Insets currentInsets = arrowButton.getInsets();
        arrowButton.setBorder(BorderFactory.createEmptyBorder(
                currentInsets.top, LEFT_MARGIN, currentInsets.bottom, RIGHT_MARGIN));
        arrowButton.setHorizontalAlignment(SwingConstants.LEFT);

        return arrowButton;
    }

    /**
     * Creates a custom {@link ButtonModel} that forces the button to be in the pressed state if the corresponding
     * combo boxe's poup is showing.
     */
    private ButtonModel createButtonModel() {
        return new DefaultButtonModel() {
            @Override
            public boolean isPressed() {
                return super.isPressed() || isPopupVisible(comboBox);
            }
        };
    }


    @Override
    protected ListCellRenderer createRenderer() {
        return new JComboBox().getRenderer();
    }

    @Override
    protected ComboPopup createPopup() {
        EPComboPopup popup = new EPComboPopup(comboBox);
        popup.setFont(HudPaintingUtils.getHudFont().deriveFont(Font.PLAIN));
        // install a custom ComboBoxVerticalCenterProvider that takes into account the size of the
        // drop shadow.
        popup.setVerticalComponentCenterProvider(createComboBoxVerticalCenterProvider());
        return popup;
    }

    @Override
    public Dimension getMinimumSize(JComponent c) {
        int width = getDisplaySize().width;
        int height = arrowButton.getPreferredSize().height;
        return new Dimension(width, height);
    }

    @Override
    protected Dimension getDefaultSize() {
        AbstractButton button = HudWidgetFactory.createHudButton("Button");
        return new Dimension(DEFAULT_WIDTH, button.getPreferredSize().height);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        HudPaintingUtils.updateGraphicsToPaintDisabledControlIfNecessary((Graphics2D) g, c);
        super.paint(g, c);

        Graphics2D graphics = (Graphics2D) g.create();
        paintUpDownArrowsIcon(graphics);
        graphics.dispose();
    }

    @Override
    public void paintCurrentValue(Graphics g, Rectangle bounds, boolean hasFocus) {
        // no painting necessary - the arrowButton handles painting of the currently selected value.
    }

    @Override
    protected Dimension getDisplaySize() {
        int maxWidth;
        if (comboBox.getPrototypeDisplayValue() != null) {
            maxWidth = getDisplayWidth(comboBox.getPrototypeDisplayValue());
        } else if (comboBox.getItemCount() > 0) {
            maxWidth = getMaxComboBoxModelDisplayWidth();
        } else {
            maxWidth = getDefaultSize().width;
        }

        Insets arrowButtonInsets = arrowButton.getInsets();
        maxWidth += arrowButtonInsets.left + arrowButtonInsets.right;

        return new Dimension(maxWidth, arrowButton.getPreferredSize().height);
    }

    /**
     * Gets the max display width in pixels of all entries in the {@link JComboBox}'s
     * {@link javax.swing.ComboBoxModel}.
     */
    private int getMaxComboBoxModelDisplayWidth() {
        int maxWidth = 0;
        for (int i = 0; i < comboBox.getModel().getSize(); i++) {
            int itemWidth = getDisplayWidth(comboBox.getModel().getElementAt(i));
            maxWidth = Math.max(maxWidth, itemWidth);
        }
        return maxWidth;
    }

    /**
     * Calculates the display width in pixels of the given object.
     */
    private int getDisplayWidth(Object object) {
        assert object != null : "The given object cannot be null";
        // TODO refactor this logic into utility class that looks for TextProvider.
        FontMetrics fontMetrics = comboBox.getFontMetrics(comboBox.getFont());
        return fontMetrics.stringWidth(object.toString());
    }

    @Override
    protected LayoutManager createLayoutManager() {
        return new LayoutManager() {
            public void addLayoutComponent(String name, Component comp) {
                throw new UnsupportedOperationException("This operation is not supported.");
            }

            public void removeLayoutComponent(Component comp) {
                throw new UnsupportedOperationException("This operation is not supported.");
            }

            public Dimension preferredLayoutSize(Container parent) {
                // the combo box's preferred size is the preferred width of the parent and the
                // preferred height of the arrowButton.
                return new Dimension(parent.getPreferredSize().width,
                        arrowButton.getPreferredSize().height);
            }

            public Dimension minimumLayoutSize(Container parent) {
                return parent.getMinimumSize();
            }

            public void layoutContainer(Container parent) {
                // make the arrowButton fill the width, and center itself in the available height.
                int buttonHeight = arrowButton.getPreferredSize().height;
                int y = parent.getHeight() / 2 - buttonHeight / 2;
                arrowButton.setBounds(0, y, parent.getWidth(), buttonHeight);
            }
        };
    }

    /**
     * Calculates the visual vertical center of this component. The visual center is what the user
     * would interpret as the center, thus we adjust the actual center to take into account the size
     * of the drop shadow.
     */
    private int calculateArrowButtonVisualVerticalCenter() {
        int arrowButtonShadowHeight = HudPaintingUtils.getHudControlShadowSize(arrowButton);
        return (comboBox.getHeight() - arrowButtonShadowHeight) / 2;
    }

    /**
     * Paints the up and down arrows on the right side of the combo box.
     */
    private void paintUpDownArrowsIcon(Graphics2D graphics) {
        Insets arrowButtonInsets = arrowButton.getInsets();
        int arrowButtonHeight = arrowButton.getHeight();

        // calculate the exact center of where both arrows will be drawn relative to.
        int centerX = arrowButton.getWidth() - arrowButtonInsets.right / 2;
        int centerY = calculateArrowButtonVisualVerticalCenter();

        // calculate how many pixels there should be between the arrows as well as how long each
        // side of the arrow should be.
        int verticalDistanceBetweenArrows = (int) (arrowButtonHeight * 0.125);
        int arrowSideLength = verticalDistanceBetweenArrows * 2;

        // calculate the upper left position of the up arrow.
        int upArrowX = centerX - arrowSideLength / 2;
        int upArrowY = centerY - verticalDistanceBetweenArrows / 2;

        graphics.setColor(this.isDarkColorScheme ? WidgetBaseColors.DARK_FONT_COLOR : WidgetBaseColors.LIGHT_FONT_COLOR);
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // translate the graphics to the upper left of each arrow and draw that arrow. each arrow
        // assumes that it is being drawn at 0,0.
        graphics.translate(upArrowX, upArrowY);
        graphics.fill(createUpArrow(arrowSideLength));
        graphics.translate(0, verticalDistanceBetweenArrows);
        graphics.fill(createDownArrow(arrowSideLength));
    }

    /**
     * Creates a path representing an up arrow, based at 0,0.
     */
    private static GeneralPath createUpArrow(int arrowSideLength) {
        GeneralPath path = new GeneralPath();
        path.moveTo(0, 0);
        path.lineTo(arrowSideLength, 0);
        path.lineTo(arrowSideLength / 2, -arrowSideLength);
        path.closePath();

        return path;
    }

    /**
     * Creates a path representing a down arrow, based at 0,0.
     */
    private static GeneralPath createDownArrow(int arrowSideLength) {
        GeneralPath path = new GeneralPath();
        path.moveTo(0, 0);
        path.lineTo(arrowSideLength, 0);
        path.lineTo(arrowSideLength / 2, arrowSideLength);
        path.closePath();

        return path;
    }

}
