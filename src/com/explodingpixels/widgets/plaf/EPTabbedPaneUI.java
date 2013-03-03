package com.explodingpixels.widgets.plaf;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Paint;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.Timer;
import javax.swing.plaf.basic.BasicTabbedPaneUI;

import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.widgets.TabCloseListener;

public class EPTabbedPaneUI extends BasicTabbedPaneUI {

    public static final String TAB_CLOSE_LISTENER_KEY = "TabbedPane.closeListener";
    public static final String CLOSE_BUTTON_LOCATION_KEY = "TabbedPane.closeButtonLocation";
    public static final Object CLOSE_BUTTON_LOCATION_VALUE_LEFT = EPTabPainter.CloseButtonLocation.LEFT;
    public static final Object CLOSE_BUTTON_LOCATION_VALUE_RIGHT = EPTabPainter.CloseButtonLocation.RIGHT;

    private EPTabPainter fTabPainter = new EPTabPainter();
    private MacWidgetsPainter<Component> fContentBorderTopEdgeBackgroundPainter = createContentBorderTopEdgeBackgroundPainter();
    private boolean fPaintFullContentBorder = true;
    private int fCurrentDefaultTabWidth = DEFAULT_TAB_WIDTH;
    private int fMouseOverCloseButtonTabIndex = NO_TAB;
    private int fMousePressedCloseButtonTabIndex = NO_TAB;
    private TabCloseListener fTabCloseListener = new DefaultTabCloseListener();
    private Timer fTabCloseTimer = new Timer(10, null);
    private CustomLayoutManager fLayoutManager = new CustomLayoutManager();

    private static final Insets FULL_CONTENT_BORDER_INSETS = new Insets(6, 0, 0, 0);
    private static final Insets HAIRLINE_BORDER_INSETS = new Insets(2, 0, 0, 0);

    private static final int DEFAULT_TAB_WIDTH = 100;
    private static final int NO_TAB = -1;
    private static final int SMALLEST_TAB_WIDTH = 35;
    private static final int TAB_ANIMATION_DELTA = 7;
    private static final int OVERFLOW_BUTTON_AREA_WIDTH = 25;

    @Override
    protected void installDefaults() {
        super.installDefaults();

        Font oldFont = tabPane.getFont();
        tabPane.setFont(oldFont.deriveFont(oldFont.getSize() - 2.0f));
        tabPane.setBorder(BorderFactory.createEmptyBorder());

        tabInsets = new Insets(2, 10, 2, 10);
        selectedTabPadInsets = new Insets(2, 0, 2, 0);

        doExtractTabCloseProperty();
        doExtractCloseButtonLocationProperty();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        tabPane.addMouseMotionListener(createCloseButtonMouseMotionListener());
        tabPane.addMouseListener(createCloseButtonMouseListener());
        tabPane.addContainerListener(createContainerListener());
        tabPane.addPropertyChangeListener(TAB_CLOSE_LISTENER_KEY,
                createTabCloseListenerPropertyChangeListener());
        tabPane.addPropertyChangeListener(CLOSE_BUTTON_LOCATION_KEY,
                createCloseButtonLocationPropertyChangeListener());
    }

    private MouseMotionListener createCloseButtonMouseMotionListener() {
        return new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                doMouseMoved(e.getPoint());
            }

            @Override
            public void mouseDragged(MouseEvent e) {
                doMousePressed(e.getPoint());
                doMouseMoved(e.getPoint());
            }
        };
    }

    private void doMouseMoved(Point point) {
        int tabIndex = tabForCoordinate(tabPane, point.x, point.y);
        int oldMouseOverCloseButtonTabIndex = fMouseOverCloseButtonTabIndex;
        if (isTabIndexValid(tabIndex)) {
            Rectangle tabBounds = getTabBounds(tabPane, tabIndex);
            fMouseOverCloseButtonTabIndex = fTabPainter.isPointOverCloseButton(tabBounds, point)
                    ? tabIndex : NO_TAB;
            repaintTab(fMouseOverCloseButtonTabIndex);
        }
        repaintTab(oldMouseOverCloseButtonTabIndex);
    }

    private MouseListener createCloseButtonMouseListener() {
        return new MouseAdapter() {
            @Override
            public void mouseExited(MouseEvent e) {
                int oldMouseOverCloseButtonTabIndex = fMouseOverCloseButtonTabIndex;
                fMouseOverCloseButtonTabIndex = NO_TAB;
                repaintTab(oldMouseOverCloseButtonTabIndex);
            }

            @Override
            public void mousePressed(MouseEvent e) {
                doMousePressed(e.getPoint());
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                closeTabUsingAnimationIfValid(fMousePressedCloseButtonTabIndex);
                int oldMousePressedOverCloseButtonTabIndex = fMouseOverCloseButtonTabIndex;
                fMousePressedCloseButtonTabIndex = NO_TAB;
                repaintTab(oldMousePressedOverCloseButtonTabIndex);
            }
        };
    }

    private void doMousePressed(Point point) {
        int tabIndex = tabForCoordinate(tabPane, point.x, point.y);
        int oldMousePressedCloseButtonIndex = fMousePressedCloseButtonTabIndex;
        if (isTabIndexValid(tabIndex)) {
            Rectangle tabBounds = getTabBounds(tabPane, tabIndex);
            fMousePressedCloseButtonTabIndex = fTabPainter.isPointOverCloseButton(tabBounds, point) ? tabIndex : NO_TAB;
            repaintTab(fMousePressedCloseButtonTabIndex);
        }
        repaintTab(oldMousePressedCloseButtonIndex);
    }

    private ContainerListener createContainerListener() {
        return new ContainerAdapter() {
            public void componentAdded(ContainerEvent e) {
                Component componentAdded = e.getChild();
                fLayoutManager.forceTabWidth(componentAdded, SMALLEST_TAB_WIDTH);
                animateTabBeingAdded(componentAdded);
            }
        };
    }

    private PropertyChangeListener createTabCloseListenerPropertyChangeListener() {
        return new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                doExtractTabCloseProperty();
            }
        };
    }

    private void doExtractTabCloseProperty() {
        Object closeListenerValue = tabPane.getClientProperty(TAB_CLOSE_LISTENER_KEY);
        if (closeListenerValue instanceof TabCloseListener) {
            fTabCloseListener = (TabCloseListener) closeListenerValue;
        }
    }

    private PropertyChangeListener createCloseButtonLocationPropertyChangeListener() {
        return new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                doExtractCloseButtonLocationProperty();
            }
        };
    }

    private void doExtractCloseButtonLocationProperty() {
        Object closeButtonLocationValue = tabPane.getClientProperty(CLOSE_BUTTON_LOCATION_KEY);
        if (closeButtonLocationValue instanceof EPTabPainter.CloseButtonLocation) {
            setCloseButtonLocation((EPTabPainter.CloseButtonLocation) closeButtonLocationValue);
        }
    }

    @Override
    protected LayoutManager createLayoutManager() {
        return fLayoutManager;
    }

    @Override
    protected Insets getContentBorderInsets(int tabPlacement) {
        return fPaintFullContentBorder ? FULL_CONTENT_BORDER_INSETS : HAIRLINE_BORDER_INSETS;
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        ((Graphics2D) g).setRenderingHint(
                RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        super.paint(g, c);
        paintContentBorder(g, tabPane.getTabPlacement(), tabPane.getSelectedIndex());
    }

    @Override
    protected void paintTab(Graphics g, int tabPlacement, Rectangle[] rects, int tabIndex,
                            Rectangle iconRect, Rectangle textRect) {
        Rectangle tabRect = rects[tabIndex];
        boolean isSelected = tabIndex == tabPane.getSelectedIndex();

        String title = tabPane.getTitleAt(tabIndex);
        Icon icon = getIconForTab(tabIndex);

        Graphics2D graphics = (Graphics2D) g;
        boolean isMouseOverCloseButton = fMouseOverCloseButtonTabIndex == tabIndex;
        boolean isMousePressedOverCloseButton = fMousePressedCloseButtonTabIndex == tabIndex;
        fTabPainter.paintTab(graphics, tabPane, tabRect, title, icon, isSelected, isMouseOverCloseButton,
                isMousePressedOverCloseButton);
    }

    @Override
    protected void paintContentBorderTopEdge(Graphics g, int tabPlacement, int selectedIndex,
                                             int x, int y, int width, int height) {
        Graphics2D graphics = (Graphics2D) g;

        graphics.translate(x, y);
        int borderHeight = getContentBorderInsets(tabPane.getTabPlacement()).top;
        fContentBorderTopEdgeBackgroundPainter.paint(graphics, tabPane, width, borderHeight);
        graphics.translate(-x, -y);

        if (tabPane.getSelectedIndex() >= 0) {
            graphics.setColor(Color.WHITE);
            Rectangle boundsOfSelectedTab = getTabBounds(tabPane, tabPane.getSelectedIndex());
            graphics.drawLine(boundsOfSelectedTab.x, y, boundsOfSelectedTab.x + boundsOfSelectedTab.width, y);
        }

        graphics.setColor(Color.RED);
        g.fillRect(x, y + height, x + width, y + height + 2);
    }

    private MacWidgetsPainter<Component> createContentBorderTopEdgeBackgroundPainter() {
        return new MacWidgetsPainter<Component>() {
            public void paint(Graphics2D graphics, Component objectToPaint, int width, int height) {
                Paint paint = new GradientPaint(0, 0, Color.WHITE, 0, height - 1, new Color(0xf8f8f8));
                graphics.setPaint(paint);
                graphics.fillRect(0, 0, width, height - 1);
                graphics.setColor(EPTabPainter.SELECTED_BORDER_COLOR);
                graphics.drawLine(0, 0, width, 0);
                graphics.setColor(new Color(0x999999));
                // TODO figure out why we need to subtract off another extra pixel here -- doesn't make sense.
                graphics.drawLine(0, height - 2, width, height - 2);
            }
        };
    }

    @Override
    protected void paintContentBorderLeftEdge(Graphics g, int tabPlacement, int selectedIndex,
                                              int x, int y, int w, int h) {
        // do nothing.
    }

    @Override
    protected void paintContentBorderRightEdge(Graphics g, int tabPlacement, int selectedIndex,
                                               int x, int y, int w, int h) {
        // do nothing.
    }

    @Override
    protected void paintContentBorderBottomEdge(Graphics g, int tabPlacement, int selectedIndex,
                                                int x, int y, int w, int h) {
        // do nothing.
    }

    @Override
    protected int getTabLabelShiftX(int tabPlacement, int tabIndex, boolean isSelected) {
        return 0;
    }

    @Override
    protected int getTabLabelShiftY(int tabPlacement, int tabIndex, boolean isSelected) {
        return 0;
    }

    // Public API methods. ////////////////////////////////////////////////////////////////////////////////////////////

    public void setPaintsFullContentBorder(boolean paintsFullContentBorder) {
        fPaintFullContentBorder = paintsFullContentBorder;
        tabPane.repaint();
    }

    public void setCloseButtonLocation(EPTabPainter.CloseButtonLocation closeButtonLocation) {
        fTabPainter.setCloseButtonLocation(closeButtonLocation);
    }

    // Helper methods. ////////////////////////////////////////////////////////////////////////////

    private void repaintTab(int tabIndex) {
        if (isTabIndexValid(tabIndex)) {
            Rectangle tabBounds = getTabBounds(tabPane, tabIndex);
            tabPane.repaint(tabBounds);
        }
    }

    private boolean isTabIndexValid(int tabIndex) {
        return tabIndex >= 0 && tabIndex < tabPane.getTabCount();
    }

    private void animateTabBeingAdded(Component tabComponent) {
        fTabCloseTimer.addActionListener(createTabAddedAnimation(tabComponent));
        fTabCloseTimer.start();
    }

    private void closeTabUsingAnimationIfValid(int tabIndex) {
        if (isTabIndexValid(tabIndex) && fTabCloseListener.tabAboutToBeClosed(tabIndex)) {
            Component tabComponentToClose = tabPane.getComponent(tabIndex);
            fTabCloseTimer.addActionListener(createTabRemovedAnimation(tabComponentToClose));
            fTabCloseTimer.start();
        }
    }

    private void closeTab(int tabIndex) {
        assert isTabIndexValid(tabIndex) : "The tab index should be valid.";

        String title = tabPane.getTitleAt(tabIndex);
        Component component = tabPane.getComponentAt(tabIndex);
        tabPane.removeTabAt(tabIndex);
        fTabCloseListener.tabClosed(title, component);
    }

    // Tab animation helper methods. //////////////////////////////////////////////////////////////

    private ActionListener createTabAddedAnimation(final Component tabComponentAdded) {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int currentTabWidth = fLayoutManager.getTabWidth(tabComponentAdded);
                int newTabWidth = Math.min(currentTabWidth + TAB_ANIMATION_DELTA, fCurrentDefaultTabWidth);
                fLayoutManager.forceTabWidth(tabComponentAdded, newTabWidth);
                if (newTabWidth == fCurrentDefaultTabWidth) {
                    animationFinished(this, tabComponentAdded);
                }
                tabPane.doLayout();
                tabPane.repaint();
            }
        };
    }

    private ActionListener createTabRemovedAnimation(final Component tabComponentToClose) {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int currentTabWidth = fLayoutManager.getTabWidth(tabComponentToClose);
                int newTabWidth = Math.max(currentTabWidth - TAB_ANIMATION_DELTA, SMALLEST_TAB_WIDTH);
                fLayoutManager.forceTabWidth(tabComponentToClose, newTabWidth);
                if (newTabWidth == SMALLEST_TAB_WIDTH) {
                    animationFinished(this, tabComponentToClose);
                    int tabIndex = tabPane.indexOfComponent(tabComponentToClose);
                    closeTab(tabIndex);
                }
                tabPane.doLayout();
                tabPane.repaint();
            }
        };
    }

    private void animationFinished(ActionListener actionListenerToRemove, Component tabComponent) {
        fTabCloseTimer.removeActionListener(actionListenerToRemove);
        fLayoutManager.useDefaultTabWidth(tabComponent);
        if (fTabCloseTimer.getActionListeners().length == 0) {
            fTabCloseTimer.stop();
        }
    }

    // CustomLayoutManager implementation. ////////////////////////////////////////////////////////

    private class CustomLayoutManager extends TabbedPaneLayout {

        private Map<Component, Integer> fTabsBeingAnimatedToWidths = new HashMap<Component, Integer>();

        private void forceTabWidth(Component tabComponent, int width) {
            fTabsBeingAnimatedToWidths.put(tabComponent, width);
        }

        private void useDefaultTabWidth(Component tabComponent) {
            fTabsBeingAnimatedToWidths.remove(tabComponent);
        }

        private int getTabWidth(Component tabComponent) {
            Integer forcedTabWidth = fTabsBeingAnimatedToWidths.get(tabComponent);
            return forcedTabWidth == null ? fCurrentDefaultTabWidth : forcedTabWidth;
        }

        protected void calculateTabRects(int tabPlacement, int tabCount) {
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
            int currentX = tabAreaInsets.left;
            int y = tabAreaInsets.top;
            int tabAreaWidth = tabPane.getWidth() - tabAreaInsets.left - tabAreaInsets.right;

            int requiredWidth = calculateRequiredWidth();
            boolean notEnoughRoom = requiredWidth > tabAreaWidth;
            int extraSpace = tabAreaWidth - requiredWidth;
//            int extraSpace = notEnoughRoom
//                    ? tabAreaWidth - requiredWidth - OVERFLOW_BUTTON_AREA_WIDTH
//                    : tabAreaWidth - requiredWidth;
            int numDefaultWidthTabs = getNumDefaultWidthTabs();

            System.out.println("tabbed pane width " + tabAreaWidth + ", extra space " + extraSpace);
            if (numDefaultWidthTabs > 0) {
                int extraSpacePerTab = extraSpace / numDefaultWidthTabs;
                int newDefaultTabWidth = fCurrentDefaultTabWidth + extraSpacePerTab;
                fCurrentDefaultTabWidth = Math.min(newDefaultTabWidth, DEFAULT_TAB_WIDTH);
                fCurrentDefaultTabWidth = Math.max(SMALLEST_TAB_WIDTH, fCurrentDefaultTabWidth);
            }

            maxTabWidth = 0;
            maxTabHeight = calculateMaxTabHeight(tabPlacement);

            // inidicate that there is one "run" of tabs. this value is used during the actual
            // laying out of the container and must be set here.
            runCount = 1;

            // iterate through tabs and lay them out in a single row (run).
            for (int i = 0; i < tabCount; i++) {
                Rectangle rect = rects[i];

                rect.width = isTabBeingAnimated(i) ? getForcedTabWidth(i) : fCurrentDefaultTabWidth;
                maxTabWidth = Math.max(maxTabWidth, rect.width);

                rect.x = currentX;
                // move the currentX variable over to the right edge of this tab, which is the
                // beginning of the next tab.
                currentX += rect.width;

                rect.y = y;
                rect.height = maxTabHeight;
            }
        }

        private boolean isTabBeingAnimated(int tabIndex) {
            Component tabComponent = tabPane.getComponentAt(tabIndex);
            return fTabsBeingAnimatedToWidths.get(tabComponent) != null;
        }

        private int getForcedTabWidth(int tabIndex) {
            Component tabComponent = tabPane.getComponentAt(tabIndex);
            return fTabsBeingAnimatedToWidths.get(tabComponent);
        }

        private int getNumDefaultWidthTabs() {
            return tabPane.getTabCount() - fTabsBeingAnimatedToWidths.size();
        }

        private int sumOfForcedTabWidths() {
            int sum = 0;
            for (int width : fTabsBeingAnimatedToWidths.values()) {
                sum += width;
            }
            return sum;
        }

        private int calculateRequiredWidth() {
            int totalDefaultWidthTabsWidth = getNumDefaultWidthTabs() * fCurrentDefaultTabWidth;
            return totalDefaultWidthTabsWidth + sumOfForcedTabWidths();
        }

    }

    // DefaultTabCloseListener implementation. ////////////////////////////////////////////////////

    private static class DefaultTabCloseListener implements TabCloseListener {

        public boolean tabAboutToBeClosed(int tabIndex) {
            return true;
        }

        public void tabClosed(String title, Component component) {
            // no implementation.
        }
    }

}
