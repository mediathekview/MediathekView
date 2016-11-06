package com.explodingpixels.macwidgets;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import com.explodingpixels.widgets.PopdownButton;
import com.explodingpixels.widgets.PopupMenuCustomizer;

public class ComponentBottomBar extends TriAreaComponent {

    protected static final ImageIcon SPLITTER_HANDLE =
            new ImageIcon(SourceListControlBar.class.getResource(
                    "/com/explodingpixels/macwidgets/images/splitter_handle.png"));

    protected JSplitPane fSplitPane;

    protected final JLabel fSplitterHandle = new JLabel(SPLITTER_HANDLE);

    protected final SplitterHandleMouseMovementHandler fMouseListener =
            new SplitterHandleMouseMovementHandler();

	public ComponentBottomBar() {
		super();
		setBackgroundPainter(MacButtonFactory.GRADIENT_BUTTON_IMAGE_PAINTER);
		getComponent().setBorder(
				BorderFactory.createMatteBorder(1, 0, 0, 0,
						MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR));
	}

	public void addComponentToLeftWithBorder(JComponent toolToAdd) {
		JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		panel.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1,
				MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR));
		panel.add(toolToAdd, BorderLayout.CENTER);
		super.addComponentToLeft(panel);
	}

	public void addComponentToCenterWithBorder(JComponent toolToAdd) {
		// TODO use matteBorder when on first center item addition.
		// if this is the first component being added, add a line to the left
		// and right of the component.
		// else add a border just to the right.
		Border matteBorder = getCenterComponentCount() == 0 ? BorderFactory
				.createMatteBorder(0, 1, 0, 1,
						MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR)
				: BorderFactory.createMatteBorder(0, 0, 0, 1,
						MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR);

		JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		panel.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1,
				MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR));
		panel.add(toolToAdd, BorderLayout.CENTER);
		super.addComponentToCenter(panel);
	}

	public void addComponentToRightWithBorder(JComponent toolToAdd) {
		JPanel panel = new JPanel(new BorderLayout());
		panel.setOpaque(false);
		panel.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0,
				MacButtonFactory.GRADIENT_BUTTON_BORDER_COLOR));
		panel.add(toolToAdd, BorderLayout.CENTER);
		super.addComponentToRight(panel);
	}
	
    /**
     * Connects the draggable widget in this {@code ComponentBottomBar} to the divider of the
     * given {@link JSplitPane}. Thus when the user drags the {@code ComponentBottomBar} draggable
     * widget, the given {@code JSplitPane}s divider location will be adjusted.
     *
     * DO THIS LAST AFTER YOU ADD ALL OTHER COMPONENTS TO THE RIGHT
     * 
     * @param splitPane the {@code JSplitPane} to connect the draggable widget to.
     */
    public void installDraggableWidgetOnSplitPane(JSplitPane splitPane) {
        if (splitPane == null) {
            throw new IllegalArgumentException("JSplitPane cannot be null.");
        }

        fSplitPane = splitPane;
        fSplitterHandle.addMouseListener(fMouseListener);
        fSplitterHandle.addMouseMotionListener(fMouseListener);
        
        addComponentToRight(fSplitterHandle);
        fSplitterHandle.setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
    }    

    /**
     * Add a new pop-down style button. The given {@link PopupMenuCustomizer} will be called just
     * prior to each showing of the menu.
     *
     * @param icon                the icon to use in the pop-down menu.
     * @param popupMenuCustomizer the {@code PopupMenuCustomizer} to be called just prior to showing
     *                            the menu.
     */
    public PopdownButton createAndAddPopdownButton(Icon icon, PopupMenuCustomizer popupMenuCustomizer) {
        PopdownButton button = MacButtonFactory.createGradientPopdownButton(
                icon, popupMenuCustomizer);
        initBottomBarButton(button.getComponent());
        addComponentToLeft(button.getComponent());
        
        return button;
    }

    /**
     * Adds a new button with the given icon. The given {@link ActionListener} will be called when
     * the button is pressed.
     *
     * @param icon           the icon to use for the button.
     * @param actionListener the {@code ActionListener} to call when the button is pressed.
     */
    public JComponent createAndAddButton(Icon icon, ActionListener actionListener) {
        JComponent button = MacButtonFactory.createGradientButton(icon, actionListener);
        initBottomBarButton(button);
        addComponentToLeft(button);
        
        return button;
    }

    static void initBottomBarButton(JComponent component) {
        component.setBorder(BorderFactory.createEmptyBorder());
    }

    /**
     * Hides the resize handle.
     */
    public void hideResizeHandle() {
        fSplitterHandle.setVisible(false);
    }
    
    // Mouse handler for splitter control widget. /////////////////////////////////////////////////

    protected class SplitterHandleMouseMovementHandler extends MouseAdapter
            implements MouseMotionListener {

        private int fDelta;

        @Override
        public void mousePressed(MouseEvent e) {
            MouseEvent convertedEvent =
                    SwingUtilities.convertMouseEvent(fSplitterHandle, e, fSplitPane);

            fDelta = fSplitPane.getDividerLocation() - convertedEvent.getPoint().x;
        }

        // MouseMotionListener implementation /////////////////////////////////////////////////////

        public void mouseDragged(MouseEvent e) {
            MouseEvent convertedEvent =
                    SwingUtilities.convertMouseEvent(fSplitterHandle, e, fSplitPane);
            int newLocation = convertedEvent.getPoint().x + fDelta;
            // bound newLocation between the minimum and maximum divider locations.
            int boundedNewLocation = Math.max(fSplitPane.getMinimumDividerLocation(),
                    Math.min(newLocation, fSplitPane.getMaximumDividerLocation()));
            fSplitPane.setDividerLocation(boundedNewLocation);
        }

        public void mouseMoved(MouseEvent e) {
        }
    }   
    
}
