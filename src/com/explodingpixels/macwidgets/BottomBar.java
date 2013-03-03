package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import com.explodingpixels.border.FocusStateMatteBorder;
import com.explodingpixels.painter.FocusStatePainter;
import com.explodingpixels.painter.GradientPainter;
import com.explodingpixels.painter.MacWidgetsPainter;
import com.explodingpixels.util.PlatformUtils;
import com.explodingpixels.widgets.WindowDragger;
import com.explodingpixels.widgets.WindowUtils;


/**
 * A Mac style Bottom Bar. For a full descrption of what a Bottom Bar is, see the
 * <a href="http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_4.html#//apple_ref/doc/uid/20000961-SW6">Bottom Bars</a>
 * section of Apple's Human Interface Guidelines. Here's an example of what this method cretes:
 * <br>
 * <img src="../../../../graphics/BottomBar.png">
 * <br>
 * Here's a simple example that creates a Bottom Bar:
 * <pre>
 * BottomBar bottomBar = BottomBar(BottomBarSize.LARGE);
 * bottomBar.addComponentToCenter(MacWidgetFactory.createEmphasizedLabel("My Label"));
 * </pre>
 */
public class BottomBar {

	protected final TriAreaComponent fBottomBar = new TriAreaComponent(5);

	protected JSplitPane fSplitPane;

	protected final SplitterHandleMouseMovementHandler fMouseListener =
            new SplitterHandleMouseMovementHandler();

	protected static final Color ACTIVE_TOP_COLOR = new Color(0xcccccc);
	protected static final Color ACTIVE_BOTTOM_COLOR = new Color(0xa7a7a7);
	protected static final Color INACTIVE_TOP_COLOR = new Color(0xe9e9e9);
	protected static final Color INACTIVE_BOTTOM_COLOR = new Color(0xd8d8d8);
	protected static final Color BORDER_HIGHLIGHT_COLOR = new Color(255, 255, 255, 100);

	protected static final Color LEOPARD_ACTIVE_TOP_COLOR = new Color(0xbbbbbb);
	protected static final Color LEOPARD_ACTIVE_BOTTOM_COLOR = new Color(0x969696);
	protected static final Color LEOPARD_INACTIVE_TOP_COLOR = new Color(0xe3e3e3);
	protected static final Color LEOPARD_INACTIVE_BOTTOM_COLOR = new Color(0xcfcfcf);
	protected static final Color LEOPARD_BORDER_HIGHLIGHT_COLOR = new Color(255, 255, 255, 110);

	/**
	 * Creates a {@code BottomBar} of the given size.
	 *
	 * @param size the height of the {@code BottomBar}.
	 */
	public BottomBar(BottomBarSize size) {

		createAndInstallBackgroundPainter();
		createAndInstallBorder();

		fBottomBar.forceOuterAreasToHaveTheSameWidth();

		// TODO use the actual border insets instead of the hard-coded value 2.
		// calculate the height of the bottom bar. this includes adding two pixels to incorporate
		// the height of the line border.
		int height = size.getHeight() + 2;
		fBottomBar.getComponent().setPreferredSize(new Dimension(-1, height));

		// install a listener that will repaint this component when the parent window's focus state
		// changes.
		WindowUtils.installJComponentRepainterOnWindowFocusChanged(fBottomBar.getComponent());
	}

	/**
	 * Adds the given component to the left side of this {@code BottomBar}.
	 *
	 * @param toolToAdd the tool to add to this {@code BottomBar}.
	 */
	public void addComponentToLeft(JComponent toolToAdd) {
		fBottomBar.addComponentToLeft(toolToAdd);
	}

	/**
	 * Adds the given component to the left side of this {@code BottomBar} followed by the
	 * given an empty space of the given pixel width.
	 *
	 * @param toolToAdd     the tool to add to this {@code BottomBar}.
	 * @param spacer_pixels the amount of space to post-pend the added component with.
	 */
	public void addComponentToLeft(JComponent toolToAdd, int spacer_pixels) {
		fBottomBar.addComponentToLeft(toolToAdd, spacer_pixels);
	}

	/**
	 * Adds the given component to the side of this {@code BottomBar}.
	 *
	 * @param toolToAdd the tool to add to this {@code BottomBar}.
	 */
	public void addComponentToCenter(JComponent toolToAdd) {
		fBottomBar.addComponentToCenter(toolToAdd);
	}

	/**
	 * Adds the given component to the center of this {@code BottomBar}. If this is not the
	 * first component to be added to the center, then the given component will be preceeded by a
	 * space of the given width.
	 *
	 * @param toolToAdd     the tool to add to this {@code BottomBar}.
	 * @param spacer_pixels the amount of space to pre-pend the added component with *if* the given
	 *                      component is *not* the first component to be added to the center.
	 */
	public void addComponentToCenter(JComponent toolToAdd, int spacer_pixels) {
		fBottomBar.addComponentToCenter(toolToAdd, spacer_pixels);
	}

	/**
	 * Adds the given component to the right side of this {@code BottomBar}.
	 *
	 * @param toolToAdd the tool to add to this {@code BottomBar}.
	 */
	public void addComponentToRight(JComponent toolToAdd) {
		fBottomBar.addComponentToRight(toolToAdd);
	}

	/**
	 * Adds the given component to the right side of this {@code BottomBar}. If this is not
	 * the first component to be added to the right, then the given component will be followed by a
	 * space of the given width.
	 *
	 * @param toolToAdd     the tool to add to this {@code BottomBar}.
	 * @param spacer_pixels the amount of space to post-pend the added component with *if* the given
	 *                      component is *not* the first component to be added to the center.
	 */
	public void addComponentToRight(JComponent toolToAdd, int spacer_pixels) {
		fBottomBar.addComponentToRight(toolToAdd, spacer_pixels);
	}

	/**
	 * Installs a drag listener on this {@code BottomBar} such that if it is dragged, it will
	 * move the given {@link java.awt.Window}.
	 *
	 * @param window the {@code Window} to move when the this {@code BottomBar} is dragged.
	 */
	public void installWindowDraggerOnWindow(Window window) {
		new WindowDragger(window, getComponent());
	}

	/**
	 * Gets the user interface component representing this {@code BottomBar}. The returned
	 * {@link JComponent} should be added to a container that will be displayed.
	 *
	 * @return the user interface component representing this {@code BottomBar}.
	 */
	public JComponent getComponent() {
		return fBottomBar.getComponent();
	}
	
    public void forceAreasToHaveTheSameWidth() {
        fBottomBar.forceAreasToHaveTheSameWidth();
    }

    public void forceOuterAreasToHaveTheSameWidth() {
        fBottomBar.forceOuterAreasToHaveTheSameWidth();
    }
    

    /**
     * Connects the draggable widget in this {@code BottomBar} to the divider of the
     * given {@link JSplitPane}. Thus when the user drags the {@code BottomBar} draggable
     * widget, the given {@code JSplitPane}s divider location will be adjusted.
     *
     * @param splitPane the {@code JSplitPane} to connect the draggable widget to.
     */
    public void installDraggableWidgetOnSplitPane(JSplitPane splitPane) {
        if (splitPane == null) {
            throw new IllegalArgumentException("JSplitPane cannot be null.");
        }

        fSplitPane = splitPane;
        this.getComponent().addMouseListener(fMouseListener);
        this.getComponent().addMouseMotionListener(fMouseListener);
        //fSplitterHandle.setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
    }

	// Private methods. ///////////////////////////////////////////////////////////////////////////

	private void createAndInstallBackgroundPainter() {
		fBottomBar.setBackgroundPainter(PlatformUtils.isLeopard()
				? createLeopardPainter() : createDefaultPainter());
	}

	private void createAndInstallBorder() {
		FocusStateMatteBorder outterBorder = new FocusStateMatteBorder(1, 0, 0, 0,
				MacColorUtils.getTexturedWindowToolbarBorderFocusedColor(),
				MacColorUtils.getTexturedWindowToolbarBorderUnfocusedColor(),
				fBottomBar.getComponent());
		Border innerBorder = BorderFactory.createMatteBorder(1, 0, 0, 0, getBorderHighlightColor());
		Border lineBorders = BorderFactory.createCompoundBorder(outterBorder, innerBorder);

		// TODO determine if there is a good standard for this. there doesn't seem to be any
		// TODO consistent value used by Apple.
		// left and right edge padding.
		int padding = 5;
		fBottomBar.getComponent().setBorder(
				BorderFactory.createCompoundBorder(lineBorders,
						BorderFactory.createEmptyBorder(0, padding, 0, padding)));
	}

	private static MacWidgetsPainter<Component> createDefaultPainter() {
		MacWidgetsPainter<Component> focusedPainter = new GradientPainter(
				ACTIVE_TOP_COLOR, ACTIVE_BOTTOM_COLOR);
		MacWidgetsPainter<Component> unfocusedPainter = new GradientPainter(
				INACTIVE_TOP_COLOR, INACTIVE_BOTTOM_COLOR);
		return new FocusStatePainter(focusedPainter, focusedPainter, unfocusedPainter);
	}

	private static MacWidgetsPainter<Component> createLeopardPainter() {
		MacWidgetsPainter<Component> focusedPainter = new GradientPainter(
				LEOPARD_ACTIVE_TOP_COLOR, LEOPARD_ACTIVE_BOTTOM_COLOR);
		MacWidgetsPainter<Component> unfocusedPainter = new GradientPainter(
				LEOPARD_INACTIVE_TOP_COLOR, LEOPARD_INACTIVE_BOTTOM_COLOR);
		return new FocusStatePainter(focusedPainter, focusedPainter, unfocusedPainter);
	}

	private static Color getBorderHighlightColor() {
		return PlatformUtils.isLeopard() ? LEOPARD_BORDER_HIGHLIGHT_COLOR : BORDER_HIGHLIGHT_COLOR;
	}
	
	   
    // Mouse handler for splitter control widget. /////////////////////////////////////////////////

    private class SplitterHandleMouseMovementHandler extends MouseAdapter
            implements MouseMotionListener {

        private int fDelta;

        @Override
        public void mousePressed(MouseEvent e) {
            MouseEvent convertedEvent =
                    SwingUtilities.convertMouseEvent(BottomBar.this.getComponent(), e, fSplitPane);

            fDelta = fSplitPane.getDividerLocation() - convertedEvent.getPoint().y;
        }
        
        // MouseMotionListener implementation /////////////////////////////////////////////////////

        public void mouseDragged(MouseEvent e) {
            MouseEvent convertedEvent =
                    SwingUtilities.convertMouseEvent(BottomBar.this.getComponent(), e, fSplitPane);
            int newLocation = convertedEvent.getPoint().y + fDelta;
            // bound newLocation between the minimum and maximum divider locations.
            int boundedNewLocation = Math.max(fSplitPane.getMinimumDividerLocation(),
                    Math.min(newLocation, fSplitPane.getMaximumDividerLocation()));
            fSplitPane.setDividerLocation(boundedNewLocation);
        }

        public void mouseMoved(MouseEvent e) {
        }
    }    

}
