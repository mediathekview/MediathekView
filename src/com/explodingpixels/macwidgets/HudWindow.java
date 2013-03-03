package com.explodingpixels.macwidgets;

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import com.explodingpixels.util.PlatformUtils;
import com.explodingpixels.widgets.WindowDragger;
import com.explodingpixels.widgets.WindowUtils;

/**
 * <p>
 * An implementation of an OS X Transparent Panel, also known as a Heads Up Display (HUD). For a
 * full descrption of what a Transparent Panel is, see the <a href="http://developer.apple.com/documentation/UserExperience/Conceptual/AppleHIGuidelines/XHIGWindows/chapter_18_section_6.html#//apple_ref/doc/uid/20000961-SW24">Transparent Panels</a>
 * section of Apple's Human Interface Guidelines.
 * </p>
 * <p>
 * HUD's are designed to offer a lightweight way to unobtrusivley offer controls to the user. The window
 * looks like this:
 * <br>
 * <img src="../../../../graphics/HeadsUpDisplay.png">
 * <br>
 * <p>
 * As Apple points out, this component is not appropriate for all situations and should be used
 * judiciously.
 * </p>
 */
public class HudWindow {

    private final JDialog fDialog;
    private JComponent fContentPane;
    private final TitlePanel fTitlePanel;
    private final HudPanel fHudPanel = new HudPanel();
    private final BottomPanel fBottomPanel;

    private static final int ROUNDED_RECT_DIAMETER = 16;

    /**
     * Creates a Heads Up Display style window.
     */
    public HudWindow() {
        this("");
    }

    /**
     * Creates a Heads Up Display style window.
     *
     * @param title the title to use for this window.
     */
    public HudWindow(String title) {
        this(title, null);
    }

    /**
     * Creates a Heads Up Display style window.
     *
     * @param title the title to use for this window.
     * @param owner the {@link Frame} that this HUD is parented to. Can be null.
     */
    public HudWindow(String title, Frame owner) {
        fDialog = new JDialog(owner);
        fDialog.setTitle(title);
        fTitlePanel = new TitlePanel(title, createCloseButtonActionListener());
        fBottomPanel = new BottomPanel(fDialog);
        init();
    }

    private void init() {
        // indicate that this frame should not make all the content draggable. by default, when you
        // set the opacity to 0 (like we do below) this property automatically gets set to true.
        // also note that this client property must be set *before* changing the opacity (not sure
        // why).
        fDialog.getRootPane().putClientProperty("apple.awt.draggableWindowBackground", Boolean.FALSE);
        fDialog.setUndecorated(true);
        fDialog.getRootPane().setOpaque(false);
        
        WindowUtils.makeWindowNonOpaque(fDialog);
        // for Java 5 on platforms other than Mac (those that don't support transparency), it looks
        // nicer to use a black background rather than the default (usually white).
        fDialog.getRootPane().setBackground(Color.BLACK);

        fHudPanel.add(fTitlePanel, BorderLayout.NORTH);

        // set the backing frame's content pane.
        fDialog.setContentPane(fHudPanel);
        // set the HUD panel's content pane to a blank JPanel by default.
        JPanel panel = new JPanel();
        panel.setOpaque(false);
        setContentPane(panel);

        // listen to the frame's title property so that we can update the label rendering the title.
        fDialog.addPropertyChangeListener("title", createTitlePropertyChangeListener());

        WindowUtils.createAndInstallRepaintWindowFocusListener(fDialog);
        new WindowDragger(fDialog, fTitlePanel);
    }

    /**
     * Gets the {@link JDialog} backing this {@code HudWindow}.
     *
     * @return the {@code JDialog} backing this {@code HudWindow}.
     */
    public JDialog getJDialog() {
        return fDialog;
    }

    /**
     * Hides the close button on this HUD's title bar.
     */
    public void hideCloseButton() {
        fTitlePanel.hideCloseButton();
    }

    /**
     * Makes this HUD resizeable. Note that there is currently a bug in the Mac JRE which causes a very bad flicker when
     * a window is programmatically resized. For now, it's suggested that you do not use this method unless you are on a
     * non-Mac platform.
     */
    public void makeResizeable() {
        fHudPanel.add(fBottomPanel, BorderLayout.SOUTH);
    }

    /**
     * Gets the {@link JComponent} to add content to.
     *
     * @return the container to add content to.
     */
    public JComponent getContentPane() {
        return fContentPane;
    }

    /**
     * Sets the {@link JComponent} to use as the container for this {@code HudWindow}'s content.
     *
     * @param contentPane the container for this {@code HudWindow}'s content.
     */
    public void setContentPane(JComponent contentPane) {
        // remove the old content pane if there was one.
        if (fContentPane != null) {
            fHudPanel.remove(fContentPane);
        }
        fContentPane = contentPane;
        fHudPanel.add(fContentPane, BorderLayout.CENTER);
    }

    private PropertyChangeListener createTitlePropertyChangeListener() {
        return new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                fTitlePanel.setTitle(fDialog.getTitle());
            }
        };
    }

    private ActionListener createCloseButtonActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // simulate clicking the "real" close button on a window.
                fDialog.dispatchEvent(new WindowEvent(fDialog, WindowEvent.WINDOW_CLOSING));
            }
        };
    }

    private static class TitlePanel extends JPanel {

        private static final Color FONT_COLOR = new Color(255, 255, 255, 255);

        private static final Color UNFOCUSED_FONT_COLOR = new Color(0xcccccc);

        private static final Color HIGHLIGHT = new Color(255, 255, 255, 25);

        private static final Color TOP_BACKGROUND_TOP = new Color(255, 255, 255, 59);

        private static final Color TOP_BACKGROUND_BOTTOM = new Color(196, 196, 196, 59);

        private static final Color BOTTOM_BACKGROUND = new Color(255, 255, 255, 30);

        private static final Color UNFOCUSED_BACKGROUND = new Color(0, 0, 0, 10);

        private static final Icon CLOSE_ICON = new ImageIcon(
                TitlePanel.class.getResource(
                        "/com/explodingpixels/macwidgets/images/close.png"));

        private static final Icon CLOSE_HOVER_ICON = new ImageIcon(
                TitlePanel.class.getResource(
                        "/com/explodingpixels/macwidgets/images/close_hover.png"));

        private static final Icon CLOSE_PRESSED_ICON = new ImageIcon(
                TitlePanel.class.getResource(
                        "/com/explodingpixels/macwidgets/images/close_pressed.png"));

        private final JButton fCloseButton = new JButton(CLOSE_ICON);

        private final JComponent fSpacer;

        private final JLabel fLabel;

        private TitlePanel(String title, ActionListener closeButtonActionListener) {
            fLabel = new JLabel(title, SwingConstants.CENTER);
            fLabel.setFont(fLabel.getFont().deriveFont(Font.BOLD, 11.0f));

            setOpaque(false);
            setPreferredSize(new Dimension(-1, 20));
            updateFocusState();

            fCloseButton.setBorder(getCloseButtonBorder());
            fCloseButton.setVerticalAlignment(SwingConstants.CENTER);
            fCloseButton.setOpaque(false);
            fCloseButton.setFocusable(false);
            fCloseButton.setBorderPainted(false);
            fCloseButton.setContentAreaFilled(false);
            fCloseButton.setRolloverIcon(CLOSE_HOVER_ICON);
            fCloseButton.setPressedIcon(CLOSE_PRESSED_ICON);
            fCloseButton.addActionListener(closeButtonActionListener);

            fSpacer = MacWidgetFactory.createSpacer(fCloseButton.getPreferredSize().width, 0);

            setLayout(new BorderLayout());
            add(fLabel, BorderLayout.CENTER);
            add(fCloseButton, PlatformUtils.isMac() ? BorderLayout.WEST : BorderLayout.EAST);
            add(fSpacer, PlatformUtils.isMac() ? BorderLayout.EAST : BorderLayout.WEST);
        }

        private void hideCloseButton() {
            fCloseButton.setVisible(false);
            fSpacer.setVisible(false);
        }

        private Border getCloseButtonBorder() {
            return PlatformUtils.isMac()
                    ? BorderFactory.createEmptyBorder(0, 5, 0, 0)
                    : BorderFactory.createEmptyBorder(0, 0, 0, 5);
        }

        private void setTitle(String title) {
            fLabel.setText(title);
        }

        private void updateFocusState() {
            Boolean focused = WindowUtils.isParentWindowFocused(this);
            fLabel.setForeground(focused == null || focused ? FONT_COLOR : UNFOCUSED_FONT_COLOR);
        }

        @Override
        protected void paintComponent(Graphics g) {
            // create a copy of the graphics object and turn on anti-aliasing.
            Graphics2D graphics2d = (Graphics2D) g.create();
            graphics2d.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            // calculate the point in the title bar at which to change the background color.
            int midPointY = ROUNDED_RECT_DIAMETER / 2 + 3;

            // if the window has focus, draw a shiny title bar.
            // else draw a flat background.
            if (WindowUtils.isParentWindowFocused(this)) {
                // 1. The top half --------------------------------------------------------------//
                // create and set the shiny paint.
                GradientPaint paint =
                        new GradientPaint(0, 0, TOP_BACKGROUND_TOP, 0, midPointY, TOP_BACKGROUND_BOTTOM);
                graphics2d.setPaint(paint);
                // create a rounded rectangle area as big as the entire title bar, then subtract
                // off the bottom half (roughly) in order to have perfectly square edges.
                Area titleArea = new Area(new Area(new RoundRectangle2D.Double(
                        0, 0, getWidth(), getHeight(), ROUNDED_RECT_DIAMETER, ROUNDED_RECT_DIAMETER)));
                titleArea.subtract(new Area(new Rectangle(0, midPointY, getWidth(), midPointY)));
                // draw the top half of the title bar (the shine).
                graphics2d.fill(titleArea);
                // 2. The bottom half -----------------------------------------------------------//
                // draw the bottom half of the title bar.
                int bottomHeight = getHeight() - midPointY;
                graphics2d.setColor(BOTTOM_BACKGROUND);
                graphics2d.fillRect(0, midPointY, getWidth(), bottomHeight);
            } else {
                // create an area that has rounded corners at the top and square corners at the
                // bottom.
                graphics2d.setColor(UNFOCUSED_BACKGROUND);
                Area titleArea = new Area(new Area(new RoundRectangle2D.Double(
                        0, 0, getWidth(), getHeight(), ROUNDED_RECT_DIAMETER, ROUNDED_RECT_DIAMETER)));
                titleArea.subtract(new Area(
                        new Rectangle(0, midPointY, getWidth(), midPointY)));
                graphics2d.fill(titleArea);
                graphics2d.setColor(HIGHLIGHT);
                graphics2d.drawLine(0, getHeight() - 1, getWidth(), getHeight() - 1);
            }

            graphics2d.dispose();
        }

    }

    private static class HudPanel extends JPanel {

        private static final Color HIGHLIGHT = new Color(255, 255, 255, 59);
        private static final Color HIGHLIGHT_BOTTOM = new Color(255, 255, 255, 25);
        private static final Color BACKGROUND = new Color(30, 30, 30, 216);

        private HudPanel() {
            setLayout(new BorderLayout());
            setOpaque(false);
        }

        @Override
        protected void paintBorder(Graphics g) {
            // create a copy of the graphics object and turn on anti-aliasing.
            Graphics2D graphics2d = (Graphics2D) g.create();
            graphics2d.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            // paint a border around the window that fades slightly to give a more pleasnt highlight
            // to the window edges.
            GradientPaint paint = new GradientPaint(0, 0, HIGHLIGHT, 0, getHeight(), HIGHLIGHT_BOTTOM);
            graphics2d.setPaint(paint);
            graphics2d.drawRoundRect(0, 0, getWidth() - 1, getHeight() - 1, ROUNDED_RECT_DIAMETER,
                    ROUNDED_RECT_DIAMETER);

            graphics2d.dispose();
        }

        @Override
        protected void paintComponent(Graphics g) {
            // create a copy of the graphics object and turn on anti-aliasing.
            Graphics2D graphics2d = (Graphics2D) g.create();
            graphics2d.setRenderingHint(
                    RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            graphics2d.setComposite(AlphaComposite.Src);

            // draw the rounded rectangle background of the window.
            graphics2d.setColor(BACKGROUND);
            graphics2d.fillRoundRect(0, 0, getWidth(), getHeight(),
                    ROUNDED_RECT_DIAMETER, ROUNDED_RECT_DIAMETER);
            // tell the shadow to revalidate.
            getRootPane().putClientProperty("apple.awt.windowShadow.revalidateNow", new Object());

            graphics2d.dispose();
        }

    }

    private static class BottomPanel extends JPanel {

        private static final Icon RESIZE_ICON = new ImageIcon(
                TitlePanel.class.getResource(
                        "/com/explodingpixels/macwidgets/images/resize_corner_dark.png"));

        private final Window fWindow;
        private final JLabel fResizeCorner = new JLabel(RESIZE_ICON);
        private int fXOffsetToWindowEdge;
        private int fYOffsetToWidnowEdge;

        public BottomPanel(Window window) {
            super(new FlowLayout(SwingConstants.RIGHT));
            fWindow = window;
            setOpaque(false);
            add(fResizeCorner);
            fResizeCorner.addMouseListener(createMouseListener());
            fResizeCorner.addMouseMotionListener(createMouseMotionListener());
        }

        private MouseAdapter createMouseListener() {
            return new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    Point windowPoint =
                            SwingUtilities.convertPoint(fResizeCorner, e.getPoint(), fWindow);
                    fXOffsetToWindowEdge = fWindow.getWidth() - windowPoint.x;
                    fYOffsetToWidnowEdge = fWindow.getHeight() - windowPoint.y;
                }
            };
        }

        private MouseMotionListener createMouseMotionListener() {
            return new MouseMotionAdapter() {
                @Override
                public void mouseDragged(MouseEvent e) {
                    Point windowPoint = SwingUtilities.convertPoint(fResizeCorner, e.getPoint(), fWindow);
                    fWindow.setSize(windowPoint.x + fXOffsetToWindowEdge,
                            windowPoint.y + fYOffsetToWidnowEdge);

                    // the following two lines are a work-around to Sun bug 6318144:
                    // http://bugs.sun.com/view_bug.do;?bug_id=6318144
                    fWindow.invalidate();
                    fWindow.validate();
                }
            };
        }

    }
}
