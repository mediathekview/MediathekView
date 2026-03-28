/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.swing;

import com.formdev.flatlaf.FlatLaf;
import com.formdev.flatlaf.FlatLightLaf;
import com.formdev.flatlaf.themes.FlatMacLightLaf;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.RoundRectangle2D;
import java.util.Objects;

public final class SwingPopoverControl {

    private JDialog window;                    // <-- was JWindow, jetzt JDialog (pro show neu)
    private final Bubble bubble;
    private final JPanel contentHost;
    private boolean perPixelTransparencyEnabled = true;

    private AWTEventListener outsideClickListener;
    private KeyEventDispatcher escDispatcher;

    private boolean dismissOnFocusLost;

    /**
     * Distance between anchor and ARROW TIP (px).
     */
    private int gap = 3;
    private int marginToScreen = 8;

    private Component currentAnchor;
    private Placement requestedPlacement = Placement.AUTO;
    private boolean anchorWasEnabled;

    private Window anchorTopLevelWindow;
    private ComponentListener topLevelMoveResizeListener;
    private HierarchyBoundsListener anchorHierarchyBoundsListener;
    private HierarchyListener anchorHierarchyListener;

    public SwingPopoverControl() {
        bubble = new Bubble();
        bubble.setOpaque(false);
        bubble.setLayout(new BorderLayout());

        contentHost = new JPanel(new BorderLayout());
        contentHost.setOpaque(false);
        contentHost.setBorder(new EmptyBorder(12, 14, 12, 14));

        bubble.add(contentHost, BorderLayout.CENTER);
    }

    // --- Window lifecycle -----------------------------------------------------

    private void ensureWindowFor(Component anchor) {
        Window owner = SwingUtilities.getWindowAncestor(anchor);

        // Owner kann bei JDialog nicht sauber "umgehängt" werden -> pro show neu bauen
        if (window != null) {
            try {
                window.setVisible(false);
            } catch (Exception ignored) {
            }
            window.dispose();
            window = null;
        }

        window = new JDialog(owner);
        window.setModalityType(Dialog.ModalityType.MODELESS);
        window.setUndecorated(true);

        // POPUP vermeiden -> kann unter macOS/Wayland Keyboard-Input blocken
        window.setType(Window.Type.UTILITY);

        //window.setAlwaysOnTop(true);
        window.setFocusableWindowState(true);
        window.setAutoRequestFocus(true);
        perPixelTransparencyEnabled = supportsPerPixelTransparency(anchor);
        bubble.setPerPixelTransparencyEnabled(perPixelTransparencyEnabled);

        if (perPixelTransparencyEnabled) {
            window.setBackground(new Color(0, 0, 0, 0));
        } else {
            Color fallbackBg = UIManager.getColor("RootPane.background");
            if (fallbackBg == null)
                fallbackBg = UIManager.getColor("Panel.background");
            if (fallbackBg == null)
                fallbackBg = Color.WHITE;
            window.setBackground(fallbackBg);
        }

        // bubble als ContentPane verwenden
        window.setContentPane(bubble);

        window.addWindowFocusListener(new WindowAdapter() {
            @Override
            public void windowLostFocus(WindowEvent e) {
                if (dismissOnFocusLost)
                    hide();
            }
        });
    }

    private static boolean supportsPerPixelTransparency(Component anchor) {
        if (isWaylandSession())
            return false;

        GraphicsConfiguration gc = (anchor != null) ? anchor.getGraphicsConfiguration() : null;
        GraphicsDevice device = (gc != null) ? gc.getDevice() : null;
        if (device == null) {
            device = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        }
        return device != null
                && device.isWindowTranslucencySupported(GraphicsDevice.WindowTranslucency.PERPIXEL_TRANSLUCENT);
    }

    private static boolean isWaylandSession() {
        if (!SystemUtils.IS_OS_LINUX)
            return false;
        String sessionType = System.getenv("XDG_SESSION_TYPE");
        if (sessionType != null && "wayland".equalsIgnoreCase(sessionType.trim()))
            return true;
        return System.getenv("WAYLAND_DISPLAY") != null;
    }

    private static Rectangle getScreenBounds(Component c) {
        Point p = c.getLocationOnScreen(); // kann IllegalComponentStateException werfen, wenn nicht showing
        return new Rectangle(p.x, p.y, c.getWidth(), c.getHeight());
    }

    private static Rectangle clamp(Rectangle r, Rectangle bounds) {
        final int x = Math.clamp(r.x, bounds.x, bounds.x + bounds.width - r.width);
        final int y = Math.clamp(r.y, bounds.y, bounds.y + bounds.height - r.height);
        return new Rectangle(x, y, r.width, r.height);
    }

    private static void focusFirstComponent(Container root) {
        Component focus = findFocusable(root);
        if (focus != null)
            focus.requestFocusInWindow();
    }

    private static Component findFocusable(Container c) {
        for (Component child : c.getComponents()) {
            if (child.isFocusable() && child.isEnabled() && child.isShowing())
                return child;
            if (child instanceof Container cc) {
                Component nested = findFocusable(cc);
                if (nested != null)
                    return nested;
            }
        }
        return null;
    }

    public void setDismissOnFocusLost(boolean v) {
        this.dismissOnFocusLost = v;
    }

    public int getGap() {
        return gap;
    }

    public void setGap(int px) {
        this.gap = Math.max(0, px);
    }

    public void setMarginToScreen(int px) {
        this.marginToScreen = Math.max(0, px);
    }

    public boolean isShowing() {
        return window != null && window.isVisible();
    }

    public void show(Component anchor, JComponent content, Placement placement) {
        Objects.requireNonNull(anchor, "anchor");
        Objects.requireNonNull(content, "content");

        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(() -> show(anchor, content, placement));
            return;
        }

        // Ohne showing kein getLocationOnScreen()
        if (!anchor.isShowing()) {
            // in vielen Fällen reicht "später nochmal" (z.B. direkt nach Layout)
            SwingUtilities.invokeLater(() -> {
                if (anchor.isShowing())
                    show(anchor, content, placement);
            });
            return;
        }

        ensureWindowFor(anchor);

        // Replace content
        contentHost.removeAll();
        contentHost.add(content, BorderLayout.CENTER);

        // Track current anchor & placement
        currentAnchor = anchor;
        requestedPlacement = (placement == null) ? Placement.AUTO : placement;

        window.pack();

        recomputePlacementAndBounds();

        installDismissHandlers();
        installTrackingHandlers();

        window.setVisible(true);
        window.toFront();
        window.requestFocusInWindow();

        anchorWasEnabled = anchor.isEnabled();
        SwingUtilities.invokeLater(() -> anchor.setEnabled(false));
        SwingUtilities.invokeLater(() -> focusFirstComponent(contentHost));
    }

    public void hide() {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(this::hide);
            return;
        }
        if (window == null || !window.isVisible())
            return;

        uninstallTrackingHandlers();
        uninstallDismissHandlers();

        if (currentAnchor != null)
            currentAnchor.setEnabled(anchorWasEnabled);
        currentAnchor = null;

        window.setVisible(false);
        window.dispose();
        window = null;
    }

    public void reposition() {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(this::reposition);
            return;
        }
        if (!isShowing() || currentAnchor == null)
            return;
        if (!currentAnchor.isShowing()) {
            hide();
            return;
        }
        recomputePlacementAndBounds();
    }

    private void recomputePlacementAndBounds() {
        if (currentAnchor == null || window == null)
            return;

        GraphicsConfiguration gc = currentAnchor.getGraphicsConfiguration();
        if (gc == null) {
            gc = GraphicsEnvironment.getLocalGraphicsEnvironment()
                    .getDefaultScreenDevice().getDefaultConfiguration();
        }

        Rectangle screen = gc.getBounds();
        Insets screenInsets = Toolkit.getDefaultToolkit().getScreenInsets(gc);

        Rectangle usable = new Rectangle(
                screen.x + screenInsets.left + marginToScreen,
                screen.y + screenInsets.top + marginToScreen,
                screen.width - screenInsets.left - screenInsets.right - marginToScreen * 2,
                screen.height - screenInsets.top - screenInsets.bottom - marginToScreen * 2
        );

        Rectangle anchorOnScreen;
        try {
            anchorOnScreen = getScreenBounds(currentAnchor);
        } catch (IllegalComponentStateException ex) {
            hide();
            return;
        }

        // Erstgröße holen
        Dimension popSize = window.getSize();

        Placement resolvedPlacement = (requestedPlacement == Placement.AUTO)
                ? pickAutoPlacement(anchorOnScreen, popSize, usable)
                : requestedPlacement;

        bubble.setPlacement(resolvedPlacement);

        // Insets können sich ändern -> pack und Größe neu holen
        window.pack();
        popSize = window.getSize();

        Insets bubbleInsets = bubble.getInsets();
        Point loc = computeLocationByArrowTip(anchorOnScreen, popSize, resolvedPlacement, bubbleInsets);

        Rectangle desired = new Rectangle(loc.x, loc.y, popSize.width, popSize.height);
        Rectangle clamped = clamp(desired, usable);

        bubble.setArrowTarget(anchorCenterFor(resolvedPlacement, anchorOnScreen, clamped));

        window.setBounds(clamped);
    }

    private Placement pickAutoPlacement(Rectangle anchor, Dimension pop, Rectangle usable) {
        int spaceAbove = anchor.y - usable.y;
        int spaceBelow = (usable.y + usable.height) - (anchor.y + anchor.height);
        int spaceLeft = anchor.x - usable.x;
        int spaceRight = (usable.x + usable.width) - (anchor.x + anchor.width);

        int reqV = pop.height + gap;
        int reqH = pop.width + gap;

        record Cand(Placement p, int score, boolean fits) {
        }

        Cand top = new Cand(Placement.TOP, spaceAbove - reqV, spaceAbove >= reqV);
        Cand bottom = new Cand(Placement.BOTTOM, spaceBelow - reqV, spaceBelow >= reqV);
        Cand left = new Cand(Placement.LEFT, spaceLeft - reqH, spaceLeft >= reqH);
        Cand right = new Cand(Placement.RIGHT, spaceRight - reqH, spaceRight >= reqH);

        Cand[] cands = new Cand[]{bottom, top, right, left}; // Bias
        Cand best = cands[0];

        for (Cand c : cands) {
            if (c.fits && !best.fits) {
                best = c;
                continue;
            }
            if (c.fits == best.fits && c.score > best.score)
                best = c;
        }
        return best.p;
    }

    private Point computeLocationByArrowTip(Rectangle anchor, Dimension pop, Placement p, Insets in) {
        int x, y;

        switch (p) {
            case TOP -> {
                y = anchor.y - gap - (pop.height - in.bottom);
                x = (int) Math.round(anchor.getCenterX() - pop.width / 2.0);
            }
            case RIGHT -> {
                x = anchor.x + anchor.width + gap - in.left;
                y = (int) Math.round(anchor.getCenterY() - pop.height / 2.0);
            }
            case LEFT -> {
                x = anchor.x - gap - (pop.width - in.right);
                y = (int) Math.round(anchor.getCenterY() - pop.height / 2.0);
            }
            default -> {
                y = anchor.y + anchor.height + gap - in.top;
                x = (int) Math.round(anchor.getCenterX() - pop.width / 2.0);
            }
        }
        return new Point(x, y);
    }

    private Point2D anchorCenterFor(Placement p, Rectangle anchor, Rectangle popoverBounds) {
        double ax = anchor.getCenterX();
        double ay = anchor.getCenterY();

        return switch (p) {
            case TOP -> new Point2D.Double(ax - popoverBounds.x, popoverBounds.height);
            case LEFT -> new Point2D.Double(popoverBounds.width, ay - popoverBounds.y);
            case RIGHT -> new Point2D.Double(0, ay - popoverBounds.y);
            default -> new Point2D.Double(ax - popoverBounds.x, 0);
        };
    }

    // --- tracking handlers ----------------------------------------------------

    private void installTrackingHandlers() {
        uninstallTrackingHandlers();

        if (currentAnchor == null)
            return;

        anchorTopLevelWindow = SwingUtilities.getWindowAncestor(currentAnchor);
        if (anchorTopLevelWindow != null) {
            topLevelMoveResizeListener = new ComponentAdapter() {
                @Override
                public void componentMoved(ComponentEvent e) {
                    reposition();
                }

                @Override
                public void componentResized(ComponentEvent e) {
                    reposition();
                }
            };
            anchorTopLevelWindow.addComponentListener(topLevelMoveResizeListener);
        }

        anchorHierarchyBoundsListener = new HierarchyBoundsAdapter() {
            @Override
            public void ancestorMoved(HierarchyEvent e) {
                reposition();
            }

            @Override
            public void ancestorResized(HierarchyEvent e) {
                reposition();
            }
        };
        currentAnchor.addHierarchyBoundsListener(anchorHierarchyBoundsListener);

        anchorHierarchyListener = e -> {
            if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0) {
                if (currentAnchor != null && !currentAnchor.isShowing())
                    hide();
                else
                    reposition();
            }
            if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                installTrackingHandlers();
                reposition();
            }
        };
        currentAnchor.addHierarchyListener(anchorHierarchyListener);
    }

    private void uninstallTrackingHandlers() {
        if (anchorTopLevelWindow != null && topLevelMoveResizeListener != null) {
            anchorTopLevelWindow.removeComponentListener(topLevelMoveResizeListener);
        }
        anchorTopLevelWindow = null;
        topLevelMoveResizeListener = null;

        if (currentAnchor != null && anchorHierarchyBoundsListener != null) {
            currentAnchor.removeHierarchyBoundsListener(anchorHierarchyBoundsListener);
        }
        anchorHierarchyBoundsListener = null;

        if (currentAnchor != null && anchorHierarchyListener != null) {
            currentAnchor.removeHierarchyListener(anchorHierarchyListener);
        }
        anchorHierarchyListener = null;
    }

    // --- dismiss handlers -----------------------------------------------------

    private boolean isEventFromAnchor(MouseEvent me) {
        Component src = me.getComponent();
        if (src == null || currentAnchor == null)
            return false;
        return SwingUtilities.isDescendingFrom(src, currentAnchor);
    }

    private void installDismissHandlers() {
        if (outsideClickListener == null) {
            outsideClickListener = event -> {
                if (!(event instanceof MouseEvent me))
                    return;
                if (me.getID() != MouseEvent.MOUSE_PRESSED)
                    return;
                if (!isShowing())
                    return;

                // Click inside popover -> ignore
                Point p = me.getLocationOnScreen();
                if (window.getBounds().contains(p))
                    return;

                // Click on anchor -> ignore (toggle entscheidet)
                if (isEventFromAnchor(me))
                    return;

                hide();
            };
            Toolkit.getDefaultToolkit().addAWTEventListener(outsideClickListener, AWTEvent.MOUSE_EVENT_MASK);
        }

        if (escDispatcher == null) {
            escDispatcher = e -> {
                if (!isShowing())
                    return false;
                if (e.getID() == KeyEvent.KEY_PRESSED && e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    hide();
                    return true;
                }
                return false;
            };
            KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(escDispatcher);
        }
    }

    private void uninstallDismissHandlers() {
        if (outsideClickListener != null) {
            Toolkit.getDefaultToolkit().removeAWTEventListener(outsideClickListener);
            outsideClickListener = null;
        }
        if (escDispatcher != null) {
            KeyboardFocusManager.getCurrentKeyboardFocusManager().removeKeyEventDispatcher(escDispatcher);
            escDispatcher = null;
        }
    }

    public enum Placement {TOP, BOTTOM, LEFT, RIGHT, AUTO}

    // --- Rendering ------------------------------------------------------------

    private static final class Bubble extends JComponent {
        private final int arrowH = 10;
        private Placement placement = Placement.BOTTOM;
        private Point2D arrowTarget = new Point2D.Double(60, 0);
        private boolean perPixelTransparencyEnabled = true;

        private static void appendArrow(Path2D path, Point2D a, Point2D tip, Point2D b) {
            Path2D tri = new Path2D.Double();
            tri.moveTo(a.getX(), a.getY());
            tri.lineTo(tip.getX(), tip.getY());
            tri.lineTo(b.getX(), b.getY());
            tri.closePath();
            path.append(tri, false);
        }

        void setPlacement(Placement p) {
            Placement np = (p == null) ? Placement.BOTTOM : p;
            if (np != placement)
                placement = np;
            revalidate();
            repaint();
        }

        void setArrowTarget(Point2D p) {
            if (p != null) {
                arrowTarget = p;
                repaint();
            }
        }

        void setPerPixelTransparencyEnabled(boolean enabled) {
            if (perPixelTransparencyEnabled != enabled) {
                perPixelTransparencyEnabled = enabled;
                repaint();
            }
        }

        @Override
        public Insets getInsets() {
            if (!perPixelTransparencyEnabled) {
                return new Insets(0, 0, 0, 0);
            }
            int top = 0, left = 0, bottom = 0, right = 0;
            switch (placement) {
                case TOP -> bottom += arrowH;
                case BOTTOM -> top += arrowH;
                case LEFT -> right += arrowH;
                case RIGHT -> left += arrowH;
            }
            return new Insets(top, left, bottom, right);
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g.create();
            try {
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

                Insets in = getInsets();
                int w = getWidth();
                int h = getHeight();

                int bx = in.left;
                int by = in.top;
                int bw = w - in.left - in.right;
                int bh = h - in.top - in.bottom;

                Color bubbleBackground = UIManager.getColor("RootPane.background");
                if (bubbleBackground == null)
                    bubbleBackground = UIManager.getColor("Panel.background");
                if (bubbleBackground == null)
                    bubbleBackground = Color.WHITE;

                if (!perPixelTransparencyEnabled) {
                    // Wayland fallback: render a plain rectangular popover style.
                    g2.setComposite(AlphaComposite.Src);
                    g2.setColor(bubbleBackground);
                    g2.fillRect(0, 0, w, h);
                    g2.setComposite(AlphaComposite.SrcOver);

                    if (FlatLaf.isLafDark())
                        g2.setColor(UIManager.getColor("Button.hoverBackground"));
                    else
                        g2.setColor(new Color(0, 0, 0, 55));
                    g2.setStroke(new BasicStroke(1f));
                    g2.drawRect(0, 0, w - 1, h - 1);
                    return;
                }

                Rectangle bubbleRect = new Rectangle(bx, by, bw, bh);
                switch (placement) {
                    case TOP -> bubbleRect.height -= arrowH;
                    case BOTTOM -> {
                        bubbleRect.y += arrowH;
                        bubbleRect.height -= arrowH;
                    }
                    case LEFT -> bubbleRect.width -= arrowH;
                    case RIGHT -> {
                        bubbleRect.x += arrowH;
                        bubbleRect.width -= arrowH;
                    }
                }

                Shape bubbleShape = makeBubbleShape(bubbleRect);

                g2.setComposite(AlphaComposite.SrcOver);
                g2.setColor(bubbleBackground);
                g2.fill(bubbleShape);

                if (FlatLaf.isLafDark())
                    g2.setColor(UIManager.getColor("Button.hoverBackground"));
                else
                    g2.setColor(new Color(0, 0, 0, 55));
                g2.setStroke(new BasicStroke(1f));
                g2.draw(bubbleShape);
            } finally {
                g2.dispose();
            }
        }

        private Shape makeBubbleShape(Rectangle r) {
            int arc = 16;
            RoundRectangle2D rr = new RoundRectangle2D.Double(r.x, r.y, r.width, r.height, arc, arc);
            Path2D path = new Path2D.Double();
            path.append(rr, false);

            double tx = arrowTarget.getX();
            double ty = arrowTarget.getY();

            int arrowW = 18;

            switch (placement) {
                case BOTTOM -> {
                    double baseY = r.y;
                    double tipY = baseY - arrowH;
                    double tipX = Math.clamp(tx, r.x + arc, r.x + r.width - arc);
                    appendArrow(path,
                            new Point2D.Double(tipX - arrowW / 2.0, baseY),
                            new Point2D.Double(tipX, tipY),
                            new Point2D.Double(tipX + arrowW / 2.0, baseY));
                }
                case TOP -> {
                    double baseY = r.y + r.height;
                    double tipY = baseY + arrowH;
                    double tipX = Math.clamp(tx, r.x + arc, r.x + r.width - arc);
                    appendArrow(path,
                            new Point2D.Double(tipX - arrowW / 2.0, baseY),
                            new Point2D.Double(tipX, tipY),
                            new Point2D.Double(tipX + arrowW / 2.0, baseY));
                }
                case RIGHT -> {
                    double baseX = r.x;
                    double tipX = baseX - arrowH;
                    double tipY = Math.clamp(ty, r.y + arc, r.y + r.height - arc);
                    appendArrow(path,
                            new Point2D.Double(baseX, tipY - arrowW / 2.0),
                            new Point2D.Double(tipX, tipY),
                            new Point2D.Double(baseX, tipY + arrowW / 2.0));
                }
                case LEFT -> {
                    double baseX = r.x + r.width;
                    double tipX = baseX + arrowH;
                    double tipY = Math.clamp(ty, r.y + arc, r.y + r.height - arc);
                    appendArrow(path,
                            new Point2D.Double(baseX, tipY - arrowW / 2.0),
                            new Point2D.Double(tipX, tipY),
                            new Point2D.Double(baseX, tipY + arrowW / 2.0));
                }
            }
            return path;
        }
    }

    static void main() {
        SwingUtilities.invokeLater(() -> {
            try {
                if (SystemUtils.IS_OS_MAC_OSX)
                    FlatMacLightLaf.setup();
                else
                    FlatLightLaf.setup();
            } catch (Exception ignored) {
            }

            JFrame frame = new JFrame("SwingPopoverControl Demo");
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            frame.setSize(640, 400);
            frame.setLocationRelativeTo(null);

            JButton btnAuto = new JButton("Popover BOTTOM");
            JButton btnRight = new JButton("Popover RIGHT");

            JPanel top = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 10));
            top.add(btnAuto);
            top.add(btnRight);

            ActionListener show = e -> {
                JPanel content = new JPanel(new GridBagLayout());
                content.setOpaque(false);

                GridBagConstraints gc = new GridBagConstraints();
                gc.insets = new Insets(4, 4, 4, 4);
                gc.anchor = GridBagConstraints.WEST;

                gc.gridx = 0;
                gc.gridy = 0;
                content.add(new JLabel("Name:"), gc);

                gc.gridx = 1;
                JTextField tfName = new JTextField(14);
                content.add(tfName, gc);

                gc.gridx = 0;
                gc.gridy = 1;
                content.add(new JLabel("Tag:"), gc);

                gc.gridx = 1;
                JTextField tfTag = new JTextField(14);
                content.add(tfTag, gc);

                Component src = (Component) e.getSource();
                SwingPopoverControl.Placement p =
                        (src == btnRight)
                                ? SwingPopoverControl.Placement.RIGHT
                                : Placement.BOTTOM;

                var popover = new SwingPopoverControl();
                popover.show((Component)e.getSource(),content,p);
            };

            btnAuto.addActionListener(show);
            btnRight.addActionListener(show);

            frame.setLayout(new BorderLayout());
            frame.add(top, BorderLayout.NORTH);
            frame.add(new JLabel("Klicke einen Button und tippe in die Felder ✍️",
                    SwingConstants.CENTER), BorderLayout.CENTER);

            frame.setVisible(true);
        });
    }

}
