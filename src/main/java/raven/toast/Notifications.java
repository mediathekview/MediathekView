package raven.toast;

import com.formdev.flatlaf.ui.FlatUIUtils;
import com.formdev.flatlaf.util.Animator;
import com.formdev.flatlaf.util.UIScale;
import raven.toast.ui.ToastNotificationPanel;
import raven.toast.util.NotificationHolder;
import raven.toast.util.UIUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * <!-- FlatLaf Property -->
 * <p>
 * Toast.outlineWidth                   int         0       (default)
 * Toast.iconTextGap                    int         5       (default)
 * Toast.closeButtonGap                 int         5       (default)
 * Toast.arc                            int         20      (default)
 * Toast.horizontalGap                  int         10      (default)
 * <p>
 * Toast.limit                          int         -1      (default)   -1 as unlimited
 * Toast.duration                       long        2500    (default)
 * Toast.animation                      int         200     (default)
 * Toast.animationResolution            int         5       (default)
 * Toast.animationMove                  int         10      (default)
 * Toast.minimumWidth                   int         50      (default)
 * Toast.maximumWidth                   int         -1      (default)   -1 as not set
 * <p>
 * Toast.shadowColor                    Color
 * Toast.shadowOpacity                  float       0.1f    (default)
 * Toast.shadowInsets                   Insets      0,0,6,6 (default)
 * <p>
 * Toast.useEffect                      boolean     true    (default)
 * Toast.effectWidth                    float       0.5f    (default)   0.5f as 50%
 * Toast.effectOpacity                  float       0.2f    (default)   0 to 1
 * Toast.effectAlignment                String      left    (default)   left, right
 * Toast.effectColor                    Color
 * Toast.success.effectColor            Color
 * Toast.info.effectColor               Color
 * Toast.warning.effectColor            Color
 * Toast.error.effectColor              Color
 * <p>
 * Toast.outlineColor                   Color
 * Toast.foreground                     Color
 * Toast.background                     Color
 * <p>
 * Toast.success.outlineColor           Color
 * Toast.success.foreground             Color
 * Toast.success.background             Color
 * Toast.info.outlineColor              Color
 * Toast.info.foreground                Color
 * Toast.info.background                Color
 * Toast.warning.outlineColor           Color
 * Toast.warning.foreground             Color
 * Toast.warning.background             Color
 * Toast.error.outlineColor             Color
 * Toast.error.foreground               Color
 * Toast.error.background               Color
 * <p>
 * Toast.frameInsets                    Insets      10,10,10,10     (default)
 * Toast.margin                         Insets      8,8,8,8         (default)
 * <p>
 * Toast.showCloseButton                boolean     true            (default)
 * Toast.closeIconColor                 Color
 *
 * <p>
 * <!-- UIManager -->
 * <p>
 * Toast.success.icon                   Icon
 * Toast.info.icon                      Icon
 * Toast.warning.icon                   Icon
 * Toast.error.icon                     Icon
 * Toast.closeIcon                      Icon
 */

/**
 * @author Raven
 */
public class Notifications {

    private static Notifications instance;
    private JFrame frame;
    private final Map<Location, List<NotificationAnimation>> lists = new HashMap<>();
    private final NotificationHolder notificationHolder = new NotificationHolder();

    private ComponentListener windowEvent;

    private void installEvent(JFrame frame) {
        if (windowEvent == null && frame != null) {
            windowEvent = new ComponentAdapter() {
                @Override
                public void componentMoved(ComponentEvent e) {
                    move(frame.getBounds());
                }

                @Override
                public void componentResized(ComponentEvent e) {
                    move(frame.getBounds());
                }
            };
        }
        if (this.frame != null) {
            this.frame.removeComponentListener(windowEvent);
        }
        if (frame != null) {
            frame.addComponentListener(windowEvent);
        }
        this.frame = frame;
    }

    public static Notifications getInstance() {
        if (instance == null) {
            instance = new Notifications();
        }
        return instance;
    }

    private int getCurrentShowCount(Location location) {
        List list = lists.get(location);
        return list == null ? 0 : list.size();
    }

    private synchronized void move(Rectangle rectangle) {
        for (Map.Entry<Location, List<NotificationAnimation>> set : lists.entrySet()) {
            for (int i = 0; i < set.getValue().size(); i++) {
                NotificationAnimation an = set.getValue().get(i);
                if (an != null) {
                    an.move(rectangle);
                }
            }
        }
    }

    public void setJFrame(JFrame frame) {
        installEvent(frame);
    }

    public void show(Type type, String message) {
        show(type, Location.TOP_CENTER, message);
    }

    public void show(Type type, long duration, String message) {
        show(type, Location.TOP_CENTER, duration, message);
    }

    public void show(Type type, Location location, String message) {
        long duration = FlatUIUtils.getUIInt("Toast.duration", 2500);
        show(type, location, duration, message);
    }

    public void show(Type type, Location location, long duration, String message) {
        initStart(new NotificationAnimation(type, location, duration, message), duration);
    }

    public void show(JComponent component) {
        show(Location.TOP_CENTER, component);
    }

    public void show(Location location, JComponent component) {
        long duration = FlatUIUtils.getUIInt("Toast.duration", 2500);
        show(location, duration, component);
    }

    public void show(Location location, long duration, JComponent component) {
        initStart(new NotificationAnimation(location, duration, component), duration);
    }

    private synchronized boolean initStart(NotificationAnimation notificationAnimation, long duration) {
        int limit = FlatUIUtils.getUIInt("Toast.limit", -1);
        if (limit == -1 || getCurrentShowCount(notificationAnimation.getLocation()) < limit) {
            notificationAnimation.start();
            return true;
        } else {
            notificationHolder.hold(notificationAnimation);
            return false;
        }
    }

    private synchronized void notificationClose(NotificationAnimation notificationAnimation) {
        NotificationAnimation hold = notificationHolder.getHold(notificationAnimation.getLocation());
        if (hold != null) {
            if (initStart(hold, hold.getDuration())) {
                notificationHolder.removeHold(hold);
            }
        }
    }

    public void clearAll() {
        notificationHolder.clearHold();
        for (Map.Entry<Location, List<NotificationAnimation>> set : lists.entrySet()) {
            for (int i = 0; i < set.getValue().size(); i++) {
                NotificationAnimation an = set.getValue().get(i);
                if (an != null) {
                    an.close();
                }
            }
        }
    }

    public void clear(Location location) {
        notificationHolder.clearHold(location);
        List<NotificationAnimation> list = lists.get(location);
        if (list != null) {
            for (NotificationAnimation an : list) {
                if (an != null) {
                    an.close();
                }
            }
        }
    }

    public void clearHold() {
        notificationHolder.clearHold();
    }

    public void clearHold(Location location) {
        notificationHolder.clearHold(location);
    }

    protected ToastNotificationPanel createNotification(Type type, String message) {
        ToastNotificationPanel toastNotificationPanel = new ToastNotificationPanel();
        toastNotificationPanel.set(type, message);
        return toastNotificationPanel;
    }

    private synchronized void updateList(Location key, NotificationAnimation values, boolean add) {
        if (add) {
            if (lists.containsKey(key)) {
                lists.get(key).add(values);
            } else {
                List<NotificationAnimation> list = new ArrayList<>();
                list.add(values);
                lists.put(key, list);
            }
        } else {
            if (lists.containsKey(key)) {
                lists.get(key).remove(values);
                if (lists.get(key).isEmpty()) {
                    lists.remove(key);
                }
            }
        }
    }

    public enum Type {
        SUCCESS, INFO, WARNING, ERROR
    }

    public enum Location {
        TOP_LEFT, TOP_CENTER, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT
    }

    public class NotificationAnimation {

        private final JWindow window;
        private Animator animator;
        private boolean show = true;
        private float animate;
        private int x;
        private int y;
        private final Location location;
        private final long duration;
        private Insets frameInsets;
        private int horizontalSpace;
        private int animationMove;
        private boolean top;
        private boolean close = false;

        public NotificationAnimation(Type type, Location location, long duration, String message) {
            installDefault();
            this.location = location;
            this.duration = duration;
            window = new JWindow(frame);
            ToastNotificationPanel toastNotificationPanel = createNotification(type, message);
            toastNotificationPanel.putClientProperty(ToastClientProperties.TOAST_CLOSE_CALLBACK, (Consumer) o -> close());
            window.setContentPane(toastNotificationPanel);
            window.setFocusableWindowState(false);
            window.pack();
            toastNotificationPanel.setDialog(window);
        }

        public NotificationAnimation(Location location, long duration, JComponent component) {
            installDefault();
            this.location = location;
            this.duration = duration;
            window = new JWindow(frame);
            window.setBackground(new Color(0, 0, 0, 0));
            window.setContentPane(component);
            window.setFocusableWindowState(false);
            window.setSize(component.getPreferredSize());
        }

        private void installDefault() {
            frameInsets = UIUtils.getInsets("Toast.frameInsets", new Insets(10, 10, 10, 10));
            horizontalSpace = FlatUIUtils.getUIInt("Toast.horizontalGap", 10);
            animationMove = FlatUIUtils.getUIInt("Toast.animationMove", 10);
        }

        public void start() {
            int animation = FlatUIUtils.getUIInt("Toast.animation", 200);
            int resolution = FlatUIUtils.getUIInt("Toast.animationResolution", 5);
            animator = new Animator(animation, new Animator.TimingTarget() {
                @Override
                public void begin() {
                    if (show) {
                        updateList(location, NotificationAnimation.this, true);
                        installLocation();
                    }
                }

                @Override
                public void timingEvent(float f) {
                    animate = show ? f : 1f - f;
                    updateLocation(true);
                }

                @Override
                public void end() {
                    if (show && !close) {
                        SwingUtilities.invokeLater(() -> new Thread(() -> {
                            sleep(duration);
                            if (!close) {
                                show = false;
                                animator.start();
                            }
                        }).start());
                    } else {
                        updateList(location, NotificationAnimation.this, false);
                        window.dispose();
                        notificationClose(NotificationAnimation.this);
                    }
                }
            });
            animator.setResolution(resolution);
            animator.start();
        }

        private void installLocation() {
            Insets insets;
            Rectangle rec;
            if (frame == null) {
                insets = UIScale.scale(frameInsets);
                rec = new Rectangle(new Point(0, 0), Toolkit.getDefaultToolkit().getScreenSize());
            } else {
                insets = UIScale.scale(FlatUIUtils.addInsets(frameInsets, frame.getInsets()));
                rec = frame.getBounds();
            }
            setupLocation(rec, insets);
            window.setOpacity(0f);
            window.setVisible(true);
        }

        private void move(Rectangle rec) {
            Insets insets = UIScale.scale(FlatUIUtils.addInsets(frameInsets, frame.getInsets()));
            setupLocation(rec, insets);
        }

        private void setupLocation(Rectangle rec, Insets insets) {
            if (location == Location.TOP_LEFT) {
                x = rec.x + insets.left;
                y = rec.y + insets.top;
                top = true;
            } else if (location == Location.TOP_CENTER) {
                x = rec.x + (rec.width - window.getWidth()) / 2;
                y = rec.y + insets.top;
                top = true;
            } else if (location == Location.TOP_RIGHT) {
                x = rec.x + rec.width - (window.getWidth() + insets.right);
                y = rec.y + insets.top;
                top = true;
            } else if (location == Location.BOTTOM_LEFT) {
                x = rec.x + insets.left;
                y = rec.y + rec.height - (window.getHeight() + insets.bottom);
                top = false;
            } else if (location == Location.BOTTOM_CENTER) {
                x = rec.x + (rec.width - window.getWidth()) / 2;
                y = rec.y + rec.height - (window.getHeight() + insets.bottom);
                top = false;
            } else if (location == Location.BOTTOM_RIGHT) {
                x = rec.x + rec.width - (window.getWidth() + insets.right);
                y = rec.y + rec.height - (window.getHeight() + insets.bottom);
                top = false;
            }
            int am = UIScale.scale(top ? animationMove : -animationMove);
            int ly = (int) (getLocation(NotificationAnimation.this) + y + animate * am);
            window.setLocation(x, ly);
        }

        private void updateLocation(boolean loop) {
            int am = UIScale.scale(top ? animationMove : -animationMove);
            int ly = (int) (getLocation(NotificationAnimation.this) + y + animate * am);
            window.setLocation(x, ly);
            window.setOpacity(animate);
            if (loop) {
                update(this);
            }
        }

        private int getLocation(NotificationAnimation notification) {
            int height = 0;
            List<NotificationAnimation> list = lists.get(location);
            for (NotificationAnimation n : list) {
                if (notification == n) {
                    return height;
                }
                double v = n.animate * (n.window.getHeight() + UIScale.scale(horizontalSpace));
                height += top ? v : -v;
            }
            return height;
        }

        private void update(NotificationAnimation except) {
            List<NotificationAnimation> list = lists.get(location);
            for (NotificationAnimation n : list) {
                if (n != except) {
                    n.updateLocation(false);
                }
            }
        }

        public void close() {
            if (show) {
                if (animator.isRunning()) {
                    animator.stop();
                }
                close = true;
                show = false;
                animator.start();
            }
        }

        private void sleep(long l) {
            try {
                Thread.sleep(l);
            } catch (InterruptedException e) {
                System.err.println(e);
            }
        }

        public Location getLocation() {
            return location;
        }

        public long getDuration() {
            return duration;
        }
    }
}
