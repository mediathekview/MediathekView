package mediathek.x11;

import mediathek.config.Konstanten;
import mediathek.mainwindow.MediathekGui;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.lang.reflect.Field;

public class MediathekGuiX11 extends MediathekGui {
    private static final Logger logger = LogManager.getLogger(MediathekGuiX11.class);

    public MediathekGuiX11() {
        setupX11WindowManagerClassName();
    }

    private void disableAccessWarnings() {
        try {
            var unsafeClass = Class.forName("sun.misc.Unsafe");
            var field = unsafeClass.getDeclaredField("theUnsafe");
            field.setAccessible(true);
            var unsafe = field.get(null);

            var putObjectVolatile = unsafeClass.getDeclaredMethod("putObjectVolatile", Object.class, long.class, Object.class);
            var staticFieldOffset = unsafeClass.getDeclaredMethod("staticFieldOffset", Field.class);

            var loggerClass = Class.forName("jdk.internal.module.IllegalAccessLogger");
            var loggerField = loggerClass.getDeclaredField("logger");
            Long offset = (Long) staticFieldOffset.invoke(unsafe, loggerField);
            putObjectVolatile.invoke(unsafe, loggerClass, offset, null);
        } catch (Exception ignored) {
        }
    }

    /**
     * Setup the X11 window manager WM_CLASS hint.
     * Enables e.g. GNOME to determine application name and to enable app specific functionality.
     */
    private void setupX11WindowManagerClassName() {
        disableAccessWarnings();

        try {
            var xToolkit = Toolkit.getDefaultToolkit();
            java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField("awtAppClassName");
            awtAppClassNameField.setAccessible(true);
            awtAppClassNameField.set(xToolkit, Konstanten.PROGRAMMNAME);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            logger.warn("Could not set awtAppClassName");
        }
    }
}
