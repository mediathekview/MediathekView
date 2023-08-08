package mediathek.mainwindow;

import com.formdev.flatlaf.FlatLaf;
import com.formdev.flatlaf.extras.FlatAnimatedLafChange;
import com.formdev.flatlaf.ui.FlatUIUtils;
import mediathek.gui.messages.FontSizeChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import org.apache.commons.configuration2.sync.LockMode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.StyleContext;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Helper class to globally change to L&F font sizes
 */
public class FontManager {
    private static final String KEY_DEFAULT_FONT = "defaultFont";
    private static final String KEY_LABEL_FONT = "Label.font";
    private static final String CONFIG_DEFAULT_FONT_SIZE = "ui.default_font.size";
    private static final String CONFIG_DEFAULT_FONT_FAMILY = "ui.default_font.family";
    private final String[] availableFontFamilyNames;
    private final JMenu fontMenu;
    private final JMenuItem restoreFontMenuItem = new JMenuItem();
    private final JMenuItem incrFontMenuItem = new JMenuItem();
    private final JMenuItem decrFontMenuItem = new JMenuItem();

    public FontManager(@NotNull JMenu fontMenu) {
        this.fontMenu = fontMenu;

        availableFontFamilyNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getAvailableFontFamilyNames().clone();
        Arrays.sort(availableFontFamilyNames);

        initStandardMenuEntries();
        createStandardMenuEntries();

        updateFontMenuItems();
    }

    private void initStandardMenuEntries() {
        restoreFontMenuItem.setText("Schrift zurücksetzen");
        restoreFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_0, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        restoreFontMenuItem.addActionListener(e -> resetFont());

        incrFontMenuItem.setText("Schrift vergrößern");
        incrFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        incrFontMenuItem.addActionListener(e -> increaseFontSize());

        decrFontMenuItem.setText("Schrift verkleinern");
        decrFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        decrFontMenuItem.addActionListener(e -> decreaseFontSize());
    }

    private void createStandardMenuEntries() {
        fontMenu.add(restoreFontMenuItem);
        fontMenu.add(incrFontMenuItem);
        fontMenu.add(decrFontMenuItem);
    }

    /**
     * Reset used L&F font back to default.
     */
    private void resetFont() {
        FlatAnimatedLafChange.showSnapshot();

        UIManager.put(KEY_DEFAULT_FONT, null);
        updateFontMenuItems();
        FlatLaf.updateUI();

        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        final var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            config.clearProperty(CONFIG_DEFAULT_FONT_SIZE);
            config.clearProperty(CONFIG_DEFAULT_FONT_FAMILY);
        } finally {
            config.unlock(LockMode.WRITE);
        }

        MessageBus.getMessageBus().publishAsync(new FontSizeChangedEvent());
    }

    private void increaseFontSize() {
        FlatAnimatedLafChange.showSnapshot();

        Font font = UIManager.getFont(KEY_DEFAULT_FONT);
        final float newFontSize = (float) (font.getSize() + 1);
        Font newFont = font.deriveFont(newFontSize);
        UIManager.put(KEY_DEFAULT_FONT, newFont);

        updateFontMenuItems();
        FlatLaf.updateUI();

        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        writeConfigData();

        MessageBus.getMessageBus().publishAsync(new FontSizeChangedEvent());
    }

    private void decreaseFontSize() {
        FlatAnimatedLafChange.showSnapshot();

        Font font = UIManager.getFont(KEY_DEFAULT_FONT);
        final float newFontSize = (float) Math.max(font.getSize() - 1, 10);
        Font newFont = font.deriveFont(newFontSize);
        UIManager.put(KEY_DEFAULT_FONT, newFont);

        updateFontMenuItems();
        FlatLaf.updateUI();

        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        writeConfigData();


        MessageBus.getMessageBus().publishAsync(new FontSizeChangedEvent());
    }

    /**
     * Store the font data in configuration
     */
    private void writeConfigData() {
        Font currentFont = UIManager.getFont(KEY_LABEL_FONT);
        final var currentFamily = currentFont.getFamily();
        final var currentSize = currentFont.getSize();

        final var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            config.setProperty(CONFIG_DEFAULT_FONT_FAMILY, currentFamily);
            config.setProperty(CONFIG_DEFAULT_FONT_SIZE, currentSize);
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    public void restoreConfigData() {
        FlatAnimatedLafChange.showSnapshot();

        final var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            final var currentFamily = config.getString(CONFIG_DEFAULT_FONT_FAMILY);
            final var currentSize = config.getInt(CONFIG_DEFAULT_FONT_SIZE);

            final Font font = UIManager.getFont(KEY_DEFAULT_FONT);
            Font newFont = StyleContext.getDefaultStyleContext().getFont(currentFamily, font.getStyle(), currentSize);
            // StyleContext.getFont() may return a UIResource, which would cause loosing user scale factor on Windows
            newFont = FlatUIUtils.nonUIResource(newFont);
            UIManager.put(KEY_DEFAULT_FONT, newFont);
        }
        catch (Exception ignored) {
        }
        finally {
            config.unlock(LockMode.READ);
        }

        FlatLaf.updateUI();
        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        updateFontMenuItems();
    }

    private void updateFontMenuItems() {
        fontMenu.removeAll();
        createStandardMenuEntries();
        fontMenu.addSeparator();

        // get current font
        Font currentFont = UIManager.getFont(KEY_LABEL_FONT);
        String currentFamily = currentFont.getFamily();
        String currentSize = Integer.toString(currentFont.getSize());

        // add font families
        ArrayList<String> families = new ArrayList<>(Arrays.asList(
                "Arial", "Cantarell", "Comic Sans MS", "DejaVu Sans",
                "Dialog", "Liberation Sans", "Noto Sans", "Roboto",
                "SansSerif", "Segoe UI", "Serif", "Tahoma", "Ubuntu", "Verdana"));
        if (!families.contains(currentFamily))
            families.add(currentFamily);
        families.sort(String.CASE_INSENSITIVE_ORDER);

        ButtonGroup familiesGroup = new ButtonGroup();
        for (String family : families) {
            if (Arrays.binarySearch(availableFontFamilyNames, family) < 0)
                continue; // not available

            JCheckBoxMenuItem item = new JCheckBoxMenuItem(family);
            item.setSelected(family.equals(currentFamily));
            item.addActionListener(this::fontFamilyChanged);
            fontMenu.add(item);

            familiesGroup.add(item);
        }

        // add font sizes
        fontMenu.addSeparator();
        ArrayList<String> sizes = new ArrayList<>(Arrays.asList(
                "10", "11", "12", "13", "14", "16", "18", "20", "24", "28"));
        if (!sizes.contains(currentSize))
            sizes.add(currentSize);
        sizes.sort(String.CASE_INSENSITIVE_ORDER);

        ButtonGroup sizesGroup = new ButtonGroup();
        for (String size : sizes) {
            JCheckBoxMenuItem item = new JCheckBoxMenuItem(size);
            item.setSelected(size.equals(currentSize));
            item.addActionListener(this::fontSizeChanged);
            fontMenu.add(item);

            sizesGroup.add(item);
        }

        // enabled/disable items
        boolean enabled = UIManager.getLookAndFeel() instanceof FlatLaf;
        for (Component item : fontMenu.getMenuComponents())
            item.setEnabled(enabled);
    }

    private void fontFamilyChanged(ActionEvent e) {
        String fontFamily = e.getActionCommand();

        FlatAnimatedLafChange.showSnapshot();

        Font font = UIManager.getFont(KEY_DEFAULT_FONT);
        Font newFont = StyleContext.getDefaultStyleContext().getFont(fontFamily, font.getStyle(), font.getSize());
        // StyleContext.getFont() may return a UIResource, which would cause loosing user scale factor on Windows
        newFont = FlatUIUtils.nonUIResource(newFont);
        UIManager.put(KEY_DEFAULT_FONT, newFont);

        FlatLaf.updateUI();
        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        writeConfigData();


        MessageBus.getMessageBus().publishAsync(new FontSizeChangedEvent());
    }

    private void fontSizeChanged(ActionEvent e) {
        String fontSizeStr = e.getActionCommand();

        FlatAnimatedLafChange.showSnapshot();

        Font font = UIManager.getFont(KEY_DEFAULT_FONT);
        Font newFont = font.deriveFont((float) Integer.parseInt(fontSizeStr));
        UIManager.put(KEY_DEFAULT_FONT, newFont);

        FlatLaf.updateUI();
        FlatAnimatedLafChange.hideSnapshotWithAnimation();

        writeConfigData();


        MessageBus.getMessageBus().publishAsync(new FontSizeChangedEvent());
    }
}
