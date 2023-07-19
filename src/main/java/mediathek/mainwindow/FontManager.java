package mediathek.mainwindow;

import com.formdev.flatlaf.FlatLaf;
import com.formdev.flatlaf.extras.FlatAnimatedLafChange;
import com.formdev.flatlaf.ui.FlatUIUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.StyleContext;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Helper class to globally change to L&F font sizes
 */
public class FontManager {
    private final MediathekGui mediathekGui;
    private final String[] availableFontFamilyNames;
    private int initialFontMenuItemCount = -1;

    public FontManager(@NotNull MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        availableFontFamilyNames = GraphicsEnvironment.getLocalGraphicsEnvironment()
                .getAvailableFontFamilyNames().clone();
        Arrays.sort(availableFontFamilyNames);
    }

    /**
     * Reset used L&F font back to default.
     */
    public void resetFont() {
        UIManager.put("defaultFont", null);
        updateFontMenuItems();
        FlatLaf.updateUI();
    }

    public void increaseFontSize() {
        Font font = UIManager.getFont("defaultFont");
        Font newFont = font.deriveFont((float) (font.getSize() + 1));
        UIManager.put("defaultFont", newFont);

        updateFontMenuItems();
        FlatLaf.updateUI();
    }

    public void decreaseFontSize() {
        Font font = UIManager.getFont("defaultFont");
        Font newFont = font.deriveFont((float) Math.max(font.getSize() - 1, 10));
        UIManager.put("defaultFont", newFont);

        updateFontMenuItems();
        FlatLaf.updateUI();
    }

    public void updateFontMenuItems() {
        if (initialFontMenuItemCount < 0)
            initialFontMenuItemCount = mediathekGui.fontMenu.getItemCount();
        else {
            // remove old font items
            for (int i = mediathekGui.fontMenu.getItemCount() - 1; i >= initialFontMenuItemCount; i--)
                mediathekGui.fontMenu.remove(i);
        }

        // get current font
        Font currentFont = UIManager.getFont("Label.font");
        String currentFamily = currentFont.getFamily();
        String currentSize = Integer.toString(currentFont.getSize());

        // add font families
        mediathekGui.fontMenu.addSeparator();
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
            mediathekGui.fontMenu.add(item);

            familiesGroup.add(item);
        }

        // add font sizes
        mediathekGui.fontMenu.addSeparator();
        ArrayList<String> sizes = new ArrayList<>(Arrays.asList(
                "10", "11", "12", "14", "16", "18", "20", "24", "28"));
        if (!sizes.contains(currentSize))
            sizes.add(currentSize);
        sizes.sort(String.CASE_INSENSITIVE_ORDER);

        ButtonGroup sizesGroup = new ButtonGroup();
        for (String size : sizes) {
            JCheckBoxMenuItem item = new JCheckBoxMenuItem(size);
            item.setSelected(size.equals(currentSize));
            item.addActionListener(this::fontSizeChanged);
            mediathekGui.fontMenu.add(item);

            sizesGroup.add(item);
        }

        // enabled/disable items
        boolean enabled = UIManager.getLookAndFeel() instanceof FlatLaf;
        for (Component item : mediathekGui.fontMenu.getMenuComponents())
            item.setEnabled(enabled);
    }

    private void fontFamilyChanged(ActionEvent e) {
        String fontFamily = e.getActionCommand();

        FlatAnimatedLafChange.showSnapshot();

        Font font = UIManager.getFont("defaultFont");
        Font newFont = StyleContext.getDefaultStyleContext().getFont(fontFamily, font.getStyle(), font.getSize());
        // StyleContext.getFont() may return a UIResource, which would cause loosing user scale factor on Windows
        newFont = FlatUIUtils.nonUIResource(newFont);
        UIManager.put("defaultFont", newFont);

        FlatLaf.updateUI();
        FlatAnimatedLafChange.hideSnapshotWithAnimation();
    }

    private void fontSizeChanged(ActionEvent e) {
        String fontSizeStr = e.getActionCommand();

        Font font = UIManager.getFont("defaultFont");
        Font newFont = font.deriveFont((float) Integer.parseInt(fontSizeStr));
        UIManager.put("defaultFont", newFont);

        FlatLaf.updateUI();
    }
}
