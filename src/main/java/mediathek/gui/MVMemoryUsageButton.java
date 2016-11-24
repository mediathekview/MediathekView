package mediathek.gui;

import mSearch.tool.Functions.OperatingSystemType;

import javax.swing.*;
import java.awt.*;

import static mSearch.tool.Functions.getOs;

@SuppressWarnings("serial")
public class MVMemoryUsageButton extends JButton {
    private final Runtime rt = Runtime.getRuntime();
    private final String MEMORY_STRING;

    private static final int MEGABYTE = 1000 * 1000;
    private static final int BAR_HEIGHT = 16;

    public MVMemoryUsageButton() {
        final long maxMemory = Math.min(rt.maxMemory() / MEGABYTE, 9999);
        MEMORY_STRING = maxMemory + " von " + maxMemory + "MiB ";

        setOpaque(false);
        setFocusable(false);

        //we may want to explicitly garbage collect :)
        addActionListener(e -> System.gc());

        updateUI();

        //Update every second...
        Timer updateTimer = new Timer(1000, e -> repaint());
        updateTimer.start();
    }

    @Override
    public void paintComponent(final Graphics g) {
        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        final long maxMem;
        if (getOs() == OperatingSystemType.LINUX) {
            maxMem = rt.totalMemory();
        } else {
            maxMem = rt.maxMemory();
        }
        final long totalMemory = rt.totalMemory();
        final long freeMemory = rt.freeMemory();
        final long usedMem = totalMemory - freeMemory;

        Insets insets = getInsets();
        final Dimension size = getSize();
        final int totalBarLength = size.width - insets.left - insets.right;
        final int yOffset = (size.height - BAR_HEIGHT) / 2;
        final int xOffset = insets.left;

        // Hintergrund (Mac-Style)
        g2d.setColor(new Color(190, 190, 190));
        g2d.fillRect(xOffset, yOffset, totalBarLength, BAR_HEIGHT);

        // Balken benutzter Speicher
        g2d.setColor(Color.RED);
        final int usedBarLength = (int) (totalBarLength * usedMem / maxMem);
        g2d.fillRect(xOffset, yOffset, usedBarLength, BAR_HEIGHT);

        // Balken unbenutzter Speicher
        g2d.setColor(Color.GREEN);
        final int unusedBarLength = (int) (totalBarLength * freeMemory / maxMem);
        g2d.fillRect(xOffset + usedBarLength, yOffset, unusedBarLength, BAR_HEIGHT);

        // Rahmen rund um die Grafik
        g2d.setColor(Color.BLACK);
        g2d.drawRect(xOffset, yOffset, totalBarLength, BAR_HEIGHT);

        // Info Text
        g2d.setFont(getFont());
        final long used = usedMem / MEGABYTE;
        final long total = maxMem / MEGABYTE;
        final String info = used + " von " + total + "MB";
        final FontMetrics fontMetrics = g2d.getFontMetrics();
        final int infoWidth = fontMetrics.charsWidth(info.toCharArray(), 0, info.length());
        final int infoHeight = fontMetrics.getAscent();
        g2d.setColor(Color.BLACK);
        g2d.drawString(info, xOffset + (totalBarLength - infoWidth) / 2, yOffset + infoHeight + (BAR_HEIGHT - infoHeight) / 2 - 1);

        g2d.dispose();
    }

    @Override
    public Dimension getPreferredSize() {
        final Insets insets = getInsets();
        int width = getFontMetrics(getFont()).stringWidth(MEMORY_STRING) + insets.left + insets.right + 2;
        int height = getFontMetrics(getFont()).getHeight() + insets.top + insets.bottom + 2;
        return new Dimension(width, height);
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }
}
