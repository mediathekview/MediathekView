/*
 * Created by JFormDesigner on Fri Jul 22 12:01:51 CEST 2022
 */

package mediathek;

import mediathek.config.Konstanten;
import mediathek.tool.TimerPool;
import mediathek.tool.UIProgressState;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;
import java.util.EnumSet;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class SplashScreen extends JWindow {
    private static final double MAXIMUM_STEPS = EnumSet.allOf(UIProgressState.class).size() - 1d;
    public JLabel versionLabel;
    private double curSteps;
    private JLabel appTitleLabel;
    private JLabel imageLabel;
    private JProgressBar progressBar;
    private JLabel statusLabel;

    public SplashScreen() {
        super();
        initComponents();

        getContentPane().setBackground(Color.BLACK);

        var res = String.format("Version: %s (%s %s)", Konstanten.MVVERSION, getOsName(), SystemUtils.OS_ARCH);
        versionLabel.setText(res);

        progressBar.setValue(0);
        setLocationRelativeTo(null);

        //strange behaviour on win where window will not come to front or stay there...
        if (SystemUtils.IS_OS_WINDOWS) {
            if (isAlwaysOnTopSupported())
                setAlwaysOnTop(true);
        }
    }

    public void update(UIProgressState state) {
        curSteps++;
        int pct = (int) Math.round(100 * (curSteps / MAXIMUM_STEPS));
        updateStatus(state.toString(), pct);
    }

    public void close() {
        TimerPool.getTimerPool().schedule(() ->
                SwingUtilities.invokeLater(() -> setVisible(false)), 2, TimeUnit.SECONDS);
        Main.splashScreen = Optional.empty();
    }

    /**
     * Return "modern" macOS string for mac instead of legacy "Mac OS X".
     * According to apple dev docs even "old" 10.6 is now named macOS.
     *
     * @return "macOS" for mac otherwise the java OS name
     */
    private String getOsName() {
        String osName;
        if (SystemUtils.IS_OS_MAC_OSX)
            osName = "macOS";
        else
            osName = SystemUtils.OS_NAME;

        return osName;
    }

    /**
     * Updates the percent complete bar and the associated status text.
     *
     * @param statusText      The new status text to display.
     * @param percentComplete The new percentage.
     */
    public void updateStatus(String statusText, int percentComplete) {
        appTitleLabel.paintImmediately(0, 0, appTitleLabel.getWidth(), appTitleLabel.getHeight());
        imageLabel.paintImmediately(0, 0, imageLabel.getWidth(), imageLabel.getHeight());
        versionLabel.paintImmediately(0, 0, versionLabel.getWidth(), versionLabel.getHeight());
        statusLabel.setText(statusText);
        statusLabel.paintImmediately(0, 0, statusLabel.getWidth(), statusLabel.getHeight());
        progressBar.setValue(percentComplete);
        progressBar.paintImmediately(0, 0, progressBar.getWidth(), progressBar.getHeight());
    }

    private void initComponents() {
        appTitleLabel = new JLabel();
        versionLabel = new JLabel();
        imageLabel = new JLabel();
        progressBar = new JProgressBar();
        statusLabel = new JLabel();

        setMinimumSize(new Dimension(640, 480));
        setBackground(Color.black);
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setAutoRequestFocus(false);
        setForeground(Color.black);
        var contentPane = getContentPane();

        appTitleLabel.setText(Konstanten.PROGRAMMNAME);
        appTitleLabel.setFont(appTitleLabel.getFont().deriveFont(appTitleLabel.getFont().getStyle() | Font.BOLD, appTitleLabel.getFont().getSize() + 45f));
        appTitleLabel.setForeground(Color.white);
        appTitleLabel.setBackground(Color.black);
        appTitleLabel.setOpaque(true);

        versionLabel.setText("Version");
        versionLabel.setOpaque(true);
        versionLabel.setForeground(Color.white);
        versionLabel.setBackground(Color.black);

        imageLabel.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView.png")));
        imageLabel.setBackground(Color.black);
        imageLabel.setOpaque(true);

        progressBar.setValue(50);
        progressBar.setPreferredSize(new Dimension(146, 10));

        statusLabel.setText("Status Text Message is here");
        statusLabel.setForeground(Color.white);
        statusLabel.setBackground(Color.black);
        statusLabel.setOpaque(true);

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
                contentPaneLayout.createParallelGroup()
                        .addGroup(contentPaneLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(contentPaneLayout.createParallelGroup()
                                        .addComponent(versionLabel, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE)
                                        .addComponent(progressBar, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE)
                                        .addGroup(contentPaneLayout.createSequentialGroup()
                                                .addComponent(appTitleLabel)
                                                .addGap(0, 257, Short.MAX_VALUE))
                                        .addComponent(statusLabel, GroupLayout.DEFAULT_SIZE, 628, Short.MAX_VALUE)
                                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                                                .addGap(0, 372, Short.MAX_VALUE)
                                                .addComponent(imageLabel)))
                                .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
                contentPaneLayout.createParallelGroup()
                        .addGroup(contentPaneLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(appTitleLabel)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(versionLabel)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 94, Short.MAX_VALUE)
                                .addComponent(imageLabel)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(statusLabel)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(progressBar, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addContainerGap())
        );
        pack();
    }
}
