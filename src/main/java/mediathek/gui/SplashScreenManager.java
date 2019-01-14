package mediathek.gui;

import mediathek.tool.UIProgressState;

import java.awt.*;
import java.util.EnumSet;

/**
 * A class to manage the splash screen.
 */
@SuppressWarnings("serial")
public class SplashScreenManager {
    /**
     * the maximum number of steps used for progress bar calculation.
     */
    private final int MAXIMUM_STEPS;
    /**
     * The JVM {@link java.awt.SplashScreen} storage
     */
    private SplashScreen splash = null;
    /**
     * helper variable to calculate splash screen progress
     */
    private int splashScreenProgress = 0;

    public SplashScreenManager() {
        MAXIMUM_STEPS = EnumSet.allOf(UIProgressState.class).size() - 1;
        initializeSplashScreen();
    }

    public void updateSplashScreenText(UIProgressState state) {
        updateSplashScreenText(state.toString());
    }

    private void updateSplashScreenText(final String text) {
        if (!splash.isVisible())
            return;

        final Graphics2D splashScreenContext = splash.createGraphics();
        final int splashScreenYPosition = 450;
        final int splashScreenXPosition = 84;
        final int splashScreenWidth = 320;

        splashScreenProgress++;

        splashScreenContext.setRenderingHint(
                RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        //clear the drawing area...
        splashScreenContext.setComposite(AlphaComposite.Clear);
        splashScreenContext.fillRect(splashScreenXPosition, (splashScreenYPosition - 10), splashScreenWidth, 40);
        splashScreenContext.setPaintMode();
        //paint the text string...
        splashScreenContext.setFont(new Font("SansSerif", Font.BOLD, 12));
        splashScreenContext.setColor(Color.WHITE);
        splashScreenContext.drawString(text, splashScreenXPosition, splashScreenYPosition + 2);
        // paint the full progress indicator...
        splashScreenContext.setColor(Color.BLUE);
        splashScreenContext.fillRect(splashScreenXPosition, splashScreenYPosition - 15, splashScreenWidth, 5);
        //paint how much is done...
        splashScreenContext.setColor(Color.GREEN);
        splashScreenContext.fillRect(splashScreenXPosition, splashScreenYPosition - 15, splashScreenProgress * (splashScreenWidth / MAXIMUM_STEPS), 5);
        splash.update();
        splashScreenContext.dispose();
    }

    /**
     * Initialize the Splash Screen variables.
     */
    private void initializeSplashScreen() {
        try {
            splash = SplashScreen.getSplashScreen();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
