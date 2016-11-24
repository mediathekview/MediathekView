package mediathek.gui;

import java.awt.*;

/**
 * A class to manage the splash screen.
 */
@SuppressWarnings("serial")
public class SplashScreenManager
{
    /**
     * The JVM {@link java.awt.SplashScreen} storage
     */
    private SplashScreen splash = null;
    /**
     * Store the splash screen {@link Graphics2D} context here for reuse
     */
    private Graphics2D splashScreenContext = null;
    /**
     * helper variable to calculate splash screen progress
     */
    private int splashScreenProgress = 0;

    /**
     * wegeb der möglichen Abfrage: "Backup laden.."
     */
    public void closeSplashScreen() {
        splashScreenContext = null;
    }

    public void updateSplashScreenText(final String text) {
        //bail out when we don´ have a splash screen...
        if (splashScreenContext == null) {
            return;
        }

        final int splashScreenYPosition = 430;
        final int splashScreenXPosition = 120;
        final int splashScreenWidth = 300;
        final int maxSteps = 11; // KEEP THIS CURRENT!

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
        splashScreenContext.fillRect(splashScreenXPosition, splashScreenYPosition - 15, splashScreenProgress * (splashScreenWidth / maxSteps), 5);
        splash.update();
    }

    /**
     * Initialize the Splash Screen variables.
     */
    public void initializeSplashScreen() {
        try {
            splash = SplashScreen.getSplashScreen();
            if (splash != null) {
                splashScreenContext = splash.createGraphics();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
