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

    private static final Font SCREEN_FONT = new Font("SansSerif", Font.BOLD, 12);
    private static final int X_POS = 85;
    private static final int Y_POS = 450;
    private static final int PROGRESS_WIDTH = 300;
    private static final int MAX_PROGRESS_STEPS = 11;// KEEP THIS CURRENT!

    public void updateSplashScreenText(final String text) {
        //bail out when we don´ have a splash screen...
        if (splashScreenContext == null) {
            return;
        }

        splashScreenProgress++;

        splashScreenContext.setRenderingHint(
                RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        //clear the drawing area...
        splashScreenContext.setComposite(AlphaComposite.Clear);
        splashScreenContext.fillRect(X_POS, (Y_POS - 10), PROGRESS_WIDTH, 40);


        splashScreenContext.setPaintMode();
        splashScreenContext.setFont(SCREEN_FONT);

        //paint the text string...
        splashScreenContext.setColor(Color.WHITE);
        splashScreenContext.drawString(text, X_POS, Y_POS + 2);
        // paint the full progress indicator...
        splashScreenContext.setColor(Color.BLUE);
        splashScreenContext.fillRect(X_POS, Y_POS - 15, PROGRESS_WIDTH, 5);
        //paint how much is done...
        splashScreenContext.setColor(Color.GREEN);
        splashScreenContext.fillRect(X_POS, Y_POS - 15, splashScreenProgress * (PROGRESS_WIDTH / MAX_PROGRESS_STEPS), 5);
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
