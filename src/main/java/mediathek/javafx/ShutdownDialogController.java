package mediathek.javafx;

import mediathek.gui.AppShutdownWindow;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ShutdownState;

import java.util.EnumSet;

/**
 * Display a wait dialog with some status message to inform user what is happening currently.
 */
public class ShutdownDialogController {
    private final MediathekGui gui;
    private final AppShutdownWindow window;
    private double curSteps;

    public ShutdownDialogController(MediathekGui gui) {
        this.gui = gui;
        window = new AppShutdownWindow(gui);
        window.progress.setMaximum(EnumSet.allOf(ShutdownState.class).size());
    }

    public void show() {
        gui.setEnabled(false);
        window.label1.setBusy(true);
        window.setVisible(true);
    }

    public void hide() {
        gui.setEnabled(true);
        window.label1.setBusy(false);
        window.setVisible(false);
    }

    public void setStatusText(ShutdownState state) {
        curSteps++;
        window.message.setText(state.toString());
        window.message.paintImmediately(0, 0, window.message.getWidth(), window.message.getHeight());
        window.progress.setValue((int) curSteps);
        window.progress.paintImmediately(0, 0, window.progress.getWidth(), window.progress.getHeight());
        window.label1.paintImmediately(0, 0, window.label1.getWidth(), window.label1.getHeight());
        /*try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }*/
    }
}
