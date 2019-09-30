package mediathek.mainwindow;

import mediathek.controller.history.SeenHistoryController;

import java.util.concurrent.Callable;

public class SeenHistoryCallable implements Callable<SeenHistoryController> {

    @Override
    public SeenHistoryController call() {
        return new SeenHistoryController();
    }
}
