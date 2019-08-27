package mediathek.mainwindow;

import mediathek.controller.history.AboHistoryController;

import java.util.concurrent.Callable;

public class AboHistoryCallable implements Callable<AboHistoryController> {

    @Override
    public AboHistoryController call() {
        return new AboHistoryController();
    }
}
