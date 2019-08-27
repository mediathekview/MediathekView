package mediathek.mainwindow;

import mediathek.controller.history.AboHistoryController;

import java.util.concurrent.Callable;

class AboHistoryCallable implements Callable<AboHistoryController> {

    @Override
    public AboHistoryController call() {
        return new AboHistoryController();
    }
}
