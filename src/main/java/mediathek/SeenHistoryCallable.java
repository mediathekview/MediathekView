package mediathek;

import mediathek.controller.history.SeenHistoryController;

import java.util.concurrent.Callable;

class SeenHistoryCallable implements Callable<SeenHistoryController> {

    @Override
    public SeenHistoryController call() {
        return new SeenHistoryController();
    }
}
