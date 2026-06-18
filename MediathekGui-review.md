# MediathekGui Architecture and Maintainability Review

Reviewed `src/main/java/mediathek/mainwindow/MediathekGui.java` as an architecture and maintenance audit. No source files were modified; the code sections below are concrete refactor targets and example implementation shapes.

## Scope and validation

- Checked git status: branch `develop`, ahead of `origin/develop` by 1.
- `MediathekGui.java` is 1,469 lines.
- Rough structure metrics from the file:
  - 67 imports.
  - 68 field declarations in the class header area.
  - 18 direct `ApplicationConfiguration.getInstance()` calls.
  - 12 `SwingUtilities.invokeLater` calls, plus a custom `invokeAndWait` wrapper.
  - 111 repository call sites of `MediathekGui.ui()`.
- Ran `git diff --check`.
  - Result: clean, no whitespace errors.

## Architecture and data flow

Current `MediathekGui` is not just the main window. It is effectively the application composition root, UI controller, lifecycle manager, event subscriber, settings adapter, and shutdown coordinator.

### Startup flow

1. Constructor wires global state:
   - Sets static `ui = this` at line 244.
   - Creates actions, panels, repositories, dialogs, toolbar/menu models, and status bar components.
2. UI initialization:
   - `setupScrollBarWidth` / `setupAlternatingRowColors` / `setIconAndWindowImage`.
   - `createMenuBar` at lines 605-635.
   - `createStatusBar` at lines 665-670.
   - `initTabs` at lines 906-930.
   - `initMenus` at lines 1202-1216.
   - `createCommonToolBar` / `installToolBar` at lines 490-502 and 475-478.
3. Data initialization:
   - `waitForHistoryDataLoadingToComplete` at lines 367-377 blocks on `Daten` history load.
   - `loadFilmlist` at lines 646-649 starts `StartupFilmlistLoader`.
   - `StartupFilmlistLoader` reads the local filmlist on `Dispatchers.IO`, publishes `FilmListReadStart/Stop` events, starts remote update if needed, then runs post-load tasks.
4. Event flow:
   - `subscribeTableModelChangeEvent` at lines 406-411 publishes `TableModelChangeEvent`, then subscribes this `MediathekGui` to `MessageBus`.
   - `@Handler` methods mutate UI/download progress/update state/settings dialogs.
5. Runtime reload/update flow:
   - `filmListListener` lines 191-207 disables/enables load action and schedules automatic reload.
   - `setupAutomaticFilmlistReload` lines 842-854 creates `AutomaticFilmlistUpdate` with a periodic Swing update action.
   - `setupUpdateCheck` lines 865-874 manages `ProgramUpdateCheck`.
6. Shutdown flow:
   - `quitApplication` lines 1265-1278 confirms termination.
   - `performApplicationShutdown` lines 1337-1407 serially closes monitors, dialogs, tray, notification center, tabs, downloads, history, config, timer pool, common pool, then calls `System.exit(0)`.

The dominant dependency direction is currently:

```text
UI components/actions/dialogs <-> MediathekGui <-> Daten/config/singletons/message bus/services
```

That is the key architectural problem: many leaf UI/actions call back into `MediathekGui.ui()`, while `MediathekGui` also directly owns those leaves.

## Problem areas

### 1. Critical structural risk: `MediathekGui` is a god object

File:

- `src/main/java/mediathek/mainwindow/MediathekGui.java:90`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:222-323`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:1337-1407`

Issue:

`MediathekGui` extends `JFrame` but also owns:

- Global UI singleton.
- Application bootstrap order.
- Menus/toolbars/tabs.
- Filmlist load progress.
- Automatic filmlist reload.
- Program update checks.
- Notification center.
- System tray/taskbar.
- Settings dialog.
- Memory/bandwidth monitors.
- Download progress integration.
- Shutdown choreography.
- Event bus subscription.

Failure mode / maintenance cost:

Any new top-level feature must edit this class in multiple places. It is hard to test because constructing the `JFrame` triggers real side effects: message-bus subscription, filmlist loading, taskbar/tray setup, update checks, timers, dialogs, and shutdown hook installation. Bugs in startup order become likely because fields are initialized before the constructor body and several objects receive `this` before initialization completes.

Suggested fix:

Turn `MediathekGui` into a shell/view and move composition/lifecycle orchestration into dedicated collaborators:

- `MainWindowController`
- `MainWindowLifecycle`
- `MainWindowMenuBuilder`
- `MainWindowTabRegistry`
- `MainWindowStatusBarController`
- `ApplicationShutdownCoordinator`
- `FilmlistLoadCoordinator`
- `PlatformIntegration`

### 2. Global static `ui()` is the largest coupling source

File:

- `src/main/java/mediathek/mainwindow/MediathekGui.java:110`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:244`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:338-340`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:357-359`

Repo evidence:

- Search found 111 `MediathekGui.ui()` call sites.

Issue:

The class exposes a process-wide mutable singleton `JFrame`. Many unrelated classes use it as:

- Dialog parent.
- Access to `tabFilme` / `tabDownloads`.
- Shutdown trigger.
- Repaint target.
- Settings and tray coordinator.
- Action dependency source.

Failure mode:

- Code that calls `MediathekGui.ui()` during startup/shutdown can receive `null` or a partially initialized frame.
- Unit tests must either construct a real `JFrame` or mock global state.
- Dependencies are invisible: a class may look independent but actually reaches into global UI.
- Public tab fields make the global singleton even more dangerous:
  - `tabFilme` is public at line 209.
  - `tabDownloads` is public at line 210.

Suggested fix:

Replace `ui()` usage gradually, not in one flag day:

- Introduce small interfaces:
  - `MainWindowHandle`: owner component, `showSettings()`, `quitApplication()`, `showStatusProgress()`.
  - `FilmSelectionHost` or `FilmActionsHost` for film-tab-specific operations.
  - `DownloadControlHost` for download-tab-specific operations.
- Inject these into new/modified actions and panels.
- Keep `MediathekGui.ui()` as a deprecated compatibility bridge until call sites are migrated.

Example:

```java
public interface MainWindowHandle {
    JFrame frame();
    StatusBarProgressHandle showStatusBarProgress();
    boolean quitApplication();
    DialogEinstellungen settingsDialog();
}

public final class SwingMainWindowHandle implements MainWindowHandle {
    private final MediathekGui gui;

    public SwingMainWindowHandle(MediathekGui gui) {
        this.gui = Objects.requireNonNull(gui);
    }

    @Override
    public JFrame frame() {
        return gui;
    }

    @Override
    public StatusBarProgressHandle showStatusBarProgress() {
        return gui.showStatusBarProgress();
    }

    @Override
    public boolean quitApplication() {
        return gui.quitApplication();
    }

    @Override
    public DialogEinstellungen settingsDialog() {
        return gui.getSettingsDialog();
    }
}
```

Then newer actions depend on `MainWindowHandle` instead of `MediathekGui.ui()`.

### 3. Threading model is inconsistent; Swing updates are not always on the EDT

File:

- `src/main/java/mediathek/mainwindow/MediathekGui.java:161-190`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:191-207`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:731-746`
- `src/main/java/mediathek/mainwindow/MediathekGui.java:1151-1158`

Issue:

Some `@Handler` methods wrap UI changes in `SwingUtilities.invokeLater`, but the `ListenerFilmeLaden` implementations directly mutate Swing state:

- `filmlistDownloadProgressListener.start` creates/closes progress handles.
- `progress` mutates `JProgressBar`/`JLabel`.
- `filmListListener.start/finish` toggles `loadFilmListAction` enabled state.

If these callbacks are fired from background filmlist loading threads, this violates Swing’s single-thread rule.

Failure mode:

Random UI race bugs, repaint inconsistencies, occasional deadlocks if a callback is later invoked while another thread waits on `invokeAndWait`.

Suggested fix:

Create a tiny EDT dispatcher and require all UI-facing coordinators to use it. Then move listener implementation out of `MediathekGui`.

Example:

```kotlin
fun interface UiDispatcher {
    fun dispatch(action: Runnable)
}

object SwingUiDispatcher : UiDispatcher {
    override fun dispatch(action: Runnable) {
        if (SwingUtilities.isEventDispatchThread()) {
            action.run()
        } else {
            SwingUtilities.invokeLater(action)
        }
    }
}
```

```java
final class FilmlistProgressPresenter implements ListenerFilmeLaden, AutoCloseable {
    private final UiDispatcher ui;
    private final Supplier<StatusBarProgressHandle> progressFactory;
    private StatusBarProgressHandle handle;

    FilmlistProgressPresenter(
            UiDispatcher ui,
            Supplier<StatusBarProgressHandle> progressFactory
    ) {
        this.ui = ui;
        this.progressFactory = progressFactory;
    }

    @Override
    public void start(@NonNull ListenerFilmeLadenEvent event) {
        ui.dispatch(() -> {
            closeCurrent();
            handle = progressFactory.get();
        });
    }

    @Override
    public void progress(@NonNull ListenerFilmeLadenEvent event) {
        ui.dispatch(() -> {
            if (handle == null) {
                return;
            }

            var bar = handle.progressBar();
            if (event.getMax() == 0 || event.getProgress() == event.getMax()) {
                bar.setIndeterminate(true);
            } else {
                bar.setIndeterminate(false);
                bar.setMinimum(0);
                bar.setMaximum(event.getMax());
                bar.setValue(event.getProgress());
            }
            handle.label().setText(event.getText());
        });
    }

    @Override
    public void fertig(@NonNull ListenerFilmeLadenEvent event) {
        ui.dispatch(this::closeCurrent);
    }

    @Override
    public void close() {
        ui.dispatch(this::closeCurrent);
    }

    private void closeCurrent() {
        if (handle != null) {
            handle.close();
            handle = null;
        }
    }
}
```

### 8. Constructor starts real work too early and is hard to test

File:

- `src/main/java/mediathek/mainwindow/MediathekGui.java:222-323`

Issue:

The constructor:

- Configures `UIManager`.
- Waits for history data.
- Subscribes to message bus.
- Starts tray/taskbar setup.
- Starts filmlist load.
- Starts update checker.
- Schedules regex warning.
- Loads bandwidth monitor.
- Performs geo startup check.

Failure mode:

A test or subclass cannot construct the view without launching background jobs and side effects. Also, actions and panels receive `this` while the object is still mid-construction.

Suggested fix:

Split construction from start:

- Constructor builds passive view/components only.
- `MainWindowController.start()` performs subscriptions and background starts.
- `MainWindowController.stop()` performs unsubscribes and closes.

## Improved architecture direction

Target package-level design:

```text
mediathek.mainwindow
  MediathekGui                  // JFrame shell, exposes view methods only
  MainWindowController          // starts/stops coordinators, binds view to model/events
  MainWindowActions             // owns shared actions
  MainWindowMenuBuilder         // builds JMenuBar from actions/tabs
  MainWindowToolbarBuilder      // builds toolbar from actions/platform policy
  MainWindowTabRegistry         // owns tab descriptors, icons, disposal
  MainWindowStatusBarController // status-bar progress handles
  MainWindowLifecycle           // message bus and closeable registrations
  ApplicationShutdownCoordinator
  FilmlistLoadCoordinator
  PlatformIntegration           // taskbar/tray/dark mode/platform hooks
```

Data flow after refactor:

- `Daten` and services emit events.
- Coordinators subscribe to events.
- Coordinators update small view interfaces on the EDT.
- Menus/actions call services/coordinators, not `MediathekGui.ui()`.
- Tabs are registered through descriptors.
- Shutdown is a list of owned lifecycle steps.

## Refactor strategy

### Phase 1: Low-risk extractions, no behavior change

1. Extract `StatusBarProgressController` from lines 665-776.
2. Extract `FilmlistProgressPresenter` from lines 161-190 and register it in a lifecycle object.
3. Make `setupAutomaticFilmlistReload` idempotent by closing the old scheduler first.
4. Extract `ShutdownCoordinator` while preserving exact step order.
5. Run:

```bash
JAVA_HOME=/Users/christianfranzke/.sdkman/candidates/java/current ./mvnw -q -DskipTests process-sources compile
```

### Phase 2: Reduce duplication

1. Introduce `MainWindowTabRegistry`.
2. Register film/download/online/livestream/audiothek tabs with descriptors.
3. Move `configureTabIcons`, `installOnlineSearchTab`, `installLivestreamsTab`, `installAudiothekTab` into registry.
4. Move tab disposal into registry shutdown step.

### Phase 3: Menus/toolbars

1. Introduce `MainWindowActions` aggregate.
2. Introduce `MainWindowMenuBuilder`.
3. Introduce `MainWindowToolbarBuilder`.
4. Keep macOS/Windows/X11 customization through a `PlatformMainWindowPolicy` instead of overriding many tiny protected methods.

### Phase 4: Kill global singleton gradually

1. Add `MainWindowHandle` and inject into new code.
2. Migrate high-churn actions first.
3. Deprecate `MediathekGui.ui()`.
4. Make `tabFilme`/`tabDownloads` private and expose narrow methods only.
5. Eventually remove static `ui`.

## Highest-priority concrete change

If implementing only one change first, fix the scheduler lifecycle leak:

```java
private void setupAutomaticFilmlistReload() {
    closeAutomaticFilmlistUpdate();

    final Runnable performUpdate = () -> {
        if (FilmListUpdateType.AUTOMATIC.isConfigured()) {
            if (daten.getListeDownloads().unfinishedDownloads() == 0) {
                loadFilmListAction.setEnabled(false);
                performFilmListLoadOperation(false);
            }
        }
    };

    automaticFilmlistUpdate = new AutomaticFilmlistUpdate(performUpdate);
    automaticFilmlistUpdate.start();
}
```

This is small, behavior-preserving, and removes a real lifecycle risk.

## Recommended next implementation slice

Implement these together because they are cohesive and easy to validate:

1. `setupAutomaticFilmlistReload` idempotence.
2. Extract `FilmlistProgressPresenter`.
3. Ensure all filmlist progress/action UI changes dispatch through EDT.
4. Compile with:

```bash
JAVA_HOME=/Users/christianfranzke/.sdkman/candidates/java/current ./mvnw -q -DskipTests process-sources compile
```

This reduces concrete risk without attempting the larger architectural migration all at once.
