package mediathek.windows;

import mediathek.tool.threads.IndicatorThread;
import org.bridj.Pointer;
import org.bridj.cpp.com.COMRuntime;
import org.bridj.cpp.com.shell.ITaskbarList3;
import org.bridj.jawt.JAWTUtils;

import java.util.concurrent.TimeUnit;

/**
 * Updates the {@link ITaskbarList3} taskbar progress.
 */
public class WinIndicatorThread extends IndicatorThread {
    private ITaskbarList3 list = null;
    private Pointer<Integer> hwnd;

    @SuppressWarnings("unchecked")
    public WinIndicatorThread(MediathekGuiWindows parent) {
        super();
        setName("WinIndicatorThread");

        try {
            if (list == null)
                list = COMRuntime.newInstance(ITaskbarList3.class);

            hwnd = (Pointer) Pointer.pointerToAddress(JAWTUtils.getNativePeerHandle(parent));
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                final long percentage = (long) calculateOverallPercentage();
                list.SetProgressValue(hwnd, percentage, 100);
                list.SetProgressState(hwnd, ITaskbarList3.TbpFlag.TBPF_NORMAL);

                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (InterruptedException ignored) {
        } finally {
            //when we are finished, stop progress
            list.SetProgressState(hwnd, ITaskbarList3.TbpFlag.TBPF_NOPROGRESS);
            list.Release();
        }
    }
}
