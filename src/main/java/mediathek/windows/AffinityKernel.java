package mediathek.windows;

import com.sun.jna.platform.win32.Kernel32;

public interface AffinityKernel extends Kernel32 {
    boolean SetProcessAffinityMask(HANDLE hProcess, int dwProcessAffinityMask);
}
