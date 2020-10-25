package mediathek.tool.affinity;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LinuxAffinity implements IAffinity {
    private static final Logger logger = LogManager.getLogger();

    @Override
    public void setDesiredCpuAffinity(int numCpus) {
        logger.warn("Not yet implemented");
    }
}
