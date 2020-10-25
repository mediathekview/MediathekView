package mediathek.tool.affinity

import org.apache.logging.log4j.LogManager

class NullAffinity : IAffinity {
    override fun setDesiredCpuAffinity(numCpus: Int) {
        logger.warn("This OS is not supported for setting cpu affinity")
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}