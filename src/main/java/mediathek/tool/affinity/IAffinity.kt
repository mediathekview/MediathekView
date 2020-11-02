package mediathek.tool.affinity

interface IAffinity {
    fun setDesiredCpuAffinity(numCpus: Int)
}