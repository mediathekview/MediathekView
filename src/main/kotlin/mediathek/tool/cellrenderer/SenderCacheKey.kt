package mediathek.tool.cellrenderer

import java.awt.Dimension

@JvmRecord
data class SenderCacheKey(
    val sender: String,
    val cellDimension: Dimension,
    val useLocalSenderIcons: Boolean,
    val selected: Boolean,
)
