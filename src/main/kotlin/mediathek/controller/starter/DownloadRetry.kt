package mediathek.controller.starter

import java.io.IOException
import java.net.SocketTimeoutException

internal fun IOException.isDownloadTimeout(): Boolean =
    generateSequence<Throwable>(this) { it.cause }.any { it is SocketTimeoutException }

internal fun nextDownloadRetryCount(previousCount: Int, attemptStart: Long, currentOffset: Long): Int =
    if (currentOffset > attemptStart) 1 else previousCount + 1
