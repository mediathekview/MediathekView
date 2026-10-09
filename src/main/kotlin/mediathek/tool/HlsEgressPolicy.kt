/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool

import mediathek.tool.dns.DnsSelector
import okhttp3.Dns
import okhttp3.HttpUrl
import okhttp3.OkHttpClient
import java.net.Inet4Address
import java.net.Inet6Address
import java.net.InetAddress
import java.net.UnknownHostException

internal object HlsEgressPolicy {
    fun clientFor(baseClient: OkHttpClient): OkHttpClient =
        baseClient.newBuilder()
            .dns(FilteringDns(baseClient.dns))
            .addNetworkInterceptor { chain ->
                requirePublicHttpUrl(chain.request().url)
                chain.proceed(chain.request())
            }
            .build()

    fun requirePublicHttpUrl(url: HttpUrl): HttpUrl {
        require(url.scheme == "http" || url.scheme == "https") { "HLS URL must use HTTP(S): $url" }
        require(!url.host.isLocalhostName()) { "HLS URL host is not allowed: ${url.host}" }

        url.host.parseIpLiteralOrNull()?.let { address ->
            require(!address.isBlockedForHls()) { "HLS URL host is not allowed: ${url.host}" }
        }

        return url
    }

    internal class FilteringDns(
        private val delegate: Dns = DnsSelector(),
    ) : Dns {
        override fun lookup(hostname: String): List<InetAddress> {
            if (hostname.isLocalhostName()) {
                throw UnknownHostException("HLS URL host is not allowed: $hostname")
            }

            val addresses = delegate.lookup(hostname)
            if (addresses.any { it.isBlockedForHls() }) {
                throw UnknownHostException("HLS URL host resolves to a local or private address: $hostname")
            }
            return addresses
        }
    }

    private fun String.isLocalhostName(): Boolean =
        equals("localhost", ignoreCase = true) || endsWith(".localhost", ignoreCase = true)

    private fun String.parseIpLiteralOrNull(): InetAddress? =
        when {
            isIpv4Literal() -> parseIpv4LiteralOrNull()
            contains(':') -> runCatching { InetAddress.getByName(this) }.getOrNull()
            else -> null
        }

    private fun String.isIpv4Literal(): Boolean =
        count { it == '.' } == 3 && all { it == '.' || it.isDigit() }

    private fun String.parseIpv4LiteralOrNull(): InetAddress? {
        val bytes = split('.')
            .map { part -> part.toIntOrNull()?.takeIf { it in 0..255 } ?: return null }
            .map(Int::toByte)
            .toByteArray()
        return InetAddress.getByAddress(bytes)
    }

    private fun InetAddress.isBlockedForHls(): Boolean =
        isAnyLocalAddress ||
            isLoopbackAddress ||
            isLinkLocalAddress ||
            isSiteLocalAddress ||
            isMulticastAddress ||
            isPrivateIpv4() ||
            isUniqueLocalIpv6()

    private fun InetAddress.isPrivateIpv4(): Boolean {
        if (this !is Inet4Address) {
            return false
        }

        val bytes = address
        val first = bytes[0].toInt() and 0xff
        val second = bytes[1].toInt() and 0xff

        return first == 10 ||
            first == 127 ||
            (first == 169 && second == 254) ||
            (first == 172 && second in 16..31) ||
            (first == 192 && second == 168)
    }

    private fun InetAddress.isUniqueLocalIpv6(): Boolean {
        if (this !is Inet6Address) {
            return false
        }

        val first = address[0].toInt() and 0xff
        return (first and 0xfe) == 0xfc
    }
}
