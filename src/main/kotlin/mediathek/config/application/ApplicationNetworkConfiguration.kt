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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration

class ApplicationNetworkConfiguration(
    private val config: XMLConfiguration,
) {
    var httpProxyHost: String
        get() = config.getString(HTTP_PROXY_HOST, "")
        set(newValue) {
            config.setProperty(HTTP_PROXY_HOST, newValue)
        }

    var httpProxyPort: String
        get() = config.getString(HTTP_PROXY_PORT, "")
        set(newValue) {
            config.setProperty(HTTP_PROXY_PORT, newValue)
        }

    var httpProxyUser: String
        get() = config.getString(HTTP_PROXY_USER, "")
        set(newValue) {
            config.setProperty(HTTP_PROXY_USER, newValue)
        }

    var httpProxyPassword: String
        get() = config.getString(HTTP_PROXY_PASSWORD, "")
        set(newValue) {
            config.setProperty(HTTP_PROXY_PASSWORD, newValue)
        }

    val httpTrafficTraceLevel: String
        get() = config.getString(APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL)

    fun setHttpProxy(
        host: String,
        port: String,
        user: String,
        password: String,
    ) {
        httpProxyHost = host
        httpProxyPort = port
        httpProxyUser = user
        httpProxyPassword = password
    }

    fun getNetworkingDnsMode(defaultValue: String): String =
        config.getString(APPLICATION_NETWORKING_DNS_MODE, defaultValue)

    fun setNetworkingDnsMode(newValue: String) {
        config.setProperty(APPLICATION_NETWORKING_DNS_MODE, newValue)
    }

    private companion object {
        private const val APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL =
            "application.debug.http_traffic_trace_level"
        private const val APPLICATION_NETWORKING_DNS_MODE = "application.networking.dns.ip_mode"
        private const val HTTP_PROXY_HOST = "http.proxy.hostname"
        private const val HTTP_PROXY_PORT = "http.proxy.port"
        private const val HTTP_PROXY_USER = "http.proxy.user"
        private const val HTTP_PROXY_PASSWORD = "http.proxy.password"
    }
}
