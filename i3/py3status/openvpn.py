"""
Display connected vpn networks

Configuration parameters:
    cache_timeout: a timeout to refresh the networks

Format placeholders:
    {conn_names} - A comma separated list of connected vpn networks

Color options:
    color_good: All VPN connections in connected state
    color_degraded: At least one of the active VPN connection is not connected

Requires:
    - `NetworkManager`
    - `python-dbus`

@author Istvan Szenasi <szeist@gmail.com>
@license MIT
"""

from time import time
import dbus

class Py3status:
    """
    Py3status module
    """
    cache_timeout = 5
    format = 'VPN: {conn_names}'

    def __init__(self):
        self._dbus = dbus.SystemBus()

    def openvpn(self):
        """
        Returns connected vpn network names
        """
        connections = self._get_active_vpn_connections()

        conn_names = [conn_info[0] for conn_info in connections]
        all_ok = False not in [conn_info[1] for conn_info in connections]

        format_data = {'conn_names': ', '.join(conn_names)}
        if all_ok:
            color = self.py3.COLOR_GOOD
        else:
            color = self.py3.COLOR_DEGRADED

        return {
            'cached_until': time() + self.cache_timeout,
            'full_text': self.py3.safe_format(self.format, format_data),
            'color': color
        }

    def _get_active_vpn_connections(self):
        active_connections = [
            self._get_connection_info(conn_path)
            for conn_path in self._get_active_connection_paths()
        ]
        return [(conn[0]['id'], conn[1]) for conn in active_connections if conn[0]['type'] == 'vpn']

    def _get_active_connection_paths(self):
        proxy = self._dbus.get_object("org.freedesktop.NetworkManager", "/org/freedesktop/NetworkManager")
        prop_interface = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
        return prop_interface.Get('org.freedesktop.NetworkManager', 'ActiveConnections')

    def _get_connection_info(self, conn_path):
        proxy = self._dbus.get_object("org.freedesktop.NetworkManager", conn_path)
        prop_interface = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
        settings_path = prop_interface.Get("org.freedesktop.NetworkManager.Connection.Active", "Connection")
        state = prop_interface.Get("org.freedesktop.NetworkManager.Connection.Active", "State")
        service_proxy = self._dbus.get_object("org.freedesktop.NetworkManager", settings_path)
        settings_interface = dbus.Interface(service_proxy, "org.freedesktop.NetworkManager.Settings.Connection")

        conn = settings_interface.GetSettings()['connection']
        is_ok = state == 2
        return (conn, is_ok)


if __name__ == "__main__":
    from py3status.module_test import module_test
    module_test(Py3status)
