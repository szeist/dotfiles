from protonvpn_cli.utils import (
    is_connected,
    get_ip_info,
    get_server_value,
    get_config_value,
    get_servers
)

class Py3status:
    _CACHE_TIMEOUT = 60
    _FEATURES = {0: "VPN", 1: "VPN[SC]", 2: "VPN[Tor]", 4: "VPN[P2P]"}
    _FORMAT = '{feature}: {protocol}/{ip} - {city}, {country}'

    def protonvpn(self):
        if not is_connected():
            return {
                'cached_until': self.py3.time_in(self._CACHE_TIMEOUT),
                'full_text': ''
            }

        format_data = self._get_protonvpn_data()

        return {
            'cached_until': self.py3.time_in(self._CACHE_TIMEOUT),
            'full_text': self.py3.safe_format(self._FORMAT, format_data),
            'color': self.py3.COLOR_GOOD
        }

    @classmethod
    def _get_protonvpn_data(cls):
        servers = get_servers()
        ip, _ = get_ip_info()
        connected_server = get_config_value("metadata", "connected_server")
        feature = get_server_value(connected_server, "Features", servers)

        return {
            'feature': cls._FEATURES[feature],
            'protocol': get_config_value("metadata", "connected_proto"),
            'ip': ip,
            'country': get_server_value(connected_server, "ExitCountry", servers),
            'city': get_server_value(connected_server, "City", servers) or '?'
        }
        

if __name__ == "__main__":
    from py3status.module_test import module_test
    module_test(Py3status)
