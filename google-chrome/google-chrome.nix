{ pkgs, ... }:

{
  # Instala Google Chrome
  home.packages = [
    pkgs.google-chrome
  ];

  # Modifica el acceso directo de Google Chrome para usar aceleración
  xdg.desktopEntries.google-chrome = {
    name = "Google Chrome";
    genericName = "Web Browser";
    exec = "google-chrome-stable --use-gl=desktop --enable-gpu-rasterization --enable-zero-copy --ignore-gpu-blocklist --enable-accelerated-video-decode %U";
    terminal = false;
    icon = "google-chrome";
    type = "Application";
    categories = [ "Network" "WebBrowser" ];
    mimeType = [
      "text/html"
      "x-scheme-handler/http"
      "x-scheme-handler/https"
    ];
  };

  # Configura Chrome como navegador predeterminado
  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "google-chrome.desktop" ];
    "x-scheme-handler/http" = [ "google-chrome.desktop" ];
    "x-scheme-handler/https" = [ "google-chrome.desktop" ];
  };

  # Variables de sesión para Chrome
  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };

}
