{ pkgs, ... }:

{
  programs.alacritty = {
    package = pkgs.runCommand "alacritty-system" {} ''
      mkdir -p $out/bin
      ln -s /usr/bin/alacritty $out/bin/alacritty
    '';
    enable = true;
    settings = {
      window = {
        padding = {
          x = 8;
          y = 8;
        };
        opacity = 0.95;
      };

      font = {
        normal = {
          family = "IBM Plex Mono";
        };
        size = 12.0;
      };

      colors = {
        primary = {
          background = "#0E0E10";
          foreground = "#D7D7D9";
        };

        cursor = {
          text = "#0E0E10";
          cursor = "#FFD479";
        };

        selection = {
          text = "#D7D7D9";
          background = "#1C1C20";
        };

        normal = {
          black = "#0E0E10";
          red = "#E87A7A";
          green = "#9CD6A3";
          yellow = "#FFD479";
          blue = "#82A7DD";
          magenta = "#C7A1E6";
          cyan = "#71D4C3";
          white = "#D7D7D9";
        };

        bright = {
          black = "#555555";
          red = "#E87A7A";
          green = "#9CD6A3";
          yellow = "#FFD479";
          blue = "#82A7DD";
          magenta = "#C7A1E6";
          cyan = "#71D4C3";
          white = "#FFFFFF";
        };
      };

      cursor = {
        style = {
          shape = "Beam";
          blinking = "Off";
        };
      };
    };
  };
}
