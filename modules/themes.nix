{ darkTheme ? "wilson" }:

let
  allDark = {
    doric-valley = rec {
      name        = "doric-valley";
      font        = "DejaVu Sans Mono";
      background  = "#383035";
      foreground  = "#e0d5b7";
      active      = "#a0c0d0";
      bgAlt       = "#484040";
      activeAlt   = "#f6c097";
      olive       = "#c0b060";
      comments    = "#afa497";
      selection   = "#554f52";
      emacsTheme  = "doric-valley";
      color0      = background;
      color1      = "#eca28f";
      color2      = "#b9d0aa";
      color3      = "#c0b060";
      color4      = "#9fbfe7";
      color5      = "#e9acbf";
      color6      = "#a0c0d0";
      color7      = "#d9cfbe";
      color8      = "#484040";
      color9      = "#eca28f";
      color10     = "#b9d0aa";
      color11     = "#c0b060";
      color12     = "#9fbfe7";
      color13     = "#e9acbf";
      color14     = "#99dec7";
      color15     = foreground;
      cursor        = "#94d0eb";
      highlight     = selection;
      highlightText = foreground;
    };

    wilson = rec {
      name        = "wilson";
      font        = "DejaVu Sans Mono";
      background  = "#222222";
      foreground  = "#BEBFB7";
      active      = "#CFB980";
      bgAlt       = "#44443C";
      activeAlt   = "#B97E56";
      olive       = "#9BA657";
      comments    = "#6C6B59";
      selection   = "#44443C";
      emacsTheme  = "wilson";
      color0      = background;
      color1      = "#A56F4B";
      color2      = "#9BA657";
      color3      = "#B9A572";
      color4      = "#6B8096";
      color5      = "#8C7060";
      color6      = "#607870";
      color7      = "#A9AAA3";
      color8      = bgAlt;
      color9      = "#B97E56";
      color10     = "#B0BE6A";
      color11     = active;
      color12     = "#7D9BAD";
      color13     = "#C48E72";
      color14     = "#84857E";
      color15     = foreground;
      cursor        = active;
      highlight     = selection;
      highlightText = foreground;
    };

    creamsody = rec {
      name        = "creamsody-darker";
      font        = "DejaVu Sans Mono";
      background  = "#1c1a18";
      foreground  = "#b5b2a0";
      active      = "#4a6a78";
      bgAlt       = "#252320";
      activeAlt   = "#9a9888";
      olive       = "#8a7040";
      comments    = "#6a6858";
      selection   = "#3a3525";
      emacsTheme  = "creamsody";
      color0      = background;
      color1      = "#884545";
      color2      = "#657050";
      color3      = "#8a7040";
      color4      = active;
      color5      = "#785a5a";
      color6      = "#4a7070";
      color7      = activeAlt;
      color8      = "#3a3830";
      color9      = "#9a5035";
      color10     = "#7a8060";
      color11     = "#9a8050";
      color12     = "#5a7888";
      color13     = "#8a7070";
      color14     = "#5a8080";
      color15     = foreground;
      cursor        = "#b0ad9a";
      highlight     = selection;
      highlightText = foreground;
    };
  };
in
{
  inherit allDark;
  dark = allDark.${darkTheme} or allDark.wilson;

  light = rec {
    name        = "doric-oak";
    font        = "DejaVu Sans Mono";
    background  = "#e0d8c7";
    foreground  = "#3a2018";
    active      = "#497020";
    bgAlt       = "#d5c9b5";
    activeAlt   = "#8f9373";
    olive       = "#595000";
    comments    = "#6b5225";
    selection   = "#c2b19e";
    emacsTheme  = "doric-oak";
    color0      = background;
    color1      = "#982500";
    color2      = "#226700";
    color3      = "#595000";
    color4      = "#103077";
    color5      = "#700054";
    color6      = "#005460";
    color7      = "#8f9373";
    color8      = "#d5c9b5";
    color9      = "#b83000";
    color10     = "#2d8000";
    color11     = "#6e6200";
    color12     = "#1a3d8f";
    color13     = "#8a0066";
    color14     = "#006878";
    color15     = foreground;
    cursor        = "#497020";
    highlight     = selection;
    highlightText = foreground;
  };
}
