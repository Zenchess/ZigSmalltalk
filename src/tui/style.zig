const terminal = @import("terminal.zig");
const Rgb = terminal.Rgb;

// Catppuccin Mocha color palette
pub const theme = struct {
    // Base colors
    pub const base = Rgb.fromHex("#1e1e2e");
    pub const mantle = Rgb.fromHex("#181825");
    pub const crust = Rgb.fromHex("#11111b");

    // Surface colors
    pub const surface0 = Rgb.fromHex("#313244");
    pub const surface1 = Rgb.fromHex("#45475a");
    pub const surface2 = Rgb.fromHex("#585b70");

    // Overlay colors
    pub const overlay0 = Rgb.fromHex("#6c7086");
    pub const overlay1 = Rgb.fromHex("#7f849c");
    pub const overlay2 = Rgb.fromHex("#9399b2");

    // Text colors
    pub const text = Rgb.fromHex("#cdd6f4");
    pub const subtext1 = Rgb.fromHex("#bac2de");
    pub const subtext0 = Rgb.fromHex("#a6adc8");

    // Accent colors
    pub const lavender = Rgb.fromHex("#b4befe");
    pub const blue = Rgb.fromHex("#89b4fa");
    pub const sapphire = Rgb.fromHex("#74c7ec");
    pub const sky = Rgb.fromHex("#89dceb");
    pub const teal = Rgb.fromHex("#94e2d5");
    pub const green = Rgb.fromHex("#a6e3a1");
    pub const yellow = Rgb.fromHex("#f9e2af");
    pub const peach = Rgb.fromHex("#fab387");
    pub const maroon = Rgb.fromHex("#eba0ac");
    pub const red = Rgb.fromHex("#f38ba8");
    pub const mauve = Rgb.fromHex("#cba6f7");
    pub const pink = Rgb.fromHex("#f5c2e7");
    pub const flamingo = Rgb.fromHex("#f2cdcd");
    pub const rosewater = Rgb.fromHex("#f5e0dc");
};

// Semantic colors for UI elements
pub const ui = struct {
    pub const background = theme.base;
    pub const background_alt = theme.mantle;
    pub const foreground = theme.text;
    pub const foreground_dim = theme.subtext0;

    pub const tab_active = theme.blue;
    pub const tab_inactive = theme.surface2;
    pub const tab_text = theme.text;

    pub const status_bar = theme.surface0;
    pub const status_text = theme.subtext1;

    pub const selection = theme.surface1;
    pub const cursor = theme.rosewater;

    pub const border = theme.surface2;
    pub const border_focused = theme.blue;

    pub const error_text = theme.red;
    pub const warning_text = theme.yellow;
    pub const success_text = theme.green;
    pub const info_text = theme.blue;

    pub const scrollbar = theme.surface2;
    pub const scrollbar_thumb = theme.overlay0;
};

// Syntax highlighting colors
pub const syntax = struct {
    pub const keyword = theme.mauve;
    pub const string = theme.green;
    pub const symbol = theme.yellow;
    pub const number = theme.peach;
    pub const comment = theme.overlay0;
    pub const self_var = theme.red;
    pub const class_name = theme.blue;
    pub const method_name = theme.sapphire;
    pub const variable = theme.text;
    pub const operator = theme.sky;
    pub const block = theme.pink;
    pub const literal = theme.flamingo;
};

// Style struct for combining attributes
pub const Style = struct {
    fg: ?Rgb = null,
    bg: ?Rgb = null,
    bold: bool = false,
    dim: bool = false,
    italic: bool = false,
    underline: bool = false,
    reverse: bool = false,

    pub const default = Style{};

    pub fn withFg(self: Style, color: Rgb) Style {
        var s = self;
        s.fg = color;
        return s;
    }

    pub fn withBg(self: Style, color: Rgb) Style {
        var s = self;
        s.bg = color;
        return s;
    }

    pub fn withBold(self: Style) Style {
        var s = self;
        s.bold = true;
        return s;
    }

    pub fn withDim(self: Style) Style {
        var s = self;
        s.dim = true;
        return s;
    }

    pub fn withItalic(self: Style) Style {
        var s = self;
        s.italic = true;
        return s;
    }

    pub fn withUnderline(self: Style) Style {
        var s = self;
        s.underline = true;
        return s;
    }

    pub fn withReverse(self: Style) Style {
        var s = self;
        s.reverse = true;
        return s;
    }

    pub fn apply(self: Style) void {
        const ansi = terminal.ansi;
        ansi.resetStyle();

        if (self.bold) ansi.setBold();
        if (self.dim) ansi.setDim();
        if (self.italic) ansi.setItalic();
        if (self.underline) ansi.setUnderline();
        if (self.reverse) ansi.setReverse();

        if (self.fg) |fg| {
            ansi.setFgRgb(fg.r, fg.g, fg.b);
        }
        if (self.bg) |bg| {
            ansi.setBgRgb(bg.r, bg.g, bg.b);
        }
    }

    pub fn eql(self: Style, other: Style) bool {
        const fg_eq = if (self.fg) |f1| (if (other.fg) |f2| f1.r == f2.r and f1.g == f2.g and f1.b == f2.b else false) else other.fg == null;
        const bg_eq = if (self.bg) |b1| (if (other.bg) |b2| b1.r == b2.r and b1.g == b2.g and b1.b == b2.b else false) else other.bg == null;
        return fg_eq and bg_eq and
            self.bold == other.bold and
            self.dim == other.dim and
            self.italic == other.italic and
            self.underline == other.underline and
            self.reverse == other.reverse;
    }
};

// Pre-defined styles for common UI elements
pub const styles = struct {
    pub const normal = Style{ .fg = ui.foreground, .bg = ui.background };
    pub const dim = Style{ .fg = ui.foreground_dim, .bg = ui.background };
    pub const selected = Style{ .fg = ui.foreground, .bg = ui.selection };
    pub const highlight_line = Style{ .fg = theme.base, .bg = theme.yellow }; // Debugger current line
    pub const tab_active_style = Style{ .fg = theme.base, .bg = ui.tab_active, .bold = true };
    pub const tab_inactive_style = Style{ .fg = ui.foreground_dim, .bg = ui.tab_inactive };
    pub const status = Style{ .fg = ui.status_text, .bg = ui.status_bar };
    pub const status_key = Style{ .fg = theme.yellow, .bg = ui.status_bar, .bold = true };
    pub const error_style = Style{ .fg = ui.error_text, .bg = ui.background };
    pub const border_style = Style{ .fg = ui.border, .bg = ui.background };
    pub const border_focused_style = Style{ .fg = ui.border_focused, .bg = ui.background };
    pub const title = Style{ .fg = theme.blue, .bg = ui.background, .bold = true };
};

// Box drawing characters as Unicode codepoints (u21)
pub const box = struct {
    pub const horizontal: u21 = '─'; // U+2500
    pub const vertical: u21 = '│'; // U+2502
    pub const top_left: u21 = '┌'; // U+250C
    pub const top_right: u21 = '┐'; // U+2510
    pub const bottom_left: u21 = '└'; // U+2514
    pub const bottom_right: u21 = '┘'; // U+2518
    pub const t_down: u21 = '┬'; // U+252C
    pub const t_up: u21 = '┴'; // U+2534
    pub const t_right: u21 = '├'; // U+251C
    pub const t_left: u21 = '┤'; // U+2524
    pub const cross: u21 = '┼'; // U+253C

    // Double line variants
    pub const horizontal_double: u21 = '═'; // U+2550
    pub const vertical_double: u21 = '║'; // U+2551

    // Rounded corners
    pub const round_top_left: u21 = '╭'; // U+256D
    pub const round_top_right: u21 = '╮'; // U+256E
    pub const round_bottom_left: u21 = '╰'; // U+2570
    pub const round_bottom_right: u21 = '╯'; // U+256F

    // Block elements
    pub const block_full: u21 = '█'; // U+2588
    pub const block_light: u21 = '░'; // U+2591
    pub const block_medium: u21 = '▒'; // U+2592
    pub const block_dark: u21 = '▓'; // U+2593

    // Tree view
    pub const tree_branch: u21 = '├'; // U+251C
    pub const tree_last: u21 = '└'; // U+2514
    pub const tree_vertical: u21 = '│'; // U+2502
    pub const tree_horizontal: u21 = '─'; // U+2500
    pub const tree_collapsed: u21 = '▶'; // U+25B6
    pub const tree_expanded: u21 = '▼'; // U+25BC
};

// Icons and symbols as Unicode codepoints (u21)
pub const icons = struct {
    pub const check: u21 = '✓'; // U+2713
    pub const cross_icon: u21 = '✗'; // U+2717
    pub const arrow_right: u21 = '→'; // U+2192
    pub const arrow_left: u21 = '←'; // U+2190
    pub const arrow_up: u21 = '↑'; // U+2191
    pub const arrow_down: u21 = '↓'; // U+2193
    pub const bullet: u21 = '•'; // U+2022
    pub const circle_empty: u21 = '○'; // U+25CB
    pub const circle_filled: u21 = '●'; // U+25CF
    pub const square_empty: u21 = '□'; // U+25A1
    pub const square_filled: u21 = '■'; // U+25A0
    pub const folder: u21 = '📁'; // U+1F4C1
    pub const file: u21 = '📄'; // U+1F4C4
    pub const gear: u21 = '⚙'; // U+2699
    pub const warning: u21 = '⚠'; // U+26A0
    pub const info_icon: u21 = 'ℹ'; // U+2139
    pub const question: u21 = '?'; // ASCII
};
