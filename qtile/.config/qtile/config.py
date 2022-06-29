from libqtile.bar import STRETCH, Bar
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.layout import Columns, Floating, Matrix, Spiral, Stack
from libqtile.widget import (
    CPU,
    Clock,
    CurrentLayout,
    GroupBox,
    Memory,
    Net,
    Prompt,
    QuickExit,
    Spacer,
    Systray,
    TextBox,
    WindowName,
)

from libqtile.command import lazy
from libqtile.hook import subscribe
from libqtile.utils import guess_terminal

from asyncio import sleep
from colors import one_dark


mod = "mod4"
terminal = guess_terminal()

keys = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod, "control"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "control"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "control"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "control"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod, "shift"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "shift"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "shift"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "shift"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod, "shift"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "shift"], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift", "control"], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "control"], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "space", lazy.spawn("rofi -show run"), desc="Spawn rofi"),
]


@subscribe.client_new
async def move_spotify(client):
    await sleep(0.01)
    if client.name.lower() == "spotify":
        client.togroup("misc")


groups = [
    Group("web", matches=[Match(wm_class="brave-browser")]),
    Group("term", matches=[Match(wm_class="Alacritty")]),
    Group("code", matches=[Match(wm_class="Code"), Match(wm_class="Alacritty")]),
    Group("docs", matches=[Match(wm_class="obsidian")]),
    Group("chat", matches=[Match(wm_class="Slack"), Match(wm_class="Element")], spawn=["slack", "element-desktop"]),
    Group("misc", matches=[Match(wm_class="Spotify")], spawn="spotify"),
]
for i, group in enumerate(groups):
    key = f"{i + 1}"
    keys.extend(
        [
            Key([mod], key, lazy.group[group.name].toscreen()),
            Key([mod, "control"], key, lazy.window.togroup(group.name, switch_group=False)),
        ]
    )

layouts = [
    Columns(grow_amount=5),
    Stack(num_stacks=1),
    Spiral(),
    Matrix(),
    Floating(float_rules=[*Floating.default_float_rules]),
]
for layout in layouts:
    layout.border_focus = one_dark["yellow"]
    layout.border_normal = one_dark["bg"]
    layout.border_width = 3
    layout.border_on_single = 3
    layout.margin = 20
    layout.margin_on_single = 20

widget_defaults = dict(
    font="Hack Nerd Font Bold",
    fontsize=14,
    padding=5,
)
extension_defaults = widget_defaults.copy()
screens = [
    Screen(
        top=Bar(
            [
                CurrentLayout(foreground=one_dark["green"], width=80),
                GroupBox(
                    active=one_dark["bright"]["black"],
                    block_highlight_text_color=one_dark["white"],
                    inactive=one_dark["bg"],
                    highlight_color=one_dark["white"],
                    highlight_method="block",
                ),
                Spacer(length=STRETCH),
                WindowName(foreground=one_dark["white"]),
                Spacer(length=STRETCH),
                Systray(),
                TextBox(foreground=one_dark["fg"], text="|"),
                CPU(
                    foreground=one_dark["yellow"],
                    format="{freq_current}GHz {load_percent}%",
                    update_interval=1.0,
                ),
                TextBox(foreground=one_dark["fg"], text="|"),
                Memory(
                    foreground=one_dark["blue"],
                    format="{MemUsed: .0f}{mm} {MemPercent}%",
                    measure_mem="M",
                    update_interval=1.0,
                ),
                TextBox(foreground=one_dark["fg"], text="|"),
                Net(
                    foreground=one_dark["magenta"],
                    format="Net {total}",
                    prefix="k",
                    max_chars=15,
                    update_interval=1.0,
                ),
                TextBox(foreground=one_dark["fg"], text="|"),
                Clock(foreground=one_dark["cyan"], format="%Y-%m-%d %H:%M %a"),
                QuickExit(foreground=one_dark["white"], font="Hack Nerd Font"),
            ],
            26,
            background=one_dark["bg"],
            border_width=[2, 1, 2, 1],
            border_color=[
                one_dark["bg"],
                one_dark["bg"],
                one_dark["fg"],
                one_dark["bg"],
            ],
        ),
        wallpaper="~/.config/qtile/731749.jpg",
        wallpaper_mode="fill",
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True
