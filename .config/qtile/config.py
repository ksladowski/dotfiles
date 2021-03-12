from typing import List
import subprocess
from subprocess import Popen

from libqtile import bar, layout, widget, hook, qtile
from libqtile.config import Click, Drag, Group, Key, KeyChord, Screen, ScratchPad, DropDown, Match, Rule
from libqtile.lazy import lazy

from libqtile.widget.sep import Sep
from libqtile.widget.cpu import CPU
from libqtile.widget.volume import Volume
from libqtile.widget.net import Net
from libqtile.widget.memory import Memory
from libqtile.widget.check_updates import CheckUpdates
from libqtile.widget.sensors import ThermalSensor
from libqtile.widget.maildir import Maildir
from libqtile.widget.notify import Notify
from libqtile.widget.generic_poll_text import GenPollText
from libqtile.widget.mpris2widget import Mpris2
from libqtile.widget.pomodoro import Pomodoro
from libqtile.widget.windowname import WindowName
from libqtile.widget.groupbox import GroupBox
from libqtile.widget.textbox import TextBox
from libqtile.widget.clock import Clock

from libqtile.command.client import InteractiveCommandClient

mod = "mod4"
terminal = "alacritty"
myFont = "Hack Nerd Font"

# resize functions allow bsp layout to grow/shrink instead of just grow


def resize(qtile, direction):
    layout = qtile.current_layout
    child = layout.current
    parent = child.parent

    while parent:
        if child in parent.children:
            layout_all = False

            if (direction == "left" and parent.split_horizontal) or (
                direction == "up" and not parent.split_horizontal
            ):
                parent.split_ratio = max(
                    5, parent.split_ratio - layout.grow_amount)
                layout_all = True
            elif (direction == "right" and parent.split_horizontal) or (
                direction == "down" and not parent.split_horizontal
            ):
                parent.split_ratio = min(
                    95, parent.split_ratio + layout.grow_amount)
                layout_all = True

            if layout_all:
                layout.group.layout_all()
                break

        child = parent
        parent = child.parent


@lazy.function
def resize_left(qtile):
    resize(qtile, "left")


@lazy.function
def resize_right(qtile):
    resize(qtile, "right")


@lazy.function
def resize_up(qtile):
    resize(qtile, "up")


@lazy.function
def resize_down(qtile):
    resize(qtile, "down")


def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)


def get_vpn():
    return subprocess.check_output(["/home/kevin/scripts/getvpn"]).decode('utf-8').strip()


keys = [
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "control"], "j", resize_down),
    Key([mod, "control"], "k", resize_up),
    Key([mod, "control"], "h", resize_left),
    Key([mod, "control"], "l", resize_right),
    Key([mod], "Tab", lazy.layout.toggle_split()),

    # monitors and layouts
    Key([mod], "backslash", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "grave", lazy.next_screen(), desc="Focus next monitor"),
    Key([mod, "shift"], "grave", lazy.function(
        switch_screens), desc="Swap monitor"),
    Key([mod, "shift"], "m", lazy.spawn("mirror-disp"), desc="Mirror disp"),

    Key([mod], "m", lazy.window.toggle_fullscreen(), desc="Maximize window"),
    Key([mod], "r", lazy.layout.normalize(), desc="Reset window? Not sure"),
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle floating"),

    # launch programs
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn(
        terminal), desc="Launch terminal"),
    Key([mod], "space", lazy.spawn("rofi -show drun")),
    Key([mod], "p", lazy.spawn("bwmenu --auto-lock -1")),
    Key([mod], "w", lazy.spawn("rofi -show window")),
    Key([mod, "mod1"], "f", lazy.spawn("firefox")),
    Key([mod, "mod1"], "s", lazy.spawn("spotify")),
    Key([mod, "mod1"], "m", lazy.spawn("alacritty -e neomutt")),
    Key([mod, "mod1"], "n", lazy.spawn("alacritty -e newsboat")),
    Key([mod, "mod1"], "d", lazy.spawn("discord")),
    Key([mod, "mod1"], "v", lazy.spawn("nvim")),
    Key([mod, "mod1"], "e", lazy.spawn("emacs")),
    Key([mod, "mod1"], "t", lazy.spawn("thunar")),

    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),

    # XF86 keys
    Key([], "XF86Calculator", lazy.spawn(
        "rofi -show calc -modi calc -no-show-match -no-sort")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn(
        "pactl set-sink-volume @DEFAULT_SINK@ +1000")),
    Key([], "XF86AudioLowerVolume", lazy.spawn(
        "pactl set-sink-volume @DEFAULT_SINK@ -1000")),
    Key([], "XF86AudioMute", lazy.spawn(
        "pactl set-sink-mute @DEFAULT_SINK@ toggle")),
    Key([], "XF86AudioNext", lazy.spawn(
        "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")),
    Key([], "XF86AudioPrev", lazy.spawn(
        "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")),
    Key([], "XF86AudioPlay", lazy.spawn(
        "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")),
    Key([mod], "Print", lazy.spawn("scrot_full")),
    Key([mod, "shift"], "Print", lazy.spawn("scrot_win")),

    # Scratchpads
    Key([mod], "v", lazy.group['scratchpad'].dropdown_toggle('pulse')),
    Key([mod, "shift"], "Return", lazy.group['scratchpad'].dropdown_toggle('term')),
    Key([mod], "F1", lazy.group['scratchpad'].dropdown_toggle('navi')),

    # quit and restart
    Key([mod], "Escape", lazy.restart(), desc="Restart qtile"),
    Key([mod, "shift"], "Escape", lazy.shutdown(), desc="Shutdown qtile"),
]

group_names = ["1",
               "2",
               "3",
               "4",
               "5",
               "6",
               "7",
               "8",
               "9"]

group_labels = ["  ",
                "  ",
                "  ",
                "  ",
                "  ",
                "  ",
                "  ",
                "  ",
                "  "]

group_matches = [None,
                 [Match(wm_class=["Firefox", "newsboat",
                                  "Tor Browser"], role=["browser"]), ],
                 None,
                 [Match(wm_class=["Steam", "Wine", "Lutris", "Dolphin,",
                                  "MAME", "libretro", "pcsx2", "yuzu", ], role=["game"]), ],
                 [Match(wm_class=["Gimp", "libreoffice", "zathura", ]), ],
                 [Match(wm_class=["Zoom"],), ],
                 [Match(wm_class=["Discord,", "weechat", "slack"],), ],
                 [Match(wm_class=["Spotify,", "mpv", "vlc"],), ],
                 [Match(wm_class=["neomutt"],), ], ]

layout_theme = {"border_width": 2,
                "margin": 10,
                "border_focus": "#81a1c1",
                "border_normal": "#3b4252"
                }

layouts = [
    layout.Bsp(fair=False, name='Bsp', ** layout_theme),
    layout.Max(**layout_theme),
]

groups = []

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            matches=group_matches[i],
            exclusive=False,
            layout='Bsp',
            persist=True,
            init=True,
            label=group_labels[i],
        ))

for i, g in enumerate(groups):
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], str(i+1), lazy.group[g.name].toscreen()),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], str(i+1), lazy.window.togroup(g.name)),
    ])

groups.append(
    ScratchPad("scratchpad", [
        DropDown("term", "alacritty", opacity=.9, width=.8),
        DropDown("navi", "alacritty -e navi", opacity=.9, width=.8),
        DropDown("pulse", "alacritty -e pulsemixer", opacity=.9, width=.8),
    ]),
)

# Nord Color Theme
colors = [["#2e3440", "#2e3440"],  # nord0
          ["#3b4252", "#3b4252"],  # nord1
          ["#434c5e", "#434c5e"],  # nord2
          ["#4c566a", "#4c566a"],  # nord3
          ["#d8dee9", "#d8dee9"],  # nord4
          ["#e5e9f0", "#e5e9f0"],  # nord5
          ["#eceff4", "#eceff4"],  # nord6
          ["#8fbcbb", "#8fbcbb"],  # nord7
          ["#88c0d0", "#88c0d0"],  # nord8
          ["#81a1c1", "#81a1c1"],  # nord9
          ["#5e81ac", "#5e81ac"],  # nord10
          ["#bf616a", "#bf616a"],  # nord11
          ["#d08770", "#d08770"],  # nord12
          ["#ebcb8b", "#ebcb8b"],  # nord13
          ["#a3be8c", "#a3be8c"],  # nord14
          ["#b48ead", "#b48ead"]]  # nord15


widget_defaults = dict(
    font=myFont,
    fontsize=14,
    padding=3,
)

extension_defaults = widget_defaults.copy()


def get_bar():
    return bar.Bar([
        Sep(
            linewidth=0,
            padding=6,
            foreground=colors[6],
            background=colors[0]
        ),
        GroupBox(font=myFont,
                 fontsize=14,
                 margin_y=3,
                 margin_x=0,
                 padding_y=5,
                 padding_x=3,
                 borderwidth=3,
                 hide_unused=True,
                 active=colors[4],
                 inactive=colors[4],
                 rounded=False,
                 highlight_color=colors[2],
                 highlight_method="line",
                 this_current_screen_border=colors[4],
                 this_screen_border=colors[4],
                 other_current_screen_border=colors[0],
                 other_screen_border=colors[0],
                 foreground=colors[4],
                 background=colors[0]
                 ),
        Sep(
            linewidth=0,
            padding=20,
            foreground=colors[6],
            background=colors[0]
        ),
        WindowName(
            foreground=colors[13],
            background=colors[0],
            padding=0,
            max_chars=15,
            format='{name}',
            for_current_screen=True
        ),
        Notify(
            background=colors[13],
            foreground=colors[0],
            default_timeout=5,
            fontsize=14
        ),
        Mpris2(background=colors[8],
               foreground=colors[0],
               name='spotify',
               stop_pause_text='Spotify - Paused',
               scroll_chars=None,
               display_metadata=['xesam:title', 'xesam:artist'],
               objname="org.mpris.MediaPlayer2.spotify"),
        CPU(
            format=' {load_percent}% |',
            update_interval=1.0,
            foreground=colors[0],
            background=colors[9],
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                terminal + ' -e htop')},
            padding=5
        ),
        ThermalSensor(
            foreground=colors[0],
            background=colors[9],
            padding=5,
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                terminal + ' -e htop')},
            threshold=90,
        ),
        Memory(
            format=' {MemPercent}% ',
            foreground=colors[0],
            background=colors[8],
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                terminal + ' -e htop')},
            padding=5
        ),
        GenPollText(
            foreground=colors[0],
            background=colors[9],
            func=get_vpn,
            mouse_callbacks={
                'Button1': lambda: qtile.cmd_spawn('togglevpn')},
            update_interval=10
        ),
        Net(
            format='| {down} ↓↑{up}',
            foreground=colors[0],
            background=colors[9],
            mouse_callbacks={
                'Button1': lambda: qtile.cmd_spawn('togglevpn')},
            padding=5
        ),
        Maildir(
            background=colors[8],
            foreground=colors[0],
            maildir_path="~/.local/share/mail/",
            sub_folders=[
                {"path": "kevm@sladowski.us/INBOX", "label": "1"}, {"path": "sladowski@wisc.edu/INBOX", "label": "2"}, {"path": "kevs1198@gmail.com/INBOX", "label": "3"}],
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                terminal + ' -e neomutt')},
            total=True,
            update_interval=600,
            subfolder_fmt="  {value} "
        ),
        CheckUpdates(
            update_interval=5000,
            foreground=colors[0],
            colour_have_updates=colors[0],
            colour_no_updates=colors[0],
            distro="Arch",
            no_update_string=' ⟳ 0',
            display_format=" ⟳ {updates} ",
            mouse_callbacks={'Button1': lambda: qtile.cmd_spawn(
                terminal + ' -e sudo pacman -Syu')},
            background=colors[9]
        ),
        Volume(
            foreground=colors[0],
            background=colors[8],
            emoji=True,
            mute_command="pactl set-sink-mute @DEFAULT_SINK@ toggle",
            padding=5
        ),
        Clock(
            foreground=colors[0],
            background=colors[9],
            format=" %a %D | %I:%M %p "
        ),
    ], 24, margin=8)


screens = [Screen(top=get_bar()), Screen(top=get_bar())]

# TODO chage to focus_change and manually update window keys
# @hook.subscribe.client_focus
# def win_trans(window):
#     window.cmd_opacity(1)

# TODO this one just doesn't fucking work


# @hook.subscribe.current_screen_change
# def bar_opacity():
#     cs = qtile.current_screen
#     qtile.current_screen.select(bar["top"]).opacity(1)
#     if(cs == qtile.screen[0]):
#         qtile.screen[1].opacity(.7)
#     else:
#         qtile.screen[0].opacity(.7)


# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
floating_layout = layout.Floating(**layout_theme)
auto_fullscreen = True
focus_on_window_activation = "smart"

wmname = "Qtile"
