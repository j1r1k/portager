{-# LANGUAGE OverloadedStrings #-}
module Main where

import Portager

cfg :: PortagerConfig
cfg = PortagerConfig { _arch = amd64 }

baseCore :: PortageR Set
baseCore = "base-core" `with` do
    pkgs [ "app-admin/python-updater"
         , "app-admin/sudo"
         , "app-backup/btrbk"
         , "app-editors/nano"
         , "app-editors/neovim" `with` do
               unstable
               use [ "python" ]
               dep [ "app-eselect/eselect-vi" `with` unstable
                   , "dev-python/neovim-python-client" `with` unstable
                   , "dev-libs/libtermkey" `with` unstable
                   , "dev-libs/libvterm" `with` unstable
                   , "dev-lua/lpeg" `with` do
                         unstable
                         use [ "luajit" ]
                   , "dev-libs/msgpack" `with` unstable
                   , "dev-lua/mpack" `with` do
                         unstable
                         use [ "luajit" ]
                   , "dev-libs/unibilium" `with` unstable
                   ]
         , "app-eselect/eselect-vi"
         , "app-misc/tmux"
         , "app-portage/eix" `with` use [ "sqlite" ]
         , "app-portage/genlop"
         , "app-portage/gentoolkit"
         , "app-portage/repoman"
         , "app-shells/bash"
         , "app-shells/bash-completion" `with` unstable
         , "dev-vcs/git"
         , "net-firewall/shorewall"
         , "net-fs/autofs"
         , "net-misc/openssh"
         , "sys-apps/etckeeper"
         , "sys-apps/gptfdisk"
         , "sys-apps/hdparm"
         , "sys-apps/kexec-tools" `with` use [ "lzma" ]
         , "sys-apps/lshw"
         , "sys-apps/pciutils"
         , "sys-apps/portage"
         , "sys-apps/progress"
         , "sys-apps/rescan-scsi-bus"
         , "sys-apps/sg3_utils"
         , "sys-apps/smartmontools"
         , "sys-apps/usbutils"
         , "sys-block/parted"
         , "sys-fs/btrfs-progs"
         , "sys-fs/cryptsetup" `with` use [ "urandom" ]
         , "sys-fs/dosfstools"
         , "sys-fs/exfat-utils"
         , "sys-fs/fuse-exfat"
         , "sys-fs/mdadm"
         , "sys-fs/multipath-tools"
         , "sys-fs/ntfs3g"
         , "sys-apps/systemd" `with` do
               unstable
               dep [ "sys-apps/util-linux" `with` unstable
                   , "sys-libs/libseccomp" `with` unstable
                   ]
         , "sys-kernel/dracut" `with` unstable
         , "sys-power/acpi"
         ]
    deps [ "dev-db/sqlite" `with` use [ "secure-delete" ]
         , "dev-lang/python" `with` do
               use [ "sqlite" ]
               dep [ "sys-libs/gdbm" `with` use [ "berkdb" ] ]
         , "sys-apps/dbus" `with` use [ "user-session" ]
         , "sys-apps/util-linux" `with` use [ "tty-helpers" ]
         , "sys-libs/ncurses" `with` use [ "gpm" ]
         , "sys-process/procps" `with` use [ "modern-top" ]
         ]

baseHardened :: PortageR Set
baseHardened = "base-hardened" `with` do
    pkgs [ "sys-apps/elfix"
         , "sys-apps/paxctl"
         , "sys-kernel/hardened-sources" `with` use [ "symlink" ]
         ]
    deps [ "sys-devel/gcc" `with` use [ "-fortran", "-sanitize" ]
         , "sys-devel/gcc-config" `with` unstable
         ]

baseTools :: PortageR Set
baseTools = "base-tools" `with` do
    uses [ "natspec" ]
    pkgs [ "app-admin/hddtemp"
         , "app-admin/kpcli" `with` do
               unstable
               dep [ "dev-perl/File-KeePass" `with` unstable ]
         , "app-admin/pwgen"
         , "app-admin/testdisk" `with` use [ "jpeg", "ntfs" ]
         , "app-arch/lz4"
         , "app-arch/p7zip" `with` use [ "-pch" ]
         , "app-arch/pax"
         , "app-arch/unrar"
         , "app-arch/unzip" `with` use [ "natspec" ]
         , "app-arch/zip" `with` use [ "natspec" ]
         , "app-crypt/easy-rsa"
         , "app-misc/beep"
         , "app-portage/pfl"
         , "app-text/sdcv"
         , "dev-tcltk/expect"
         , "dev-vcs/git-annex" `with` use [ "-assistant", "-feed", "-quvi", "-s3", "-tahoe", "-torrentparser", "-webdav", "-xmpp", "-webapp" ]
         , "net-analyzer/iftop"
         , "net-analyzer/nmap" `with` use [ "ncat", "ndiff", "nping" ]
         , "net-analyzer/tcpdump"
         , "net-dns/bind-tools" `with` use [ "urandom" ]
         , "net-misc/autossh"
         , "net-misc/curl"
         , "net-misc/proxytunnel" `with` unstable
         , "net-misc/telnet-bsd"
         , "net-misc/throttle"
         , "net-misc/unison"
         , "net-misc/wakeonlan"
         , "net-misc/wget" `with` use [ "gnutls", "uuid" ]
         , "net-p2p/syncthing" `with` unstable
         , "net-proxy/sshuttle"
         , "sys-apps/ethtool"
         , "sys-apps/firejail" `with` unstable
         , "sys-apps/lm_sensors"
         , "sys-apps/merge-bash-history"
         , "sys-devel/bc"
         , "sys-fs/ddrescue"
         , "sys-fs/duperemove" `with` unstable
         , "sys-fs/ncdu"
         , "sys-power/powertop"
         , "sys-process/iotop"
         , "sys-process/lsof"
         , "sys-process/parallel"
         ]

baseMedia :: PortageR Set
baseMedia = "base-media" `with`
    pkgs [ "app-text/texlive" `with` do
               use [ "dvipdfm", "extra", "science" ]
               dep [ "app-text/texlive-core" `with` use [ "xetex" ] ]
         , "media-gfx/imagemagick" `with` use [ "fontconfig", "jpeg", "jpeg2k", "png", "svg", "tiff", "truetype" ]
         , "media-sound/lame"
         , "media-video/mediainfo" `with` do
               use [ "curl" ]
               dep [ "media-libs/libmediainfo" `with` use [ "curl" ]
                   , "net-misc/youtube-dl"
                   , "sys-apps/mlocate"
                   ]
         ]

desktopCore :: PortageR Set
desktopCore = "desktop-core" `with` do
    uses [ "xinerama", "X" ]
    pkgs [ "media-fonts/dejavu"
         , "x11-apps/setxkbmap"
         , "x11-apps/xdm"
         , "x11-apps/xdpyinfo"
         , "x11-apps/xev"
         , "x11-apps/xfd"
         , "x11-apps/xinput"
         , "x11-apps/xkill"
         , "x11-apps/xlsfonts"
         , "x11-apps/xmodmap"
         , "x11-apps/xrandr"
         , "x11-apps/xsetroot"
         , "x11-base/xorg-server" `with` 
               dep [ "x11-libs/libXfont" `with` use [ "truetype" ] ]
         , "x11-misc/colord" `with` do
               use [ "argyllcms" ]
               dep [ "media-libs/lcms" `with` use [ "jpeg", "tiff" ] ]
         , "x11-misc/dunst"
         , "x11-misc/i3lock" `with` 
               dep [ "x11-libs/cairo" `with` use [ "xcb" ]
                   , "x11-libs/libxkbcommon" `with` use [ "X" ]
                   ]
         , "x11-misc/redshift" `with` do
               use [ "gtk" ]
               dep [ "dev-python/pygobject" `with` use [ "-cairo" ]
                   , "x11-libs/gtk+" `with` use [ "introspection" ]
                   ]
         , "x11-misc/slim"
         , "x11-misc/unclutter"
         , "x11-misc/xautolock"
         , "x11-misc/xclip"
         , "x11-misc/xdg-user-dirs" `with` use [ "gtk" ]
         , "x11-misc/xdotool"
         , "x11-misc/xmobar" `with` use [ "-dbus", "xft" ]
         , "x11-terms/rxvt-unicode" `with` use [ "256-color", "unicode3", "xft" ]
         , "x11-themes/arc-theme" `with` do
               unstable
               use [ "gtk2", "gtk3" ]
         , "x11-themes/faenza-icon-theme" `with` unstable
         ]
    deps [ "media-libs/freetype" `with` use [ "infinality", "png", "X" ]
         , "media-libs/mesa" `with` use [ "vaapi" ]
         , "x11-libs/gtk+" `with` do
               use [ "colord" ]
               dep [ "app-accessibility/at-spi2-atk"
                   , "x11-libs/gdk-pixbuf"
                   ]
         , "x11-libs/libxcb" `with` use [ "xkb" ]
         , "media-fonts/*" `with` use [ "X" ]
         ]

desktopApps :: PortageR Set
desktopApps = "desktop-apps" `with` do
    uses [ "gtk", "jpeg", "png", "pulseaudio", "tiff", "X" ]
    pkgs [ "app-arch/xarchiver"
         , "app-admin/keepassx"
         , "app-office/libreoffice-bin" `with` 
               dep [ "app-text/ghostscript-gpl"
                   , "app-text/poppler" `with` use [ "tiff" ]
                   , "media-libs/jbig2dec"
                   , "net-libs/neon" `with` use [ "gnutls" ]
                   , "net-nds/openldap" `with` use [ "minimal", "-syslog" ]
                   , "net-print/cups-filters"
                   , "net-print/cups" `with` use [ "-acl" ]
                   ]
         , "app-text/zathura" `with` do
               unstable
               dep [ "dev-libs/girara" `with` unstable ]
         , "app-text/zathura-meta" `with` do
               use [ "cb", "djvu", "postscript" ]
               dep [ "app-text/djvu" ]
         , "lxde-base/lxmenu-data"
         , "mail-client/claws-mail" `with` do
               use [ "gdata", "-libcanberra", "-libindicate", "spell" ]
               dep [ "net-libs/libetpan" `with` use [ "gnutls" ] ]
         , "media-gfx/gfxtablet"
         , "net-analyzer/wireshark" `with` use [ "geoip", "-qt4" ]
         , "net-misc/rdesktop" `with` use [ "alsa" ]
         , "www-client/firefox" `with` do
               unstable
               use [ "apng", "-gmp-autoupdate", "jit" ]
               dep [ "dev-libs/atk" `with` use [ "introspection" ]
                   , "media-libs/libpng" `with` use [ "apng" ]
                   , "x11-libs/gdk-pixbuf" `with` use [ "introspection" ]
                   , "x11-libs/pango" `with` use [ "introspection", "X" ]
                   , "x11-libs/pango" `with` use [ "X" ]
                   , "dev-libs/nss" `with` unstable
                   ]
         , "x11-misc/pcmanfm" `with` 
               dep [ "app-crypt/gcr"
                   , "dev-libs/libcdio" `with` use [ "cddb" ]
                   , "x11-libs/libfm" `with` do
                         use [ "exif", "gtk", "udisks" ]
                         dep [ "gnome-base/gvfs" `with` use [ "archive", "fuse", "gphoto2", "mtp", "udisks" ] ]
                   ]
         ]

desktopMedia :: PortageR Set
desktopMedia = "desktop-media" `with` do
    uses [ "ffmpeg", "gtk", "jpeg", "mp3", "opengl", "png", "pulseaudio", "svg", "tiff", "vaapi", "X", "xinerama" ]
    pkgs [ "media-gfx/argyllcms"
         , "media-gfx/gimp" `with` do
               use [ "curl", "exif", "jpeg2k", "mng", "pdf", "python" ]
               dep [ "media-libs/gegl" `with` use [ "raw" ]
                   , "media-libs/jasper"
                   , "media-libs/libgphoto2" `with` use [ "exif" ]
                   , "media-libs/libopenraw"
                   ]
         , "media-gfx/gpicview"
         , "media-gfx/inkscape" `with` do
               use [ "exif", "imagemagick" ]
               dep [ "app-text/poppler" `with` use [ "cairo" ]
                   , "dev-python/pillow" `with` use [ "jpeg2k", "truetype" ]
                   ]
         , "media-plugins/gst-plugins-meta" `with` do
               use [ "a52", "aac", "cdda", "dts", "dvd", "flac", "http", "lame", "libass", "mpeg", "ogg", "taglib", "vorbis", "x264" ]
               dep [ "media-libs/libcanberra" `with` use [ "gstreamer" ]
                   , "media-libs/libdvdcss" `with` unstable
                   , "media-libs/gst-plugins-bad"
                   , "media-libs/gst-plugins-base" `with` use [ "X" ]
                   , "media-plugins/gst-plugins-vaapi" `with` 
                         dep [ "x11-libs/libva" ]
                   , "net-libs/glib-networking" `with` use [ "-gnome" ]
                   ]
         , "media-sound/alsa-utils"
         , "media-sound/easytag" `with` do
               use [ "flac", "mp4", "vorbis" ]
               dep [ "dev-libs/libxml2" `with` use [ "python" ] ]
         , "media-sound/pavucontrol" `with` 
               dep [ "media-libs/libcanberra" `with` use [ "alsa" ] ]
         , "media-video/ffmpeg" `with` do
               unstable
               use [ "aac", "fontconfig", "gnutls", "libass", "opus", "theora", "truetype", "v4l", "vorbis", "x264", "x265", "xvid" ]
               dep [ "virtual/ffmpeg" `with` use [ "opus", "theora", "truetype", "vaapi", "x264" ]
                   , "media-plugins/alsa-plugins" `with` use [ "pulseaudio" ]
                   ]
         , "media-video/vlc" `with` do
               use [ "a52", "cdda", "cddb", "dts", "dvd", "flac", "fontconfig", "gnutls", "jpeg", "libass", "live", "matroska", "mpeg", "mtp", "ogg", "opus", "rtsp", "truetype", "twolame", "v4l", "vorbis", "x264", "x265", "xv" ]
               dep [ "media-libs/libass" `with` unstable
                   , "media-libs/libmpeg2"
                   , "sys-libs/zlib" `with` use [ "minizip" ]
                   ]
         ]
    deps [ "dev-cpp/cairomm"
         , "dev-cpp/gtkmm"
         , "dev-qt/qtgui" `with` use [ "gtkstyle" ]
         , "media-libs/flac" `with` use [ "ogg" ]
         , "media-sound/pulseaudio" `with` do
               use [ "bluetooth" ]
               dep [ "media-libs/speex" `with` use [ "ogg" ] ]
         , "x11-libs/cairo"
         , "x11-misc/xdg-utils" `with` use [ "-perl" ]
         ]

games :: PortageR Set
games = "games" `with` do
    pkgs [ "app-emulation/playonlinux" `with` do
               unstable
               dep [ "app-emulation/wine" `with` use [ "-gecko" ]
                   , "dev-python/wxpython" `with` use [ "opengl" ]
                   , "x11-libs/wxGTK" `with` use [ "gstreamer", "opengl", "tiff" ]
                   ]
         , "games-util/steam-meta" `with` do
               unstable
               dep [ "games-util/steam-client-meta" `with` unstable
                   , "games-util/steam-launcher" `with` unstable
                   , "games-util/steam-games-meta" `with` unstable
                   ]
         ]
    deps $ map (`with` use [ "abi_x86_32" ])
         [ "app-accessibility/at-spi2-atk"
         , "app-accessibility/at-spi2-core"
         , "app-arch/bzip2"
         , "app-arch/lz4"
         , "dev-db/sqlite"
         , "dev-libs/atk"
         , "dev-libs/expat"
         , "dev-libs/glib"
         , "dev-libs/gmp"
         , "dev-libs/icu"
         , "dev-libs/libcroco"
         , "dev-libs/libffi"
         , "dev-libs/libgcrypt"
         , "dev-libs/libgpg-error"
         , "dev-libs/libgudev"
         , "dev-libs/libgusb"
         , "dev-libs/libpcre"
         , "dev-libs/libpthread-stubs"
         , "dev-libs/libtasn1"
         , "dev-libs/libusb-compat"
         , "dev-libs/libusb"
         , "dev-libs/libxml2"
         , "dev-libs/libxslt"
         , "dev-libs/lzo"
         , "dev-libs/nettle"
         , "dev-libs/nspr"
         , "dev-libs/nss"
         , "dev-libs/openssl"
         , "dev-util/pkgconfig"
         , "gnome-base/librsvg"
         , "media-gfx/graphite2"
         , "media-libs/alsa-lib"
         , "media-libs/fontconfig"
         , "media-libs/freetype"
         , "media-libs/glu"
         , "media-libs/harfbuzz"
         , "media-libs/lcms"
         , "media-libs/libepoxy"
         , "media-libs/libjpeg-turbo"
         , "media-libs/libpng"
         , "media-libs/libtxc_dxtn"
         , "media-libs/mesa"
         , "media-libs/tiff"
         , "net-libs/gnutls"
         , "net-misc/curl"
         , "sys-apps/attr"
         , "sys-apps/dbus"
         , "sys-apps/systemd"
         , "sys-apps/util-linux"
         , "sys-devel/gettext"
         , "sys-devel/llvm"
         , "sys-fs/udev"
         , "sys-libs/binutils-libs"
         , "sys-libs/gpm"
         , "sys-libs/libcap"
         , "sys-libs/ncurses"
         , "sys-libs/readline"
         , "sys-libs/zlib"
         , "virtual/glu"
         , "virtual/jpeg"
         , "virtual/libffi"
         , "virtual/libgudev"
         , "virtual/libiconv"
         , "virtual/libintl"
         , "virtual/libudev"
         , "virtual/libusb"
         , "virtual/opengl"
         , "virtual/pkgconfig"
         , "x11-libs/cairo"
         , "x11-libs/gdk-pixbuf"
         , "x11-libs/gtk+"
         , "x11-libs/libICE"
         , "x11-libs/libSM"
         , "x11-libs/libX11"
         , "x11-libs/libXau"
         , "x11-libs/libXcomposite"
         , "x11-libs/libXcursor"
         , "x11-libs/libXdamage"
         , "x11-libs/libXdmcp"
         , "x11-libs/libXext"
         , "x11-libs/libXfixes"
         , "x11-libs/libXft"
         , "x11-libs/libXi"
         , "x11-libs/libXinerama"
         , "x11-libs/libXrandr"
         , "x11-libs/libXrender"
         , "x11-libs/libXtst"
         , "x11-libs/libXxf86vm"
         , "x11-libs/libdrm"
         , "x11-libs/libpciaccess"
         , "x11-libs/libva"
         , "x11-libs/libxcb"
         , "x11-libs/libxshmfence"
         , "x11-libs/pango"
         , "x11-libs/pixman"
         , "x11-misc/colord"
         , "x11-proto/compositeproto"
         , "x11-proto/damageproto"
         , "x11-proto/dri2proto"
         , "x11-proto/dri3proto"
         , "x11-proto/fixesproto"
         , "x11-proto/glproto"
         , "x11-proto/inputproto"
         , "x11-proto/kbproto"
         , "x11-proto/presentproto"
         , "x11-proto/randrproto"
         , "x11-proto/recordproto"
         , "x11-proto/renderproto"
         , "x11-proto/xcb-proto"
         , "x11-proto/xextproto"
         , "x11-proto/xf86bigfontproto"
         , "x11-proto/xf86driproto"
         , "x11-proto/xf86vidmodeproto"
         , "x11-proto/xineramaproto"
         , "x11-proto/xproto"
         , "sys-devel/flex"
         , "sys-libs/pam"
         , "sys-libs/cracklib"
         , "virtual/pam"
         ]

laptop :: PortageR Set
laptop = "laptop" `with`
    pkgs [ "app-laptop/laptop-mode-tools" `with` use [ "bluetooth" ]
         , "net-wireless/kismet" `with` do
               unstable
               use [ "-ruby", "-plugin_autowep", "-plugin-btscan", "-plguin-ptw", "-plugin-spectools", "-plugin-syslog" ]
         , "net-wireless/rfkill"
         , "net-wireless/wpa_supplicant"
         , "net-misc/openvpn"
         , "x11-apps/xbacklight"
         ]

dev :: PortageR Set
dev = "dev" `with` do
    pkgs [ "app-editors/visual-studio-code"
         , "app-emacs/ghc-mod"
         , "app-portage/hackport"
         , "app-misc/colordiff"
         , "app-misc/jq"
         , "dev-haskell/hakyll"
         , "dev-haskell/hlint"
         , "dev-haskell/ghcid"
         , "dev-haskell/stack-bin" `with` 
               dep [ "sys-libs/ncurses" `with` use [ "tinfo" ] ]
         , "dev-python/pip"
         , "dev-python/matplotlib" `with` use [ "cairo", "gtk" ]
         , "dev-util/android-sdk-update-manager" `with` do
               unstable
               dep [ "dev-java/swt" `with` use [ "cairo" ] ]
         , "dev-util/idea-community" `with` use [ "-custom-jdk" ]
         , "dev-util/shellcheck"
         , "<net-libs/nodejs-7" `with` do
               unstable
               dep [ "dev-libs/libuv" `with` unstable ]
         ]
    deps [ "dev-java/oracle-jdk-bin" `with` license "Oracle-BCLA-JavaSE" ]

eos :: PortageR Set
eos = "eos" `with` do
    uses [ "opengl", "pulseaudio" ]
    pkgs [ "media-tv/kodi" `with` do
               unstable
               use [ "-airplay", "bluetooth", "css", "-fishbmc", "-projectm", "-samba", "udisks", "vaapi" ]
               dep [ "dev-libs/crossguid" `with` unstable
                   , "media-libs/jasper"
                   , "media-libs/libjpeg-turbo" `with` unstable
                   , "media-libs/taglib" `with` unstable
                   , "media-sound/dcadec" `with` unstable
                   ]
         , "sys-boot/grub" `with` use [ "-fonts", "-themes" ]
         , "sys-power/apcupsd" `with` unstable
         , "=x11-drivers/nvidia-drivers-370.28" `with` do
               unstable
               use [ "acpi", "gtk3", "uvm" ]
         , "x11-misc/vdpauinfo"
         ]

eurus :: PortageR Set
eurus = "eurus" `with` do
    pkgs [ "sys-boot/efibootmgr"
         , "sys-boot/gummiboot"
         , "sys-firmware/iwl7260-ucode" `with` do
               unstable
               use [ "bluetooth" ]
               dep [ "sys-firmware/iwl3160-7260-bt-ucode" `with` unstable ]
         , "sys-kernel/dracut-ykfde" `with` 
               dep [ "sys-auth/ykpers" `with` unstable
                   , "sys-auth/libyubikey" `with` unstable
                   ]
         ]
    deps [ "x11-libs/libva-intel-driver" `with` use [ "abi_x86_32" ]
         , "sys-apps/systemd" `with` use [ "gnuefi" ]
         ]

main :: IO ()
main = portager cfg
    [ baseCore 
    , baseHardened
    , baseTools
    , baseMedia
    , desktopCore
    , desktopApps
    , desktopMedia
    , games
    , laptop
    , dev
    , eos
    , eurus
    ]
