-- XMobar config
--
Config { font    = "xft:JetBrains Mono:weight=Bold:pixelsize=11:antialias=true:hinting=true"
       , additionalFonts =  [ "xft:Mononoki:pixelsize=11:antialias=true:hinting=true"
                            , "xft:Font Awesome 6 Free Solid:pixelsize=12"
                            , "xft:Font Awesome 6 Brands:pixelsize=12"
                            ]

       , bgColor = "#000000"
       , fgColor = "#afd75f"
       , position = Top
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/xarch/.config/xmobar/"  -- default: "[NERD]"
       , commands = [ Run Date " <fn=2>\xf017</fn> %b %d %Y  %H:%M:%S" "date" 10
                    , Run Cpu ["-t",    "<fn=2>\xf108</fn> CPU: <bar> (<total>%)","-H","50","--high","red"] 10
                    , Run Memory ["-t", "<fn=2>\xf233</fn>  MEM:<usedbar> (<usedratio>%)"] 10
                    , Run DiskU [("/",  "<fn=2>\xf0c7</fn> SSD:<usedbar> (<used>)")] [] 3600
                    , Run BatteryP ["BAT0"] ["-t", " <fn=2></fn> BAT:<acstatus><watts> (<left>%)"] 360
                    , Run Uptime ["-t", " <fn=2></fn> UP: <days>d <hours>h"] 36000
                        -- Uptime
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> <fc=#d7d7d7> | </fc>%UnsafeStdinReader% }{ <fc=#ffff5f>%cpu%</fc> | <fc=#ff5f87>%memory%</fc> | <fc=#87afd7>%disku%</fc> | <fc=#d79921>%battery%</fc> | <fc=#b294bb>%uptime%</fc> | <fc=#afd75f>%date%</fc> "
