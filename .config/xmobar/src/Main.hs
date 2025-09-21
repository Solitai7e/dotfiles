{-# LANGUAGE OverloadedStrings #-}
import Xmobar
import RawCommandReader

config :: Config
config = defaultConfig {
  font     = "Monospace 11.25",
  bgColor  = "black",
  fgColor  = "#ff3393",
  alpha    = 150,
  position = TopSize C 100 30,
  sepChar  = "%",
  alignSep = "}{",
  template = " %XMonadLog% } { %input-method% │ %dynnetwork% | %battery% │ %alsa:default:Master% │ %date% ",
  commands = [
    Run XMonadLog,
    --Run MPD ["-t", "[<fc=#ffff70><statei></fc>] <fc=#e7abff><artist> - <title></fc> [<fc=#ffff70><lapsed></fc>/<fc=#fff><length></fc>]", "--",
    --         "-P", "\xF04B", "-S", "\xF04D", "-Z", "\xF04C"]
    --        10,
    Run $ RawCommandReader "monitor-input-method" [] "input-method",
    Run $ DynNetwork ["-t", "<dev> [<fc=#ffff70>\xF063 <rx></fc>│<fc=#e7abff>\xF062 <tx></fc>]", "-STrue"] 10,
    Run $ RawCommandReader "monitor-battery" [] "battery",
    Run $ Alsa "default" "Master"
               ["-t", "<status> [<fc=#ffff70><volume>%</fc>]", "--",
                "-O", "\xF028", "-C", "#ff3393",
                "-o", "\xF026", "-c", "#ff3393"],
    Run $ Date "%A, %m/%d/%Y, %I:%M %p" "date" 55
  ]
}

main :: IO ()
main = configFromArgs config >>= xmobar
