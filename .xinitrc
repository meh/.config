nvidia-settings --assign CurrentMetaMode="DP-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On,Primary=On}, HDMI-0: nvidia-auto-select +3840+0 {ForceCompositionPipeline=On, Rotation=left}"
xrandr --output DP-0 --primary
xset -dpms
exec xmonad
