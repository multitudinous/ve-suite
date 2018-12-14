# Rock Island Arsenal VR Juggler configs

The following directories contain complete VR Juggler configuration files for use
in the RIA's immersive engineering lab:

* `cave_configs_optitrak_active`
* `cave_configs_optitrak_active_sdl`
* `cave_configs_motive-via-vrpn`
* `cave_configs_motive-via-vrpn_sdl`

The configuration in `cave_configs_optitrak_active` is the main/base configuration,
for use with VE-Suite 3.0 on the `master` branch of this repository.

The configuration in `cave_configs_optitrak_active_sdl` is a modified version of
the base configuration for use with the "part manipulator" feature branch on
`feature/ria-interactive-scripting-tools`. The main difference in this configuration
is a switch from the Linux joystick to the SDL joystick Gadgeteer driver. This
switch makes the full functionality of the RIA lab's gamepad available to VE-Suite,
and enables the button-based modal UI used in the part manipulator branch.

The `cave_configs_motive-via-vrpn*` directories contain VRPN-ified versions of
these configs, for use with the newer OptiTrack Motive, which is incompatible
with VR Juggler's OptiTrack driver.
