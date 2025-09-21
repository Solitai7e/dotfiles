project('emenus', 'vala', 'c')

common_deps = [
  dependency('glib-2.0'),
  dependency('gio-2.0'),
  dependency('gio-unix-2.0'),
  dependency('udisks2'),
  dependency('gee-0.8'),
  dependency('libnotify'),
  meson.get_compiler('c').find_library('m', required : false),
  meson.get_compiler('vala').find_library('posix')]

executable(
  'emount',
  files('emount.vala', 'util.vala'),
  dependencies: common_deps)

executable(
  'eunmount',
  files('eunmount.vala', 'util.vala'),
  dependencies: common_deps)

executable(
  'eopen',
  files('eopen.vala', 'util.vala'),
  dependencies: common_deps)