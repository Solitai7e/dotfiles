using Gee;

private static async int main(string[] argv) {
  try {
    bool print = false;

    var option_context = new OptionContext("[PATH_OR_URI] ...");
    option_context.add_main_entries({
      {"print", 'P', OptionFlags.NONE, OptionArg.NONE, ref print,
       "print the resolved absolute path instead of opening it"},
      {null}}, null);
    option_context.parse(ref argv);

    string arg = argv.length > 1 ? argv[1] : dmenu_ask("Open");
    if (arg == null) return 1;
    var file = File.new_for_commandline_arg(arg);
    if (file.get_path() == null && !file.has_uri_scheme("file")) {
      yield file.mount_enclosing_volume(MountMountFlags.NONE, null);
      var mountpoint = file.find_enclosing_mount().get_root();
      notify("Mounted %s", mountpoint.get_uri());
    }

    if (print)
      stdout.printf("%s\n", file.get_path());
    else
      AppInfo.launch_default_for_uri(file.get_uri(), null);
    return 0;
  } catch (Error error) {
    notify("%s", error.message);
		return 1;
	}
}