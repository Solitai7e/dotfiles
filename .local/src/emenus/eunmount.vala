using Gee;

private static ArrayList<Mount> query_mounts() {
  var mounts = new ArrayList<Mount>();
  foreach (var mount in VolumeMonitor.get().get_mounts())
    mounts.add(mount);
  return mounts;
}
private static Mount? find_mount(string uri) throws Error {
  try {
    var file = File.new_for_uri(uri);
    var mount = File.new_for_uri(uri).find_enclosing_mount();
    if (!file.equal(mount.get_root()))
      return null;
    return mount;
  } catch (IOError error) {
    if (error.code == IOError.NOT_FOUND)
      return null;
    throw error;
  }
}
private static Mount? dmenu_mounts(string prompt) throws Error {
  var mounts = query_mounts();
  var rows = new ArrayList<ArrayList<string>>();
  foreach (var mount in mounts) {
    var root = mount.get_root();
    if (root.has_uri_scheme("file")) {
      var mount_entry = new UnixMountEntry(root.get_path());
      rows.add(new ArrayList<string>.wrap({
        mount_entry.get_device_path(),
        mount_entry.get_mount_path(),
        mount.get_name()}));
    } else {
      rows.add(new ArrayList<string>.wrap({
        root.get_uri(),
        root.get_path(),
        mount.get_name()}));
    }
  }
  var i = dmenu_choice(prompt, tabulate(rows));
  return i >= 0 ? mounts[i] : null;
}
private static async int main(string[] argv) {
  try {
    bool force = false;

    var option_context = new OptionContext("[TARGET]");
    option_context.add_main_entries({
      {"force", 'f', OptionFlags.NONE, OptionArg.NONE, ref force,
       "force unmount"},
      {null}}, null);
    option_context.parse(ref argv);

    var flags = MountUnmountFlags.NONE;
    if (force) flags |= MountUnmountFlags.FORCE;

    Mount mount;
    if (argv.length > 1) {
      mount = find_mount(argv[1]);
      if (mount == null)
        throw new IOError.NOT_FOUND("No such mount");
    } else {
      mount = dmenu_mounts("Unmount");
      if (mount == null) return 1;
    }
    var mountpoint = mount.get_root().get_path();
    yield mount.unmount_with_operation(flags, null);
    notify("Unmounted %s", mountpoint);

    return 0;
  } catch (Error error) {
    notify("%s", error.message);
    return 1;
  }
}