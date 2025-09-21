using Gee;

private static ArrayList<UDisks.Object> query_udisks_mountables() throws Error {
  var client = new UDisks.Client.sync();
  var mountables = new ArrayList<UDisks.Object>();
  foreach (var dbus_object in client.object_manager.get_objects()) {
    var object = client.get_object(dbus_object.get_object_path());
    if (object?.block != null &&
        object?.filesystem?.mount_points?.length == 0)
      mountables.add(object);
  }
  return mountables;
}
private static UDisks.Object? find_udisks_mountable(File device) throws Error {
  var path = device.get_path();
  foreach (var mountable in query_udisks_mountables())
    if (mountable.block.preferred_device == path)
      return mountable;
    else
      foreach (var link in mountable.block.symlinks)
        if (link == path) return mountable;
  return null;
}
private static UDisks.Object? dmenu_mountables(string prompt) throws Error {
  var mountables = query_udisks_mountables();
  var rows = new ArrayList<ArrayList<string>>();
  foreach (var mountable in mountables)
    rows.add(new ArrayList<string>.wrap({
      mountable.block.preferred_device,
      mountable.block.id_type,
      format_size(mountable.block.size),
      mountable.block.id_label}));
  var i = dmenu_choice(prompt, tabulate(rows));
  return i >= 0 ? mountables[i] : null;
}
private static File udisks_mount(UDisks.Object target,
                                 string? fstype = null,
                                 string? mountopts = null) throws Error {
  var options = new VariantBuilder(new VariantType("a{sv}"));
  if (fstype != null)
    options.add("{sv}", "fstype", new Variant.string(fstype));
  if (mountopts != null)
    options.add("{sv}", "options", new Variant.string(mountopts));

  string mountpoint;
  target.filesystem.call_mount_sync(options.end(), out mountpoint);
  return File.new_for_path(mountpoint);
}
private static async File gio_mount(File target) throws Error {
  yield target.mount_enclosing_volume(MountMountFlags.NONE, null);
  return target.find_enclosing_mount().get_root();
}
private static async int main(string[] argv) {
  try {
    string? fstype = null;
    string? mountopts = null;
    bool open_mountpoint = true;

    var option_context = new OptionContext("[TARGET]");
    option_context.add_main_entries({
      {"options", 'o', OptionFlags.NONE, OptionArg.STRING, ref mountopts,
       "fstab-compatible mount options, ignored for non-blockdevices"},
      {"type", 't', OptionFlags.NONE, OptionArg.STRING, ref fstype,
       "fstab-compatible filesystem name, ignored for non-blockdevices"},
      {null, 'O', OptionFlags.REVERSE, OptionArg.NONE, ref open_mountpoint,
       "don't open the mountpoint in the file manager automatically"},
      {null}}, null);
    option_context.parse(ref argv);

    File target, mountpoint;
    if (argv.length > 1) {
      target = File.new_for_commandline_arg(argv[1]);
      if (target.has_uri_scheme("file")) {
        var mountable = find_udisks_mountable(target);
        if (mountable == null)
          throw new IOError.NOT_FOUND("No such device");
        mountpoint = udisks_mount(mountable, fstype, mountopts);
      } else {
        mountpoint = yield gio_mount(target);
      }
    } else {
      var mountable = dmenu_mountables("Mount");
      if (mountable == null) return 1;
      target = File.new_for_path(mountable.block.preferred_device);
      mountpoint = udisks_mount(mountable, fstype, mountopts);
    }
    if (target.has_uri_scheme("file"))
      notify("Mounted %s at %s", target.get_path(), mountpoint.get_path());
    else
      notify("Mounted %s", target.get_path());

    if (open_mountpoint)
      AppInfo.launch_default_for_uri(mountpoint.get_uri(), null);

    return 0;
  } catch (Error error) {
    notify(" %s", error.message);
		return 1;
	}
}