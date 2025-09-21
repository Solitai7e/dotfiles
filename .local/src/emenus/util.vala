using Gee;

[PrintfFunction]
private static void notify(string format, ...) {
  var args = va_list();
  try {
    var prgname = Environment.get_prgname();
    var body = format.vprintf(args);
    printerr("%s\n", body);

    if (!Notify.is_initted()) Notify.init(prgname); // :)
    new Notify.Notification(prgname, body, null).show();
  } catch (Error error) {}
}
private static string? dmenu_ask(string prompt) throws Error {
	int status;
	string output;
	Process.spawn_sync(
		null, {"dmenu", "-p", prompt},
		null, SpawnFlags.SEARCH_PATH, null,
		out output, null, out status);
	if (status != 0 || output.length == 0)
    return null;
	return output.substring(0, output.index_of_char('\n'));
}
private static int dmenu_choice(string prompt,
                                ArrayList<string> lines) throws Error {
	int input_fd, output_fd;
	Process.spawn_async_with_pipes(
		null, {"dmenu", "-l", "20", "-p", prompt},
		null, SpawnFlags.SEARCH_PATH, null,
		null, out input_fd, out output_fd, null);
	/* HACK: we have to close the stream here to signal EOF to dmenu */ {
		var input = FileStream.fdopen(input_fd, "w");
		int padding = "%d".printf(lines.size).length + 1;
		for (var i = 0; i < lines.size; i++)
			input.printf("%-*s %s\n", padding, "%i.".printf(i + 1), lines[i]);
	}
	var output = FileStream.fdopen(output_fd, "r");
	int index = 0; output.scanf("%d", &index); index--;
	if (index < 0 || index >= lines.size)
    return -1;
	return index;
}
private static ArrayList<string> tabulate(ArrayList<ArrayList<string>> rows) {
	var column_lengths = new ArrayList<int>();
	foreach (var columns in rows) {
		for (var n = columns.size - column_lengths.size; n > 0; n--)
			column_lengths.add(0);
		for (var i = 0; i < columns.size; i++)
			column_lengths[i] = int.max(column_lengths[i], columns[i].length);
	}
	var lines = new ArrayList<string>();
	var line_length = column_lengths.fold<int>((a, b) => a + b, 0)
									+ (column_lengths.size - 1) * 4;
	foreach (var columns in rows) {
		var line = string.nfill(line_length, ' ');
		for (var i = 0, p = 0; i < columns.size; p += column_lengths[i++] + 4)
			Memory.copy(line.data + p, columns[i].data, columns[i].length);
		lines.add(line);
	}
	return lines;
}
private static string format_size(uint64 size) {
	switch ((int) Math.floor(Math.log2(size) / 10)) {
		case 0: return ("%" + uint64.FORMAT + "B").printf(size);
		case 1: return "%.1fK".printf(size / 1024.0);
		case 2: return "%.1fM".printf(size / 1048576.0);
		case 3: return "%.1fG".printf(size / 1073741824.0);
		case 4: return "%.1fT".printf(size / 1099511627776.0);
		default: return "%.1fP".printf(size / 1125899906842624.0);
	}
}
//private static bool is_block_device(string path) throws IOError {
//  Posix.Stat s;
//  if (Posix.stat(path, out s) != 0)
//    throw IOError.from_errno(Posix.errno);
//  return Posix.S_ISBLK(s.st_mode);
//}
//private static string setup_loop(string path) throws Error {
//  var manager = new UDisks.Client.sync().manager;
//  var fd = Posix.open(path, Posix.O_CLOEXEC);
//  if (fd == -1) throw IOError.from_errno(Posix.errno);
//  string device;
//  manager.call_loop_setup_sync(
//    new Variant("h", 0),
//    new Variant("s{av}"),
//    new UnixFDList.from_array({fd}),
//    out device, null);
//  return device;
//}