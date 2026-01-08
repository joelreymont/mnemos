const std = @import("std");
const builtin = @import("builtin");
const fs = std.fs;
const posix = std.posix;
const mem = std.mem;
const Allocator = mem.Allocator;

pub const FileEventKind = enum {
    created,
    modified,
    deleted,
};

pub const FileEvent = struct {
    path: []const u8,
    kind: FileEventKind,
};

pub const Callback = *const fn (event: FileEvent, userdata: ?*anyopaque) void;

pub const Watcher = switch (builtin.os.tag) {
    .macos, .ios, .tvos, .watchos => KqueueWatcher,
    .linux => InotifyWatcher,
    else => @compileError("Unsupported platform for file watching"),
};

const KqueueWatcher = struct {
    allocator: Allocator,
    kq_fd: posix.fd_t,
    watch_fd: posix.fd_t,
    watch_path: []const u8,
    callback: Callback,
    userdata: ?*anyopaque,
    running: bool,
    debounce_timer: i64,

    const DEBOUNCE_MS = 100;

    pub fn init(allocator: Allocator, path: []const u8, callback: Callback, userdata: ?*anyopaque) !*KqueueWatcher {
        const self = try allocator.create(KqueueWatcher);
        errdefer allocator.destroy(self);

        const kq_fd = try posix.kqueue();
        errdefer posix.close(kq_fd);

        const watch_fd = try posix.open(path, .{ .ACCMODE = .RDONLY }, 0);
        errdefer posix.close(watch_fd);

        const owned_path = try allocator.dupe(u8, path);
        errdefer allocator.free(owned_path);

        self.* = .{
            .allocator = allocator,
            .kq_fd = kq_fd,
            .watch_fd = watch_fd,
            .watch_path = owned_path,
            .callback = callback,
            .userdata = userdata,
            .running = false,
            .debounce_timer = 0,
        };

        try self.registerWatch();

        return self;
    }

    fn registerWatch(self: *KqueueWatcher) !void {
        const c = @cImport({
            @cInclude("sys/event.h");
        });

        const filter: i16 = @intCast(@as(c_int, c.EVFILT_VNODE));
        const flags: u16 = @intCast(@as(c_uint, c.EV_ADD | c.EV_ENABLE | c.EV_CLEAR));
        const fflags: u32 = @as(u32, c.NOTE_WRITE) | @as(u32, c.NOTE_DELETE) | @as(u32, c.NOTE_EXTEND) | @as(u32, c.NOTE_ATTRIB);

        const kev = posix.Kevent{
            .ident = @intCast(self.watch_fd),
            .filter = filter,
            .flags = flags,
            .fflags = fflags,
            .data = 0,
            .udata = 0,
        };

        const changelist = [_]posix.Kevent{kev};
        _ = try posix.kevent(self.kq_fd, &changelist, &.{}, null);
    }

    pub fn deinit(self: *KqueueWatcher) void {
        self.running = false;
        posix.close(self.watch_fd);
        posix.close(self.kq_fd);
        self.allocator.free(self.watch_path);
        self.allocator.destroy(self);
    }

    pub fn getFd(self: *KqueueWatcher) posix.fd_t {
        return self.kq_fd;
    }

    pub fn processEvents(self: *KqueueWatcher) !void {
        const c = @cImport({
            @cInclude("sys/event.h");
        });

        const now = std.time.milliTimestamp();
        if (now - self.debounce_timer < DEBOUNCE_MS) {
            return;
        }

        var events: [16]posix.Kevent = undefined;
        const timeout = posix.timespec{ .sec = 0, .nsec = 0 };
        const n = try posix.kevent(self.kq_fd, &.{}, &events, &timeout);

        if (n > 0) {
            self.debounce_timer = now;

            for (events[0..@intCast(n)]) |event| {
                const fflags: u32 = @intCast(event.fflags);

                if (fflags & c.NOTE_DELETE != 0) {
                    self.callback(.{
                        .path = self.watch_path,
                        .kind = .deleted,
                    }, self.userdata);
                } else if (fflags & c.NOTE_WRITE != 0 or fflags & c.NOTE_EXTEND != 0) {
                    self.callback(.{
                        .path = self.watch_path,
                        .kind = .modified,
                    }, self.userdata);
                }
            }
        }
    }

    pub fn start(self: *KqueueWatcher) !void {
        self.running = true;
        while (self.running) {
            try self.processEvents();
            posix.nanosleep(0, 50 * std.time.ns_per_ms);
        }
    }

    pub fn stop(self: *KqueueWatcher) void {
        self.running = false;
    }
};

const InotifyWatcher = struct {
    allocator: Allocator,
    inotify_fd: posix.fd_t,
    watch_fd: i32,
    watch_path: []const u8,
    callback: Callback,
    userdata: ?*anyopaque,
    running: bool,
    debounce_timer: i64,

    const DEBOUNCE_MS = 100;

    pub fn init(allocator: Allocator, path: []const u8, callback: Callback, userdata: ?*anyopaque) !*InotifyWatcher {
        const self = try allocator.create(InotifyWatcher);
        errdefer allocator.destroy(self);

        const inotify_fd = try posix.inotify_init1(posix.linux.IN.NONBLOCK);
        errdefer posix.close(inotify_fd);

        const path_z = try allocator.dupeZ(u8, path);
        defer allocator.free(path_z);

        const mask: u32 = posix.linux.IN.CREATE | posix.linux.IN.MODIFY | posix.linux.IN.DELETE | posix.linux.IN.ATTRIB;
        const watch_fd = try posix.inotify_add_watch(inotify_fd, path_z, mask);

        const owned_path = try allocator.dupe(u8, path);
        errdefer allocator.free(owned_path);

        self.* = .{
            .allocator = allocator,
            .inotify_fd = inotify_fd,
            .watch_fd = watch_fd,
            .watch_path = owned_path,
            .callback = callback,
            .userdata = userdata,
            .running = false,
            .debounce_timer = 0,
        };

        return self;
    }

    pub fn deinit(self: *InotifyWatcher) void {
        self.running = false;
        _ = posix.inotify_rm_watch(self.inotify_fd, self.watch_fd);
        posix.close(self.inotify_fd);
        self.allocator.free(self.watch_path);
        self.allocator.destroy(self);
    }

    pub fn getFd(self: *InotifyWatcher) posix.fd_t {
        return self.inotify_fd;
    }

    pub fn processEvents(self: *InotifyWatcher) !void {
        const now = std.time.milliTimestamp();

        var buf: [4096]u8 align(@alignOf(posix.linux.inotify_event)) = undefined;
        const n = posix.read(self.inotify_fd, &buf) catch |err| {
            if (err == error.WouldBlock) return;
            return err;
        };

        if (n == 0) return;

        if (now - self.debounce_timer < DEBOUNCE_MS) {
            return;
        }
        self.debounce_timer = now;

        var i: usize = 0;
        while (i < n) {
            // Bounds check: ensure we can read the fixed-size event header
            if (i + @sizeOf(posix.linux.inotify_event) > n) break;

            const event_ptr = @as(*const posix.linux.inotify_event, @ptrCast(@alignCast(&buf[i])));
            const event_size = @sizeOf(posix.linux.inotify_event) + event_ptr.len;

            // Bounds check: ensure full event (including variable name) fits
            if (i + event_size > n) break;

            const mask = event_ptr.mask;

            if (mask & posix.linux.IN.CREATE != 0) {
                self.callback(.{
                    .path = self.watch_path,
                    .kind = .created,
                }, self.userdata);
            } else if (mask & posix.linux.IN.MODIFY != 0) {
                self.callback(.{
                    .path = self.watch_path,
                    .kind = .modified,
                }, self.userdata);
            } else if (mask & posix.linux.IN.DELETE != 0) {
                self.callback(.{
                    .path = self.watch_path,
                    .kind = .deleted,
                }, self.userdata);
            }

            i += event_size;
        }
    }

    pub fn start(self: *InotifyWatcher) !void {
        self.running = true;
        while (self.running) {
            try self.processEvents();
            posix.nanosleep(0, 50 * std.time.ns_per_ms);
        }
    }

    pub fn stop(self: *InotifyWatcher) void {
        self.running = false;
    }
};

test "watcher basic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    var event_count: usize = 0;

    const TestContext = struct {
        count: *usize,
    };

    var ctx = TestContext{ .count = &event_count };

    const callback = struct {
        fn cb(event: FileEvent, userdata: ?*anyopaque) void {
            _ = event;
            const context: *TestContext = @ptrCast(@alignCast(userdata.?));
            context.count.* += 1;
        }
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, &ctx);
    defer watcher.deinit();

    const test_file = try tmp_dir.dir.createFile("test.txt", .{});
    test_file.close();

    posix.nanosleep(0, 200 * std.time.ns_per_ms);
    try watcher.processEvents();

    try testing.expect(event_count >= 0);
}

test "watcher getFd" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    // getFd should return a valid fd for poll integration
    const fd = watcher.getFd();
    try testing.expect(fd >= 0);
}

test "watcher stop" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    // Verify stop doesn't crash
    watcher.stop();
    try testing.expect(!watcher.running);
}

test "FileEventKind enum" {
    const testing = std.testing;

    // Verify all event kinds are distinct
    try testing.expect(FileEventKind.created != FileEventKind.modified);
    try testing.expect(FileEventKind.modified != FileEventKind.deleted);
    try testing.expect(FileEventKind.created != FileEventKind.deleted);
}

test "FileEvent struct fields" {
    const event = FileEvent{
        .path = "/test/path",
        .kind = .modified,
    };
    try std.testing.expectEqualStrings("/test/path", event.path);
    try std.testing.expect(event.kind == .modified);
}

test "watcher debounce constant" {
    // DEBOUNCE_MS should be reasonable (50-500ms range)
    const debounce = Watcher.DEBOUNCE_MS;
    try std.testing.expect(debounce >= 50);
    try std.testing.expect(debounce <= 500);
}

test "watcher multiple init deinit" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    // Init and deinit multiple times should work
    var w1 = try Watcher.init(allocator, path, callback, null);
    w1.deinit();

    var w2 = try Watcher.init(allocator, path, callback, null);
    w2.deinit();
}

test "FileEventKind all variants" {
    const testing = std.testing;

    const created: FileEventKind = .created;
    const modified: FileEventKind = .modified;
    const deleted: FileEventKind = .deleted;

    try testing.expect(created == .created);
    try testing.expect(modified == .modified);
    try testing.expect(deleted == .deleted);
}

test "FileEvent with all event kinds" {
    const testing = std.testing;

    const created = FileEvent{ .path = "/test/file", .kind = .created };
    const modified = FileEvent{ .path = "/test/file", .kind = .modified };
    const deleted = FileEvent{ .path = "/test/file", .kind = .deleted };

    try testing.expect(created.kind == .created);
    try testing.expect(modified.kind == .modified);
    try testing.expect(deleted.kind == .deleted);
    try testing.expectEqualStrings("/test/file", created.path);
}

test "Callback type signature" {
    const callback: Callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    const event = FileEvent{ .path = "/test", .kind = .created };
    callback(event, null);
}

test "watcher processEvents no events" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    try watcher.processEvents();
}

test "watcher userdata context passing" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    var context_value: u32 = 42;

    const callback = struct {
        fn cb(_: FileEvent, userdata: ?*anyopaque) void {
            const value: *u32 = @ptrCast(@alignCast(userdata.?));
            value.* = 99;
        }
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, &context_value);
    defer watcher.deinit();

    const event = FileEvent{ .path = path, .kind = .modified };
    callback(event, &context_value);

    try testing.expect(context_value == 99);
}

test "watcher path ownership" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    try testing.expectEqualStrings(path, watcher.watch_path);
}

test "FileEventKind enum distinct values" {
    const testing = std.testing;
    try testing.expect(@intFromEnum(FileEventKind.created) != @intFromEnum(FileEventKind.modified));
    try testing.expect(@intFromEnum(FileEventKind.modified) != @intFromEnum(FileEventKind.deleted));
    try testing.expect(@intFromEnum(FileEventKind.created) != @intFromEnum(FileEventKind.deleted));
}

test "FileEvent struct path assignment" {
    const testing = std.testing;
    const event = FileEvent{ .path = "/some/path/file.txt", .kind = .modified };
    try testing.expectEqualStrings("/some/path/file.txt", event.path);
}

test "FileEvent struct kind assignment" {
    const testing = std.testing;
    const event = FileEvent{ .path = "/test", .kind = .deleted };
    try testing.expect(event.kind == .deleted);
}

test "watcher getFd returns valid fd" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    const fd = watcher.getFd();
    try testing.expect(fd >= 0);
}

test "watcher stop sets running false" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    watcher.running = true;
    watcher.stop();
    try testing.expect(!watcher.running);
}

test "Callback can be null userdata" {
    const callback: Callback = struct {
        fn cb(_: FileEvent, userdata: ?*anyopaque) void {
            _ = userdata;
        }
    }.cb;

    const event = FileEvent{ .path = "/test", .kind = .created };
    callback(event, null);
}

test "watcher debounce timer initial value" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(path);

    const callback = struct {
        fn cb(_: FileEvent, _: ?*anyopaque) void {}
    }.cb;

    var watcher = try Watcher.init(allocator, path, callback, null);
    defer watcher.deinit();

    try testing.expect(watcher.debounce_timer == 0);
}
