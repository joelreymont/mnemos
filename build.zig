const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main executable
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "hemis",
        .root_module = exe_mod,
    });

    // Link SQLite and libgit2
    exe.linkSystemLibrary("sqlite3");
    exe.linkSystemLibrary("git2");
    exe.linkLibC();

    // Link tree-sitter
    exe.linkSystemLibrary("tree-sitter");
    exe.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run hemis");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_unit_tests = b.addTest(.{
        .root_module = test_mod,
    });

    // Link SQLite and libgit2 for tests too
    exe_unit_tests.linkSystemLibrary("sqlite3");
    exe_unit_tests.linkSystemLibrary("git2");
    exe_unit_tests.linkLibC();

    // Link tree-sitter for tests
    exe_unit_tests.linkSystemLibrary("tree-sitter");
    exe_unit_tests.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Debug under lldb
    const lldb = b.addSystemCommand(&.{ "lldb", "--" });
    lldb.addArtifactArg(exe_unit_tests);
    const lldb_step = b.step("debug", "Run tests under lldb");
    lldb_step.dependOn(&lldb.step);
}
