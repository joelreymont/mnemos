const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options
    const dynamic = b.option(bool, "dynamic", "Use dynamic linking (default: static)") orelse false;

    // Get git hash at build time
    const git_hash = b.run(&.{ "git", "rev-parse", "--short", "HEAD" });

    // Build options for source code
    const options = b.addOptions();
    options.addOption([]const u8, "git_hash", std.mem.trim(u8, git_hash, "\n\r "));

    // Main executable
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addOptions("build_options", options);

    const exe = b.addExecutable(.{
        .name = "mnemos",
        .root_module = exe_mod,
    });

    linkLibraries(exe, dynamic);
    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run mnemos");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const test_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_mod.addOptions("build_options", options);

    // Add quickcheck module for property-based testing
    const quickcheck_mod = b.createModule(.{
        .root_source_file = b.path("src/util/quickcheck.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_mod.addImport("quickcheck", quickcheck_mod);

    // Add ohsnap module for snapshot testing
    if (b.lazyDependency("ohsnap", .{
        .target = target,
        .optimize = optimize,
    })) |ohsnap_dep| {
        test_mod.addImport("ohsnap", ohsnap_dep.module("ohsnap"));
    }

    const exe_unit_tests = b.addTest(.{
        .root_module = test_mod,
    });
    linkLibraries(exe_unit_tests, dynamic);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Debug under lldb
    const lldb = b.addSystemCommand(&.{ "lldb", "--" });
    lldb.addArtifactArg(exe_unit_tests);
    const lldb_step = b.step("debug", "Run tests under lldb");
    lldb_step.dependOn(&lldb.step);
}

fn linkLibraries(step: *std.Build.Step.Compile, dynamic: bool) void {
    step.linkLibC();

    // Include paths
    step.addIncludePath(.{ .cwd_relative = "/opt/homebrew/include" });

    if (dynamic) {
        // Dynamic linking - simpler, for development
        step.linkSystemLibrary("git2");
        step.linkSystemLibrary("tree-sitter");
    } else {
        // Static linking - single binary deployment
        // Static libraries (order matters for dependency resolution)
        step.addObjectFile(.{ .cwd_relative = "/opt/homebrew/opt/tree-sitter/lib/libtree-sitter.a" });
        step.addObjectFile(.{ .cwd_relative = "/opt/homebrew/opt/libgit2/lib/libgit2.a" });
        step.addObjectFile(.{ .cwd_relative = "/opt/homebrew/opt/libssh2/lib/libssh2.a" });
        step.addObjectFile(.{ .cwd_relative = "/opt/homebrew/opt/openssl/lib/libssl.a" });
        step.addObjectFile(.{ .cwd_relative = "/opt/homebrew/opt/openssl/lib/libcrypto.a" });

        // System dependencies (macOS)
        step.linkFramework("CoreFoundation");
        step.linkFramework("Security");
        step.linkSystemLibrary("z");
        step.linkSystemLibrary("iconv");
    }
}
