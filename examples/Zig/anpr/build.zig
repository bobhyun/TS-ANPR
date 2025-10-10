const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create executable (Zig 0.15.1 API)
    const exe = b.addExecutable(.{
        .name = "anpr",
        .root_module = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/main.zig" },
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.linkLibC();
    
    // Add C source file for TSANPR loading functions
    exe.addCSourceFile(.{ .file = .{ .cwd_relative = "src/tsanpr.c" }, .flags = &.{} });

    // Note: Do NOT link tsanpr at build time; we load the DLL at runtime via TSANPR_load
    // exe.linkSystemLibrary("tsanpr");
    
    // Windows-specific libraries
    if (target.result.os.tag == .windows) {
        exe.linkSystemLibrary("kernel32");
        exe.linkSystemLibrary("user32");
        exe.linkSystemLibrary("gdi32");
        exe.linkSystemLibrary("ole32");
        exe.linkSystemLibrary("oleaut32");
        exe.linkSystemLibrary("uuid");
    }

    // Add include paths
    exe.addIncludePath(.{ .cwd_relative = "src" });
    
    // Install the executable
    b.installArtifact(exe);

    // Run configuration
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Test configuration
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = .{ .cwd_relative = "src/main.zig" },
            .target = target,
            .optimize = optimize,
        }),
    });
    unit_tests.linkLibC();
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);
}