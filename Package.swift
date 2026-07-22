// swift-tools-version:5.9
import PackageDescription

/// Tree-sitter grammar for PowerShell.
/// Always include scanner.c (upstream Package.swift probes the path at manifest
/// evaluation time with the wrong cwd and can drop scanner symbols).
let package = Package(
    name: "TreeSitterPowershell",
    platforms: [.iOS(.v15), .macOS(.v13)],
    products: [
        .library(name: "TreeSitterPowershell", targets: ["TreeSitterPowershell"]),
    ],
    targets: [
        .target(
            name: "TreeSitterPowershell",
            path: ".",
            exclude: [
                "Cargo.toml",
                "binding.gyp",
                "bindings/c",
                "bindings/go",
                "bindings/node",
                "bindings/python",
                "bindings/rust",
                "prebuilds",
                "grammar.js",
                "package.json",
                "package-lock.json",
                "pyproject.toml",
                "setup.py",
                "test",
                "examples",
                ".editorconfig",
                ".github",
                ".gitignore",
                ".gitattributes",
                ".gitmodules",
            ],
            sources: [
                "src/parser.c",
                "src/scanner.c",
            ],
            resources: [
                .copy("queries"),
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
    ],
    cLanguageStandard: .c11
)
