// swift-tools-version:5.10
import PackageDescription

let package = Package(
    name: "anpr",
    products: [
        .executable(name: "anpr", targets: ["anpr"])
    ],
    targets: [
        // System library target for CBridge
        .systemLibrary(
            name: "CBridge",
            path: "CBridge",
            pkgConfig: nil
        ),
        // Main Swift executable target
        .executableTarget(
            name: "anpr",
            dependencies: ["CBridge"],
            path: "Sources"
        )
    ]
)
