{
  "targets": [
    {
      "target_name": "tsanpr-addon",
      "sources": [
        "native/tsanpr.cpp",
        "native/tsanpr-addon.cpp"
      ],
      "include_dirs": [
        "node_modules/node-addon-api"
      ],
      "dependencies": [
        "<!(node -p \"require('node-addon-api').gyp\")"
      ],
      "cflags!": [ "-fno-exceptions" ],
      "cflags_cc!": [ "-fno-exceptions" ],
      "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ]
    }
  ]
}
