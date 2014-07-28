requirejs.config({
  shim: {

  },
  paths: {
    d3: "../lib/d3/d3",
    react: "../lib/react/react",
    requirejs: "../lib/requirejs/require",
    underscore: "../lib/underscore/underscore"
  },
  packages: [

  ]
});

requirejs(['main']);
