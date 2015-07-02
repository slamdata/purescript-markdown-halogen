"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");
var rimraf = require("rimraf");
var webpack = require("webpack-stream");

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(["src/**/*.purs", "example/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(purescript.pscMake());
});

gulp.task("example", ["make"], function() {
  return gulp.src("example/src/entry.js")
    .pipe(webpack({
      resolve: { modulesDirectories: [ "node_modules", "output" ] },
      output: { filename: "example.js" }
    }))
    .pipe(gulp.dest("example/output"));
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(jsvalidate());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, ["clean-docs"], function () {
    return gulp.src("src/" + name.replace(/\./g, "/") + ".purs")
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Text.Markdown.SlamDown.Html"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("default", ["jsvalidate", "docs", "make", "example"]);
