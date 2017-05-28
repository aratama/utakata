const path = require("path");
global.module.paths.unshift(path.resolve(path.join(__dirname, "purs_modules")));
const Main = require("Main");
Main.main();
