"use strict";

var cs = require("use-context-selector");

exports.useContextSelector_ = cs.useContextSelector;

exports.createContextSelector = (defaultValue) => () =>
  cs.createContext(defaultValue);
