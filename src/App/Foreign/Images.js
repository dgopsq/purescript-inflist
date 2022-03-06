"use strict";

var logoImage = require("../../src/assets/images/logo.svg");

var getLogoImage = () => logoImage || null;

exports.getLogoImage_ = getLogoImage;
