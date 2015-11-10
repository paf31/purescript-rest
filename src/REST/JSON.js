"use strict";

// module REST.JSON

exports.prettyJSON = function(a) {
    return JSON.stringify(a, null, 2);
};
