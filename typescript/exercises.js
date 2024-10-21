"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __asyncValues = (this && this.__asyncValues) || function (o) {
    if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
    var m = o[Symbol.asyncIterator], i;
    return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
    function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
    function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
};
var __values = (this && this.__values) || function(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.change = change;
exports.firstThenApply = firstThenApply;
exports.powersGenerator = powersGenerator;
exports.meaningfulLineCount = meaningfulLineCount;
exports.surfaceArea = surfaceArea;
exports.volume = volume;
exports.shapeToString = shapeToString;
var promises_1 = require("node:fs/promises");
function change(amount) {
    if (amount < 0) {
        throw new RangeError("Amount cannot be negative");
    }
    var counts = new Map();
    var remaining = amount;
    for (var _i = 0, _a = [25n, 10n, 5n, 1n]; _i < _a.length; _i++) {
        var denomination = _a[_i];
        counts.set(denomination, remaining / denomination);
        remaining %= denomination;
    }
    return counts;
}
function firstThenApply(items, predicate, consumer) {
    var foundItem = items.find(predicate);
    return foundItem !== undefined ? consumer(foundItem) : undefined;
}
function powersGenerator(base) {
    var power;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                power = 1n;
                _a.label = 1;
            case 1: return [4 /*yield*/, power];
            case 2:
                _a.sent();
                _a.label = 3;
            case 3:
                power *= base;
                return [3 /*break*/, 1];
            case 4: return [2 /*return*/];
        }
    });
}
// Write your line count function here
function meaningfulLineCount(filename) {
    return __awaiter(this, void 0, void 0, function () {
        var count, file, _a, _b, _c, line, trimmed, e_1_1;
        var _d, e_1, _e, _f;
        return __generator(this, function (_g) {
            switch (_g.label) {
                case 0:
                    count = 0;
                    return [4 /*yield*/, (0, promises_1.open)(filename, "r")];
                case 1:
                    file = _g.sent();
                    _g.label = 2;
                case 2:
                    _g.trys.push([2, 7, 8, 13]);
                    _a = true, _b = __asyncValues(file.readLines());
                    _g.label = 3;
                case 3: return [4 /*yield*/, _b.next()];
                case 4:
                    if (!(_c = _g.sent(), _d = _c.done, !_d)) return [3 /*break*/, 6];
                    _f = _c.value;
                    _a = false;
                    line = _f;
                    trimmed = line.trim();
                    if (trimmed && !trimmed.startsWith("#")) {
                        count++;
                    }
                    _g.label = 5;
                case 5:
                    _a = true;
                    return [3 /*break*/, 3];
                case 6: return [3 /*break*/, 13];
                case 7:
                    e_1_1 = _g.sent();
                    e_1 = { error: e_1_1 };
                    return [3 /*break*/, 13];
                case 8:
                    _g.trys.push([8, , 11, 12]);
                    if (!(!_a && !_d && (_e = _b.return))) return [3 /*break*/, 10];
                    return [4 /*yield*/, _e.call(_b)];
                case 9:
                    _g.sent();
                    _g.label = 10;
                case 10: return [3 /*break*/, 12];
                case 11:
                    if (e_1) throw e_1.error;
                    return [7 /*endfinally*/];
                case 12: return [7 /*endfinally*/];
                case 13: return [2 /*return*/, count];
            }
        });
    });
}
function surfaceArea(shape) {
    switch (shape.kind) {
        case "Box":
            return 2 * (shape.width * shape.height + shape.width * shape.depth + shape.height * shape.depth);
        case "Sphere":
            return 4 * Math.PI * Math.pow(shape.radius, 2);
    }
}
function volume(shape) {
    switch (shape.kind) {
        case "Box":
            return shape.width * shape.height * shape.depth;
        case "Sphere":
            return (4 / 3) * Math.PI * Math.pow(shape.radius, 3);
    }
}
function shapeToString(shape) {
    switch (shape.kind) {
        case "Box":
            return "Box(width=".concat(shape.width, ", length=").concat(shape.height, ", depth=").concat(shape.depth, ")");
        case "Sphere":
            return "Sphere(radius=".concat(shape.radius, ")");
    }
}
var Empty = /** @class */ (function () {
    function Empty() {
    }
    Empty.prototype.insert = function (value) {
        return new Node(value, Empty.instance, Empty.instance);
    };
    Empty.prototype.contains = function (_) { return false; };
    Empty.prototype.size = function () { return 0; };
    Empty.prototype.inorder = function () { return __generator(this, function (_a) {
        return [2 /*return*/];
    }); };
    Empty.prototype.toString = function () { return "()"; };
    Empty.instance = new Empty();
    return Empty;
}());
var Node = /** @class */ (function () {
    function Node(value, left, right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }
    Node.prototype.insert = function (value) {
        if (value.compareTo(this.value) < 0) {
            return new Node(this.value, this.left.insert(value), this.right);
        }
        else if (value.compareTo(this.value) > 0) {
            return new Node(this.value, this.left, this.right.insert(value));
        }
        return this;
    };
    Node.prototype.contains = function (value) {
        if (value.compareTo(this.value) < 0) {
            return this.left.contains(value);
        }
        else if (value.compareTo(this.value) > 0) {
            return this.right.contains(value);
        }
        return true;
    };
    Node.prototype.size = function () {
        return 1 + this.left.size() + this.right.size();
    };
    Node.prototype.inorder = function () {
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [5 /*yield**/, __values(this.left.inorder())];
                case 1:
                    _a.sent();
                    return [4 /*yield*/, this.value];
                case 2:
                    _a.sent();
                    return [5 /*yield**/, __values(this.right.inorder())];
                case 3:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    };
    Node.prototype.toString = function () {
        return "(".concat(this.left).concat(this.value).concat(this.right, ")");
    };
    return Node;
}());
var ComparableString = /** @class */ (function () {
    function ComparableString(str) {
        this.str = str;
    }
    ComparableString.prototype.compareTo = function (other) {
        return this.str.localeCompare(other.str);
    };
    ComparableString.prototype.toString = function () {
        return this.str;
    };
    return ComparableString;
}());
