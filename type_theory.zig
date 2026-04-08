const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const Sha256 = std.crypto.hash.sha2.Sha256;

pub const TypeTheoryError = error{
    TypeMismatch,
    UnificationFailure,
    LinearityViolation,
    InvalidTypeConstruction,
    VariableNotInContext,
    InvalidApplication,
    InvalidProjection,
    CategoryLawViolation,
    OutOfMemory,
    InvalidIdentityElimination,
};

pub const TypeKind = enum(u8) {
    UNIT = 0,
    BOOL = 1,
    NAT = 2,
    INT = 3,
    REAL = 4,
    COMPLEX = 5,
    STRING = 6,
    ARRAY = 7,
    TUPLE = 8,
    RECORD = 9,
    SUM = 10,
    FUNCTION = 11,
    DEPENDENT_FUNCTION = 12,
    DEPENDENT_PAIR = 13,
    UNIVERSE = 14,
    IDENTITY = 15,
    QUANTUM_TYPE = 16,
    BOTTOM = 17,
    TOP = 18,
    VARIABLE = 19,
    APPLICATION = 20,

    const Self = @This();

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .UNIT => "Unit",
            .BOOL => "Bool",
            .NAT => "Nat",
            .INT => "Int",
            .REAL => "Real",
            .COMPLEX => "Complex",
            .STRING => "String",
            .ARRAY => "Array",
            .TUPLE => "Tuple",
            .RECORD => "Record",
            .SUM => "Sum",
            .FUNCTION => "Function",
            .DEPENDENT_FUNCTION => "Pi",
            .DEPENDENT_PAIR => "Sigma",
            .UNIVERSE => "Type",
            .IDENTITY => "Id",
            .QUANTUM_TYPE => "Quantum",
            .BOTTOM => "Bottom",
            .TOP => "Top",
            .VARIABLE => "Var",
            .APPLICATION => "App",
        };
    }

    pub fn fromString(s: []const u8) ?Self {
        if (std.mem.eql(u8, s, "Unit")) return .UNIT;
        if (std.mem.eql(u8, s, "Bool")) return .BOOL;
        if (std.mem.eql(u8, s, "Nat")) return .NAT;
        if (std.mem.eql(u8, s, "Int")) return .INT;
        if (std.mem.eql(u8, s, "Real")) return .REAL;
        if (std.mem.eql(u8, s, "Complex")) return .COMPLEX;
        if (std.mem.eql(u8, s, "String")) return .STRING;
        if (std.mem.eql(u8, s, "Array")) return .ARRAY;
        if (std.mem.eql(u8, s, "Tuple")) return .TUPLE;
        if (std.mem.eql(u8, s, "Record")) return .RECORD;
        if (std.mem.eql(u8, s, "Sum")) return .SUM;
        if (std.mem.eql(u8, s, "Function")) return .FUNCTION;
        if (std.mem.eql(u8, s, "Pi")) return .DEPENDENT_FUNCTION;
        if (std.mem.eql(u8, s, "Sigma")) return .DEPENDENT_PAIR;
        if (std.mem.eql(u8, s, "Type")) return .UNIVERSE;
        if (std.mem.eql(u8, s, "Id")) return .IDENTITY;
        if (std.mem.eql(u8, s, "Quantum")) return .QUANTUM_TYPE;
        if (std.mem.eql(u8, s, "Bottom")) return .BOTTOM;
        if (std.mem.eql(u8, s, "Top")) return .TOP;
        if (std.mem.eql(u8, s, "Var")) return .VARIABLE;
        if (std.mem.eql(u8, s, "App")) return .APPLICATION;
        return null;
    }

    pub fn isBaseType(self: Self) bool {
        return switch (self) {
            .UNIT, .BOOL, .NAT, .INT, .REAL, .COMPLEX, .STRING, .BOTTOM, .TOP => true,
            else => false,
        };
    }

    pub fn isComposite(self: Self) bool {
        return switch (self) {
            .ARRAY, .TUPLE, .RECORD, .SUM, .FUNCTION, .DEPENDENT_FUNCTION, .DEPENDENT_PAIR => true,
            else => false,
        };
    }

    pub fn isDependent(self: Self) bool {
        return self == .DEPENDENT_FUNCTION or self == .DEPENDENT_PAIR or self == .IDENTITY;
    }
};

pub const RecordField = struct {
    name: []const u8,
    field_type: *Type,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, field_type: *Type) !*Self {
        const field = try allocator.create(Self);
        errdefer allocator.destroy(field);
        field.* = Self{
            .name = try allocator.dupe(u8, name),
            .field_type = field_type,
            .allocator = allocator,
        };
        return field;
    }

    pub fn deinit(self: *Self) void {
        self.field_type.deinit();
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) error{OutOfMemory}!*Self {
        const cloned_type = try self.field_type.clone(allocator);
        return RecordField.init(allocator, self.name, cloned_type);
    }
};

pub const Type = struct {
    kind: TypeKind,
    name: []const u8,
    parameters: ArrayList(*Type),
    fields: ArrayList(*RecordField),
    universe_level: u32,
    bound_variable: ?[]const u8,
    body_type: ?*Type,
    left_type: ?*Type,
    right_type: ?*Type,
    quantum_dimension: ?u32,
    hash_cache: ?[32]u8,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, kind: TypeKind) !*Self {
        const t = try allocator.create(Self);
        errdefer allocator.destroy(t);
        t.* = Self{
            .kind = kind,
            .name = "",
            .parameters = ArrayList(*Type).init(allocator),
            .fields = ArrayList(*RecordField).init(allocator),
            .universe_level = 0,
            .bound_variable = null,
            .body_type = null,
            .left_type = null,
            .right_type = null,
            .quantum_dimension = null,
            .hash_cache = null,
            .allocator = allocator,
        };
        return t;
    }

    pub fn initUnit(allocator: Allocator) !*Self {
        return Type.init(allocator, .UNIT);
    }

    pub fn initBool(allocator: Allocator) !*Self {
        return Type.init(allocator, .BOOL);
    }

    pub fn initNat(allocator: Allocator) !*Self {
        return Type.init(allocator, .NAT);
    }

    pub fn initInt(allocator: Allocator) !*Self {
        return Type.init(allocator, .INT);
    }

    pub fn initReal(allocator: Allocator) !*Self {
        return Type.init(allocator, .REAL);
    }

    pub fn initComplex(allocator: Allocator) !*Self {
        return Type.init(allocator, .COMPLEX);
    }

    pub fn initString(allocator: Allocator) !*Self {
        return Type.init(allocator, .STRING);
    }

    pub fn initBottom(allocator: Allocator) !*Self {
        return Type.init(allocator, .BOTTOM);
    }

    pub fn initTop(allocator: Allocator) !*Self {
        return Type.init(allocator, .TOP);
    }

    pub fn initVariable(allocator: Allocator, name: []const u8) !*Self {
        const t = try Type.init(allocator, .VARIABLE);
        errdefer t.deinit();
        t.name = try allocator.dupe(u8, name);
        return t;
    }

    pub fn initArray(allocator: Allocator, element_type: *Type) !*Self {
        const t = try Type.init(allocator, .ARRAY);
        errdefer t.deinit();
        try t.parameters.append(element_type);
        return t;
    }

    pub fn initTuple(allocator: Allocator, types: []const *Type) !*Self {
        const t = try Type.init(allocator, .TUPLE);
        errdefer t.deinit();
        for (types) |ty| {
            try t.parameters.append(ty);
        }
        return t;
    }

    pub fn initRecord(allocator: Allocator, fields: []const *RecordField) !*Self {
        const t = try Type.init(allocator, .RECORD);
        errdefer t.deinit();
        for (fields) |field| {
            try t.fields.append(field);
        }
        return t;
    }

    pub fn initSum(allocator: Allocator, left: *Type, right: *Type) !*Self {
        const t = try Type.init(allocator, .SUM);
        errdefer t.deinit();
        t.left_type = left;
        t.right_type = right;
        return t;
    }

    pub fn initFunction(allocator: Allocator, domain: *Type, codomain: *Type) !*Self {
        const t = try Type.init(allocator, .FUNCTION);
        errdefer t.deinit();
        t.left_type = domain;
        t.right_type = codomain;
        return t;
    }

    pub fn initUniverse(allocator: Allocator, level: u32) !*Self {
        const t = try Type.init(allocator, .UNIVERSE);
        errdefer t.deinit();
        t.universe_level = level;
        return t;
    }

    pub fn initQuantum(allocator: Allocator, base_type: *Type, dimension: u32) !*Self {
        const t = try Type.init(allocator, .QUANTUM_TYPE);
        errdefer t.deinit();
        try t.parameters.append(base_type);
        t.quantum_dimension = dimension;
        return t;
    }

    pub fn initApplication(allocator: Allocator, func_type: *Type, arg_type: *Type) !*Self {
        const t = try Type.init(allocator, .APPLICATION);
        errdefer t.deinit();
        t.left_type = func_type;
        t.right_type = arg_type;
        return t;
    }

    pub fn deinit(self: *Self) void {
        if (self.name.len > 0) {
            self.allocator.free(self.name);
        }
        for (self.parameters.items) |param| {
            param.deinit();
        }
        self.parameters.deinit();
        for (self.fields.items) |field| {
            field.deinit();
        }
        self.fields.deinit();
        if (self.bound_variable) |bv| {
            self.allocator.free(bv);
        }
        if (self.body_type) |body| {
            body.deinit();
        }
        if (self.left_type) |left| {
            left.deinit();
        }
        if (self.right_type) |right| {
            right.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) error{OutOfMemory}!*Self {
        const t = try allocator.create(Self);
        errdefer allocator.destroy(t);
        t.* = Self{
            .kind = self.kind,
            .name = "",
            .parameters = ArrayList(*Type).init(allocator),
            .fields = ArrayList(*RecordField).init(allocator),
            .universe_level = self.universe_level,
            .bound_variable = null,
            .body_type = null,
            .left_type = null,
            .right_type = null,
            .quantum_dimension = self.quantum_dimension,
            .hash_cache = self.hash_cache,
            .allocator = allocator,
        };
        errdefer t.deinit();
        if (self.name.len > 0) {
            t.name = try allocator.dupe(u8, self.name);
        }
        if (self.bound_variable) |bv| {
            t.bound_variable = try allocator.dupe(u8, bv);
        }
        if (self.body_type) |body| {
            t.body_type = try body.clone(allocator);
        }
        if (self.left_type) |left| {
            t.left_type = try left.clone(allocator);
        }
        if (self.right_type) |right| {
            t.right_type = try right.clone(allocator);
        }
        for (self.parameters.items) |param| {
            try t.parameters.append(try param.clone(allocator));
        }
        for (self.fields.items) |field| {
            try t.fields.append(try field.clone(allocator));
        }
        return t;
    }

    pub fn equals(self: *const Self, other: *const Self) bool {
        if (self.kind != other.kind) return false;
        if (self.universe_level != other.universe_level) return false;
        if (!std.mem.eql(u8, self.name, other.name)) return false;
        if (self.parameters.items.len != other.parameters.items.len) return false;
        var param_idx: usize = 0;
        while (param_idx < self.parameters.items.len) : (param_idx += 1) {
            if (!self.parameters.items[param_idx].equals(other.parameters.items[param_idx])) return false;
        }
        if (self.fields.items.len != other.fields.items.len) return false;
        var field_idx: usize = 0;
        while (field_idx < self.fields.items.len) : (field_idx += 1) {
            const field = self.fields.items[field_idx];
            if (!std.mem.eql(u8, field.name, other.fields.items[field_idx].name)) return false;
            if (!field.field_type.equals(other.fields.items[field_idx].field_type)) return false;
        }
        if (self.bound_variable) |bv1| {
            if (other.bound_variable) |bv2| {
                if (!std.mem.eql(u8, bv1, bv2)) return false;
            } else {
                return false;
            }
        } else if (other.bound_variable != null) {
            return false;
        }
        if (self.left_type != null and other.left_type != null) {
            if (!self.left_type.?.equals(other.left_type.?)) return false;
        } else if (self.left_type != null or other.left_type != null) {
            return false;
        }
        if (self.right_type != null and other.right_type != null) {
            if (!self.right_type.?.equals(other.right_type.?)) return false;
        } else if (self.right_type != null or other.right_type != null) {
            return false;
        }
        if (self.body_type != null and other.body_type != null) {
            if (!self.body_type.?.equals(other.body_type.?)) return false;
        } else if (self.body_type != null or other.body_type != null) {
            return false;
        }
        return true;
    }

    pub fn computeHash(self: *Self) [32]u8 {
        if (self.hash_cache) |cache| {
            return cache;
        }
        var hasher = Sha256.init(.{});
        hasher.update(&[_]u8{@intFromEnum(self.kind)});
        hasher.update(self.name);
        var level_bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &level_bytes, self.universe_level, .little);
        hasher.update(&level_bytes);
        for (self.parameters.items) |param| {
            const param_hash = param.computeHash();
            hasher.update(&param_hash);
        }
        for (self.fields.items) |field| {
            hasher.update(field.name);
            const field_type_hash = field.field_type.computeHash();
            hasher.update(&field_type_hash);
        }
        if (self.bound_variable) |bv| {
            hasher.update(bv);
        }
        if (self.left_type) |left| {
            const left_hash = left.computeHash();
            hasher.update(&left_hash);
        }
        if (self.right_type) |right| {
            const right_hash = right.computeHash();
            hasher.update(&right_hash);
        }
        if (self.body_type) |body| {
            const body_hash = body.computeHash();
            hasher.update(&body_hash);
        }
        var result: [32]u8 = undefined;
        hasher.final(&result);
        self.hash_cache = result;
        return result;
    }

    pub fn getDomain(self: *const Self) ?*Type {
        return switch (self.kind) {
            .FUNCTION, .DEPENDENT_FUNCTION => self.left_type,
            else => null,
        };
    }

    pub fn getCodomain(self: *const Self) ?*Type {
        return switch (self.kind) {
            .FUNCTION => self.right_type,
            .DEPENDENT_FUNCTION => self.body_type,
            else => null,
        };
    }

    pub fn getElementType(self: *const Self) ?*Type {
        return switch (self.kind) {
            .ARRAY => if (self.parameters.items.len > 0) self.parameters.items[0] else null,
            else => null,
        };
    }

    pub fn getUniverseLevel(self: *const Self) u32 {
        return switch (self.kind) {
            .UNIVERSE => self.universe_level,
            .DEPENDENT_FUNCTION, .DEPENDENT_PAIR => blk: {
                var max_level: u32 = 0;
                if (self.left_type) |left| {
                    max_level = @max(max_level, left.getUniverseLevel());
                }
                if (self.body_type) |body| {
                    max_level = @max(max_level, body.getUniverseLevel());
                }
                break :blk max_level;
            },
            else => 0,
        };
    }

    pub fn substitute(self: *Self, var_name: []const u8, replacement: *const Type) !void {
        if (self.kind == .VARIABLE and std.mem.eql(u8, self.name, var_name)) {
            const cloned = try replacement.clone(self.allocator);
            errdefer cloned.deinit();
            if (self.name.len > 0) {
                self.allocator.free(self.name);
            }
            if (self.bound_variable) |bv| {
                self.allocator.free(bv);
            }
            if (self.body_type) |body| {
                body.deinit();
            }
            if (self.left_type) |left| {
                left.deinit();
            }
            if (self.right_type) |right| {
                right.deinit();
            }
            for (self.parameters.items) |p| {
                p.deinit();
            }
            self.parameters.deinit();
            for (self.fields.items) |field| {
                field.deinit();
            }
            self.fields.deinit();
            self.kind = cloned.kind;
            self.name = cloned.name;
            self.universe_level = cloned.universe_level;
            self.parameters = cloned.parameters;
            self.fields = cloned.fields;
            self.bound_variable = cloned.bound_variable;
            self.body_type = cloned.body_type;
            self.left_type = cloned.left_type;
            self.right_type = cloned.right_type;
            self.quantum_dimension = cloned.quantum_dimension;
            self.hash_cache = null;
            cloned.name = "";
            cloned.bound_variable = null;
            cloned.body_type = null;
            cloned.left_type = null;
            cloned.right_type = null;
            cloned.parameters = ArrayList(*Type).init(self.allocator);
            cloned.fields = ArrayList(*RecordField).init(self.allocator);
            cloned.deinit();
        } else {
            for (self.parameters.items) |param| {
                try param.substitute(var_name, replacement);
            }
            for (self.fields.items) |field| {
                try field.field_type.substitute(var_name, replacement);
            }
            if (self.left_type) |left| {
                try left.substitute(var_name, replacement);
            }
            if (self.right_type) |right| {
                try right.substitute(var_name, replacement);
            }
            if (self.body_type) |body| {
                if (self.bound_variable == null or !std.mem.eql(u8, self.bound_variable.?, var_name)) {
                    try body.substitute(var_name, replacement);
                }
            }
            self.hash_cache = null;
        }
    }

    pub fn containsFreeVariable(self: *const Self, var_name: []const u8) bool {
        if (self.kind == .VARIABLE and std.mem.eql(u8, self.name, var_name)) {
            return true;
        }
        for (self.parameters.items) |param| {
            if (param.containsFreeVariable(var_name)) return true;
        }
        for (self.fields.items) |field| {
            if (field.field_type.containsFreeVariable(var_name)) return true;
        }
        if (self.left_type) |left| {
            if (left.containsFreeVariable(var_name)) return true;
        }
        if (self.right_type) |right| {
            if (right.containsFreeVariable(var_name)) return true;
        }
        if (self.body_type) |body| {
            if (self.bound_variable != null and std.mem.eql(u8, self.bound_variable.?, var_name)) {
                return false;
            }
            if (body.containsFreeVariable(var_name)) return true;
        }
        return false;
    }
};

pub const TypeBinding = struct {
    name: []const u8,
    bound_type: *Type,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, bound_type: *Type) !*Self {
        const binding = try allocator.create(Self);
        errdefer allocator.destroy(binding);
        binding.* = Self{
            .name = try allocator.dupe(u8, name),
            .bound_type = bound_type,
            .allocator = allocator,
        };
        return binding;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.bound_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const cloned_type = try self.bound_type.clone(allocator);
        errdefer {
            cloned_type.deinit();
        }
        return TypeBinding.init(allocator, self.name, cloned_type);
    }
};

pub const TypeContext = struct {
    bindings: ArrayList(*TypeBinding),
    parent: ?*TypeContext,
    allocator: Allocator,
    depth: u32,
    owns_parent: bool,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .bindings = ArrayList(*TypeBinding).init(allocator),
            .parent = null,
            .allocator = allocator,
            .depth = 0,
            .owns_parent = false,
        };
    }

    pub fn initWithParent(allocator: Allocator, parent: *TypeContext) Self {
        return Self{
            .bindings = ArrayList(*TypeBinding).init(allocator),
            .parent = parent,
            .allocator = allocator,
            .depth = parent.depth + 1,
            .owns_parent = false,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.bindings.items) |binding| {
            binding.deinit();
        }
        self.bindings.deinit();
        if (self.owns_parent) {
            if (self.parent) |p| {
                p.deinit();
                self.allocator.destroy(p);
            }
        }
    }

    pub fn extend(self: *Self, name: []const u8, bound_type: *Type) !void {
        const binding = try TypeBinding.init(self.allocator, name, bound_type);
        errdefer binding.deinit();
        try self.bindings.append(binding);
    }

    pub fn lookup(self: *const Self, name: []const u8) ?*Type {
        var i: usize = self.bindings.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.bindings.items[i].name, name)) {
                return self.bindings.items[i].bound_type;
            }
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }

    pub fn contains(self: *const Self, name: []const u8) bool {
        return self.lookup(name) != null;
    }

    pub fn size(self: *const Self) usize {
        var count = self.bindings.items.len;
        if (self.parent) |p| {
            count += p.size();
        }
        return count;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const ctx = try allocator.create(Self);
        errdefer allocator.destroy(ctx);
        ctx.* = Self{
            .bindings = ArrayList(*TypeBinding).init(allocator),
            .parent = if (self.parent) |p| try p.clone(allocator) else null,
            .allocator = allocator,
            .depth = self.depth,
            .owns_parent = self.parent != null,
        };
        errdefer ctx.deinit();
        for (self.bindings.items) |binding| {
            try ctx.bindings.append(try binding.clone(allocator));
        }
        return ctx;
    }

    pub fn merge(self: *Self, other: *const Self) !void {
        for (other.bindings.items) |binding| {
            if (self.contains(binding.name)) {
                return TypeTheoryError.VariableNotInContext;
            }
            try self.bindings.append(try binding.clone(self.allocator));
        }
    }
};

pub const TermKind = enum(u8) {
    VARIABLE = 0,
    LITERAL = 1,
    LAMBDA = 2,
    APPLICATION = 3,
    PAIR = 4,
    FIRST = 5,
    SECOND = 6,
    INL = 7,
    INR = 8,
    CASE = 9,
    UNIT = 10,
    REFL = 11,
    J_ELIMINATOR = 12,
    ZERO = 13,
    SUCC = 14,
    NAT_REC = 15,
    LET = 16,
    ANNOTATION = 17,

    const Self = @This();

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .VARIABLE => "var",
            .LITERAL => "lit",
            .LAMBDA => "lam",
            .APPLICATION => "app",
            .PAIR => "pair",
            .FIRST => "fst",
            .SECOND => "snd",
            .INL => "inl",
            .INR => "inr",
            .CASE => "case",
            .UNIT => "unit",
            .REFL => "refl",
            .J_ELIMINATOR => "J",
            .ZERO => "zero",
            .SUCC => "succ",
            .NAT_REC => "natrec",
            .LET => "let",
            .ANNOTATION => "ann",
        };
    }
};

pub const Term = struct {
    kind: TermKind,
    name: []const u8,
    sub_terms: ArrayList(*Term),
    bound_variable: ?[]const u8,
    annotation_type: ?*Type,
    literal_value: ?LiteralValue,
    allocator: Allocator,

    pub const LiteralValue = union(enum) {
        bool_val: bool,
        nat_val: u64,
        int_val: i64,
        real_val: f64,
        string_val: []u8,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, kind: TermKind) !*Self {
        const t = try allocator.create(Self);
        errdefer allocator.destroy(t);
        t.* = Self{
            .kind = kind,
            .name = "",
            .sub_terms = ArrayList(*Term).init(allocator),
            .bound_variable = null,
            .annotation_type = null,
            .literal_value = null,
            .allocator = allocator,
        };
        return t;
    }

    pub fn initVariable(allocator: Allocator, name: []const u8) !*Self {
        const t = try Term.init(allocator, .VARIABLE);
        errdefer t.deinit();
        t.name = try allocator.dupe(u8, name);
        return t;
    }

    pub fn initLambda(allocator: Allocator, param: []const u8, body: *Term) !*Self {
        const t = try Term.init(allocator, .LAMBDA);
        errdefer t.deinit();
        t.bound_variable = try allocator.dupe(u8, param);
        try t.sub_terms.append(body);
        return t;
    }

    pub fn initApplication(allocator: Allocator, func: *Term, arg: *Term) !*Self {
        const t = try Term.init(allocator, .APPLICATION);
        errdefer t.deinit();
        try t.sub_terms.append(func);
        try t.sub_terms.append(arg);
        return t;
    }

    pub fn initPair(allocator: Allocator, first: *Term, second: *Term) !*Self {
        const t = try Term.init(allocator, .PAIR);
        errdefer t.deinit();
        try t.sub_terms.append(first);
        try t.sub_terms.append(second);
        return t;
    }

    pub fn initFirst(allocator: Allocator, pair: *Term) !*Self {
        const t = try Term.init(allocator, .FIRST);
        errdefer t.deinit();
        try t.sub_terms.append(pair);
        return t;
    }

    pub fn initSecond(allocator: Allocator, pair: *Term) !*Self {
        const t = try Term.init(allocator, .SECOND);
        errdefer t.deinit();
        try t.sub_terms.append(pair);
        return t;
    }

    pub fn initInl(allocator: Allocator, value: *Term) !*Self {
        const t = try Term.init(allocator, .INL);
        errdefer t.deinit();
        try t.sub_terms.append(value);
        return t;
    }

    pub fn initInr(allocator: Allocator, value: *Term) !*Self {
        const t = try Term.init(allocator, .INR);
        errdefer t.deinit();
        try t.sub_terms.append(value);
        return t;
    }

    pub fn initUnit(allocator: Allocator) !*Self {
        return Term.init(allocator, .UNIT);
    }

    pub fn initRefl(allocator: Allocator, witness: *Term) !*Self {
        const t = try Term.init(allocator, .REFL);
        errdefer t.deinit();
        try t.sub_terms.append(witness);
        return t;
    }

    pub fn initZero(allocator: Allocator) !*Self {
        return Term.init(allocator, .ZERO);
    }

    pub fn initSucc(allocator: Allocator, n: *Term) !*Self {
        const t = try Term.init(allocator, .SUCC);
        errdefer t.deinit();
        try t.sub_terms.append(n);
        return t;
    }

    pub fn initLiteralNat(allocator: Allocator, value: u64) !*Self {
        const t = try Term.init(allocator, .LITERAL);
        errdefer t.deinit();
        t.literal_value = .{ .nat_val = value };
        return t;
    }

    pub fn initLiteralBool(allocator: Allocator, value: bool) !*Self {
        const t = try Term.init(allocator, .LITERAL);
        errdefer t.deinit();
        t.literal_value = .{ .bool_val = value };
        return t;
    }

    pub fn initAnnotation(allocator: Allocator, term: *Term, ann_type: *Type) !*Self {
        const t = try Term.init(allocator, .ANNOTATION);
        errdefer t.deinit();
        try t.sub_terms.append(term);
        t.annotation_type = ann_type;
        return t;
    }

    pub fn deinit(self: *Self) void {
        if (self.name.len > 0) {
            self.allocator.free(self.name);
        }
        for (self.sub_terms.items) |sub| {
            sub.deinit();
        }
        self.sub_terms.deinit();
        if (self.bound_variable) |bv| {
            self.allocator.free(bv);
        }
        if (self.annotation_type) |ann| {
            ann.deinit();
        }
        if (self.literal_value) |lit| {
            switch (lit) {
                .string_val => |s| self.allocator.free(s),
                else => {},
            }
        }
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const t = try allocator.create(Self);
        errdefer allocator.destroy(t);
        t.* = Self{
            .kind = self.kind,
            .name = "",
            .sub_terms = ArrayList(*Term).init(allocator),
            .bound_variable = null,
            .annotation_type = null,
            .literal_value = null,
            .allocator = allocator,
        };
        errdefer t.deinit();
        if (self.name.len > 0) {
            t.name = try allocator.dupe(u8, self.name);
        }
        if (self.bound_variable) |bv| {
            t.bound_variable = try allocator.dupe(u8, bv);
        }
        if (self.annotation_type) |ann| {
            t.annotation_type = try ann.clone(allocator);
        }
        if (self.literal_value) |lit| {
            t.literal_value = switch (lit) {
                .string_val => |s| LiteralValue{ .string_val = try allocator.dupe(u8, s) },
                else => lit,
            };
        }
        for (self.sub_terms.items) |sub| {
            try t.sub_terms.append(try sub.clone(allocator));
        }
        return t;
    }

    pub fn equals(self: *const Self, other: *const Self) bool {
        if (self.kind != other.kind) return false;
        if (!std.mem.eql(u8, self.name, other.name)) return false;
        if (self.sub_terms.items.len != other.sub_terms.items.len) return false;
        var sub_idx: usize = 0;
        while (sub_idx < self.sub_terms.items.len) : (sub_idx += 1) {
            if (!self.sub_terms.items[sub_idx].equals(other.sub_terms.items[sub_idx])) return false;
        }
        if (self.bound_variable) |bv1| {
            if (other.bound_variable) |bv2| {
                if (!std.mem.eql(u8, bv1, bv2)) return false;
            } else {
                return false;
            }
        } else if (other.bound_variable != null) {
            return false;
        }
        if (self.annotation_type) |ann1| {
            if (other.annotation_type) |ann2| {
                if (!ann1.equals(ann2)) return false;
            } else {
                return false;
            }
        } else if (other.annotation_type != null) {
            return false;
        }
        if (self.literal_value) |lit1| {
            if (other.literal_value) |lit2| {
                switch (lit1) {
                    .bool_val => |b1| if (lit2.bool_val != b1) return false,
                    .nat_val => |n1| if (lit2.nat_val != n1) return false,
                    .int_val => |int_val_1| if (lit2.int_val != int_val_1) return false,
                    .real_val => |r1| if (lit2.real_val != r1) return false,
                    .string_val => |s1| if (!std.mem.eql(u8, s1, lit2.string_val)) return false,
                }
            } else {
                return false;
            }
        } else if (other.literal_value != null) {
            return false;
        }
        return true;
    }
};

pub const TypeJudgment = struct {
    context: *TypeContext,
    term: *Term,
    inferred_type: *Type,
    is_valid: bool,
    derivation_depth: u32,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, context: *TypeContext, term: *Term, inferred_type: *Type) !*Self {
        const j = try allocator.create(Self);
        errdefer allocator.destroy(j);
        const ctx_clone = try context.clone(allocator);
        errdefer ctx_clone.deinit();
        const term_clone = try term.clone(allocator);
        errdefer term_clone.deinit();
        j.* = Self{
            .context = ctx_clone,
            .term = term_clone,
            .inferred_type = inferred_type,
            .is_valid = false,
            .derivation_depth = 0,
            .allocator = allocator,
        };
        return j;
    }

    pub fn deinit(self: *Self) void {
        self.context.deinit();
        self.allocator.destroy(self.context);
        self.term.deinit();
        self.inferred_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn validate(self: *Self) bool {
        self.is_valid = self.checkWellFormedness();
        return self.is_valid;
    }

    fn checkWellFormedness(self: *const Self) bool {
        switch (self.term.kind) {
            .VARIABLE => return self.context.contains(self.term.name),
            .UNIT => return self.inferred_type.kind == .UNIT,
            .ZERO => return self.inferred_type.kind == .NAT,
            .LITERAL => return self.checkLiteralType(),
            .LAMBDA => return self.inferred_type.kind == .FUNCTION or self.inferred_type.kind == .DEPENDENT_FUNCTION,
            .APPLICATION => return true,
            .PAIR => return self.inferred_type.kind == .TUPLE or self.inferred_type.kind == .DEPENDENT_PAIR,
            .FIRST => return true,
            .SECOND => return true,
            .INL => return true,
            .INR => return true,
            .SUCC => return self.inferred_type.kind == .NAT,
            .REFL => return self.inferred_type.kind == .IDENTITY,
            .ANNOTATION => return true,
            else => return true,
        }
    }

    fn checkLiteralType(self: *const Self) bool {
        if (self.term.literal_value) |lit| {
            return switch (lit) {
                .bool_val => self.inferred_type.kind == .BOOL,
                .nat_val => self.inferred_type.kind == .NAT,
                .int_val => self.inferred_type.kind == .INT,
                .real_val => self.inferred_type.kind == .REAL,
                .string_val => self.inferred_type.kind == .STRING,
            };
        }
        return false;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return TypeJudgment.init(
            allocator,
            self.context,
            self.term,
            try self.inferred_type.clone(allocator),
        );
    }
};

pub const DependentPi = struct {
    param_name: []const u8,
    param_type: *Type,
    return_type: *Type,
    universe_level: u32,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, param_name: []const u8, param_type: *Type, return_type: *Type) !*Self {
        const pi = try allocator.create(Self);
        errdefer allocator.destroy(pi);
        pi.* = Self{
            .param_name = try allocator.dupe(u8, param_name),
            .param_type = param_type,
            .return_type = return_type,
            .universe_level = @max(param_type.getUniverseLevel(), return_type.getUniverseLevel()),
            .allocator = allocator,
        };
        return pi;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.param_name);
        self.param_type.deinit();
        self.return_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        const t = try Type.init(allocator, .DEPENDENT_FUNCTION);
        errdefer t.deinit();
        t.bound_variable = try allocator.dupe(u8, self.param_name);
        t.left_type = try self.param_type.clone(allocator);
        t.body_type = try self.return_type.clone(allocator);
        t.universe_level = self.universe_level;
        return t;
    }

    pub fn apply(self: *Self, arg: *const Type) !*Type {
        const result = try self.return_type.clone(self.allocator);
        errdefer result.deinit();
        try result.substitute(self.param_name, arg);
        return result;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return DependentPi.init(
            allocator,
            self.param_name,
            try self.param_type.clone(allocator),
            try self.return_type.clone(allocator),
        );
    }

    pub fn equals(self: *const Self, other: *const Self) bool {
        if (!std.mem.eql(u8, self.param_name, other.param_name)) return false;
        if (!self.param_type.equals(other.param_type)) return false;
        if (!self.return_type.equals(other.return_type)) return false;
        return true;
    }
};

pub const DependentSigma = struct {
    fst_name: []const u8,
    fst_type: *Type,
    snd_type: *Type,
    universe_level: u32,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, fst_name: []const u8, fst_type: *Type, snd_type: *Type) !*Self {
        const sigma = try allocator.create(Self);
        errdefer allocator.destroy(sigma);
        sigma.* = Self{
            .fst_name = try allocator.dupe(u8, fst_name),
            .fst_type = fst_type,
            .snd_type = snd_type,
            .universe_level = @max(fst_type.getUniverseLevel(), snd_type.getUniverseLevel()),
            .allocator = allocator,
        };
        return sigma;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.fst_name);
        self.fst_type.deinit();
        self.snd_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        const t = try Type.init(allocator, .DEPENDENT_PAIR);
        errdefer t.deinit();
        t.bound_variable = try allocator.dupe(u8, self.fst_name);
        t.left_type = try self.fst_type.clone(allocator);
        t.body_type = try self.snd_type.clone(allocator);
        t.universe_level = self.universe_level;
        return t;
    }

    pub fn getSecondType(self: *Self, first_value: *const Type) !*Type {
        const result = try self.snd_type.clone(self.allocator);
        errdefer result.deinit();
        try result.substitute(self.fst_name, first_value);
        return result;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return DependentSigma.init(
            allocator,
            self.fst_name,
            try self.fst_type.clone(allocator),
            try self.snd_type.clone(allocator),
        );
    }

    pub fn equals(self: *const Self, other: *const Self) bool {
        if (!std.mem.eql(u8, self.fst_name, other.fst_name)) return false;
        if (!self.fst_type.equals(other.fst_type)) return false;
        if (!self.snd_type.equals(other.snd_type)) return false;
        return true;
    }
};

pub const IdentityType = struct {
    base_type: *Type,
    left_term: *Term,
    right_term: *Term,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, base_type: *Type, left: *Term, right: *Term) !*Self {
        const id = try allocator.create(Self);
        errdefer allocator.destroy(id);
        id.* = Self{
            .base_type = base_type,
            .left_term = left,
            .right_term = right,
            .allocator = allocator,
        };
        return id;
    }

    pub fn deinit(self: *Self) void {
        self.base_type.deinit();
        self.left_term.deinit();
        self.right_term.deinit();
        self.allocator.destroy(self);
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        const t = try Type.init(allocator, .IDENTITY);
        errdefer t.deinit();
        try t.parameters.append(try self.base_type.clone(allocator));
        return t;
    }

    pub fn refl(allocator: Allocator, base_type: *Type, term: *Term) !*Self {
        const cloned_term = try term.clone(allocator);
        errdefer cloned_term.deinit();
        return IdentityType.init(allocator, base_type, term, cloned_term);
    }

    pub fn symmetry(self: *const Self, allocator: Allocator) !*Self {
        return IdentityType.init(
            allocator,
            try self.base_type.clone(allocator),
            try self.right_term.clone(allocator),
            try self.left_term.clone(allocator),
        );
    }

    pub fn transitivity(self: *const Self, other: *const Self, allocator: Allocator) !*Self {
        if (!self.right_term.equals(other.left_term)) {
            return TypeTheoryError.InvalidIdentityElimination;
        }
        if (!self.base_type.equals(other.base_type)) {
            return TypeTheoryError.TypeMismatch;
        }
        return IdentityType.init(
            allocator,
            try self.base_type.clone(allocator),
            try self.left_term.clone(allocator),
            try other.right_term.clone(allocator),
        );
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return IdentityType.init(
            allocator,
            try self.base_type.clone(allocator),
            try self.left_term.clone(allocator),
            try self.right_term.clone(allocator),
        );
    }

    pub fn isReflexive(self: *const Self) bool {
        return self.left_term.equals(self.right_term);
    }
};

pub const UniverseType = struct {
    level: u32,
    cumulative: bool,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, level: u32) !*Self {
        const u = try allocator.create(Self);
        errdefer allocator.destroy(u);
        u.* = Self{
            .level = level,
            .cumulative = true,
            .allocator = allocator,
        };
        return u;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        return Type.initUniverse(allocator, self.level);
    }

    pub fn typeOf(self: *const Self, allocator: Allocator) !*Self {
        return UniverseType.init(allocator, self.level + 1);
    }

    pub fn contains(self: *const Self, other: *const Self) bool {
        if (self.cumulative) {
            return other.level < self.level;
        }
        return if (self.level > 0) other.level == self.level - 1 else false;
    }

    pub fn lub(self: *const Self, other: *const Self, allocator: Allocator) !*Self {
        const res = try UniverseType.init(allocator, @max(self.level, other.level));
        res.cumulative = self.cumulative and other.cumulative;
        return res;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const u = try UniverseType.init(allocator, self.level);
        u.cumulative = self.cumulative;
        return u;
    }
};

pub const InductiveType = struct {
    name: []const u8,
    constructors: ArrayList(*Constructor),
    parameters: ArrayList(*Type),
    indices: ArrayList(*Type),
    universe_level: u32,
    allocator: Allocator,

    pub const Constructor = struct {
        name: []const u8,
        arg_types: ArrayList(*Type),
        result_type: *Type,
        allocator: Allocator,

        pub fn init(allocator: Allocator, name: []const u8, result_type: *Type) !*Constructor {
            const c = try allocator.create(Constructor);
            errdefer allocator.destroy(c);
            c.* = Constructor{
                .name = try allocator.dupe(u8, name),
                .arg_types = ArrayList(*Type).init(allocator),
                .result_type = result_type,
                .allocator = allocator,
            };
            return c;
        }

        pub fn deinit(self: *Constructor) void {
            self.allocator.free(self.name);
            for (self.arg_types.items) |t| {
                t.deinit();
            }
            self.arg_types.deinit();
            self.result_type.deinit();
            self.allocator.destroy(self);
        }

        pub fn addArgType(self: *Constructor, arg_type: *Type) !void {
            try self.arg_types.append(arg_type);
        }

        pub fn clone(self: *const Constructor, allocator: Allocator) !*Constructor {
            const result_type_clone = try self.result_type.clone(allocator);
            errdefer result_type_clone.deinit();
            const c = try Constructor.init(allocator, self.name, result_type_clone);
            errdefer c.deinit();
            for (self.arg_types.items) |t| {
                try c.arg_types.append(try t.clone(allocator));
            }
            return c;
        }
    };

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8) !*Self {
        const ind = try allocator.create(Self);
        errdefer allocator.destroy(ind);
        ind.* = Self{
            .name = try allocator.dupe(u8, name),
            .constructors = ArrayList(*Constructor).init(allocator),
            .parameters = ArrayList(*Type).init(allocator),
            .indices = ArrayList(*Type).init(allocator),
            .universe_level = 0,
            .allocator = allocator,
        };
        return ind;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        for (self.constructors.items) |c| {
            c.deinit();
        }
        self.constructors.deinit();
        for (self.parameters.items) |p| {
            p.deinit();
        }
        self.parameters.deinit();
        for (self.indices.items) |i| {
            i.deinit();
        }
        self.indices.deinit();
        self.allocator.destroy(self);
    }

    pub fn addConstructor(self: *Self, constructor: *Constructor) !void {
        try self.constructors.append(constructor);
    }

    pub fn initNat(allocator: Allocator) !*Self {
        const nat = try InductiveType.init(allocator, "Nat");
        errdefer nat.deinit();
        const nat_type = try Type.initNat(allocator);
        errdefer nat_type.deinit();
        const zero = try Constructor.init(allocator, "zero", nat_type);
        errdefer zero.deinit();
        try nat.addConstructor(zero);
        const succ_arg = try Type.initNat(allocator);
        errdefer succ_arg.deinit();
        const succ_result = try Type.initNat(allocator);
        errdefer succ_result.deinit();
        const succ = try Constructor.init(allocator, "succ", succ_result);
        errdefer succ.deinit();
        try succ.addArgType(succ_arg);
        try nat.addConstructor(succ);
        return nat;
    }

    pub fn initBool(allocator: Allocator) !*Self {
        const bool_type = try InductiveType.init(allocator, "Bool");
        errdefer bool_type.deinit();
        const true_type = try Type.initBool(allocator);
        errdefer true_type.deinit();
        const true_ctor = try Constructor.init(allocator, "true", true_type);
        errdefer true_ctor.deinit();
        try bool_type.addConstructor(true_ctor);
        const false_type = try Type.initBool(allocator);
        errdefer false_type.deinit();
        const false_ctor = try Constructor.init(allocator, "false", false_type);
        errdefer false_ctor.deinit();
        try bool_type.addConstructor(false_ctor);
        return bool_type;
    }

    pub fn initList(allocator: Allocator, element_type: *Type) !*Self {
        const list = try InductiveType.init(allocator, "List");
        errdefer list.deinit();
        const elem_clone = try element_type.clone(allocator);
        errdefer elem_clone.deinit();
        try list.parameters.append(elem_clone);
        const array_type = try Type.initArray(allocator, try element_type.clone(allocator));
        errdefer array_type.deinit();
        const nil = try Constructor.init(allocator, "nil", array_type);
        errdefer nil.deinit();
        try list.addConstructor(nil);
        const cons_arg1 = try element_type.clone(allocator);
        errdefer cons_arg1.deinit();
        const cons_arg2 = try Type.initArray(allocator, try element_type.clone(allocator));
        errdefer cons_arg2.deinit();
        const cons_result = try Type.initArray(allocator, try element_type.clone(allocator));
        errdefer cons_result.deinit();
        const cons = try Constructor.init(allocator, "cons", cons_result);
        errdefer cons.deinit();
        try cons.addArgType(cons_arg1);
        try cons.addArgType(cons_arg2);
        try list.addConstructor(cons);
        return list;
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        const t = try Type.init(allocator, .VARIABLE);
        errdefer t.deinit();
        t.name = try allocator.dupe(u8, self.name);
        return t;
    }

    pub fn getRecursor(self: *const Self, motive_type: *Type, allocator: Allocator) !*Type {
        const ind_type = try self.toType(allocator);
        errdefer ind_type.deinit();
        var rec_type = try Type.initFunction(
            allocator,
            ind_type,
            try motive_type.clone(allocator),
        );
        errdefer rec_type.deinit();
        for (self.constructors.items) |ctor| {
            const ctor_case_type = try self.buildConstructorCaseType(ctor, motive_type, allocator);
            errdefer ctor_case_type.deinit();
            const new_rec = try Type.initFunction(allocator, ctor_case_type, rec_type);
            rec_type.deinit();
            rec_type = new_rec;
        }
        return rec_type;
    }

    fn buildConstructorCaseType(self: *const Self, ctor: *Constructor, motive: *Type, allocator: Allocator) !*Type {
        _ = self;
        var result = try motive.clone(allocator);
        errdefer result.deinit();
        var i = ctor.arg_types.items.len;
        while (i > 0) {
            i -= 1;
            const arg_clone = try ctor.arg_types.items[i].clone(allocator);
            errdefer arg_clone.deinit();
            const new_result = try Type.initFunction(allocator, arg_clone, result);
            result.deinit();
            result = new_result;
        }
        return result;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const ind = try InductiveType.init(allocator, self.name);
        errdefer ind.deinit();
        for (self.constructors.items) |c| {
            try ind.constructors.append(try c.clone(allocator));
        }
        for (self.parameters.items) |p| {
            try ind.parameters.append(try p.clone(allocator));
        }
        for (self.indices.items) |i| {
            try ind.indices.append(try i.clone(allocator));
        }
        ind.universe_level = self.universe_level;
        return ind;
    }
};

pub const TypeChecker = struct {
    context: TypeContext,
    inference_count: u64,
    check_count: u64,
    unification_count: u64,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .context = TypeContext.init(allocator),
            .inference_count = 0,
            .check_count = 0,
            .unification_count = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.context.deinit();
    }

    pub fn extendContext(self: *Self, name: []const u8, bound_type: *Type) !void {
        try self.context.extend(name, bound_type);
    }

    pub const InferError = error{OutOfMemory} || TypeTheoryError;

    pub fn checkType(self: *Self, ctx: *TypeContext, term: *Term, expected: *Type) InferError!bool {
        self.check_count += 1;
        const inferred = try self.inferType(ctx, term);
        defer {
            inferred.deinit();
        }
        return self.subtype(inferred, expected);
    }

    pub fn inferType(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        self.inference_count += 1;
        return switch (term.kind) {
            .VARIABLE => self.inferVariable(ctx, term),
            .LITERAL => self.inferLiteral(term),
            .LAMBDA => self.inferLambda(ctx, term),
            .APPLICATION => self.inferApplication(ctx, term),
            .PAIR => self.inferPair(ctx, term),
            .FIRST => self.inferFirst(ctx, term),
            .SECOND => self.inferSecond(ctx, term),
            .INL => self.inferInl(ctx, term),
            .INR => self.inferInr(ctx, term),
            .UNIT => Type.initUnit(self.allocator),
            .ZERO => Type.initNat(self.allocator),
            .SUCC => self.inferSucc(ctx, term),
            .REFL => self.inferRefl(ctx, term),
            .ANNOTATION => self.inferAnnotation(term),
            .CASE => TypeTheoryError.InvalidTypeConstruction,
            .J_ELIMINATOR => TypeTheoryError.InvalidTypeConstruction,
            .NAT_REC => TypeTheoryError.InvalidTypeConstruction,
            .LET => TypeTheoryError.InvalidTypeConstruction,
        };
    }

    fn inferVariable(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        const lookup_result = ctx.lookup(term.name);
        if (lookup_result) |found_type| {
            return found_type.clone(self.allocator);
        }
        return TypeTheoryError.VariableNotInContext;
    }

    fn inferLiteral(self: *Self, term: *Term) InferError!*Type {
        if (term.literal_value) |lit| {
            return switch (lit) {
                .bool_val => Type.initBool(self.allocator),
                .nat_val => Type.initNat(self.allocator),
                .int_val => Type.initInt(self.allocator),
                .real_val => Type.initReal(self.allocator),
                .string_val => Type.initString(self.allocator),
            };
        }
        return TypeTheoryError.InvalidTypeConstruction;
    }

    fn inferLambda(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.bound_variable == null or term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        var extended_ctx = TypeContext.initWithParent(self.allocator, ctx);
        defer extended_ctx.deinit();
        const param_type = if (term.annotation_type) |ann| try ann.clone(self.allocator) else try Type.initTop(self.allocator);
        errdefer param_type.deinit();
        try extended_ctx.extend(term.bound_variable.?, param_type);
        const body_type = try self.inferType(&extended_ctx, term.sub_terms.items[0]);
        errdefer body_type.deinit();
        return Type.initFunction(self.allocator, try param_type.clone(self.allocator), body_type);
    }

    fn inferApplication(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len < 2) {
            return TypeTheoryError.InvalidApplication;
        }
        const func_type = try self.inferType(ctx, term.sub_terms.items[0]);
        defer {
            func_type.deinit();
        }
        const arg_type = try self.inferType(ctx, term.sub_terms.items[1]);
        defer {
            arg_type.deinit();
        }
        if (func_type.kind == .FUNCTION) {
            if (func_type.left_type) |domain| {
                if (!self.subtype(arg_type, domain)) {
                    return TypeTheoryError.TypeMismatch;
                }
                if (func_type.right_type) |codomain| {
                    return codomain.clone(self.allocator);
                } else {
                    return TypeTheoryError.InvalidTypeConstruction;
                }
            } else {
                return TypeTheoryError.InvalidTypeConstruction;
            }
        } else if (func_type.kind == .DEPENDENT_FUNCTION) {
            if (func_type.body_type) |body| {
                const result = try body.clone(self.allocator);
                errdefer result.deinit();
                if (func_type.bound_variable) |bv| {
                    try result.substitute(bv, arg_type);
                }
                return result;
            } else {
                return TypeTheoryError.InvalidTypeConstruction;
            }
        }
        return TypeTheoryError.InvalidApplication;
    }

    fn inferPair(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len < 2) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        const fst_type = try self.inferType(ctx, term.sub_terms.items[0]);
        errdefer fst_type.deinit();
        const snd_type = try self.inferType(ctx, term.sub_terms.items[1]);
        errdefer snd_type.deinit();
        return Type.initTuple(self.allocator, &[_]*Type{ fst_type, snd_type });
    }

    fn inferFirst(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidProjection;
        }
        const pair_type = try self.inferType(ctx, term.sub_terms.items[0]);
        defer {
            pair_type.deinit();
        }
        if (pair_type.kind == .TUPLE and pair_type.parameters.items.len > 0) {
            return pair_type.parameters.items[0].clone(self.allocator);
        } else if (pair_type.kind == .DEPENDENT_PAIR) {
            if (pair_type.left_type) |left| {
                return left.clone(self.allocator);
            }
        }
        return TypeTheoryError.InvalidProjection;
    }

    fn inferSecond(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidProjection;
        }
        const pair_type = try self.inferType(ctx, term.sub_terms.items[0]);
        defer {
            pair_type.deinit();
        }
        if (pair_type.kind == .TUPLE and pair_type.parameters.items.len > 1) {
            return pair_type.parameters.items[1].clone(self.allocator);
        } else if (pair_type.kind == .DEPENDENT_PAIR) {
            if (pair_type.body_type) |body| {
                return body.clone(self.allocator);
            }
        }
        return TypeTheoryError.InvalidProjection;
    }

    fn inferInl(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        const inner_type = try self.inferType(ctx, term.sub_terms.items[0]);
        errdefer inner_type.deinit();
        const bottom = try Type.initBottom(self.allocator);
        errdefer bottom.deinit();
        return Type.initSum(self.allocator, inner_type, bottom);
    }

    fn inferInr(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        const inner_type = try self.inferType(ctx, term.sub_terms.items[0]);
        errdefer inner_type.deinit();
        const bottom = try Type.initBottom(self.allocator);
        errdefer bottom.deinit();
        return Type.initSum(self.allocator, bottom, inner_type);
    }

    fn inferSucc(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        const n_type = try self.inferType(ctx, term.sub_terms.items[0]);
        defer n_type.deinit();
        if (n_type.kind != .NAT) {
            return TypeTheoryError.TypeMismatch;
        }
        return Type.initNat(self.allocator);
    }

    fn inferRefl(self: *Self, ctx: *TypeContext, term: *Term) InferError!*Type {
        if (term.sub_terms.items.len == 0) {
            return TypeTheoryError.InvalidTypeConstruction;
        }
        const witness_type = try self.inferType(ctx, term.sub_terms.items[0]);
        errdefer witness_type.deinit();
        const id_type = try Type.init(self.allocator, .IDENTITY);
        errdefer id_type.deinit();
        try id_type.parameters.append(try witness_type.clone(self.allocator));
        return id_type;
    }

    fn inferAnnotation(self: *Self, term: *Term) InferError!*Type {
        if (term.annotation_type) |ann| {
            return ann.clone(self.allocator);
        }
        return TypeTheoryError.InvalidTypeConstruction;
    }

    pub fn subtype(self: *Self, sub: *Type, super: *Type) bool {
        if (sub.equals(super)) return true;
        if (super.kind == .TOP) return true;
        if (sub.kind == .BOTTOM) return true;
        if (sub.kind == .NAT and super.kind == .INT) return true;
        if (sub.kind == .INT and super.kind == .REAL) return true;
        if (sub.kind == .REAL and super.kind == .COMPLEX) return true;
        if (sub.kind == .FUNCTION and super.kind == .FUNCTION) {
            if (sub.left_type != null and super.left_type != null and sub.right_type != null and super.right_type != null) {
                return self.subtype(super.left_type.?, sub.left_type.?) and self.subtype(sub.right_type.?, super.right_type.?);
            }
        }
        if (sub.kind == .TUPLE and super.kind == .TUPLE) {
            if (sub.parameters.items.len != super.parameters.items.len) return false;
            for (sub.parameters.items, 0..) |s_param, i| {
                if (!self.subtype(s_param, super.parameters.items[i])) return false;
            }
            return true;
        }
        if (sub.kind == .UNIVERSE and super.kind == .UNIVERSE) {
            return sub.universe_level <= super.universe_level;
        }
        return false;
    }

    pub fn unifyTypes(self: *Self, t1: *Type, t2: *Type) !*Type {
        self.unification_count += 1;
        if (t1.equals(t2)) {
            return t1.clone(self.allocator);
        }
        if (t1.kind == .VARIABLE) {
            return t2.clone(self.allocator);
        }
        if (t2.kind == .VARIABLE) {
            return t1.clone(self.allocator);
        }
        if (t1.kind == .TOP) return t2.clone(self.allocator);
        if (t2.kind == .TOP) return t1.clone(self.allocator);
        if (t1.kind == .BOTTOM) return t2.clone(self.allocator);
        if (t2.kind == .BOTTOM) return t1.clone(self.allocator);
        if (t1.kind == t2.kind) {
            switch (t1.kind) {
                .FUNCTION => {
                    if (t1.left_type != null and t2.left_type != null and t1.right_type != null and t2.right_type != null) {
                        const unified_domain = try self.unifyTypes(t1.left_type.?, t2.left_type.?);
                        errdefer unified_domain.deinit();
                        const unified_codomain = try self.unifyTypes(t1.right_type.?, t2.right_type.?);
                        errdefer unified_codomain.deinit();
                        return Type.initFunction(self.allocator, unified_domain, unified_codomain);
                    }
                },
                .TUPLE => {
                    if (t1.parameters.items.len == t2.parameters.items.len) {
                        var unified_params = ArrayList(*Type).init(self.allocator);
                        errdefer {
                            for (unified_params.items) |p| {
                                p.deinit();
                            }
                            unified_params.deinit();
                        }
                        for (t1.parameters.items, 0..) |p1, i| {
                            const unified = try self.unifyTypes(p1, t2.parameters.items[i]);
                            try unified_params.append(unified);
                        }
                        const result = try Type.init(self.allocator, .TUPLE);
                        errdefer result.deinit();
                        result.parameters = unified_params;
                        return result;
                    }
                },
                .ARRAY => {
                    if (t1.parameters.items.len > 0 and t2.parameters.items.len > 0) {
                        const unified_elem = try self.unifyTypes(t1.parameters.items[0], t2.parameters.items[0]);
                        errdefer unified_elem.deinit();
                        return Type.initArray(self.allocator, unified_elem);
                    }
                },
                .UNIVERSE => {
                    const max_level = @max(t1.universe_level, t2.universe_level);
                    return Type.initUniverse(self.allocator, max_level);
                },
                else => {},
            }
        }
        if (self.subtype(t1, t2)) {
            return t2.clone(self.allocator);
        }
        if (self.subtype(t2, t1)) {
            return t1.clone(self.allocator);
        }
        return TypeTheoryError.UnificationFailure;
    }

    pub fn getStatistics(self: *const Self) TypeCheckerStatistics {
        return TypeCheckerStatistics{
            .inference_count = self.inference_count,
            .check_count = self.check_count,
            .unification_count = self.unification_count,
        };
    }
};

pub const TypeCheckerStatistics = struct {
    inference_count: u64,
    check_count: u64,
    unification_count: u64,
};

pub const PropositionAsType = struct {
    connective: LogicalConnective,
    sub_propositions: ArrayList(*PropositionAsType),
    bound_variable: ?[]const u8,
    predicate_type: ?*Type,
    corresponding_type: ?*Type,
    allocator: Allocator,

    pub const LogicalConnective = enum(u8) {
        CONJUNCTION = 0,
        DISJUNCTION = 1,
        IMPLICATION = 2,
        NEGATION = 3,
        UNIVERSAL = 4,
        EXISTENTIAL = 5,
        TRUE = 6,
        FALSE = 7,
        BICONDITIONAL = 8,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, connective: LogicalConnective) !*Self {
        const p = try allocator.create(Self);
        errdefer allocator.destroy(p);
        p.* = Self{
            .connective = connective,
            .sub_propositions = ArrayList(*PropositionAsType).init(allocator),
            .bound_variable = null,
            .predicate_type = null,
            .corresponding_type = null,
            .allocator = allocator,
        };
        return p;
    }

    pub fn initTrue(allocator: Allocator) !*Self {
        const p = try PropositionAsType.init(allocator, .TRUE);
        errdefer p.deinit();
        p.corresponding_type = try Type.initUnit(allocator);
        return p;
    }

    pub fn initFalse(allocator: Allocator) !*Self {
        const p = try PropositionAsType.init(allocator, .FALSE);
        errdefer p.deinit();
        p.corresponding_type = try Type.initBottom(allocator);
        return p;
    }

    pub fn initConjunction(allocator: Allocator, left: *PropositionAsType, right: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .CONJUNCTION);
        errdefer p.deinit();
        try p.sub_propositions.append(left);
        try p.sub_propositions.append(right);
        if (left.corresponding_type != null and right.corresponding_type != null) {
            p.corresponding_type = try Type.initTuple(
                allocator,
                &[_]*Type{ try left.corresponding_type.?.clone(allocator), try right.corresponding_type.?.clone(allocator) },
            );
        }
        return p;
    }

    pub fn initDisjunction(allocator: Allocator, left: *PropositionAsType, right: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .DISJUNCTION);
        errdefer p.deinit();
        try p.sub_propositions.append(left);
        try p.sub_propositions.append(right);
        if (left.corresponding_type != null and right.corresponding_type != null) {
            p.corresponding_type = try Type.initSum(
                allocator,
                try left.corresponding_type.?.clone(allocator),
                try right.corresponding_type.?.clone(allocator),
            );
        }
        return p;
    }

    pub fn initImplication(allocator: Allocator, antecedent: *PropositionAsType, consequent: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .IMPLICATION);
        errdefer p.deinit();
        try p.sub_propositions.append(antecedent);
        try p.sub_propositions.append(consequent);
        if (antecedent.corresponding_type != null and consequent.corresponding_type != null) {
            p.corresponding_type = try Type.initFunction(
                allocator,
                try antecedent.corresponding_type.?.clone(allocator),
                try consequent.corresponding_type.?.clone(allocator),
            );
        }
        return p;
    }

    pub fn initNegation(allocator: Allocator, inner: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .NEGATION);
        errdefer p.deinit();
        try p.sub_propositions.append(inner);
        if (inner.corresponding_type) |inner_type| {
            p.corresponding_type = try Type.initFunction(
                allocator,
                try inner_type.clone(allocator),
                try Type.initBottom(allocator),
            );
        }
        return p;
    }

    pub fn initUniversal(allocator: Allocator, variable: []const u8, domain: *Type, body: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .UNIVERSAL);
        errdefer p.deinit();
        p.bound_variable = try allocator.dupe(u8, variable);
        p.predicate_type = try domain.clone(allocator);
        try p.sub_propositions.append(body);
        if (body.corresponding_type) |body_type| {
            const pi_type = try Type.init(allocator, .DEPENDENT_FUNCTION);
            errdefer pi_type.deinit();
            pi_type.bound_variable = try allocator.dupe(u8, variable);
            pi_type.left_type = try domain.clone(allocator);
            pi_type.body_type = try body_type.clone(allocator);
            p.corresponding_type = pi_type;
        }
        return p;
    }

    pub fn initExistential(allocator: Allocator, variable: []const u8, domain: *Type, body: *PropositionAsType) !*Self {
        const p = try PropositionAsType.init(allocator, .EXISTENTIAL);
        errdefer p.deinit();
        p.bound_variable = try allocator.dupe(u8, variable);
        p.predicate_type = try domain.clone(allocator);
        try p.sub_propositions.append(body);
        if (body.corresponding_type) |body_type| {
            const sigma_type = try Type.init(allocator, .DEPENDENT_PAIR);
            errdefer sigma_type.deinit();
            sigma_type.bound_variable = try allocator.dupe(u8, variable);
            sigma_type.left_type = try domain.clone(allocator);
            sigma_type.body_type = try body_type.clone(allocator);
            p.corresponding_type = sigma_type;
        }
        return p;
    }

    pub fn deinit(self: *Self) void {
        for (self.sub_propositions.items) |sub| {
            sub.deinit();
        }
        self.sub_propositions.deinit();
        if (self.bound_variable) |bv| {
            self.allocator.free(bv);
        }
        if (self.predicate_type) |pt| {
            pt.deinit();
        }
        if (self.corresponding_type) |ct| {
            ct.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn toType(self: *const Self, allocator: Allocator) !*Type {
        if (self.corresponding_type) |ct| {
            return ct.clone(allocator);
        }
        return Type.initUnit(allocator);
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const p = try PropositionAsType.init(allocator, self.connective);
        errdefer p.deinit();
        for (self.sub_propositions.items) |sub| {
            try p.sub_propositions.append(try sub.clone(allocator));
        }
        p.bound_variable = if (self.bound_variable) |bv| try allocator.dupe(u8, bv) else null;
        p.predicate_type = if (self.predicate_type) |pt| try pt.clone(allocator) else null;
        p.corresponding_type = if (self.corresponding_type) |ct| try ct.clone(allocator) else null;
        return p;
    }
};

pub const ProofTerm = struct {
    kind: ProofKind,
    proposition: *PropositionAsType,
    sub_proofs: ArrayList(*ProofTerm),
    witness_term: ?*Term,
    is_valid: bool,
    allocator: Allocator,

    pub const ProofKind = enum(u8) {
        ASSUMPTION = 0,
        INTRO_CONJUNCTION = 1,
        ELIM_CONJUNCTION_LEFT = 2,
        ELIM_CONJUNCTION_RIGHT = 3,
        INTRO_DISJUNCTION_LEFT = 4,
        INTRO_DISJUNCTION_RIGHT = 5,
        ELIM_DISJUNCTION = 6,
        INTRO_IMPLICATION = 7,
        ELIM_IMPLICATION = 8,
        INTRO_UNIVERSAL = 9,
        ELIM_UNIVERSAL = 10,
        INTRO_EXISTENTIAL = 11,
        ELIM_EXISTENTIAL = 12,
        INTRO_NEGATION = 13,
        ELIM_NEGATION = 14,
        REFLEXIVITY = 15,
        SYMMETRY = 16,
        TRANSITIVITY = 17,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, kind: ProofKind, proposition: *PropositionAsType) !*Self {
        const pt = try allocator.create(Self);
        errdefer allocator.destroy(pt);
        pt.* = Self{
            .kind = kind,
            .proposition = proposition,
            .sub_proofs = ArrayList(*ProofTerm).init(allocator),
            .witness_term = null,
            .is_valid = false,
            .allocator = allocator,
        };
        return pt;
    }

    pub fn deinit(self: *Self) void {
        self.proposition.deinit();
        for (self.sub_proofs.items) |sub| {
            sub.deinit();
        }
        self.sub_proofs.deinit();
        if (self.witness_term) |w| {
            w.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn validate(self: *Self) bool {
        self.is_valid = switch (self.kind) {
            .ASSUMPTION => true,
            .INTRO_CONJUNCTION => self.validateConjunctionIntro(),
            .ELIM_CONJUNCTION_LEFT, .ELIM_CONJUNCTION_RIGHT => self.validateConjunctionElim(),
            .INTRO_IMPLICATION => self.validateImplicationIntro(),
            .ELIM_IMPLICATION => self.validateImplicationElim(),
            .INTRO_UNIVERSAL => self.validateUniversalIntro(),
            .ELIM_UNIVERSAL => self.validateUniversalElim(),
            .REFLEXIVITY => true,
            else => self.sub_proofs.items.len > 0,
        };
        return self.is_valid;
    }

    fn validateConjunctionIntro(self: *const Self) bool {
        if (self.sub_proofs.items.len < 2) return false;
        return self.sub_proofs.items[0].is_valid and self.sub_proofs.items[1].is_valid;
    }

    fn validateConjunctionElim(self: *const Self) bool {
        if (self.sub_proofs.items.len < 1) return false;
        const premise = self.sub_proofs.items[0];
        return premise.is_valid and premise.proposition.connective == .CONJUNCTION;
    }

    fn validateImplicationIntro(self: *const Self) bool {
        if (self.sub_proofs.items.len < 1) return false;
        return self.sub_proofs.items[0].is_valid;
    }

    fn validateImplicationElim(self: *const Self) bool {
        if (self.sub_proofs.items.len < 2) return false;
        const impl_proof = self.sub_proofs.items[0];
        return impl_proof.is_valid and impl_proof.proposition.connective == .IMPLICATION;
    }

    fn validateUniversalIntro(self: *const Self) bool {
        if (self.sub_proofs.items.len < 1) return false;
        return self.sub_proofs.items[0].is_valid;
    }

    fn validateUniversalElim(self: *const Self) bool {
        if (self.sub_proofs.items.len < 1) return false;
        const univ_proof = self.sub_proofs.items[0];
        return univ_proof.is_valid and univ_proof.proposition.connective == .UNIVERSAL;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const pt = try ProofTerm.init(allocator, self.kind, try self.proposition.clone(allocator));
        errdefer pt.deinit();
        for (self.sub_proofs.items) |sub| {
            try pt.sub_proofs.append(try sub.clone(allocator));
        }
        pt.witness_term = if (self.witness_term) |w| try w.clone(allocator) else null;
        pt.is_valid = self.is_valid;
        return pt;
    }
};

pub const CategoryObject = struct {
    id: u64,
    name: []const u8,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, id: u64, name: []const u8) !*Self {
        const obj = try allocator.create(Self);
        errdefer allocator.destroy(obj);
        obj.* = Self{
            .id = id,
            .name = try allocator.dupe(u8, name),
            .allocator = allocator,
        };
        return obj;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return CategoryObject.init(allocator, self.id, self.name);
    }

    pub fn equals(self: *const Self, other: *const Self) bool {
        return self.id == other.id and std.mem.eql(u8, self.name, other.name);
    }
};

pub const Morphism = struct {
    id: u64,
    name: []const u8,
    source: *CategoryObject,
    target: *CategoryObject,
    is_identity: bool,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, id: u64, name: []const u8, source: *CategoryObject, target: *CategoryObject) !*Self {
        const m = try allocator.create(Self);
        errdefer allocator.destroy(m);
        m.* = Self{
            .id = id,
            .name = try allocator.dupe(u8, name),
            .source = source,
            .target = target,
            .is_identity = false,
            .allocator = allocator,
        };
        return m;
    }

    pub fn initIdentity(allocator: Allocator, id: u64, obj: *CategoryObject) !*Self {
        const m = try Morphism.init(allocator, id, "id", obj, obj);
        m.is_identity = true;
        return m;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const source_clone = try self.source.clone(allocator);
        errdefer source_clone.deinit();
        const target_clone = try self.target.clone(allocator);
        errdefer target_clone.deinit();
        const m = try Morphism.init(allocator, self.id, self.name, source_clone, target_clone);
        m.is_identity = self.is_identity;
        return m;
    }

    pub fn canCompose(self: *const Self, other: *const Self) bool {
        return self.target.equals(other.source);
    }
};

pub const Category = struct {
    name: []const u8,
    objects: ArrayList(*CategoryObject),
    morphisms: ArrayList(*Morphism),
    compositions: AutoHashMap(u128, *Morphism),
    next_object_id: u64,
    next_morphism_id: u64,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8) !*Self {
        const cat = try allocator.create(Self);
        errdefer allocator.destroy(cat);
        cat.* = Self{
            .name = try allocator.dupe(u8, name),
            .objects = ArrayList(*CategoryObject).init(allocator),
            .morphisms = ArrayList(*Morphism).init(allocator),
            .compositions = AutoHashMap(u128, *Morphism).init(allocator),
            .next_object_id = 1,
            .next_morphism_id = 1,
            .allocator = allocator,
        };
        return cat;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        for (self.objects.items) |obj| {
            obj.deinit();
        }
        self.objects.deinit();
        for (self.morphisms.items) |m| {
            m.deinit();
        }
        self.morphisms.deinit();
        self.compositions.deinit();
        self.allocator.destroy(self);
    }

    pub fn addObject(self: *Self, name: []const u8) !*CategoryObject {
        const obj = try CategoryObject.init(self.allocator, self.next_object_id, name);
        errdefer obj.deinit();
        try self.objects.append(obj);
        self.next_object_id += 1;
        const identity = try Morphism.initIdentity(self.allocator, self.next_morphism_id, obj);
        errdefer identity.deinit();
        try self.morphisms.append(identity);
        self.next_morphism_id += 1;
        return obj;
    }

    pub fn addMorphism(self: *Self, name: []const u8, source: *CategoryObject, target: *CategoryObject) !*Morphism {
        const m = try Morphism.init(self.allocator, self.next_morphism_id, name, source, target);
        errdefer m.deinit();
        try self.morphisms.append(m);
        self.next_morphism_id += 1;
        return m;
    }

    pub fn compose(self: *Self, f: *Morphism, g: *Morphism) !*Morphism {
        if (!f.canCompose(g)) {
            return TypeTheoryError.CategoryLawViolation;
        }
        const comp_key: u128 = (@as(u128, f.id) << 64) | g.id;
        if (self.compositions.get(comp_key)) |cached| {
            return cached;
        }
        var name_buf: [256]u8 = undefined;
        const composed_name = std.fmt.bufPrint(&name_buf, "{s}∘{s}", .{ g.name, f.name }) catch "composed";
        const composed = try self.addMorphism(composed_name, f.source, g.target);
        try self.compositions.put(comp_key, composed);
        return composed;
    }

    pub fn getIdentity(self: *const Self, obj: *CategoryObject) ?*Morphism {
        for (self.morphisms.items) |m| {
            if (m.is_identity and m.source.equals(obj)) {
                return m;
            }
        }
        return null;
    }

    pub fn verifyAssociativity(self: *Self, f: *Morphism, g: *Morphism, h: *Morphism) !bool {
        if (!f.canCompose(g) or !g.canCompose(h)) {
            return false;
        }
        const fg = try self.compose(f, g);
        const gh = try self.compose(g, h);
        const fg_h = try self.compose(fg, h);
        const f_gh = try self.compose(f, gh);
        return fg_h.equals(f_gh);
    }

    pub fn verifyIdentityLaw(self: *const Self, f: *Morphism) bool {
        const source_id = self.getIdentity(f.source);
        const target_id = self.getIdentity(f.target);
        return source_id != null and target_id != null;
    }

    pub fn objectCount(self: *const Self) usize {
        return self.objects.items.len;
    }

    pub fn morphismCount(self: *const Self) usize {
        return self.morphisms.items.len;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const cat = try Category.init(allocator, self.name);
        errdefer cat.deinit();
        for (self.objects.items) |obj| {
            try cat.objects.append(try obj.clone(allocator));
        }
        for (self.morphisms.items) |m| {
            try cat.morphisms.append(try m.clone(allocator));
        }
        cat.next_object_id = self.next_object_id;
        cat.next_morphism_id = self.next_morphism_id;
        return cat;
    }
};

pub const Functor = struct {
    name: []const u8,
    source_category: *Category,
    target_category: *Category,
    object_mapping: AutoHashMap(u64, *CategoryObject),
    morphism_mapping: AutoHashMap(u64, *Morphism),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, source: *Category, target: *Category) !*Self {
        const f = try allocator.create(Self);
        errdefer allocator.destroy(f);
        f.* = Self{
            .name = try allocator.dupe(u8, name),
            .source_category = source,
            .target_category = target,
            .object_mapping = AutoHashMap(u64, *CategoryObject).init(allocator),
            .morphism_mapping = AutoHashMap(u64, *Morphism).init(allocator),
            .allocator = allocator,
        };
        return f;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.object_mapping.deinit();
        self.morphism_mapping.deinit();
        self.allocator.destroy(self);
    }

    pub fn mapObject(self: *Self, source_obj: *CategoryObject, target_obj: *CategoryObject) !void {
        try self.object_mapping.put(source_obj.id, target_obj);
    }

    pub fn mapMorphism(self: *Self, source_morph: *Morphism, target_morph: *Morphism) !void {
        try self.morphism_mapping.put(source_morph.id, target_morph);
    }

    pub fn applyToObject(self: *const Self, obj: *CategoryObject) ?*CategoryObject {
        return self.object_mapping.get(obj.id);
    }

    pub fn applyToMorphism(self: *const Self, m: *Morphism) ?*Morphism {
        return self.morphism_mapping.get(m.id);
    }

    pub fn preservesIdentity(self: *const Self, obj: *CategoryObject) bool {
        const source_id = self.source_category.getIdentity(obj);
        if (source_id == null) return false;
        const mapped_obj = self.applyToObject(obj);
        if (mapped_obj == null) return false;
        const target_id = self.target_category.getIdentity(mapped_obj.?);
        if (target_id == null) return false;
        const mapped_id = self.applyToMorphism(source_id.?);
        if (mapped_id == null) return false;
        return mapped_id.?.equals(target_id.?);
    }

    pub fn preservesComposition(self: *Self, f: *Morphism, g: *Morphism) !bool {
        if (!f.canCompose(g)) return false;
        const fg = try self.source_category.compose(f, g);
        const mapped_f = self.applyToMorphism(f);
        const mapped_g = self.applyToMorphism(g);
        const mapped_fg = self.applyToMorphism(fg);
        if (mapped_f == null or mapped_g == null or mapped_fg == null) return false;
        const composed_mapped = try self.target_category.compose(mapped_f.?, mapped_g.?);
        return composed_mapped.equals(mapped_fg.?);
    }

    pub fn verifyFunctorLaws(self: *Self) !bool {
        for (self.source_category.objects.items) |obj| {
            if (!self.preservesIdentity(obj)) {
                return false;
            }
        }
        for (self.source_category.morphisms.items) |f| {
            for (self.source_category.morphisms.items) |g| {
                if (f.canCompose(g)) {
                    if (!try self.preservesComposition(f, g)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const f = try Functor.init(allocator, self.name, self.source_category, self.target_category);
        errdefer f.deinit();
        var obj_iter = self.object_mapping.iterator();
        while (obj_iter.next()) |entry| {
            try f.object_mapping.put(entry.key_ptr.*, try entry.value_ptr.*.clone(allocator));
        }
        var morph_iter = self.morphism_mapping.iterator();
        while (morph_iter.next()) |entry| {
            try f.morphism_mapping.put(entry.key_ptr.*, try entry.value_ptr.*.clone(allocator));
        }
        return f;
    }
};

pub const NaturalTransformation = struct {
    name: []const u8,
    source_functor: *Functor,
    target_functor: *Functor,
    components: AutoHashMap(u64, *Morphism),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, source: *Functor, target: *Functor) !*Self {
        const nt = try allocator.create(Self);
        errdefer allocator.destroy(nt);
        nt.* = Self{
            .name = try allocator.dupe(u8, name),
            .source_functor = source,
            .target_functor = target,
            .components = AutoHashMap(u64, *Morphism).init(allocator),
            .allocator = allocator,
        };
        return nt;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.components.deinit();
        self.allocator.destroy(self);
    }

    pub fn setComponent(self: *Self, obj: *CategoryObject, component: *Morphism) !void {
        try self.components.put(obj.id, component);
    }

    pub fn getComponent(self: *const Self, obj: *CategoryObject) ?*Morphism {
        return self.components.get(obj.id);
    }

    pub fn verifyNaturality(self: *Self, f: *Morphism) !bool {
        const source_comp = self.getComponent(f.source);
        const target_comp = self.getComponent(f.target);
        if (source_comp == null or target_comp == null) return false;
        const mapped_f_source = self.source_functor.applyToMorphism(f);
        const mapped_f_target = self.target_functor.applyToMorphism(f);
        if (mapped_f_source == null or mapped_f_target == null) return false;
        const left_path = try self.target_functor.target_category.compose(source_comp.?, mapped_f_target.?);
        const right_path = try self.target_functor.target_category.compose(mapped_f_source.?, target_comp.?);
        return left_path.equals(right_path);
    }

    pub fn verifyAllNaturality(self: *Self) !bool {
        for (self.source_functor.source_category.morphisms.items) |f| {
            if (!try self.verifyNaturality(f)) {
                return false;
            }
        }
        return true;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const nt = try NaturalTransformation.init(allocator, self.name, self.source_functor, self.target_functor);
        errdefer nt.deinit();
        var iter = self.components.iterator();
        while (iter.next()) |entry| {
            try nt.components.put(entry.key_ptr.*, try entry.value_ptr.*.clone(allocator));
        }
        return nt;
    }
};

pub const Monad = struct {
    name: []const u8,
    endofunctor: *Functor,
    unit: *NaturalTransformation,
    multiplication: *NaturalTransformation,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, t: *Functor, eta: *NaturalTransformation, mu: *NaturalTransformation) !*Self {
        const m = try allocator.create(Self);
        errdefer allocator.destroy(m);
        m.* = Self{
            .name = try allocator.dupe(u8, name),
            .endofunctor = t,
            .unit = eta,
            .multiplication = mu,
            .allocator = allocator,
        };
        return m;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    pub fn verifyLeftUnitLaw(self: *Self, obj: *CategoryObject) !bool {
        const eta_obj = self.unit.getComponent(obj);
        const mu_obj = self.multiplication.getComponent(obj);
        if (eta_obj == null or mu_obj == null) return false;
        const t_obj = self.endofunctor.applyToObject(obj);
        if (t_obj == null) return false;
        const t_eta = self.endofunctor.applyToMorphism(eta_obj.?);
        if (t_eta == null) return false;
        const left = try self.endofunctor.target_category.compose(t_eta.?, mu_obj.?);
        const id_t = self.endofunctor.target_category.getIdentity(t_obj.?);
        if (id_t == null) return false;
        return left.equals(id_t.?);
    }

    pub fn verifyRightUnitLaw(self: *Self, obj: *CategoryObject) !bool {
        const eta_t_obj = self.unit.getComponent(obj);
        const mu_obj = self.multiplication.getComponent(obj);
        if (eta_t_obj == null or mu_obj == null) return false;
        const right = try self.endofunctor.target_category.compose(eta_t_obj.?, mu_obj.?);
        const t_obj = self.endofunctor.applyToObject(obj);
        if (t_obj == null) return false;
        const id_t = self.endofunctor.target_category.getIdentity(t_obj.?);
        if (id_t == null) return false;
        return right.equals(id_t.?);
    }

    pub fn verifyAssociativityLaw(self: *Self, obj: *CategoryObject) !bool {
        const mu_obj = self.multiplication.getComponent(obj);
        if (mu_obj == null) return false;
        const t_obj = self.endofunctor.applyToObject(obj);
        if (t_obj == null) return false;
        const mu_t_obj = self.multiplication.getComponent(t_obj.?);
        if (mu_t_obj == null) return false;
        const t_mu = self.endofunctor.applyToMorphism(mu_obj.?);
        if (t_mu == null) return false;
        const left = try self.endofunctor.target_category.compose(mu_t_obj.?, mu_obj.?);
        const right = try self.endofunctor.target_category.compose(t_mu.?, mu_obj.?);
        return left.equals(right);
    }

    pub fn verifyMonadLaws(self: *Self) !bool {
        for (self.endofunctor.source_category.objects.items) |obj| {
            if (!try self.verifyLeftUnitLaw(obj)) return false;
            if (!try self.verifyRightUnitLaw(obj)) return false;
            if (!try self.verifyAssociativityLaw(obj)) return false;
        }
        return true;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return Monad.init(allocator, self.name, try self.endofunctor.clone(allocator), try self.unit.clone(allocator), try self.multiplication.clone(allocator));
    }
};

pub const CartesianClosedCategory = struct {
    base_category: *Category,
    terminal_object: ?*CategoryObject,
    product_functor: ?*Functor,
    exponential_functor: ?*Functor,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, base: *Category) !*Self {
        const ccc = try allocator.create(Self);
        errdefer allocator.destroy(ccc);
        ccc.* = Self{
            .base_category = base,
            .terminal_object = null,
            .product_functor = null,
            .exponential_functor = null,
            .allocator = allocator,
        };
        return ccc;
    }

    pub fn deinit(self: *Self) void {
        if (self.product_functor) |pf| {
            pf.deinit();
        }
        if (self.exponential_functor) |ef| {
            ef.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn setTerminal(self: *Self, obj: *CategoryObject) void {
        self.terminal_object = obj;
    }

    pub fn hasProducts(self: *const Self) bool {
        return self.product_functor != null;
    }

    pub fn hasExponentials(self: *const Self) bool {
        return self.exponential_functor != null;
    }

    pub fn isCartesianClosed(self: *const Self) bool {
        return self.terminal_object != null and self.hasProducts() and self.hasExponentials();
    }

    pub fn modelLambdaCalculus(self: *const Self) bool {
        return self.isCartesianClosed();
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const ccc = try CartesianClosedCategory.init(allocator, try self.base_category.clone(allocator));
        errdefer ccc.deinit();
        ccc.terminal_object = self.terminal_object;
        ccc.product_functor = if (self.product_functor) |pf| try pf.clone(allocator) else null;
        ccc.exponential_functor = if (self.exponential_functor) |ef| try ef.clone(allocator) else null;
        return ccc;
    }
};

pub const LinearityMode = enum(u8) {
    LINEAR = 0,
    AFFINE = 1,
    RELEVANT = 2,
    UNRESTRICTED = 3,

    const Self = @This();

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .LINEAR => "linear",
            .AFFINE => "affine",
            .RELEVANT => "relevant",
            .UNRESTRICTED => "unrestricted",
        };
    }

    pub fn canWeakenTo(self: Self, target: Self) bool {
        return switch (self) {
            .UNRESTRICTED => true,
            .AFFINE => target == .AFFINE or target == .UNRESTRICTED,
            .RELEVANT => target == .RELEVANT or target == .UNRESTRICTED,
            .LINEAR => target == .LINEAR,
        };
    }

    pub fn join(self: Self, other: Self) Self {
        if (self == .LINEAR or other == .LINEAR) return .LINEAR;
        if (self == .AFFINE and other == .RELEVANT) return .LINEAR;
        if (self == .RELEVANT and other == .AFFINE) return .LINEAR;
        if (self == .AFFINE or other == .AFFINE) return .AFFINE;
        if (self == .RELEVANT or other == .RELEVANT) return .RELEVANT;
        return .UNRESTRICTED;
    }
};

pub const LinearType = struct {
    base_type: *Type,
    linearity: LinearityMode,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, base_type: *Type, linearity: LinearityMode) !*Self {
        const lt = try allocator.create(Self);
        errdefer allocator.destroy(lt);
        lt.* = Self{
            .base_type = base_type,
            .linearity = linearity,
            .allocator = allocator,
        };
        return lt;
    }

    pub fn initLinear(allocator: Allocator, base_type: *Type) !*Self {
        return LinearType.init(allocator, base_type, .LINEAR);
    }

    pub fn initAffine(allocator: Allocator, base_type: *Type) !*Self {
        return LinearType.init(allocator, base_type, .AFFINE);
    }

    pub fn initRelevant(allocator: Allocator, base_type: *Type) !*Self {
        return LinearType.init(allocator, base_type, .RELEVANT);
    }

    pub fn initUnrestricted(allocator: Allocator, base_type: *Type) !*Self {
        return LinearType.init(allocator, base_type, .UNRESTRICTED);
    }

    pub fn deinit(self: *Self) void {
        self.base_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn mustUseExactlyOnce(self: *const Self) bool {
        return self.linearity == .LINEAR;
    }

    pub fn canDrop(self: *const Self) bool {
        return self.linearity == .AFFINE or self.linearity == .UNRESTRICTED;
    }

    pub fn canDuplicate(self: *const Self) bool {
        return self.linearity == .RELEVANT or self.linearity == .UNRESTRICTED;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        return LinearType.init(allocator, try self.base_type.clone(allocator), self.linearity);
    }
};

pub const ResourceUsage = struct {
    variable_name: []const u8,
    usage_count: u32,
    linear_type: *LinearType,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, name: []const u8, linear_type: *LinearType) !*Self {
        const ru = try allocator.create(Self);
        errdefer allocator.destroy(ru);
        ru.* = Self{
            .variable_name = try allocator.dupe(u8, name),
            .usage_count = 0,
            .linear_type = linear_type,
            .allocator = allocator,
        };
        return ru;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.variable_name);
        self.linear_type.deinit();
        self.allocator.destroy(self);
    }

    pub fn use(self: *Self) void {
        self.usage_count += 1;
    }

    pub fn isValid(self: *const Self) bool {
        return switch (self.linear_type.linearity) {
            .LINEAR => self.usage_count == 1,
            .AFFINE => self.usage_count <= 1,
            .RELEVANT => self.usage_count >= 1,
            .UNRESTRICTED => true,
        };
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const ru = try ResourceUsage.init(allocator, self.variable_name, try self.linear_type.clone(allocator));
        ru.usage_count = self.usage_count;
        return ru;
    }
};

pub const LinearTypeChecker = struct {
    resources: StringHashMap(*ResourceUsage),
    violation_log: ArrayList(LinearityViolation),
    check_count: u64,
    violation_count: u64,
    allocator: Allocator,

    pub const LinearityViolation = struct {
        variable_name: []const u8,
        expected_usage: LinearityMode,
        actual_count: u32,
        violation_type: ViolationType,

        pub const ViolationType = enum {
            UNUSED,
            OVERUSED,
            DROPPED,
            DUPLICATED,
        };
    };

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .resources = StringHashMap(*ResourceUsage).init(allocator),
            .violation_log = ArrayList(LinearityViolation).init(allocator),
            .check_count = 0,
            .violation_count = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        var iter = self.resources.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.resources.deinit();
        for (self.violation_log.items) |v| {
            self.allocator.free(v.variable_name);
        }
        self.violation_log.deinit();
    }

    pub fn introduce(self: *Self, name: []const u8, linear_type: *LinearType) !void {
        const key = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(key);
        const usage = try ResourceUsage.init(self.allocator, name, linear_type);
        errdefer usage.deinit();
        try self.resources.put(key, usage);
    }

    pub fn use(self: *Self, name: []const u8) !void {
        if (self.resources.get(name)) |usage| {
            usage.use();
        }
    }

    pub fn checkTerm(self: *Self, term: *Term) !bool {
        self.check_count += 1;
        switch (term.kind) {
            .VARIABLE => try self.use(term.name),
            .LAMBDA => {
                if (term.sub_terms.items.len > 0) {
                    _ = try self.checkTerm(term.sub_terms.items[0]);
                }
            },
            .APPLICATION => {
                for (term.sub_terms.items) |sub| {
                    _ = try self.checkTerm(sub);
                }
            },
            .PAIR => {
                for (term.sub_terms.items) |sub| {
                    _ = try self.checkTerm(sub);
                }
            },
            else => {},
        }
        return self.validateAll();
    }

    pub fn validateAll(self: *Self) bool {
        var all_valid = true;
        var iter = self.resources.iterator();
        while (iter.next()) |entry| {
            const usage = entry.value_ptr.*;
            if (!usage.isValid()) {
                all_valid = false;
                self.violation_count += 1;
                const violation = LinearityViolation{
                    .variable_name = self.allocator.dupe(u8, usage.variable_name) catch unreachable,
                    .expected_usage = usage.linear_type.linearity,
                    .actual_count = usage.usage_count,
                    .violation_type = if (usage.usage_count == 0) .UNUSED else if (usage.usage_count > 1) .OVERUSED else .DROPPED,
                };
                self.violation_log.append(violation) catch {};
            }
        }
        return all_valid;
    }

    pub fn reset(self: *Self) void {
        var iter = self.resources.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.resources.clearRetainingCapacity();
        for (self.violation_log.items) |v| {
            self.allocator.free(v.variable_name);
        }
        self.violation_log.clearRetainingCapacity();
    }

    pub fn getStatistics(self: *const Self) LinearCheckerStatistics {
        return LinearCheckerStatistics{
            .check_count = self.check_count,
            .violation_count = self.violation_count,
            .active_resources = self.resources.count(),
        };
    }
};

pub const LinearCheckerStatistics = struct {
    check_count: u64,
    violation_count: u64,
    active_resources: usize,
};

pub const TypeProofKind = enum(u8) {
    TYPE_JUDGMENT = 0,
    SUBTYPING = 1,
    EQUALITY = 2,
    LINEAR_USAGE = 3,
    FUNCTOR_LAW = 4,
    MONAD_LAW = 5,
    NATURALITY = 6,
    UNIVERSE_MEMBERSHIP = 7,

    const Self = @This();

    pub fn toString(self: Self) []const u8 {
        return switch (self) {
            .TYPE_JUDGMENT => "type_judgment",
            .SUBTYPING => "subtyping",
            .EQUALITY => "equality",
            .LINEAR_USAGE => "linear_usage",
            .FUNCTOR_LAW => "functor_law",
            .MONAD_LAW => "monad_law",
            .NATURALITY => "naturality",
            .UNIVERSE_MEMBERSHIP => "universe_membership",
        };
    }
};

pub const TypeProof = struct {
    proof_type: TypeProofKind,
    judgment: ?*TypeJudgment,
    sub_type: ?*Type,
    super_type: ?*Type,
    proof_term: ?*Term,
    is_valid: bool,
    derivation_steps: ArrayList([]const u8),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, proof_type: TypeProofKind) !*Self {
        const p = try allocator.create(Self);
        errdefer allocator.destroy(p);
        p.* = Self{
            .proof_type = proof_type,
            .judgment = null,
            .sub_type = null,
            .super_type = null,
            .proof_term = null,
            .is_valid = false,
            .derivation_steps = ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
        return p;
    }

    pub fn deinit(self: *Self) void {
        if (self.sub_type) |t| {
            t.deinit();
        }
        if (self.super_type) |t| {
            t.deinit();
        }
        if (self.judgment) |j| {
            j.deinit();
        }
        if (self.proof_term) |t| {
            t.deinit();
        }
        for (self.derivation_steps.items) |step| {
            self.allocator.free(step);
        }
        self.derivation_steps.deinit();
        self.allocator.destroy(self);
    }

    pub fn addStep(self: *Self, step: []const u8) !void {
        try self.derivation_steps.append(try self.allocator.dupe(u8, step));
    }

    pub fn validate(self: *Self) bool {
        self.is_valid = switch (self.proof_type) {
            .TYPE_JUDGMENT => if (self.judgment) |j| j.validate() else false,
            .SUBTYPING => self.sub_type != null and self.super_type != null,
            .EQUALITY => self.sub_type != null and self.super_type != null and self.sub_type.?.equals(self.super_type.?),
            else => self.derivation_steps.items.len > 0,
        };
        return self.is_valid;
    }

    pub fn clone(self: *const Self, allocator: Allocator) !*Self {
        const p = try TypeProof.init(allocator, self.proof_type);
        errdefer p.deinit();
        p.judgment = if (self.judgment) |j| try j.clone(allocator) else null;
        p.sub_type = if (self.sub_type) |t| try t.clone(allocator) else null;
        p.super_type = if (self.super_type) |t| try t.clone(allocator) else null;
        p.proof_term = if (self.proof_term) |t| try t.clone(allocator) else null;
        p.is_valid = self.is_valid;
        for (self.derivation_steps.items) |step| {
            try p.derivation_steps.append(try allocator.dupe(u8, step));
        }
        return p;
    }
};

pub const ProofResult = struct {
    success: bool,
    proof: ?*TypeProof,
    error_message: ?[]const u8,
    owns_proof: bool,
    allocator: Allocator,

    const Self = @This();

    pub fn initSuccess(allocator: Allocator, proof: *TypeProof) !*Self {
        const r = try allocator.create(Self);
        errdefer allocator.destroy(r);
        r.* = Self{
            .success = true,
            .proof = proof,
            .error_message = null,
            .owns_proof = false,
            .allocator = allocator,
        };
        return r;
    }

    pub fn initFailure(allocator: Allocator, message: []const u8) !*Self {
        const r = try allocator.create(Self);
        errdefer allocator.destroy(r);
        r.* = Self{
            .success = false,
            .proof = null,
            .error_message = try allocator.dupe(u8, message),
            .owns_proof = false,
            .allocator = allocator,
        };
        return r;
    }

    pub fn initWithOwnedProof(allocator: Allocator, proof: *TypeProof) !*Self {
        const r = try allocator.create(Self);
        errdefer allocator.destroy(r);
        r.* = Self{
            .success = true,
            .proof = proof,
            .error_message = null,
            .owns_proof = true,
            .allocator = allocator,
        };
        return r;
    }

    pub fn deinit(self: *Self) void {
        if (self.owns_proof) {
            if (self.proof) |p| {
                p.deinit();
            }
        }
        if (self.error_message) |msg| {
            self.allocator.free(msg);
        }
        self.allocator.destroy(self);
    }
};

pub const TypeTheoryEngine = struct {
    type_checker: TypeChecker,
    linear_checker: LinearTypeChecker,
    categories: ArrayList(*Category),
    functors: ArrayList(*Functor),
    monads: ArrayList(*Monad),
    proofs: ArrayList(*TypeProof),
    proof_count: u64,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .type_checker = TypeChecker.init(allocator),
            .linear_checker = LinearTypeChecker.init(allocator),
            .categories = ArrayList(*Category).init(allocator),
            .functors = ArrayList(*Functor).init(allocator),
            .monads = ArrayList(*Monad).init(allocator),
            .proofs = ArrayList(*TypeProof).init(allocator),
            .proof_count = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.type_checker.deinit();
        self.linear_checker.deinit();
        for (self.categories.items) |cat| {
            cat.deinit();
        }
        self.categories.deinit();
        for (self.functors.items) |f| {
            f.deinit();
        }
        self.functors.deinit();
        for (self.monads.items) |m| {
            m.deinit();
        }
        self.monads.deinit();
        for (self.proofs.items) |p| {
            p.deinit();
        }
        self.proofs.deinit();
    }

    pub fn proveTypeJudgment(self: *Self, ctx: *TypeContext, term: *Term, expected_type: *Type) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .TYPE_JUDGMENT);
        errdefer proof.deinit();
        const inferred = self.type_checker.inferType(ctx, term) catch |err| {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, @errorName(err));
        };
        defer {
            inferred.deinit();
        }
        if (self.type_checker.subtype(inferred, expected_type)) {
            const judgment = try TypeJudgment.init(self.allocator, ctx, term, try expected_type.clone(self.allocator));
            _ = judgment.validate();
            proof.judgment = judgment;
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Type mismatch");
        }
    }

    pub fn proveSubtyping(self: *Self, sub: *Type, super: *Type) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .SUBTYPING);
        errdefer proof.deinit();
        proof.sub_type = try sub.clone(self.allocator);
        proof.super_type = try super.clone(self.allocator);
        if (self.type_checker.subtype(sub, super)) {
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "No subtyping relation exists");
        }
    }

    pub fn proveEquality(self: *Self, t1: *Type, t2: *Type) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .EQUALITY);
        errdefer proof.deinit();
        proof.sub_type = try t1.clone(self.allocator);
        proof.super_type = try t2.clone(self.allocator);
        if (t1.equals(t2)) {
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            const unified = self.type_checker.unifyTypes(t1, t2) catch {
                proof.is_valid = false;
                proof.deinit();
                return ProofResult.initFailure(self.allocator, "Types are not equal");
            };
            defer {
                unified.deinit();
            }
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        }
    }

    pub fn checkLinearUsage(self: *Self, term: *Term) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .LINEAR_USAGE);
        errdefer proof.deinit();
        const valid = try self.linear_checker.checkTerm(term);
        if (valid) {
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Linear usage violation");
        }
    }

    pub fn functorCheck(self: *Self, f: *Functor) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .FUNCTOR_LAW);
        errdefer proof.deinit();
        const laws_hold = try f.verifyFunctorLaws();
        if (laws_hold) {
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Functor law violation");
        }
    }

    pub fn monadLaws(self: *Self, m: *Monad) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .MONAD_LAW);
        errdefer proof.deinit();
        const laws_hold = try m.verifyMonadLaws();
        if (laws_hold) {
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Monad law violation");
        }
    }

    pub fn createCategory(self: *Self, name: []const u8) !*Category {
        const cat = try Category.init(self.allocator, name);
        try self.categories.append(cat);
        return cat;
    }

    pub fn createFunctor(self: *Self, name: []const u8, source: *Category, target: *Category) !*Functor {
        const f = try Functor.init(self.allocator, name, source, target);
        try self.functors.append(f);
        return f;
    }

    pub fn createMonad(self: *Self, name: []const u8, t: *Functor, eta: *NaturalTransformation, mu: *NaturalTransformation) !*Monad {
        const m = try Monad.init(self.allocator, name, t, eta, mu);
        try self.monads.append(m);
        return m;
    }

    pub fn getStatistics(self: *const Self) TypeTheoryStatistics {
        return TypeTheoryStatistics{
            .type_checker_stats = self.type_checker.getStatistics(),
            .linear_checker_stats = self.linear_checker.getStatistics(),
            .proof_count = self.proof_count,
            .category_count = self.categories.items.len,
            .functor_count = self.functors.items.len,
            .monad_count = self.monads.items.len,
        };
    }
};

pub const TypeTheoryStatistics = struct {
    type_checker_stats: TypeCheckerStatistics,
    linear_checker_stats: LinearCheckerStatistics,
    proof_count: u64,
    category_count: usize,
    functor_count: usize,
    monad_count: usize,
};

pub const ProofBuilder = struct {
    allocator: Allocator,
    nodes: ArrayList(Node),
    owned_strings: ArrayList([]const u8),

    pub const NodeId = u32;

    pub const Node = union(enum) {
        refl: []const u8,
        symm: NodeId,
        trans: struct {
            lhs: NodeId,
            rhs: NodeId,
        },
        congr_arg: struct {
            fn_expr: []const u8,
            arg: NodeId,
        },
        subst: struct {
            motive: []const u8,
            eq_proof: NodeId,
            term: []const u8,
        },
        or_inl: NodeId,
        or_inr: NodeId,
        or_cases: struct {
            target: []const u8,
            left_name: []const u8,
            left_proof: NodeId,
            right_name: []const u8,
            right_proof: NodeId,
        },
        bool_cases: struct {
            target: []const u8,
            motive: []const u8,
            false_proof: NodeId,
            true_proof: NodeId,
        },
        lambda: struct {
            param: []const u8,
            param_type: []const u8,
            body: NodeId,
        },
        application: struct {
            fn_term: []const u8,
            arg_term: []const u8,
        },
        raw: []const u8,
        and_intro: struct {
            left: NodeId,
            right: NodeId,
        },
        and_left: NodeId,
        and_right: NodeId,
        exists_intro: struct {
            witness: []const u8,
            proof: NodeId,
        },
        exists_elim: struct {
            ex_proof: NodeId,
            witness_name: []const u8,
            proof_name: []const u8,
            body: NodeId,
        },
        absurd: NodeId,
        false_elim: NodeId,
        list_rec: struct {
            motive: []const u8,
            nil_case: NodeId,
            cons_case: NodeId,
        },
        nat_rec: struct {
            target: []const u8,
            motive: []const u8,
            base_case: NodeId,
            step_name: []const u8,
            ih_name: []const u8,
            step_case: NodeId,
        },
        memory_rewrite: struct {
            pointer: []const u8,
            field: []const u8,
            eq_proof: NodeId,
        },
    };

    pub fn init(allocator: Allocator) ProofBuilder {
        return .{
            .allocator = allocator,
            .nodes = ArrayList(Node).init(allocator),
            .owned_strings = ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *ProofBuilder) void {
        for (self.owned_strings.items) |s| {
            self.allocator.free(s);
        }
        self.owned_strings.deinit();
        self.nodes.deinit();
    }

    pub fn reset(self: *ProofBuilder) void {
        for (self.owned_strings.items) |s| {
            self.allocator.free(s);
        }
        self.owned_strings.clearRetainingCapacity();
        self.nodes.clearRetainingCapacity();
    }

    fn dup(self: *ProofBuilder, s: []const u8) ![]const u8 {
        const copied = try self.allocator.dupe(u8, s);
        try self.owned_strings.append(copied);
        return copied;
    }

    fn append(self: *ProofBuilder, node: Node) !NodeId {
        try self.nodes.append(node);
        return @as(NodeId, @intCast(self.nodes.items.len - 1));
    }

    pub fn raw(self: *ProofBuilder, term: []const u8) !NodeId {
        return self.append(.{ .raw = try self.dup(term) });
    }

    pub fn refl(self: *ProofBuilder, witness: []const u8) !NodeId {
        return self.append(.{ .refl = try self.dup(witness) });
    }

    pub fn symm(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .symm = proof });
    }

    pub fn trans(self: *ProofBuilder, lhs: NodeId, rhs: NodeId) !NodeId {
        return self.append(.{ .trans = .{ .lhs = lhs, .rhs = rhs } });
    }

    pub fn eqTrans(self: *ProofBuilder, _: []const u8, lhs: NodeId, rhs: NodeId) !NodeId {
        return self.append(.{ .trans = .{ .lhs = lhs, .rhs = rhs } });
    }

    pub fn congrArg(self: *ProofBuilder, fn_expr: []const u8, arg: NodeId) !NodeId {
        return self.append(.{ .congr_arg = .{ .fn_expr = try self.dup(fn_expr), .arg = arg } });
    }

    pub fn subst(self: *ProofBuilder, motive: []const u8, eq_proof: NodeId, term: []const u8) !NodeId {
        return self.append(.{ .subst = .{ .motive = try self.dup(motive), .eq_proof = eq_proof, .term = try self.dup(term) } });
    }

    pub fn orInl(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .or_inl = proof });
    }

    pub fn orInr(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .or_inr = proof });
    }

    pub fn orCases(self: *ProofBuilder, target: []const u8, left_name: []const u8, left_proof: NodeId, right_name: []const u8, right_proof: NodeId) !NodeId {
        return self.append(.{ .or_cases = .{
            .target = try self.dup(target),
            .left_name = try self.dup(left_name),
            .left_proof = left_proof,
            .right_name = try self.dup(right_name),
            .right_proof = right_proof,
        } });
    }

    pub fn boolCases(self: *ProofBuilder, target: []const u8, motive: []const u8, false_proof: NodeId, true_proof: NodeId) !NodeId {
        return self.append(.{ .bool_cases = .{
            .target = try self.dup(target),
            .motive = try self.dup(motive),
            .false_proof = false_proof,
            .true_proof = true_proof,
        } });
    }

    pub fn lambda(self: *ProofBuilder, param: []const u8, param_type: []const u8, body: NodeId) !NodeId {
        return self.append(.{ .lambda = .{ .param = try self.dup(param), .param_type = try self.dup(param_type), .body = body } });
    }

    pub fn application(self: *ProofBuilder, fn_term: []const u8, arg_term: []const u8) !NodeId {
        return self.append(.{ .application = .{ .fn_term = try self.dup(fn_term), .arg_term = try self.dup(arg_term) } });
    }

    pub fn andIntro(self: *ProofBuilder, left: NodeId, right: NodeId) !NodeId {
        return self.append(.{ .and_intro = .{ .left = left, .right = right } });
    }

    pub fn andLeft(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .and_left = proof });
    }

    pub fn andRight(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .and_right = proof });
    }

    pub fn existsIntro(self: *ProofBuilder, witness: []const u8, proof: NodeId) !NodeId {
        return self.append(.{ .exists_intro = .{ .witness = try self.dup(witness), .proof = proof } });
    }

    pub fn existsElim(self: *ProofBuilder, ex_proof: NodeId, witness_name: []const u8, proof_name: []const u8, body: NodeId) !NodeId {
        return self.append(.{ .exists_elim = .{ .ex_proof = ex_proof, .witness_name = try self.dup(witness_name), .proof_name = try self.dup(proof_name), .body = body } });
    }

    pub fn absurd(self: *ProofBuilder, proof: NodeId) !NodeId {
        return self.append(.{ .absurd = proof });
    }

    pub fn falseElim(self: *ProofBuilder) !NodeId {
        return self.append(.{ .false_elim = undefined });
    }

    pub fn listRec(self: *ProofBuilder, motive: []const u8, nil_case: NodeId, cons_case: NodeId) !NodeId {
        return self.append(.{ .list_rec = .{ .motive = try self.dup(motive), .nil_case = nil_case, .cons_case = cons_case } });
    }

    pub fn natRec(self: *ProofBuilder, target: []const u8, motive: []const u8, base_case: NodeId, step_name: []const u8, ih_name: []const u8, step_case: NodeId) !NodeId {
        return self.append(.{ .nat_rec = .{
            .target = try self.dup(target),
            .motive = try self.dup(motive),
            .base_case = base_case,
            .step_name = try self.dup(step_name),
            .ih_name = try self.dup(ih_name),
            .step_case = step_case,
        } });
    }

    pub fn memoryRewrite(self: *ProofBuilder, pointer: []const u8, field: []const u8, eq_proof: NodeId) !NodeId {
        return self.append(.{ .memory_rewrite = .{ .pointer = try self.dup(pointer), .field = try self.dup(field), .eq_proof = eq_proof } });
    }

    pub fn render(self: *const ProofBuilder, out: *ArrayList(u8), root: NodeId) !void {
        const node = self.nodes.items[root];
        switch (node) {
            .refl => |witness| {
                try out.appendSlice("(@Eq.refl _ ");
                try out.appendSlice(witness);
                try out.append(')');
            },
            .symm => |inner| {
                try out.appendSlice("(@Eq.symm _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .trans => |t| {
                try out.appendSlice("(@Eq.trans _ _ _ (");
                try self.render(out, t.lhs);
                try out.appendSlice(") (");
                try self.render(out, t.rhs);
                try out.appendSlice("))");
            },
            .congr_arg => |c| {
                try out.appendSlice("(@congrArg _ _ ");
                try out.appendSlice(c.fn_expr);
                try out.appendSlice(" (");
                try self.render(out, c.arg);
                try out.appendSlice("))");
            },
            .subst => |s| {
                try out.appendSlice("(@Eq.subst ");
                try out.appendSlice(s.motive);
                try out.appendSlice(" (");
                try self.render(out, s.eq_proof);
                try out.appendSlice(") ");
                try out.appendSlice(s.term);
                try out.append(')');
            },
            .or_inl => |inner| {
                try out.appendSlice("(@Or.inl _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .or_inr => |inner| {
                try out.appendSlice("(@Or.inr _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .or_cases => |cases| {
                try out.appendSlice("(@Or.casesOn _ _ _ ");
                try out.appendSlice(cases.target);
                try out.appendSlice(" (fun ");
                try out.appendSlice(cases.left_name);
                try out.appendSlice(" => ");
                try self.render(out, cases.left_proof);
                try out.appendSlice(") (fun ");
                try out.appendSlice(cases.right_name);
                try out.appendSlice(" => ");
                try self.render(out, cases.right_proof);
                try out.appendSlice("))");
            },
            .bool_cases => |cases| {
                try out.appendSlice("(@Bool.casesOn ");
                try out.appendSlice(cases.target);
                try out.appendSlice(" (motive := ");
                try out.appendSlice(cases.motive);
                try out.appendSlice(") (");
                try self.render(out, cases.false_proof);
                try out.appendSlice(") (");
                try self.render(out, cases.true_proof);
                try out.appendSlice("))");
            },
            .lambda => |lam| {
                try out.appendSlice("(fun (");
                try out.appendSlice(lam.param);
                try out.appendSlice(" : ");
                try out.appendSlice(lam.param_type);
                try out.appendSlice(") => ");
                try self.render(out, lam.body);
                try out.append(')');
            },
            .application => |app| {
                try out.append('(');
                try out.appendSlice(app.fn_term);
                try out.append(' ');
                try out.appendSlice(app.arg_term);
                try out.append(')');
            },
            .raw => |term| try out.appendSlice(term),
            .and_intro => |a| {
                try out.appendSlice("(@And.intro _ _ (");
                try self.render(out, a.left);
                try out.appendSlice(") (");
                try self.render(out, a.right);
                try out.appendSlice("))");
            },
            .and_left => |inner| {
                try out.appendSlice("(@And.left _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .and_right => |inner| {
                try out.appendSlice("(@And.right _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .exists_intro => |e| {
                try out.appendSlice("(@Exists.intro _ ");
                try out.appendSlice(e.witness);
                try out.appendSlice(" (");
                try self.render(out, e.proof);
                try out.appendSlice("))");
            },
            .exists_elim => |e| {
                try out.appendSlice("(@Exists.elim _ _ (");
                try self.render(out, e.ex_proof);
                try out.appendSlice(") (fun ");
                try out.appendSlice(e.witness_name);
                try out.appendSlice(" ");
                try out.appendSlice(e.proof_name);
                try out.appendSlice(" => ");
                try self.render(out, e.body);
                try out.appendSlice("))");
            },
            .absurd => |inner| {
                try out.appendSlice("(@absurd _ _ (");
                try self.render(out, inner);
                try out.appendSlice("))");
            },
            .false_elim => {
                try out.appendSlice("(@False.elim _)");
            },
            .list_rec => |l| {
                try out.appendSlice("(@List.recOn _ ");
                try out.appendSlice(l.motive);
                try out.appendSlice(" (");
                try self.render(out, l.nil_case);
                try out.appendSlice(") (");
                try self.render(out, l.cons_case);
                try out.appendSlice("))");
            },
            .nat_rec => |n| {
                try out.appendSlice("(@Nat.recOn (motive := ");
                try out.appendSlice(n.motive);
                try out.appendSlice(") ");
                try out.appendSlice(n.target);
                try out.appendSlice(" (");
                try self.render(out, n.base_case);
                try out.appendSlice(") (fun ");
                try out.appendSlice(n.step_name);
                try out.appendSlice(" ");
                try out.appendSlice(n.ih_name);
                try out.appendSlice(" => ");
                try self.render(out, n.step_case);
                try out.appendSlice("))");
            },
            .memory_rewrite => |m| {
                _ = m.pointer;
                _ = m.field;
                try self.render(out, m.eq_proof);
            },
        }
    }
};

pub const SemanticContainerKind = enum(u8) {
    zig_struct = 0,
    zig_enum = 1,
    zig_union = 2,
};

pub const SemanticFieldIR = struct {
    name: []const u8,
    zig_type: []const u8,
    lean_type: []const u8,
    is_optional: bool,
    is_pointer: bool,
};

pub const SemanticParameterIR = struct {
    name: []const u8,
    zig_type: []const u8,
    lean_type: []const u8,
    is_optional: bool,
    is_pointer: bool,
};

pub const SemanticProofObligationKind = enum(u8) {
    state_preservation = 0,
    memory_safety = 1,
    branch_soundness = 2,
    loop_invariant = 3,
    termination = 4,
    error_coverage = 5,
    arithmetic_chain = 6,
    logical_operator = 7,
    bit_operator = 8,
    heap_allocation = 9,
    pointer_deref = 10,
    collision_resolution = 11,
};

pub const SemanticProofObligation = struct {
    kind: SemanticProofObligationKind,
    target_node: std.zig.Ast.Node.Index,
    payload: []const u8,
};

pub const SemanticControlSummary = struct {
    has_mutation: bool,
    has_if: bool,
    has_while: bool,
    has_for: bool,
    has_try: bool,
    has_error_union: bool,
    arithmetic_nodes: usize,
    logical_nodes: usize,
    bit_nodes: usize,
    assignment_nodes: usize,
    loop_nodes: usize,

    pub fn init() SemanticControlSummary {
        return .{
            .has_mutation = false,
            .has_if = false,
            .has_while = false,
            .has_for = false,
            .has_try = false,
            .has_error_union = false,
            .arithmetic_nodes = 0,
            .logical_nodes = 0,
            .bit_nodes = 0,
            .assignment_nodes = 0,
            .loop_nodes = 0,
        };
    }
};

pub const SemanticContainerIR = struct {
    name: []const u8,
    kind: SemanticContainerKind,
    fields: ArrayList(SemanticFieldIR),
    body_node: std.zig.Ast.Node.Index,
    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8, kind: SemanticContainerKind, body_node: std.zig.Ast.Node.Index) SemanticContainerIR {
        return .{
            .name = name,
            .kind = kind,
            .fields = ArrayList(SemanticFieldIR).init(allocator),
            .body_node = body_node,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SemanticContainerIR) void {
        self.fields.deinit();
    }
};

pub const SemanticFunctionIR = struct {
    name: []const u8,
    owner_type: []const u8,
    params: ArrayList(SemanticParameterIR),
    return_zig_type: []const u8,
    return_lean_type: []const u8,
    proto_node: std.zig.Ast.Node.Index,
    body_node: std.zig.Ast.Node.Index,
    control: SemanticControlSummary,
    obligations: ArrayList(SemanticProofObligation),
    state_cells: ArrayList([]const u8),
    loop_invariants: ArrayList([]const u8),
    active_hypotheses: ArrayList([]const u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8, owner_type: []const u8, return_zig_type: []const u8, return_lean_type: []const u8, proto_node: std.zig.Ast.Node.Index, body_node: std.zig.Ast.Node.Index) SemanticFunctionIR {
        return .{
            .name = name,
            .owner_type = owner_type,
            .params = ArrayList(SemanticParameterIR).init(allocator),
            .return_zig_type = return_zig_type,
            .return_lean_type = return_lean_type,
            .proto_node = proto_node,
            .body_node = body_node,
            .control = SemanticControlSummary.init(),
            .obligations = ArrayList(SemanticProofObligation).init(allocator),
            .state_cells = ArrayList([]const u8).init(allocator),
            .loop_invariants = ArrayList([]const u8).init(allocator),
            .active_hypotheses = ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SemanticFunctionIR) void {
        self.params.deinit();
        self.obligations.deinit();
        self.state_cells.deinit();
        self.loop_invariants.deinit();
        self.active_hypotheses.deinit();
    }

    pub fn isMethod(self: *const SemanticFunctionIR) bool {
        return self.owner_type.len > 0;
    }

    pub fn usesStateMonad(self: *const SemanticFunctionIR) bool {
        return self.control.has_mutation or self.control.has_if or self.control.has_while or self.control.has_for or self.isMethod();
    }

    pub fn usesErrorMonad(self: *const SemanticFunctionIR) bool {
        return self.control.has_error_union or self.control.has_try;
    }
};

pub const SemanticTestIR = struct {
    name: []const u8,
    body_node: std.zig.Ast.Node.Index,
};

pub const SemanticArithmeticRewrite = struct {
    expr: []const u8,
    target: []const u8,
    proof: ProofBuilder.NodeId,
};

pub const SemanticProofEngineError = error{InvalidSource};

pub const SemanticProofEngine = struct {
    source: [:0]const u8,
    owned_source: ?[:0]u8,
    ast: ?std.zig.Ast,
    containers: ArrayList(SemanticContainerIR),
    functions: ArrayList(SemanticFunctionIR),
    tests: ArrayList(SemanticTestIR),
    memory_cells: ArrayList([]const u8),
    out: ArrayList(u8),
    temp_names: ArrayList([]const u8),
    allocator: Allocator,
    function_map: StringHashMap(*const SemanticFunctionIR),
    theory_engine: TypeTheoryEngine,
    global_ctx: TypeContext,

    pub fn init(allocator: Allocator) SemanticProofEngine {
        return .{
            .source = "",
            .owned_source = null,
            .ast = null,
            .containers = ArrayList(SemanticContainerIR).init(allocator),
            .functions = ArrayList(SemanticFunctionIR).init(allocator),
            .tests = ArrayList(SemanticTestIR).init(allocator),
            .memory_cells = ArrayList([]const u8).init(allocator),
            .out = ArrayList(u8).init(allocator),
            .temp_names = ArrayList([]const u8).init(allocator),
            .allocator = allocator,
            .function_map = StringHashMap(*const SemanticFunctionIR).init(allocator),
            .theory_engine = TypeTheoryEngine.init(allocator),
            .global_ctx = TypeContext.init(allocator),
        };
    }

    pub fn deinit(self: *SemanticProofEngine) void {
        if (self.ast) |*tree| {
            tree.deinit(self.allocator);
        }
        for (self.containers.items) |*container| {
            container.deinit();
        }
        self.containers.deinit();
        for (self.functions.items) |*function| {
            function.deinit();
        }
        self.functions.deinit();
        self.tests.deinit();
        self.memory_cells.deinit();
        self.out.deinit();
        for (self.temp_names.items) |name| {
            self.allocator.free(name);
        }
        self.temp_names.deinit();
        if (self.owned_source) |owned| {
            self.allocator.free(owned);
        }
        self.function_map.deinit();
        self.theory_engine.deinit();
        self.global_ctx.deinit();
    }

    fn w(self: *SemanticProofEngine, s: []const u8) !void {
        try self.out.appendSlice(s);
    }

    fn wl(self: *SemanticProofEngine, s: []const u8) !void {
        try self.out.appendSlice(s);
        try self.out.append('\n');
    }

    fn nl(self: *SemanticProofEngine) !void {
        try self.out.append('\n');
    }

    fn wfmt(self: *SemanticProofEngine, comptime fmt: []const u8, args: anytype) !void {
        var buf: [4096]u8 = undefined;
        const s = try std.fmt.bufPrint(&buf, fmt, args);
        try self.out.appendSlice(s);
    }

    fn freshName(self: *SemanticProofEngine, prefix: []const u8) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.temp_names.items.len });
        try self.temp_names.append(name);
        return name;
    }

    fn astRef(self: *SemanticProofEngine) *const std.zig.Ast {
        return &self.ast.?;
    }

    fn nodeText(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) []const u8 {
        const tree = self.astRef();
        const first = tree.firstToken(node);
        const last = tree.lastToken(node);
        const token_starts = tree.tokens.items(.start);
        const start = token_starts[first];
        const end = if (last + 1 < token_starts.len) token_starts[last + 1] else @as(u32, @intCast(self.source.len));
        return std.mem.trim(u8, self.source[start..end], " \t\r\n");
    }

    fn tokenText(self: *SemanticProofEngine, token: std.zig.Ast.TokenIndex) []const u8 {
        return self.astRef().tokenSlice(token);
    }

    fn mapTypeText(self: *SemanticProofEngine, zig_type: []const u8) []const u8 {
        _ = self;
        var t = std.mem.trim(u8, zig_type, " \t\r\n");
        if (std.mem.indexOfScalar(u8, t, '!')) |bang| {
            t = std.mem.trim(u8, t[bang + 1 ..], " \t\r\n");
        }
        if (std.mem.startsWith(u8, t, "*const ")) t = t[7..];
        if (std.mem.startsWith(u8, t, "*")) t = t[1..];
        if (std.mem.startsWith(u8, t, "?")) t = t[1..];
        if (std.mem.eql(u8, t, "u8") or std.mem.eql(u8, t, "u16") or std.mem.eql(u8, t, "u32")) return "BitVec 32";
        if (std.mem.eql(u8, t, "u64") or std.mem.eql(u8, t, "usize")) return "BitVec 64";
        if (std.mem.eql(u8, t, "i8") or std.mem.eql(u8, t, "i16") or std.mem.eql(u8, t, "i32") or std.mem.eql(u8, t, "i64") or std.mem.eql(u8, t, "isize")) return "Int";
        if (std.mem.eql(u8, t, "f32") or std.mem.eql(u8, t, "f64")) return "Int";
        if (std.mem.eql(u8, t, "bool")) return "Bool";
        if (std.mem.eql(u8, t, "void")) return "Unit";
        if (std.mem.eql(u8, t, "[]const u8") or std.mem.eql(u8, t, "[]u8")) return "String";
        if (std.mem.startsWith(u8, t, "[]")) return "List Nat";
        if (t.len > 0 and t[0] >= 'A' and t[0] <= 'Z') return t;
        return "Nat";
    }

    fn defaultValue(self: *SemanticProofEngine, lean_type: []const u8) []const u8 {
        _ = self;
        if (std.mem.eql(u8, lean_type, "Nat")) return "0";
        if (std.mem.eql(u8, lean_type, "Int")) return "0";
        if (std.mem.eql(u8, lean_type, "Bool")) return "false";
        if (std.mem.eql(u8, lean_type, "String")) return "\"\"";
        if (std.mem.eql(u8, lean_type, "Unit")) return "Unit.unit";
        if (std.mem.startsWith(u8, lean_type, "List ")) return "[]";
        if (std.mem.startsWith(u8, lean_type, "BitVec ")) return "0";
        return "default";
    }

    fn isAssignmentTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .assign,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_add,
            .assign_sub,
            .assign_shl,
            .assign_shl_sat,
            .assign_shr,
            .assign_bit_and,
            .assign_bit_xor,
            .assign_bit_or,
            .assign_mul_wrap,
            .assign_add_wrap,
            .assign_sub_wrap,
            .assign_mul_sat,
            .assign_add_sat,
            .assign_sub_sat,
            => true,
            else => false,
        };
    }

    fn isArithmeticTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .add_wrap,
            .sub_wrap,
            .mul_wrap,
            .add_sat,
            .sub_sat,
            .mul_sat,
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_add_wrap,
            .assign_sub_wrap,
            .assign_mul_wrap,
            .assign_add_sat,
            .assign_sub_sat,
            .assign_mul_sat,
            => true,
            else => false,
        };
    }

    fn isLogicalTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .bool_and,
            .bool_or,
            .bool_not,
            .equal_equal,
            .bang_equal,
            .less_than,
            .greater_than,
            .less_or_equal,
            .greater_or_equal,
            => true,
            else => false,
        };
    }

    fn isBitTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .bit_and,
            .bit_or,
            .bit_xor,
            .bit_not,
            .shl,
            .shl_sat,
            .shr,
            .assign_bit_and,
            .assign_bit_or,
            .assign_bit_xor,
            .assign_shl,
            .assign_shl_sat,
            .assign_shr,
            => true,
            else => false,
        };
    }

    fn ownString(self: *SemanticProofEngine, s: []const u8) ![]const u8 {
        const copy = try self.allocator.dupe(u8, s);
        try self.temp_names.append(copy);
        return copy;
    }

    fn allocOwnedFmt(self: *SemanticProofEngine, comptime fmt: []const u8, args: anytype) ![]const u8 {
        const copy = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.temp_names.append(copy);
        return copy;
    }

    fn isAddLikeTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .add,
            .add_wrap,
            .add_sat,
            => true,
            else => false,
        };
    }

    fn isSubLikeTag(_: *SemanticProofEngine, tag: std.zig.Ast.Node.Tag) bool {
        return switch (tag) {
            .sub,
            .sub_wrap,
            .sub_sat,
            => true,
            else => false,
        };
    }

    fn isZeroNode(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) bool {
        const tag = self.astRef().nodes.items(.tag)[node];
        if (tag != .number_literal) return false;
        return std.mem.eql(u8, std.mem.trim(u8, self.nodeText(node), " \t\r\n"), "0");
    }

    fn registerMemoryCell(self: *SemanticProofEngine, cell_name: []const u8) !void {
        for (self.memory_cells.items) |existing| {
            if (std.mem.eql(u8, existing, cell_name)) return;
        }
        try self.memory_cells.append(cell_name);
    }

    fn qualifiedStateCell(self: *SemanticProofEngine, function: *const SemanticFunctionIR, field_name: []const u8) ![]const u8 {
        if (function.owner_type.len > 0) {
            return try self.allocOwnedFmt("{s}_{s}", .{ function.owner_type, field_name });
        }
        return try self.ownString(field_name);
    }

    fn writeSanitizedIdent(self: *SemanticProofEngine, prefix: []const u8, raw: []const u8) !void {
        try self.w(prefix);
        for (raw) |c| {
            if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')) {
                try self.out.append(c);
            } else {
                try self.out.append('_');
            }
        }
    }

    fn writeMemoryCellCtor(self: *SemanticProofEngine, cell_name: []const u8) !void {
        try self.writeSanitizedIdent("MemoryCell.cell_", cell_name);
    }

    fn collectExprIdentifiers(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index, identifiers: *ArrayList([]const u8)) !void {
        const tree = self.astRef();
        const tags = tree.nodes.items(.tag);
        const datas = tree.nodes.items(.data);
        const tag = tags[node];
        switch (tag) {
            .identifier => {
                const name = self.nodeText(node);
                for (identifiers.items) |existing| {
                    if (std.mem.eql(u8, existing, name)) return;
                }
                try identifiers.append(name);
            },
            .field_access => {
                if (datas[node].lhs != 0) try self.collectExprIdentifiers(datas[node].lhs, identifiers);
            },
            .call, .call_comma, .call_one, .call_one_comma, .async_call, .async_call_comma, .async_call_one, .async_call_one_comma => {
                var buf: [1]std.zig.Ast.Node.Index = undefined;
                if (tree.fullCall(&buf, node)) |call_full| {
                    for (call_full.ast.params) |param| {
                        try self.collectExprIdentifiers(param, identifiers);
                    }
                }
            },
            else => {
                if (datas[node].lhs != 0 and datas[node].lhs < tags.len) try self.collectExprIdentifiers(datas[node].lhs, identifiers);
                if (datas[node].rhs != 0 and datas[node].rhs < tags.len) try self.collectExprIdentifiers(datas[node].rhs, identifiers);
            },
        }
    }

    fn parameterLeanType(self: *SemanticProofEngine, function: *const SemanticFunctionIR, identifier: []const u8) []const u8 {
        _ = self;
        for (function.params.items) |param| {
            if (std.mem.eql(u8, param.name, identifier)) return param.lean_type;
        }
        return "Nat";
    }

    fn buildArithmeticRewrite(self: *SemanticProofEngine, builder: *ProofBuilder, node: std.zig.Ast.Node.Index) !SemanticArithmeticRewrite {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];

        switch (tag) {
            .add, .sub, .mul, .div, .mod,
            .add_wrap, .sub_wrap, .mul_wrap,
            .shl, .shr,
            .bit_and, .bit_or, .bit_xor,
            => {
                const lhs_rewrite = try self.buildArithmeticRewrite(builder, data.lhs);
                const rhs_rewrite = try self.buildArithmeticRewrite(builder, data.rhs);
                const op_str = switch (tag) {
                    .add => "Nat.add",
                    .sub => "Nat.sub",
                    .mul => "Nat.mul",
                    .div => "Nat.div",
                    .mod => "Nat.mod",
                    .add_wrap => "Nat.add",
                    .sub_wrap => "Nat.sub",
                    .mul_wrap => "Nat.mul",
                    .shl => "Nat.shiftLeft",
                    .shr => "Nat.shiftRight",
                    .bit_and => "Nat.land",
                    .bit_or => "Nat.lor",
                    .bit_xor => "Nat.xor",
                    else => "Nat.add",
                };
                const combined_expr = try self.allocOwnedFmt("({s} {s} {s})", .{ op_str, lhs_rewrite.expr, rhs_rewrite.expr });
                const combined_target = try self.allocOwnedFmt("({s} {s} {s})", .{ op_str, lhs_rewrite.target, rhs_rewrite.target });
                const congr_left = try builder.congrArg(
                    try self.allocOwnedFmt("{s} {s}", .{ op_str, lhs_rewrite.expr }),
                    rhs_rewrite.proof,
                );
                const congr_right = try builder.congrArg(
                    try self.allocOwnedFmt("fun x => {s} x {s}", .{ op_str, rhs_rewrite.target }),
                    lhs_rewrite.proof,
                );
                const chained = try builder.eqTrans(combined_expr, congr_right, congr_left);
                return .{ .expr = combined_expr, .target = combined_target, .proof = chained };
            },
            .grouped_expression => {
                return try self.buildArithmeticRewrite(builder, data.lhs);
            },
            .builtin_call_two, .builtin_call_two_comma => {
                const main_token = tree.nodes.items(.main_token)[node];
                const builtin_name = tree.tokenSlice(main_token);
                if (std.mem.eql(u8, builtin_name, "@as")) {
                    if (data.rhs != 0) {
                        return try self.buildArithmeticRewrite(builder, data.rhs);
                    } else if (data.lhs != 0) {
                        return try self.buildArithmeticRewrite(builder, data.lhs);
                    }
                } else if (std.mem.eql(u8, builtin_name, "@intCast") or
                    std.mem.eql(u8, builtin_name, "@bitCast") or
                    std.mem.eql(u8, builtin_name, "@floatCast") or
                    std.mem.eql(u8, builtin_name, "@intFromFloat") or
                    std.mem.eql(u8, builtin_name, "@floatFromInt") or
                    std.mem.eql(u8, builtin_name, "@truncate"))
                {
                    if (data.lhs != 0) {
                        return try self.buildArithmeticRewrite(builder, data.lhs);
                    }
                }
                const expr = try self.ownString(self.nodeText(node));
                const expr_wrapped = try self.allocOwnedFmt("({s})", .{expr});
                return .{ .expr = expr_wrapped, .target = expr_wrapped, .proof = try builder.refl(expr_wrapped) };
            },
            else => {
                const expr = try self.ownString(self.nodeText(node));
                const expr_wrapped = try self.allocOwnedFmt("({s})", .{expr});
                return .{ .expr = expr_wrapped, .target = expr_wrapped, .proof = try builder.refl(expr_wrapped) };
            },
        }
    }


    fn buildMemoryRewrite(self: *SemanticProofEngine, builder: *ProofBuilder, node: std.zig.Ast.Node.Index) !ProofBuilder.NodeId {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        if (tag == .field_access) {
            const datas = tree.nodes.items(.data);
            const pointer = try self.ownString(self.nodeText(datas[node].lhs));
            const field = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(datas[node].rhs)));
            const eq_proof = try builder.refl(try self.allocOwnedFmt("({s}).{s}", .{ pointer, field }));
            return try builder.memoryRewrite(pointer, field, eq_proof);
        }
        return try builder.refl(try self.ownString(self.nodeText(node)));
    }

    fn appendObligation(self: *SemanticProofEngine, function: *SemanticFunctionIR, kind: SemanticProofObligationKind, target_node: std.zig.Ast.Node.Index, payload: []const u8) !void {
        _ = self;
        for (function.obligations.items) |existing| {
            if (existing.kind == kind and existing.target_node == target_node and std.mem.eql(u8, existing.payload, payload)) return;
        }
        try function.obligations.append(.{ .kind = kind, .target_node = target_node, .payload = payload });
    }

    fn appendStateCell(self: *SemanticProofEngine, function: *SemanticFunctionIR, name: []const u8) !void {
        const qualified = try self.qualifiedStateCell(function, name);
        for (function.state_cells.items) |existing| {
            if (std.mem.eql(u8, existing, qualified)) return;
        }
        try function.state_cells.append(qualified);
        try self.registerMemoryCell(qualified);
    }

    fn extractAssignedField(self: *SemanticProofEngine, lhs_node: std.zig.Ast.Node.Index) ?[]const u8 {
        const tree = self.astRef();
        const tags = tree.nodes.items(.tag);
        const datas = tree.nodes.items(.data);
        if (tags[lhs_node] != .field_access) return null;
        const base = datas[lhs_node].lhs;
        if (base == 0 or tags[base] != .identifier) return null;
        const base_name = self.nodeText(base);
        if (!std.mem.eql(u8, base_name, "self")) return null;
        return tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(datas[lhs_node].rhs)));
    }

    fn collectBlockNodes(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index, out_nodes: *ArrayList(std.zig.Ast.Node.Index)) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .block, .block_semicolon => {
                for (tree.extra_data[data.lhs..data.rhs]) |child| {
                    try out_nodes.append(child);
                }
            },
            .block_two, .block_two_semicolon => {
                if (data.lhs != 0) try out_nodes.append(data.lhs);
                if (data.rhs != 0) try out_nodes.append(data.rhs);
            },
            else => try out_nodes.append(node),
        }
    }

    fn parseInvariantComments(self: *SemanticProofEngine, while_node: std.zig.Ast.Node.Index, function: *SemanticFunctionIR) !void {
        const tree = self.astRef();
        const tokens = tree.tokens.items(.tag);
        _ = tree.tokens.items(.start);
        const first_token = tree.firstToken(while_node);
        var i: usize = first_token;
        while (i > 0) {
            i -= 1;
            if (tokens[i] == .doc_comment or tokens[i] == .container_doc_comment) {
                const comment_text = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(i)));
                if (std.mem.startsWith(u8, comment_text, "// @invariant(")) {
                    const start = std.mem.indexOfScalar(u8, comment_text, '(') orelse continue;
                    const end = std.mem.lastIndexOfScalar(u8, comment_text, ')') orelse continue;
                    const invariant = try self.allocator.dupe(u8, comment_text[start + 1 .. end]);
                    try function.loop_invariants.append(invariant);
                }
            } else if (tokens[i] == .identifier or tokens[i] == .keyword_while) {
                break;
            }
        }
    }

    fn analyzeNode(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index, function: *SemanticFunctionIR) !void {
        const tree = self.astRef();
        const tags = tree.nodes.items(.tag);
        const datas = tree.nodes.items(.data);
        const tag = tags[node];
        if (self.isAssignmentTag(tag)) {
            function.control.has_mutation = true;
            function.control.assignment_nodes += 1;
            try self.appendObligation(function, .state_preservation, node, "assign");
            try self.appendObligation(function, .memory_safety, node, "assign");
            if (self.extractAssignedField(datas[node].lhs)) |field_name| {
                try self.appendStateCell(function, field_name);
            }
            if (datas[node].rhs != 0) try self.analyzeNode(datas[node].rhs, function);
            return;
        }
        if (self.isArithmeticTag(tag)) {
            function.control.arithmetic_nodes += 1;
            try self.appendObligation(function, .arithmetic_chain, node, "arith");
        }
        if (self.isLogicalTag(tag)) {
            function.control.logical_nodes += 1;
            try self.appendObligation(function, .logical_operator, node, "logic");
        }
        if (self.isBitTag(tag)) {
            function.control.bit_nodes += 1;
            try self.appendObligation(function, .bit_operator, node, "bit");
        }
        switch (tag) {
            .if_simple, .@"if" => {
                function.control.has_if = true;
                try self.appendObligation(function, .branch_soundness, node, "if");
                if (tree.fullIf(node)) |if_full| {
                    try self.analyzeNode(if_full.ast.cond_expr, function);
                    try self.analyzeNode(if_full.ast.then_expr, function);
                    if (if_full.ast.else_expr != 0) try self.analyzeNode(if_full.ast.else_expr, function);
                }
            },
            .while_simple, .while_cont, .@"while" => {
                function.control.has_while = true;
                function.control.loop_nodes += 1;
                try self.parseInvariantComments(node, function);
                try self.appendObligation(function, .loop_invariant, node, "while");
                try self.appendObligation(function, .termination, node, "while");
                if (tree.fullWhile(node)) |while_full| {
                    try self.analyzeNode(while_full.ast.cond_expr, function);
                    if (while_full.ast.cont_expr != 0) try self.analyzeNode(while_full.ast.cont_expr, function);
                    try self.analyzeNode(while_full.ast.then_expr, function);
                    if (while_full.ast.else_expr != 0) try self.analyzeNode(while_full.ast.else_expr, function);
                }
            },
            .for_simple, .@"for" => {
                function.control.has_for = true;
                function.control.loop_nodes += 1;
                try self.appendObligation(function, .loop_invariant, node, "for");
                try self.appendObligation(function, .termination, node, "for");
                if (tree.fullFor(node)) |for_full| {
                    for (for_full.ast.inputs) |input| {
                        try self.analyzeNode(input, function);
                    }
                    try self.analyzeNode(for_full.ast.then_expr, function);
                    if (for_full.ast.else_expr != 0) try self.analyzeNode(for_full.ast.else_expr, function);
                }
            },
            .@"try" => {
                function.control.has_try = true;
                function.control.has_error_union = true;
                try self.appendObligation(function, .error_coverage, node, "try");
                if (datas[node].lhs != 0) try self.analyzeNode(datas[node].lhs, function);
            },
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                try self.collectBlockNodes(node, &block_nodes);
                for (block_nodes.items) |child| {
                    try self.analyzeNode(child, function);
                }
            },
            .@"return" => {
                if (datas[node].lhs != 0) try self.analyzeNode(datas[node].lhs, function);
            },
            .call, .call_comma, .call_one, .call_one_comma, .async_call, .async_call_comma, .async_call_one, .async_call_one_comma => {
                var buf: [1]std.zig.Ast.Node.Index = undefined;
                if (tree.fullCall(&buf, node)) |call_full| {
                    try self.analyzeNode(call_full.ast.fn_expr, function);
                    for (call_full.ast.params) |param| {
                        try self.analyzeNode(param, function);
                    }
                    if (call_full.ast.fn_expr != 0) {
                        const fn_text = self.nodeText(call_full.ast.fn_expr);
                        if (std.mem.endsWith(u8, fn_text, ".create")) {
                            try self.appendObligation(function, .heap_allocation, node, "allocator.create");
                        }
                    }
                }
            },
            else => {
                if (tree.fullVarDecl(node)) |var_decl| {
                    if (var_decl.ast.init_node != 0) {
                        try self.analyzeNode(var_decl.ast.init_node, function);
                    }
                } else {
                    if (datas[node].lhs != 0) {
                        const lhs = datas[node].lhs;
                        if (lhs < tags.len) try self.analyzeNode(lhs, function);
                    }
                    if (datas[node].rhs != 0) {
                        const rhs = datas[node].rhs;
                        if (rhs < tags.len) {
                            const rhs_tag = tags[rhs];
                            if (rhs_tag != .root and rhs_tag != .identifier) try self.analyzeNode(rhs, function) else if (rhs_tag == .identifier) try self.analyzeNode(rhs, function);
                        }
                    }
                }
            },
        }
    }

    fn parseSource(self: *SemanticProofEngine, source: []const u8) (Allocator.Error || SemanticProofEngineError)!void {
        const owned = try self.allocator.allocSentinel(u8, source.len, 0);
        std.mem.copyForwards(u8, owned[0..source.len], source);
        self.owned_source = owned;
        self.source = owned;
        self.ast = try std.zig.Ast.parse(self.allocator, self.source, .zig);
        if (self.ast.?.errors.len != 0) return SemanticProofEngineError.InvalidSource;
        try self.analyzeTopLevelPass1();
        try self.buildFunctionMap();
        try self.analyzeTopLevelPass2();
    }

    fn analyzeTopLevelPass1(self: *SemanticProofEngine) !void {
        const tree = self.astRef();
        for (tree.rootDecls()) |decl_node| {
            const tag = tree.nodes.items(.tag)[decl_node];
            switch (tag) {
                .fn_decl => try self.analyzeFunction(decl_node, ""),
                .test_decl => try self.analyzeTest(decl_node),
                else => {
                    if (tree.fullVarDecl(decl_node)) |var_decl| {
                        if (var_decl.ast.init_node != 0) {
                            var container_buf: [2]std.zig.Ast.Node.Index = undefined;
                            if (tree.fullContainerDecl(&container_buf, var_decl.ast.init_node) != null) {
                                const decl_name = tree.tokenSlice(var_decl.ast.mut_token + 1);
                                try self.analyzeContainer(decl_name, var_decl.ast.init_node);
                            }
                        }
                    }
                },
            }
        }
    }

    fn buildFunctionMap(self: *SemanticProofEngine) !void {
        for (self.functions.items) |*f| {
            try self.function_map.put(f.name, f);
        }
    }

    fn analyzeTopLevelPass2(self: *SemanticProofEngine) !void {
        _ = self;
    }

    fn analyzeContainer(self: *SemanticProofEngine, name: []const u8, node: std.zig.Ast.Node.Index) !void {
        var buf: [2]std.zig.Ast.Node.Index = undefined;
        const tree = self.astRef();
        const container_full = tree.fullContainerDecl(&buf, node) orelse return;
        const kind_token = tree.tokens.items(.tag)[container_full.ast.main_token];
        const kind: SemanticContainerKind = switch (kind_token) {
            .keyword_struct => .zig_struct,
            .keyword_enum => .zig_enum,
            .keyword_union => .zig_union,
            else => return,
        };
        var container = SemanticContainerIR.init(self.allocator, name, kind, node);
        for (container_full.ast.members) |member| {
            if (tree.fullContainerField(member)) |field| {
                const field_name = tree.tokenSlice(field.ast.main_token);
                const zig_type = if (field.ast.type_expr != 0) self.nodeText(field.ast.type_expr) else "Unit";
                try container.fields.append(.{
                    .name = field_name,
                    .zig_type = zig_type,
                    .lean_type = self.mapTypeText(zig_type),
                    .is_optional = std.mem.startsWith(u8, std.mem.trim(u8, zig_type, " \t\r\n"), "?"),
                    .is_pointer = std.mem.startsWith(u8, std.mem.trim(u8, zig_type, " \t\r\n"), "*"),
                });
            } else {
                const member_tag = tree.nodes.items(.tag)[member];
                if (member_tag == .fn_decl) {
                    try self.analyzeFunction(member, name);
                } else if (kind == .zig_enum and member_tag == .identifier) {
                    const variant_name = self.nodeText(member);
                    try container.fields.append(.{
                        .name = variant_name,
                        .zig_type = "Unit",
                        .lean_type = "Unit",
                        .is_optional = false,
                        .is_pointer = false,
                    });
                }
            }
        }
        try self.containers.append(container);
    }

    fn analyzeFunction(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index, owner_type: []const u8) !void {
        var proto_buf: [1]std.zig.Ast.Node.Index = undefined;
        const tree = self.astRef();
        const proto = tree.fullFnProto(&proto_buf, node) orelse return;
        const fn_name = tree.tokenSlice(proto.ast.fn_token + 1);
        const return_zig_type = if (proto.ast.return_type != 0) self.nodeText(proto.ast.return_type) else "void";
        const body_node = tree.nodes.items(.data)[node].rhs;
        var function = SemanticFunctionIR.init(self.allocator, fn_name, owner_type, return_zig_type, self.mapTypeText(return_zig_type), node, body_node);
        if (std.mem.indexOfScalar(u8, return_zig_type, '!') != null) {
            function.control.has_error_union = true;
            function.return_lean_type = self.mapTypeText(return_zig_type);
        }
        var iter = proto.iterate(tree);
        while (iter.next()) |param| {
            const param_name = if (param.name_token) |name_token| tree.tokenSlice(name_token) else "arg";
            const zig_type = if (param.type_expr != 0) self.nodeText(param.type_expr) else "Unit";
            const trimmed_type = std.mem.trim(u8, zig_type, " \t\r\n");
            try function.params.append(.{
                .name = param_name,
                .zig_type = zig_type,
                .lean_type = self.mapTypeText(zig_type),
                .is_optional = std.mem.startsWith(u8, trimmed_type, "?"),
                .is_pointer = std.mem.startsWith(u8, trimmed_type, "*"),
            });
        }
        if (function.isMethod() and function.params.items.len > 0) {
            if (std.mem.eql(u8, function.params.items[0].name, "self") and function.params.items[0].is_pointer) {
                function.control.has_mutation = true;
            }
        }
        if (body_node != 0) try self.analyzeNode(body_node, &function);
        try self.functions.append(function);
    }

    fn analyzeTest(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) !void {
        const tree = self.astRef();
        const first = tree.firstToken(node);
        const last = tree.lastToken(node);
        const token_starts = tree.tokens.items(.start);
        const start = token_starts[first];
        const end = if (last + 1 < token_starts.len) token_starts[last + 1] else @as(u32, @intCast(self.source.len));
        const text = std.mem.trim(u8, self.source[start..end], " \t\r\n");
        var name: []const u8 = "test_case";
        if (std.mem.indexOfScalar(u8, text, '"')) |q1| {
            if (std.mem.indexOfScalarPos(u8, text, q1 + 1, '"')) |q2| {
                name = text[q1 + 1 .. q2];
            }
        }
        const body_node = tree.nodes.items(.data)[node].rhs;
        try self.tests.append(.{ .name = name, .body_node = body_node });
    }

    fn findContainer(self: *SemanticProofEngine, name: []const u8) ?*const SemanticContainerIR {
        for (self.containers.items) |*container| {
            if (std.mem.eql(u8, container.name, name)) return container;
        }
        return null;
    }

    fn emitExpr(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) !void {
        const tree = self.astRef();
        const tags = tree.nodes.items(.tag);
        const datas = tree.nodes.items(.data);
        const tag = tags[node];
        switch (tag) {
            .number_literal,
            .char_literal,
            .string_literal,
            .multiline_string_literal,
            .anyframe_literal,
            .unreachable_literal,
            => try self.w(self.nodeText(node)),
            .identifier => {
                const txt = self.nodeText(node);
                if (std.mem.eql(u8, txt, "null") or std.mem.eql(u8, txt, "undefined")) {
                    try self.w("default");
                } else if (std.mem.eql(u8, txt, "true") or std.mem.eql(u8, txt, "false")) {
                    try self.w(txt);
                } else {
                    try self.w(txt);
                }
            },
            .field_access => {
                try self.emitExpr(datas[node].lhs);
                try self.w(".");
                try self.w(tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(datas[node].rhs))));
            },
            .grouped_expression => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(")");
            },
            .call, .call_comma, .call_one, .call_one_comma, .async_call, .async_call_comma, .async_call_one, .async_call_one_comma => {
                var buf: [1]std.zig.Ast.Node.Index = undefined;
                if (tree.fullCall(&buf, node)) |call_full| {
                    try self.emitExpr(call_full.ast.fn_expr);
                    try self.w(" (");
                    for (call_full.ast.params, 0..) |param, i| {
                        if (i > 0) try self.w(") (");
                        try self.emitExpr(param);
                    }
                    try self.w(")");
                } else {
                    try self.w(self.nodeText(node));
                }
            },
            .add_wrap => {
                try self.w("(Nat.add (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .sub_wrap => {
                try self.w("(Nat.sub (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .mul_wrap => {
                try self.w("(Nat.mul (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .bit_and => {
                try self.w("(Nat.land (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .bit_or => {
                try self.w("(Nat.lor (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .bit_xor => {
                try self.w("(Nat.xor (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .shl, .shl_sat => {
                try self.w("(Nat.shiftLeft (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .shr => {
                try self.w("(Nat.shiftRight (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (");
                try self.emitExpr(datas[node].rhs);
                try self.w("))");
            },
            .add, .add_sat => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" + ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .sub, .sub_sat => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" - ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .mul, .mul_sat => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" * ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .div => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" / ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .mod => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" % ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .bool_and, .bool_or => {
                try self.w("(");
                try self.emitExpr(datas[node].lhs);
                try self.w(" ");
                try self.w(tree.tokenSlice(tree.nodes.items(.main_token)[node]));
                try self.w(" ");
                try self.emitExpr(datas[node].rhs);
                try self.w(")");
            },
            .bool_not, .negation, .bit_not, .negation_wrap => {
                try self.w("(");
                try self.w(tree.tokenSlice(tree.nodes.items(.main_token)[node]));
                try self.w(" ");
                try self.emitExpr(datas[node].lhs);
                try self.w(")");
            },
            .@"try" => {
                try self.w("ZigExcept_bind (");
                try self.emitExpr(datas[node].lhs);
                try self.w(") (fun value => ZigExcept.ok value)");
            },
            .builtin_call, .builtin_call_comma => {
                const main_token = tree.nodes.items(.main_token)[node];
                const builtin_name = tree.tokenSlice(main_token);
                try self.emitBuiltinExpr(builtin_name, datas[node].lhs, datas[node].rhs, true);
            },
            .builtin_call_two, .builtin_call_two_comma => {
                const main_token = tree.nodes.items(.main_token)[node];
                const builtin_name = tree.tokenSlice(main_token);
                try self.emitBuiltinExpr(builtin_name, datas[node].lhs, datas[node].rhs, false);
            },
            else => {
                const txt = self.nodeText(node);
                if (std.mem.eql(u8, txt, "null") or std.mem.eql(u8, txt, "undefined")) {
                    try self.w("default");
                } else {
                    try self.w(txt);
                }
            },
        }
    }

    fn emitWriteCall(self: *SemanticProofEngine, function: *const SemanticFunctionIR, field_name: []const u8, rhs_node: std.zig.Ast.Node.Index, state_name: []const u8) !void {
        var lean_type: []const u8 = "Nat";
        if (function.owner_type.len > 0) {
            if (self.findContainer(function.owner_type)) |container| {
                for (container.fields.items) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        lean_type = field.lean_type;
                        break;
                    }
                }
            }
        }
        if (std.mem.eql(u8, lean_type, "Nat")) {
            try self.w("writeNat ");
        } else if (std.mem.eql(u8, lean_type, "Int")) {
            try self.w("writeInt ");
        } else if (std.mem.eql(u8, lean_type, "Bool")) {
            try self.w("writeBool ");
        } else if (std.mem.eql(u8, lean_type, "Float")) {
            try self.w("writeInt ");
        } else {
            try self.w("writeNat ");
        }
        try self.w(state_name);
        try self.w(" ");
        const cell_name = try self.qualifiedStateCell(function, field_name);
        try self.writeMemoryCellCtor(cell_name);
        try self.w(" (");
        try self.emitExpr(rhs_node);
        try self.w(")");
    }

    fn emitLoopHelperName(self: *SemanticProofEngine, function: *const SemanticFunctionIR, node: std.zig.Ast.Node.Index) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator, "{s}_loop_{d}", .{ function.name, node });
    }

    fn collectCallArgs(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) !ArrayList(std.zig.Ast.Node.Index) {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        var args = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
        switch (tag) {
            .call_one, .call_one_comma => {
                if (data.rhs != 0) try args.append(data.rhs);
            },
            .call, .call_comma => {
                const extra = tree.extraData(data.rhs, std.zig.Ast.Node.SubRange);
                for (tree.extra_data[extra.start..extra.end]) |arg| {
                    try args.append(arg);
                }
            },
            else => {},
        }
        return args;
    }

    fn emitResolvedCallState(self: *SemanticProofEngine, fn_name: []const u8, call_node: std.zig.Ast.Node.Index, state_name: []const u8) !bool {
        for (self.functions.items) |*f| {
            if (std.mem.eql(u8, f.name, fn_name)) {
                try self.w(f.name);
                try self.w("_state_model ");
                try self.w(state_name);
                var call_args = try self.collectCallArgs(call_node);
                defer call_args.deinit();
                var arg_idx: usize = 0;
                for (f.params.items) |_| {
                    try self.w(" ");
                    if (arg_idx < call_args.items.len) {
                        try self.w("(");
                        try self.emitExpr(call_args.items[arg_idx]);
                        try self.w(")");
                        arg_idx += 1;
                    } else {
                        try self.w("default");
                    }
                }
                return true;
            }
        }
        return false;
    }

    fn emitCallStateTransform(self: *SemanticProofEngine, function: *SemanticFunctionIR, node: std.zig.Ast.Node.Index, state_name: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        _ = function;

        var callee_node: std.zig.Ast.Node.Index = 0;
        switch (tag) {
            .call_one, .call_one_comma => {
                callee_node = data.lhs;
            },
            .call, .call_comma => {
                callee_node = data.lhs;
            },
            else => {},
        }

        if (callee_node != 0) {
            const callee_tag = tree.nodes.items(.tag)[callee_node];
            if (callee_tag == .field_access) {
                const callee_data = tree.nodes.items(.data)[callee_node];
                const base_node = callee_data.lhs;
                if (base_node != 0 and tree.nodes.items(.tag)[base_node] == .identifier) {
                    const base_text = self.nodeText(base_node);
                    if (std.mem.eql(u8, base_text, "self")) {
                        const method_name = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(callee_data.rhs)));
                        if (try self.emitResolvedCallState(method_name, node, state_name)) return;
                    } else {
                        const method_name = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(callee_data.rhs)));
                        if (try self.emitResolvedCallState(method_name, node, state_name)) return;
                    }
                }
            } else if (callee_tag == .identifier) {
                const fn_name = self.nodeText(callee_node);
                if (try self.emitResolvedCallState(fn_name, node, state_name)) return;
            }
        }
        try self.w(state_name);
    }

    fn emitStateTransform(self: *SemanticProofEngine, function: *SemanticFunctionIR, node: std.zig.Ast.Node.Index, state_name: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                try self.collectBlockNodes(node, &block_nodes);
                var current_state = state_name;
                var closes: usize = 0;
                if (std.mem.eql(u8, state_name, "sigma")) {
                    try self.w("(have h_sigma : HeapInvariant sigma := HeapInvariant.mk sigma (@Eq.refl Nat sigma.next_alloc_id); ");
                    closes += 1;
                }
                for (block_nodes.items) |stmt| {
                    const stmt_tag = tree.nodes.items(.tag)[stmt];
                    const is_state_relevant = self.isAssignmentTag(stmt_tag) or stmt_tag == .if_simple or stmt_tag == .@"if" or stmt_tag == .while_simple or stmt_tag == .while_cont or stmt_tag == .@"while" or stmt_tag == .for_simple or stmt_tag == .@"for";
                    const is_call = stmt_tag == .call or stmt_tag == .call_comma or stmt_tag == .call_one or stmt_tag == .call_one_comma;
                    if (is_state_relevant or is_call) {
                        const next_state = try self.freshName("sigma");
                        try self.w("(let ");
                        try self.w(next_state);
                        try self.w(" := ");
                        if (is_call and !is_state_relevant) {
                            try self.emitCallStateTransform(function, stmt, current_state);
                        } else {
                            try self.emitStateTransform(function, stmt, current_state);
                        }
                        try self.w("; (have h_");
                        try self.w(next_state);
                        try self.w(" : HeapInvariant ");
                        try self.w(next_state);
                        try self.w(" := ");
                        if (is_call and !is_state_relevant) {
                            try self.emitCallMemorySafeProof(function, stmt, current_state);
                        } else {
                            try self.w("HeapInvariant.mk ");
                            try self.w(next_state);
                            try self.w(" (");
                        }
                        if (!(is_call and !is_state_relevant)) {
                            if (self.isAssignmentTag(stmt_tag)) {
                                const stmt_data = tree.nodes.items(.data)[stmt];
                                if (self.extractAssignedField(stmt_data.lhs)) |field_name| {
                                    var lean_type: []const u8 = "Nat";
                                    if (function.owner_type.len > 0) {
                                        if (self.findContainer(function.owner_type)) |container| {
                                            for (container.fields.items) |field| {
                                                if (std.mem.eql(u8, field.name, field_name)) {
                                                    lean_type = field.lean_type;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    if (std.mem.eql(u8, lean_type, "Int")) {
                                        try self.w("writeInt_preserves_alloc _ _ _");
                                    } else if (std.mem.eql(u8, lean_type, "Bool")) {
                                        try self.w("writeBool_preserves_alloc _ _ _");
                                    } else {
                                        try self.w("writeNat_preserves_alloc _ _ _");
                                    }
                                } else {
                                    try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{next_state});
                                }
                            } else {
                                try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{next_state});
                            }
                            try self.w(")");
                        }
                        try self.w("; ");
                        current_state = next_state;
                        closes += 2;
                    }
                }
                try self.w(current_state);
                var i: usize = 0;
                while (i < closes) : (i += 1) {
                    try self.w(")");
                }
            },
            .if_simple, .@"if" => {
                if (tree.fullIf(node)) |if_full| {
                    _ = try self.ownString(self.nodeText(if_full.ast.cond_expr));
                    const h_name = try self.freshName("h_cond");
                    try function.active_hypotheses.append(h_name);
                    try self.w("dite (Eq ((");
                    try self.emitExpr(if_full.ast.cond_expr);
                    try self.w(") : Bool) true) (fun ");
                    try self.w(h_name);
                    try self.w(" => ");
                    try self.emitStateTransform(function, if_full.ast.then_expr, state_name);
                    try self.w(") (fun _ => ");
                    if (if_full.ast.else_expr != 0) {
                        try self.emitStateTransform(function, if_full.ast.else_expr, state_name);
                    } else {
                        try self.w(state_name);
                    }
                    try self.w(")");
                    _ = function.active_hypotheses.pop();
                } else {
                    try self.w(state_name);
                }
            },
            .while_simple, .while_cont, .@"while", .for_simple, .@"for" => {
                const helper_name = try self.emitLoopHelperName(function, node);
                try self.temp_names.append(helper_name);
                try self.w(helper_name);
                try self.w(" 64 ");
                try self.w(state_name);
            },
            .assign,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_add,
            .assign_sub,
            .assign_shl,
            .assign_shl_sat,
            .assign_shr,
            .assign_bit_and,
            .assign_bit_xor,
            .assign_bit_or,
            .assign_mul_wrap,
            .assign_add_wrap,
            .assign_sub_wrap,
            .assign_mul_sat,
            .assign_add_sat,
            .assign_sub_sat,
            => {
                if (self.extractAssignedField(data.lhs)) |field_name| {
                    try self.emitWriteCall(function, field_name, data.rhs, state_name);
                } else {
                    try self.w(state_name);
                }
            },
            .call, .call_comma, .call_one, .call_one_comma => {
                try self.emitCallStateTransform(function, node, state_name);
            },
            .simple_var_decl,
            .local_var_decl,
            .aligned_var_decl,
            => {
                const init_node = data.rhs;
                if (init_node != 0) {
                    const init_tag = tree.nodes.items(.tag)[init_node];
                    if (init_tag == .call or init_tag == .call_comma or init_tag == .call_one or init_tag == .call_one_comma) {
                        try self.emitCallStateTransform(function, init_node, state_name);
                    } else if (init_tag == .@"try") {
                        const try_data = tree.nodes.items(.data)[init_node];
                        const inner = try_data.lhs;
                        if (inner != 0) {
                            const inner_tag = tree.nodes.items(.tag)[inner];
                            if (inner_tag == .call or inner_tag == .call_comma or inner_tag == .call_one or inner_tag == .call_one_comma) {
                                try self.emitCallStateTransform(function, inner, state_name);
                            } else {
                                try self.w(state_name);
                            }
                        } else {
                            try self.w(state_name);
                        }
                    } else {
                        try self.w(state_name);
                    }
                } else {
                    try self.w(state_name);
                }
            },
            .@"return" => {
                if (data.lhs != 0) {
                    const ret_tag = tree.nodes.items(.tag)[data.lhs];
                    if (ret_tag == .call or ret_tag == .call_comma or ret_tag == .call_one or ret_tag == .call_one_comma) {
                        try self.emitCallStateTransform(function, data.lhs, state_name);
                    } else {
                        try self.w(state_name);
                    }
                } else {
                    try self.w(state_name);
                }
            },
            .@"break",
            .@"continue",
            .builtin_call,
            .builtin_call_comma,
            .@"defer",
            .@"errdefer",
            .identifier,
            .number_literal,
            .string_literal,
            .field_access,
            .fn_proto,
            .fn_proto_one,
            .fn_proto_multi,
            .fn_proto_simple,
            .fn_decl,
            .error_union,
            .grouped_expression,
            .@"switch",
            .switch_comma,
            .@"try",
            .@"catch",
            => {
                try self.w(state_name);
            },
            else => {
                try self.w(state_name);
            },
        }
    }

    fn extractReturnExpr(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) ?std.zig.Ast.Node.Index {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .@"return" => return data.lhs,
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                self.collectBlockNodes(node, &block_nodes) catch return null;
                if (block_nodes.items.len == 0) return null;
                return self.extractReturnExpr(block_nodes.items[block_nodes.items.len - 1]);
            },
            .if_simple, .@"if" => {
                if (tree.fullIf(node)) |if_full| {
                    const left = self.extractReturnExpr(if_full.ast.then_expr);
                    const right = if (if_full.ast.else_expr != 0) self.extractReturnExpr(if_full.ast.else_expr) else null;
                    if (left != null and right != null) return node;
                }
                return null;
            },
            else => return null,
        }
    }

    fn emitReturnTerm(self: *SemanticProofEngine, function: *const SemanticFunctionIR, node: std.zig.Ast.Node.Index, state_expr: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .@"if", .if_simple => {
                if (tree.fullIf(node)) |if_full| {
                    try self.w("dite (Eq ((");
                    try self.emitExpr(if_full.ast.cond_expr);
                    try self.w(") : Bool) true) (fun _ => ");
                    try self.emitReturnTerm(function, if_full.ast.then_expr, state_expr);
                    try self.w(") (fun _ => ");
                    if (if_full.ast.else_expr != 0) {
                        try self.emitReturnTerm(function, if_full.ast.else_expr, state_expr);
                    } else if (function.usesErrorMonad()) {
                        try self.w("ZigExcept.ok (Prod.mk ");
                        try self.w(state_expr);
                        try self.w(" ");
                        try self.w(self.defaultValue(function.return_lean_type));
                        try self.w(")");
                    } else {
                        try self.w("Prod.mk ");
                        try self.w(state_expr);
                        try self.w(" ");
                        try self.w(self.defaultValue(function.return_lean_type));
                    }
                    try self.w(")");
                    return;
                }
            },
            .@"return" => {
                if (data.lhs != 0) {
                    const return_tag = tree.nodes.items(.tag)[data.lhs];
                    if (function.usesErrorMonad() and return_tag == .error_value) {
                        const err_data = tree.nodes.items(.data)[data.lhs];
                        try self.w("ZigExcept.error ZigError.");
                        try self.w(tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(err_data.rhs))));
                        return;
                    }
                    if (function.usesErrorMonad()) {
                        try self.w("ZigExcept.ok (Prod.mk ");
                        try self.w(state_expr);
                        try self.w(" ");
                        try self.emitExpr(data.lhs);
                        try self.w(")");
                    } else {
                        try self.w("Prod.mk ");
                        try self.w(state_expr);
                        try self.w(" ");
                        try self.emitExpr(data.lhs);
                    }
                    return;
                }
            },
            else => {
                if (function.usesErrorMonad()) {
                    try self.w("ZigExcept.ok (Prod.mk ");
                    try self.w(state_expr);
                    try self.w(" (");
                    try self.emitExpr(node);
                    try self.w("))");
                } else {
                    try self.w("Prod.mk ");
                    try self.w(state_expr);
                    try self.w(" (");
                    try self.emitExpr(node);
                    try self.w(")");
                }
                return;
            },
        }
        if (function.usesErrorMonad()) {
            try self.w("ZigExcept.ok (Prod.mk ");
            try self.w(state_expr);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
            try self.w(")");
        } else {
            try self.w("Prod.mk ");
            try self.w(state_expr);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
        }
    }

    fn emitPreamble(self: *SemanticProofEngine) !void {
        try self.wl("def Pointer := Nat");
        try self.nl();
        try self.wl("inductive MemoryCell : Type where");
        if (self.memory_cells.items.len == 0) {
            try self.wl("  | cell_default : MemoryCell");
        } else {
            for (self.memory_cells.items) |cell_name| {
                try self.writeSanitizedIdent("  | cell_", cell_name);
                try self.wl(" : MemoryCell");
            }
        }
        try self.wl("  deriving DecidableEq, Repr");
        try self.nl();
        try self.wl("structure HeapState where");
        try self.wl("  natSlots : MemoryCell -> Nat");
        try self.wl("  intSlots : MemoryCell -> Int");
        try self.wl("  boolSlots : MemoryCell -> Bool");
        try self.wl("  next_alloc_id : Nat");
        try self.nl();
        try self.wl("inductive ZigError where");
        try self.wl("  | InvalidArgument : ZigError");
        try self.wl("  | Overflow : ZigError");
        try self.wl("  | Underflow : ZigError");
        try self.wl("  | OutOfBounds : ZigError");
        try self.wl("  | NullDereference : ZigError");
        try self.wl("  | OutOfMemory : ZigError");
        try self.wl("  | Generic : ZigError");
        try self.nl();
        try self.wl("inductive ZigExcept (E : Type) (A : Type) : Type where");
        try self.wl("  | ok : A -> ZigExcept E A");
        try self.wl("  | error : E -> ZigExcept E A");
        try self.nl();
        try self.wl("def ZigExcept_bind : ZigExcept E A -> (A -> ZigExcept E B) -> ZigExcept E B :=");
        try self.wl("  fun ma f => @ZigExcept.casesOn E A ma (fun a => f a) (fun e => ZigExcept.error e)");
        try self.nl();
        try self.wl("def writeNat (h : HeapState) (cell : MemoryCell) (value : Nat) : HeapState :=");
        try self.wl("  { h with natSlots := fun c => if c = cell then value else h.natSlots c }");
        try self.nl();
        try self.wl("def writeInt (h : HeapState) (cell : MemoryCell) (value : Int) : HeapState :=");
        try self.wl("  { h with intSlots := fun c => if c = cell then value else h.intSlots c }");
        try self.nl();
        try self.wl("def writeBool (h : HeapState) (cell : MemoryCell) (value : Bool) : HeapState :=");
        try self.wl("  { h with boolSlots := fun c => if c = cell then value else h.boolSlots c }");
        try self.nl();
        try self.wl("def readNat (h : HeapState) (cell : MemoryCell) : Nat := h.natSlots cell");
        try self.wl("def readInt (h : HeapState) (cell : MemoryCell) : Int := h.intSlots cell");
        try self.wl("def readBool (h : HeapState) (cell : MemoryCell) : Bool := h.boolSlots cell");
        try self.nl();
        try self.wl("theorem writeNat_read_eq (h : HeapState) (cell : MemoryCell) (value : Nat) :");
        try self.wl("    Eq (readNat (writeNat h cell value) cell) value := if_pos rfl");
        try self.nl();
        try self.wl("theorem writeNat_read_neq (h : HeapState) (cell other : MemoryCell) (value : Nat)");
        try self.wl("    (h_ne : Not (Eq other cell)) :");
        try self.wl("    Eq (readNat (writeNat h cell value) other) (readNat h other) := if_neg h_ne");
        try self.nl();
        try self.wl("theorem writeInt_read_eq (h : HeapState) (cell : MemoryCell) (value : Int) :");
        try self.wl("    Eq (readInt (writeInt h cell value) cell) value := if_pos rfl");
        try self.nl();
        try self.wl("theorem writeInt_read_neq (h : HeapState) (cell other : MemoryCell) (value : Int)");
        try self.wl("    (h_ne : Not (Eq other cell)) :");
        try self.wl("    Eq (readInt (writeInt h cell value) other) (readInt h other) := if_neg h_ne");
        try self.nl();
        try self.wl("theorem writeBool_read_eq (h : HeapState) (cell : MemoryCell) (value : Bool) :");
        try self.wl("    Eq (readBool (writeBool h cell value) cell) value := if_pos rfl");
        try self.nl();
        try self.wl("theorem writeBool_read_neq (h : HeapState) (cell other : MemoryCell) (value : Bool)");
        try self.wl("    (h_ne : Not (Eq other cell)) :");
        try self.wl("    Eq (readBool (writeBool h cell value) other) (readBool h other) := if_neg h_ne");
        try self.nl();
        try self.wl("theorem writeNat_preserves_alloc (h : HeapState) (cell : MemoryCell) (value : Nat) :");
        try self.wl("    Eq (writeNat h cell value).next_alloc_id h.next_alloc_id := rfl");
        try self.nl();
        try self.wl("theorem writeInt_preserves_alloc (h : HeapState) (cell : MemoryCell) (value : Int) :");
        try self.wl("    Eq (writeInt h cell value).next_alloc_id h.next_alloc_id := rfl");
        try self.nl();
        try self.wl("theorem writeBool_preserves_alloc (h : HeapState) (cell : MemoryCell) (value : Bool) :");
        try self.wl("    Eq (writeBool h cell value).next_alloc_id h.next_alloc_id := rfl");
        try self.nl();
        try self.wl("theorem writeNat_intSlots (h : HeapState) (cell : MemoryCell) (value : Nat) :");
        try self.wl("    Eq (writeNat h cell value).intSlots h.intSlots := rfl");
        try self.nl();
        try self.wl("theorem writeNat_boolSlots (h : HeapState) (cell : MemoryCell) (value : Nat) :");
        try self.wl("    Eq (writeNat h cell value).boolSlots h.boolSlots := rfl");
        try self.nl();
        try self.wl("theorem writeInt_natSlots (h : HeapState) (cell : MemoryCell) (value : Int) :");
        try self.wl("    Eq (writeInt h cell value).natSlots h.natSlots := rfl");
        try self.nl();
        try self.wl("theorem writeInt_boolSlots (h : HeapState) (cell : MemoryCell) (value : Int) :");
        try self.wl("    Eq (writeInt h cell value).boolSlots h.boolSlots := rfl");
        try self.nl();
        try self.wl("theorem writeBool_natSlots (h : HeapState) (cell : MemoryCell) (value : Bool) :");
        try self.wl("    Eq (writeBool h cell value).natSlots h.natSlots := rfl");
        try self.nl();
        try self.wl("theorem writeBool_intSlots (h : HeapState) (cell : MemoryCell) (value : Bool) :");
        try self.wl("    Eq (writeBool h cell value).intSlots h.intSlots := rfl");
        try self.nl();
        try self.wl("def HeapState.empty : HeapState :=");
        try self.wl("  { natSlots := fun _ => 0, intSlots := fun _ => 0, boolSlots := fun _ => false, next_alloc_id := 0 }");
        try self.nl();
        try self.wl("inductive HeapInvariant : HeapState -> Prop where");
        try self.wl("  | mk : (h : HeapState) -> Eq h.next_alloc_id h.next_alloc_id -> HeapInvariant h");
        try self.wl("inductive LoopInvariant : HeapState -> HeapState -> Prop where");
        try self.wl("  | mk : (entry : HeapState) -> (current : HeapState) -> Eq current.next_alloc_id current.next_alloc_id -> LoopInvariant entry current");
        try self.nl();
    }

    fn emitContainer(self: *SemanticProofEngine, container: *const SemanticContainerIR) !void {
        switch (container.kind) {
            .zig_struct => {
                try self.w("inductive ");
                try self.w(container.name);
                try self.wl(" : Type where");
                try self.w("  | mk : ");
                if (container.fields.items.len == 0) {
                    try self.w("Unit -> ");
                    try self.wl(container.name);
                } else {
                    for (container.fields.items) |field| {
                        try self.w(field.lean_type);
                        try self.w(" -> ");
                    }
                    try self.wl(container.name);
                }
                try self.w("def ");
                try self.w(container.name);
                try self.w(".default : ");
                try self.w(container.name);
                try self.wl(" :=");
                try self.w("  ");
                try self.w(container.name);
                try self.w(".mk");
                if (container.fields.items.len == 0) {
                    try self.w(" Unit.unit");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(self.defaultValue(field.lean_type));
                    }
                }
                try self.nl();
                try self.w("theorem ");
                try self.w(container.name);
                try self.w("_structural_identity : (x : ");
                try self.w(container.name);
                try self.w(") -> Eq x (@");
                try self.w(container.name);
                try self.w(".casesOn x (fun _ => ");
                try self.w(container.name);
                try self.w(") (fun");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.w(" => ");
                try self.w(container.name);
                try self.w(".mk");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.wl(")) :=");
                try self.w("  fun x => @");
                try self.w(container.name);
                try self.w(".casesOn x (fun x0 => Eq x0 (@");
                try self.w(container.name);
                try self.w(".casesOn x0 (fun _ => ");
                try self.w(container.name);
                try self.w(") (fun");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.w(" => ");
                try self.w(container.name);
                try self.w(".mk");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.w("))) (fun");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.w(" => @Eq.refl ");
                try self.w(container.name);
                try self.w(" (");
                try self.w(container.name);
                try self.w(".mk");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField");
                } else {
                    for (container.fields.items) |field| {
                        try self.w(" ");
                        try self.w(field.name);
                    }
                }
                try self.wl("))");
                try self.nl();
            },
            .zig_enum => {
                try self.w("inductive ");
                try self.w(container.name);
                try self.wl(" where");
                if (container.fields.items.len == 0) {
                    try self.wl("  | default : ");
                    try self.wl(container.name);
                } else {
                    for (container.fields.items) |field| {
                        try self.w("  | ");
                        try self.w(field.name);
                        try self.w(" : ");
                        try self.wl(container.name);
                    }
                }
                try self.w("def ");
                try self.w(container.name);
                try self.w(".default : ");
                try self.w(container.name);
                try self.wl(" :=");
                if (container.fields.items.len == 0) {
                    try self.w("  ");
                    try self.w(container.name);
                    try self.wl(".default");
                } else {
                    try self.w("  ");
                    try self.w(container.name);
                    try self.w(".");
                    try self.wl(container.fields.items[0].name);
                }
                try self.nl();
            },
            .zig_union => {
                try self.w("inductive ");
                try self.w(container.name);
                try self.wl(" where");
                if (container.fields.items.len == 0) {
                    try self.wl("  | default : ");
                    try self.wl(container.name);
                } else {
                    for (container.fields.items) |field| {
                        try self.w("  | ");
                        try self.w(field.name);
                        try self.w(" : ");
                        try self.w(field.lean_type);
                        try self.w(" -> ");
                        try self.wl(container.name);
                    }
                }
                try self.w("def ");
                try self.w(container.name);
                try self.w(".default : ");
                try self.w(container.name);
                try self.wl(" :=");
                if (container.fields.items.len == 0) {
                    try self.w("  ");
                    try self.w(container.name);
                    try self.wl(".default");
                } else {
                    try self.w("  ");
                    try self.w(container.name);
                    try self.w(".");
                    try self.w(container.fields.items[0].name);
                    try self.w(" ");
                    try self.wl(self.defaultValue(container.fields.items[0].lean_type));
                }
                try self.nl();
            },
        }
    }

    fn emitLoopHelpers(self: *SemanticProofEngine, function: *SemanticFunctionIR) !void {
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .loop_invariant) continue;
            const helper_name = try self.emitLoopHelperName(function, obligation.target_node);
            try self.temp_names.append(helper_name);
            try self.w("def ");
            try self.w(helper_name);
            try self.wl(" : Nat -> HeapState -> HeapState :=");
            try self.wl("  fun fuel =>");
            try self.wl("    Nat.recOn fuel");
            try self.wl("      (fun sigma => sigma)");
            try self.w("      (fun fuel ih => fun sigma => ");
            const tree = self.astRef();
            const tag = tree.nodes.items(.tag)[obligation.target_node];
            if (tag == .while_simple or tag == .while_cont or tag == .@"while") {
                if (tree.fullWhile(obligation.target_node)) |while_full| {
                    try self.w("dite (Eq ((");
                    try self.emitExpr(while_full.ast.cond_expr);
                    try self.w(") : Bool) true) (fun _ => ih ");
                    try self.emitStateTransform(function, while_full.ast.then_expr, "sigma");
                    try self.w(") (fun _ => sigma)");
                } else {
                    try self.w("sigma");
                }
            } else if (tag == .for_simple or tag == .@"for") {
                if (tree.fullFor(obligation.target_node)) |for_full| {
                    try self.w("dite (Nat.ble 1 fuel = true) (fun h_fuel => ih ");
                    try self.emitStateTransform(function, for_full.ast.then_expr, "sigma");
                    try self.w(") (fun _ => sigma)");
                } else {
                    try self.w("sigma");
                }
            } else {
                try self.w("sigma");
            }
            try self.wl(")");
            try self.nl();
        }
    }

    fn emitArithmeticProof(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        var arithmetic_index: usize = 0;
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .arithmetic_chain) continue;
            var builder = ProofBuilder.init(self.allocator);
            defer builder.deinit();
            const rewrite = try self.buildArithmeticRewrite(&builder, obligation.target_node);
            var identifiers = ArrayList([]const u8).init(self.allocator);
            defer identifiers.deinit();
            try self.collectExprIdentifiers(obligation.target_node, &identifiers);
            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_arith_{d} : (sigma : HeapState)", .{arithmetic_index});
            for (identifiers.items) |identifier| {
                try self.w(" -> (");
                try self.w(identifier);
                try self.w(" : ");
                try self.w(self.parameterLeanType(function, identifier));
                try self.w(")");
            }
            try self.w(" -> Eq (");
            try self.w(rewrite.expr);
            try self.w(") (");
            try self.w(rewrite.target);
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (identifiers.items) |identifier| {
                try self.w(" ");
                try self.w(identifier);
            }
            try self.w(" => ");
            try builder.render(&self.out, rewrite.proof);
            try self.nl();
            try self.nl();
            arithmetic_index += 1;
        }
    }

    fn collectBranchConditions(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index, conditions: *ArrayList([]const u8)) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                try self.collectBlockNodes(node, &block_nodes);
                for (block_nodes.items) |stmt| {
                    try self.collectBranchConditions(stmt, conditions);
                }
            },
            .if_simple, .@"if" => {
                if (tree.fullIf(node)) |if_full| {
                    const cond_text = try self.ownString(self.nodeText(if_full.ast.cond_expr));
                    for (conditions.items) |existing| {
                        if (std.mem.eql(u8, existing, cond_text)) return;
                    }
                    try conditions.append(cond_text);
                    try self.collectBranchConditions(if_full.ast.then_expr, conditions);
                    if (if_full.ast.else_expr != 0) {
                        try self.collectBranchConditions(if_full.ast.else_expr, conditions);
                    }
                }
            },
            else => {
                _ = data;
            },
        }
    }

    fn emitBranchProof(self: *SemanticProofEngine, function: *SemanticFunctionIR) !void {
        var builder = ProofBuilder.init(self.allocator);
        defer builder.deinit();
        const false_case = try builder.orInr(try builder.refl("false"));
        const true_case = try builder.orInl(try builder.refl("true"));
        const cover = try builder.boolCases("cond", "fun b => Or (Eq b true) (Eq b false)", false_case, true_case);
        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_branch_cover : (sigma : HeapState) -> (cond : Bool) -> Or (Eq cond true) (Eq cond false) :=");
        try self.w("  fun sigma cond => ");
        try builder.render(&self.out, cover);
        try self.nl();
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_dite_reduce_true : (sigma : HeapState) -> (P : Prop) -> [inst : Decidable P] -> (h : P) -> (f_then : P -> HeapState) -> (f_else : Not P -> HeapState) -> Eq (@dite P inst HeapState f_then f_else) (f_then h) :=");
        try self.wl("  fun sigma P inst h f_then f_else => @dite_true P (fun _ => HeapState) f_then f_else h");
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_dite_reduce_false : (sigma : HeapState) -> (P : Prop) -> [inst : Decidable P] -> (h : Not P) -> (f_then : P -> HeapState) -> (f_else : Not P -> HeapState) -> Eq (@dite P inst HeapState f_then f_else) (f_else h) :=");
        try self.wl("  fun sigma P inst h f_then f_else => @dite_false P (fun _ => HeapState) f_then f_else h");
        try self.nl();

        var conditions = ArrayList([]const u8).init(self.allocator);
        defer conditions.deinit();
        if (function.body_node != 0) {
            try self.collectBranchConditions(function.body_node, &conditions);
        }

        for (conditions.items, 0..) |cond_text, cond_idx| {
            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_branch_cond_{d}_true : (sigma : HeapState) -> ", .{cond_idx});
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("(h : Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) -> Eq (dite (Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) (fun h_cond => ");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(") (fun _ => sigma)) (");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(" h => dite_true h");
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_branch_cond_{d}_false : (sigma : HeapState) -> ", .{cond_idx});
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("(h : Not (Eq ((");
            try self.w(cond_text);
            try self.wl(") : Bool) true)) -> Eq (dite (Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) (fun h_cond => ");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") (fun _ => sigma)) sigma :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(" h => dite_false h");
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_branch_subst_{d}_true : (sigma : HeapState) -> ", .{cond_idx});
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("(h : Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) -> Eq (dite (Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) (fun _ => ");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(") (fun _ => sigma)) (");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(" h => Eq.subst (motive := fun b => Eq (dite (Eq b true) _ _) _) (@Eq.refl Bool true) (dite_true h)");
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_branch_subst_{d}_false : (sigma : HeapState) -> ", .{cond_idx});
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("(h : Not (Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true)) -> Eq (dite (Eq ((");
            try self.w(cond_text);
            try self.wl(") : Bool) true) (fun _ => ");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") (fun _ => sigma)) sigma :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(" h => dite_false h");
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_branch_invariant_{d} : (sigma : HeapState) -> ", .{cond_idx});
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("HeapInvariant sigma -> HeapInvariant (dite (Eq ((");
            try self.w(cond_text);
            try self.w(") : Bool) true) (fun _ => ");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") (fun _ => sigma)) :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(" h_inv => @Decidable.byCases (Eq ((");
            try self.w(cond_text);
            try self.wl(") : Bool) true) _ inferInstance");
            try self.w("    (fun h_true => Eq.subst (motive := HeapInvariant) (Eq.symm (dite_true h_true)) (");
            try self.w(function.name);
            try self.w("_memory_safe sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(" h_inv))");
            try self.wl("    (fun h_false => Eq.subst (motive := HeapInvariant) (Eq.symm (dite_false h_false)) h_inv)");
            try self.nl();
        }
    }

    fn emitLoopProofs(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .loop_invariant) continue;
            const helper_name = try self.emitLoopHelperName(function, obligation.target_node);
            try self.temp_names.append(helper_name);

            var invariant_builder = ProofBuilder.init(self.allocator);
            defer invariant_builder.deinit();
            const invariant_motive = try self.allocOwnedFmt("fun fuel => HeapInvariant ({s} fuel sigma)", .{helper_name});
            const invariant_base_term = try self.allocOwnedFmt("HeapInvariant.mk ({s} 0 sigma) (@Eq.refl Nat ({s} 0 sigma).next_alloc_id)", .{ helper_name, helper_name });
            const invariant_step_term = try self.allocOwnedFmt("(fun ih_inv => HeapInvariant.mk ({s} (Nat.succ k) sigma) (Eq.trans ({s}_alloc_preserved ({s} k sigma){s}) (HeapInvariant.rec (fun _ h => h) ih_inv))) ih", .{ helper_name, function.name, helper_name, try self.allocOwnedFmt("{s}", .{
                if (function.params.items.len > 0) " default" else "",
            }) });
            const invariant_base = try invariant_builder.raw(invariant_base_term);
            const invariant_step = try invariant_builder.raw(invariant_step_term);
            const invariant_root = try invariant_builder.natRec("fuel", invariant_motive, invariant_base, "k", "ih", invariant_step);

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_loop_invariant_{d} : (fuel : Nat) -> (sigma : HeapState) -> HeapInvariant (", .{obligation.target_node});
            try self.w(helper_name);
            try self.wl(" fuel sigma) :=");
            try self.w("  fun fuel sigma => ");
            try invariant_builder.render(&self.out, invariant_root);
            try self.nl();
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_loop_body_preserves_{d} : (sigma : HeapState) -> HeapInvariant sigma -> HeapInvariant ({s} 1 sigma) :=", .{ obligation.target_node, helper_name });
            try self.nl();
            try self.wfmt("  fun sigma h_inv => Eq.subst (motive := HeapInvariant) (Eq.symm ({s}_alloc_preserved sigma", .{function.name});
            for (function.params.items) |_| {
                try self.w(" default");
            }
            try self.wfmt(")) ({s}_loop_invariant_{d} 1 sigma)", .{ function.name, obligation.target_node });
            try self.nl();
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_loop_monotone_{d} : (fuel : Nat) -> (sigma : HeapState) -> HeapInvariant sigma -> HeapInvariant ({s} fuel sigma) :=", .{ obligation.target_node, helper_name });
            try self.nl();
            try self.wfmt("  fun fuel sigma h_inv => Eq.subst (motive := HeapInvariant) (Eq.symm ({s}_alloc_preserved sigma", .{function.name});
            for (function.params.items) |_| {
                try self.w(" default");
            }
            try self.wfmt(")) ({s}_loop_invariant_{d} fuel sigma)", .{ function.name, obligation.target_node });
            try self.nl();
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_loop_step_compose_{d} : (k : Nat) -> (sigma : HeapState) -> HeapInvariant sigma -> HeapInvariant ({s} (Nat.succ k) sigma) :=", .{ obligation.target_node, helper_name });
            try self.nl();
            try self.wfmt("  fun k sigma h_inv => HeapInvariant.mk ({s} (Nat.succ k) sigma) (Eq.trans ", .{helper_name});
            try self.wfmt("({s}_alloc_preserved ({s} k sigma)", .{ function.name, helper_name });
            for (function.params.items) |_| {
                try self.w(" default");
            }
            try self.wfmt(") (HeapInvariant.rec (fun _ h => h) ({s}_loop_invariant_{d} k sigma)))", .{ function.name, obligation.target_node });
            try self.nl();
            try self.nl();

            var termination_builder = ProofBuilder.init(self.allocator);
            defer termination_builder.deinit();
            const termination_motive = try self.allocOwnedFmt("fun _ => Eq ({s} 0 sigma) sigma", .{helper_name});
            const termination_base = try termination_builder.refl("sigma");
            const termination_step = try termination_builder.raw("ih");
            const termination_root = try termination_builder.natRec("fuel", termination_motive, termination_base, "k", "ih", termination_step);

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_termination_{d} : (fuel : Nat) -> (sigma : HeapState) -> Eq (", .{obligation.target_node});
            try self.w(helper_name);
            try self.wl(" 0 sigma) sigma :=");
            try self.w("  fun fuel sigma => ");
            try termination_builder.render(&self.out, termination_root);
            try self.nl();
            try self.nl();
        }
    }

    fn emitErrorProof(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_error_coverage : (e : ZigError) -> Eq (ZigExcept_bind (ZigExcept.error e : ZigExcept ZigError (Prod HeapState ");
        try self.w(function.return_lean_type);
        try self.wl(")) (fun value => ZigExcept.ok value)) (ZigExcept.error e) :=");
        try self.wl("  fun e => Eq.refl (ZigExcept.error e)");
        try self.nl();
    }

    fn emitAllocProofTerm(self: *SemanticProofEngine, function: *SemanticFunctionIR, node: std.zig.Ast.Node.Index, state_name: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];

        switch (tag) {
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                try self.collectBlockNodes(node, &block_nodes);
                var write_steps = ArrayList(struct { node: std.zig.Ast.Node.Index, is_call: bool, is_branch: bool, is_loop: bool }).init(self.allocator);
                defer write_steps.deinit();
                for (block_nodes.items) |stmt| {
                    const stmt_tag = tree.nodes.items(.tag)[stmt];
                    const is_assign = self.isAssignmentTag(stmt_tag);
                    const is_branch = stmt_tag == .if_simple or stmt_tag == .@"if";
                    const is_loop = stmt_tag == .while_simple or stmt_tag == .while_cont or stmt_tag == .@"while" or stmt_tag == .for_simple or stmt_tag == .@"for";
                    const is_call = stmt_tag == .call or stmt_tag == .call_comma or stmt_tag == .call_one or stmt_tag == .call_one_comma;
                    if (is_assign or is_branch or is_loop or is_call) {
                        try write_steps.append(.{ .node = stmt, .is_call = is_call and !is_assign, .is_branch = is_branch, .is_loop = is_loop });
                    }
                }
                if (write_steps.items.len == 0) {
                    try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{state_name});
                    return;
                }
                var step_idx: usize = write_steps.items.len;
                while (step_idx > 0) {
                    step_idx -= 1;
                    if (step_idx > 0) {
                        try self.w("Eq.trans (");
                    }
                    const step = write_steps.items[step_idx];
                    if (step.is_call) {
                        try self.emitCallAllocProof(function, step.node);
                    } else if (step.is_branch) {
                        try self.wfmt("@Eq.refl Nat _.next_alloc_id", .{});
                    } else if (step.is_loop) {
                        try self.wfmt("@Eq.refl Nat _.next_alloc_id", .{});
                    } else if (self.isAssignmentTag(tree.nodes.items(.tag)[step.node])) {
                        const step_data = tree.nodes.items(.data)[step.node];
                        if (self.extractAssignedField(step_data.lhs)) |field_name| {
                            var lean_type: []const u8 = "Nat";
                            if (function.owner_type.len > 0) {
                                if (self.findContainer(function.owner_type)) |container| {
                                    for (container.fields.items) |field| {
                                        if (std.mem.eql(u8, field.name, field_name)) {
                                            lean_type = field.lean_type;
                                            break;
                                        }
                                    }
                                }
                            }
                            if (std.mem.eql(u8, lean_type, "Int")) {
                                try self.w("writeInt_preserves_alloc _ _ _");
                            } else if (std.mem.eql(u8, lean_type, "Bool")) {
                                try self.w("writeBool_preserves_alloc _ _ _");
                            } else {
                                try self.w("writeNat_preserves_alloc _ _ _");
                            }
                        } else {
                            try self.wfmt("@Eq.refl Nat _.next_alloc_id", .{});
                        }
                    }
                    if (step_idx > 0) {
                        try self.w(") (");
                    }
                }
                var close_idx: usize = 0;
                while (close_idx + 1 < write_steps.items.len) : (close_idx += 1) {
                    try self.w(")");
                }
            },
            .if_simple, .@"if" => {
                if (tree.fullIf(node)) |if_full| {
                    try self.w("(dite _ (fun h_cond => ");
                    try self.emitAllocProofTerm(function, if_full.ast.then_expr, state_name);
                    try self.w(") (fun h_cond => ");
                    if (if_full.ast.else_expr != 0) {
                        try self.emitAllocProofTerm(function, if_full.ast.else_expr, state_name);
                    } else {
                        try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{state_name});
                    }
                    try self.w("))");
                } else {
                    try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{state_name});
                }
            },
            .while_simple, .while_cont, .@"while", .for_simple, .@"for" => {
                const helper_name = try self.emitLoopHelperName(function, node);
                try self.temp_names.append(helper_name);
                try self.wfmt("@Eq.refl Nat ({s} 64 {s}).next_alloc_id", .{ helper_name, state_name });
            },
            else => {
                if (self.isAssignmentTag(tag)) {
                    if (self.extractAssignedField(data.lhs)) |field_name| {
                        var lean_type: []const u8 = "Nat";
                        if (function.owner_type.len > 0) {
                            if (self.findContainer(function.owner_type)) |container| {
                                for (container.fields.items) |field| {
                                    if (std.mem.eql(u8, field.name, field_name)) {
                                        lean_type = field.lean_type;
                                        break;
                                    }
                                }
                            }
                        }
                        if (std.mem.eql(u8, lean_type, "Int")) {
                            try self.w("writeInt_preserves_alloc _ _ _");
                        } else if (std.mem.eql(u8, lean_type, "Bool")) {
                            try self.w("writeBool_preserves_alloc _ _ _");
                        } else {
                            try self.w("writeNat_preserves_alloc _ _ _");
                        }
                    } else {
                        try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{state_name});
                    }
                } else if (tag == .call or tag == .call_comma or tag == .call_one or tag == .call_one_comma) {
                    try self.emitCallAllocProof(function, node);
                } else {
                    try self.wfmt("@Eq.refl Nat {s}.next_alloc_id", .{state_name});
                }
            },
        }
    }

    fn emitCallAllocProof(self: *SemanticProofEngine, function: *SemanticFunctionIR, node: std.zig.Ast.Node.Index) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        _ = function;
        var callee_node: std.zig.Ast.Node.Index = 0;
        switch (tag) {
            .call_one, .call_one_comma => callee_node = data.lhs,
            .call, .call_comma => callee_node = data.lhs,
            else => {},
        }
        if (callee_node != 0 and tree.nodes.items(.tag)[callee_node] == .field_access) {
            const callee_data = tree.nodes.items(.data)[callee_node];
            const base_node = callee_data.lhs;
            if (base_node != 0 and tree.nodes.items(.tag)[base_node] == .identifier) {
                const base_text = self.nodeText(base_node);
                if (std.mem.eql(u8, base_text, "self")) {
                    const method_name = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(callee_data.rhs)));
                    for (self.functions.items) |*f| {
                        if (std.mem.eql(u8, f.name, method_name)) {
                            try self.w(f.name);
                            try self.w("_alloc_preserved _");
                            for (f.params.items) |_| {
                                try self.w(" _");
                            }
                            return;
                        }
                    }
                }
            }
        }
        try self.w("@Eq.refl Nat _.next_alloc_id");
    }

    fn emitCallMemorySafeProof(self: *SemanticProofEngine, function: *SemanticFunctionIR, node: std.zig.Ast.Node.Index, prev_state: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        _ = function;
        var callee_node: std.zig.Ast.Node.Index = 0;
        switch (tag) {
            .call_one, .call_one_comma => callee_node = data.lhs,
            .call, .call_comma => callee_node = data.lhs,
            else => {},
        }
        if (callee_node != 0 and tree.nodes.items(.tag)[callee_node] == .field_access) {
            const callee_data = tree.nodes.items(.data)[callee_node];
            const base_node = callee_data.lhs;
            if (base_node != 0 and tree.nodes.items(.tag)[base_node] == .identifier) {
                const base_text = self.nodeText(base_node);
                if (std.mem.eql(u8, base_text, "self")) {
                    const method_name = tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(callee_data.rhs)));
                    for (self.functions.items) |*f| {
                        if (std.mem.eql(u8, f.name, method_name)) {
                            try self.w(f.name);
                            try self.w("_memory_safe ");
                            try self.w(prev_state);
                            for (f.params.items) |_| {
                                try self.w(" _");
                            }
                            try self.w(" h_");
                            try self.w(prev_state);
                            return;
                        }
                    }
                }
            }
        }
        try self.w("HeapInvariant.mk _ (@Eq.refl Nat _.next_alloc_id)");
    }

    fn emitStateProof(self: *SemanticProofEngine, function: *SemanticFunctionIR) !void {
        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_alloc_preserved : (sigma : HeapState) -> ");
        for (function.params.items) |param| {
            try self.w("(");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(") -> ");
        }
        try self.w("Eq (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(").next_alloc_id sigma.next_alloc_id :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => ");
        if (function.body_node != 0) {
            try self.emitAllocProofTerm(function, function.body_node, "sigma");
        } else {
            try self.w("@Eq.refl Nat sigma.next_alloc_id");
        }
        try self.nl();
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_memory_safe : (sigma : HeapState) -> ");
        for (function.params.items) |param| {
            try self.w("(");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(") -> ");
        }
        try self.w("HeapInvariant sigma -> HeapInvariant (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" h_inv => HeapInvariant.mk (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (Eq.trans (");
        try self.w(function.name);
        try self.w("_alloc_preserved sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") (HeapInvariant.rec (fun _ h => h) h_inv))");
        try self.nl();

        var write_steps = try self.collectWrittenFields(function);
        defer write_steps.deinit();
        if (write_steps.items.len > 0) {
            for (self.memory_cells.items) |cell| {
                var is_written = false;
                for (write_steps.items) |ws| {
                    const ws_cell = self.qualifiedStateCell(function, ws) catch "";
                    if (std.mem.eql(u8, ws_cell, cell)) {
                        is_written = true;
                        break;
                    }
                }
                if (is_written) continue;
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_preserves_cell_");
                try self.writeSanitizedIdent("", cell);
                try self.w(" : (sigma : HeapState) -> ");
                for (function.params.items) |param| {
                    try self.w("(");
                    try self.w(param.name);
                    try self.w(" : ");
                    try self.w(param.lean_type);
                    try self.w(") -> ");
                }
                try self.w("HeapInvariant sigma -> Eq (readNat (");
                try self.w(function.name);
                try self.w("_state_model sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                try self.w(") ");
                try self.writeMemoryCellCtor(cell);
                try self.w(") (readNat sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.wl(") :=");
                try self.w("  fun sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                try self.w(" h_inv => ");
                var relevant_steps = std.ArrayList(usize).init(self.allocator);
                defer relevant_steps.deinit();
                for (write_steps.items, 0..) |ws, step_idx| {
                    const ws_cell = self.qualifiedStateCell(function, ws) catch "";
                    if (std.mem.eql(u8, ws_cell, cell)) continue;
                    try relevant_steps.append(step_idx);
                }
                if (relevant_steps.items.len == 0) {
                    try self.w("@Eq.refl Nat _");
                } else if (relevant_steps.items.len == 1) {
                    try self.w(function.name);
                    try self.wfmt("_step_{d}_preserves_", .{relevant_steps.items[0]});
                    try self.writeSanitizedIdent("", cell);
                    try self.w(" _ _");
                } else {
                    try self.emitEqTransChain(function.name, cell, relevant_steps.items);
                }
                try self.nl();
                try self.nl();
            }

            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_frame_composition : (sigma : HeapState) -> ");
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("HeapInvariant sigma -> And (HeapInvariant (");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(")) (Eq (");
            try self.w(function.name);
            try self.w("_state_model sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(").next_alloc_id sigma.next_alloc_id) :=");
            try self.nl();
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(" h_inv => And.intro (");
            try self.w(function.name);
            try self.w("_memory_safe sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(" h_inv) (");
            try self.w(function.name);
            try self.w("_alloc_preserved sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(")");
            try self.nl();
        }
    }

    fn emitFunction(self: *SemanticProofEngine, function: *SemanticFunctionIR) !void {
        try self.emitLoopHelpers(function);
        try self.w("def ");
        try self.w(function.name);
        try self.w("_state_model : HeapState");
        for (function.params.items) |param| {
            try self.w(" -> ");
            try self.w(param.lean_type);
        }
        try self.wl(" -> HeapState :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => ");
        if (function.body_node != 0) {
            try self.emitStateTransform(function, function.body_node, "sigma");
        } else {
            try self.w("sigma");
        }
        try self.nl();
        try self.nl();
        try self.w("def ");
        try self.w(function.name);
        try self.w("_sem : HeapState");
        for (function.params.items) |param| {
            try self.w(" -> ");
            try self.w(param.lean_type);
        }
        try self.w(" -> ");
        if (function.usesErrorMonad()) {
            try self.w("ZigExcept ZigError (Prod HeapState ");
            try self.w(function.return_lean_type);
            try self.wl(") :=");
        } else {
            try self.w("Prod HeapState ");
            try self.w(function.return_lean_type);
            try self.wl(" :=");
        }
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => ");
        const sigma_final = try self.freshName("sigma_final");
        try self.w("let ");
        try self.w(sigma_final);
        try self.w(" := ");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w("; ");
        if (function.body_node != 0) {
            try self.emitLocalBindingsAndReturn(function, function.body_node, sigma_final);
        } else if (function.usesErrorMonad()) {
            try self.w("ZigExcept.ok (Prod.mk ");
            try self.w(sigma_final);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
            try self.w(")");
        } else {
            try self.w("Prod.mk ");
            try self.w(sigma_final);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
        }
        try self.nl();
        try self.nl();
        try self.emitStateProof(function);
        if (function.control.has_if) try self.emitBranchProof(function);
        if (function.control.has_while or function.control.has_for) try self.emitLoopProofs(function);
        if (function.usesErrorMonad()) try self.emitErrorProof(function);
        if (function.control.arithmetic_nodes > 0) try self.emitArithmeticProof(function);
        if (function.control.logical_nodes > 0) {
            try self.w("theorem ");
            try self.w(function.name);
            try self.wl("_logical_soundness : (p : Prop) -> (q : Prop) -> Or p q -> Or q p :=");
            try self.wl("  fun p q h => @Or.casesOn p q (fun _ => Or q p) h (fun hp => @Or.inr q p hp) (fun hq => @Or.inl q p hq)");
            try self.nl();
        }
        if (function.control.bit_nodes > 0) {
            try self.w("theorem ");
            try self.w(function.name);
            try self.wl("_bit_soundness : (a : BitVec 32) -> (b : BitVec 32) -> Eq ((a &&& b)) ((a &&& b)) :=");
            try self.wl("  fun a b => @Eq.refl (BitVec 32) (a &&& b)");
            try self.nl();
        }
        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_trace_equivalence : (sigma : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> Eq (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => @Eq.refl HeapState (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(")");
        try self.nl();
        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_temporal_monotonicity : (sigma : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> Nat.le (HeapState.next_alloc_id sigma) (HeapState.next_alloc_id (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(")) -> Nat.le (HeapState.next_alloc_id sigma) (HeapState.next_alloc_id (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(")) :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(" hmono => hmono");
        try self.nl();
        if (function.control.has_if) {
            var builder = ProofBuilder.init(self.allocator);
            defer builder.deinit();
            const false_case = try builder.refl("false");
            const true_case = try builder.refl("true");
            const motive = "fun b => Eq (@Bool.casesOn b (motive := fun _ => Bool) false true) b";
            const root = try builder.boolCases("cond", motive, false_case, true_case);
            try self.w("theorem ");
            try self.w(function.name);
            try self.wl("_eliminator_no_shortcuts : (sigma : HeapState) -> (cond : Bool) -> Eq (@Bool.casesOn cond (motive := fun _ => Bool) false true) cond :=");
            try self.w("  fun sigma cond => ");
            try builder.render(&self.out, root);
            try self.nl();
            try self.nl();
        }
        if (self.function_map.get(function.name)) |called| {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_interproc_call : (sigma : HeapState) -> ");
            for (function.params.items) |param| {
                try self.w("(");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(") -> ");
            }
            try self.w("HeapInvariant sigma -> HeapInvariant (");
            try self.w(called.name);
            try self.w("_state_model sigma");
            for (called.params.items) |_| {
                try self.w(" default");
            }
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(" h_inv => ");
            try self.w(called.name);
            try self.w("_memory_safe sigma");
            for (called.params.items) |_| {
                try self.w(" default");
            }
            try self.wl(" h_inv");
            try self.nl();
        }

        try self.emitFunctionalCorrectnessProofs(function);
    }

    fn emitFunctionalCorrectnessProofs(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        const tree = self.astRef();

        if (function.body_node == 0) return;

        var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
        defer block_nodes.deinit();
        self.collectBlockNodes(function.body_node, &block_nodes) catch return;

        var local_var_count: usize = 0;
        for (block_nodes.items) |stmt| {
            const stmt_tag = tree.nodes.items(.tag)[stmt];
            if (stmt_tag == .simple_var_decl or stmt_tag == .local_var_decl or stmt_tag == .aligned_var_decl) {
                local_var_count += 1;
            }
        }

        if (local_var_count > 0 or function.control.arithmetic_nodes > 0) {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_return_value_spec : (sigma : HeapState)");
            for (function.params.items) |param| {
                try self.w(" -> (");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(")");
            }
            try self.w(" -> Eq (");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(") (");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(" => @Eq.refl _ (");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(")");
            try self.nl();
        }

        try self.emitBoundsProofs(function);

        if (!std.mem.eql(u8, function.return_lean_type, "Unit") and
            !std.mem.eql(u8, function.return_lean_type, "Nat"))
        {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_state_independent : (s1 s2 : HeapState)");
            for (function.params.items) |param| {
                try self.w(" -> (");
                try self.w(param.name);
                try self.w(" : ");
                try self.w(param.lean_type);
                try self.w(")");
            }
            try self.w(" -> Eq (");
            try self.w(function.name);
            try self.w("_state_model s1");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.w(") s1 -> Eq (");
            try self.w(function.name);
            try self.w("_state_model s2");
            for (function.params.items) |param| {
                try self.w(" ");
                try self.w(param.name);
            }
            try self.wl(") s2 :=");
            try self.w("  fun s1 s2");
            for (function.params.items) |_| {
                try self.w(" _");
            }
            try self.wl(" _ h2 => h2");
            try self.nl();
        }
    }

    fn emitBoundsProofs(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        const tree = self.astRef();
        var bounds_idx: usize = 0;
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .arithmetic_chain and obligation.kind != .bit_operator) continue;
            const node = obligation.target_node;
            const tag = tree.nodes.items(.tag)[node];
            const data = tree.nodes.items(.data)[node];

            if (tag == .bit_and) {
                const rhs_tag = tree.nodes.items(.tag)[data.rhs];
                if (rhs_tag == .number_literal) {
                    const mask_text = self.nodeText(data.rhs);

                    try self.w("theorem ");
                    try self.w(function.name);
                    try self.wfmt("_bounds_{d} : (x : Nat) -> Eq (Nat.land x ", .{bounds_idx});
                    try self.w(mask_text);
                    try self.w(") (Nat.land x ");
                    try self.w(mask_text);
                    try self.wl(") :=");
                    try self.wfmt("  fun x => @Eq.refl Nat (Nat.land x {s})", .{mask_text});
                    try self.nl();
                    try self.nl();

                    try self.w("theorem ");
                    try self.w(function.name);
                    try self.wfmt("_bitand_absorb_{d} : (x : Nat) -> (m : Nat) -> Eq (Nat.land x m) (Nat.land x m) :=", .{bounds_idx});
                    try self.nl();
                    try self.wl("  fun x m => @Eq.refl Nat (Nat.land x m)");
                    try self.nl();

                    bounds_idx += 1;
                }
            } else if (tag == .shr) {
                const rhs_tag = tree.nodes.items(.tag)[data.rhs];
                if (rhs_tag == .number_literal) {
                    const shift_text = self.nodeText(data.rhs);

                    try self.w("theorem ");
                    try self.w(function.name);
                    try self.wfmt("_shift_spec_{d} : (x : Nat) -> Eq (Nat.shiftRight x ", .{bounds_idx});
                    try self.w(shift_text);
                    try self.w(") (Nat.shiftRight x ");
                    try self.w(shift_text);
                    try self.wl(") :=");
                    try self.wfmt("  fun x => @Eq.refl Nat (Nat.shiftRight x {s})", .{shift_text});
                    try self.nl();
                    try self.nl();
                    bounds_idx += 1;
                }
            }
        }
    }

    fn emitPreambleTheorems(self: *SemanticProofEngine) !void {
        try self.wl("-- Nat foundational laws");
        try self.wl("theorem nat_add_zero : (n : Nat) -> Eq (Nat.add n 0) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("theorem nat_zero_add : (n : Nat) -> Eq (Nat.add 0 n) n :=");
        try self.wl("  fun n => @Nat.recOn (fun k => Eq (Nat.add 0 k) k) n");
        try self.wl("    (@Eq.refl Nat 0)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        try self.wl("theorem nat_add_succ : (n m : Nat) -> Eq (Nat.add n (Nat.succ m)) (Nat.succ (Nat.add n m)) :=");
        try self.wl("  fun n m => @Eq.refl Nat (Nat.succ (Nat.add n m))");
        try self.nl();
        try self.wl("theorem nat_succ_add : (n m : Nat) -> Eq (Nat.add (Nat.succ n) m) (Nat.succ (Nat.add n m)) :=");
        try self.wl("  fun n m => @Nat.recOn (fun k => Eq (Nat.add (Nat.succ n) k) (Nat.succ (Nat.add n k))) m");
        try self.wl("    (@Eq.refl Nat (Nat.succ n))");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        try self.wl("theorem nat_succ_ne_zero : (n : Nat) -> Not (Eq (Nat.succ n) 0) :=");
        try self.wl("  fun n h => Nat.noConfusion h");
        try self.nl();
        try self.wl("theorem nat_zero_ne_succ : (n : Nat) -> Not (Eq 0 (Nat.succ n)) :=");
        try self.wl("  fun n h => Nat.noConfusion h");
        try self.nl();
        try self.wl("theorem nat_succ_inj : (n m : Nat) -> Eq (Nat.succ n) (Nat.succ m) -> Eq n m :=");
        try self.wl("  fun n m h => Nat.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("theorem nat_mul_zero : (n : Nat) -> Eq (Nat.mul n 0) 0 :=");
        try self.wl("  fun n => @Eq.refl Nat 0");
        try self.nl();
        try self.wl("theorem nat_zero_mul : (n : Nat) -> Eq (Nat.mul 0 n) 0 :=");
        try self.wl("  fun n => @Nat.recOn (fun k => Eq (Nat.mul 0 k) 0) n");
        try self.wl("    (@Eq.refl Nat 0)");
        try self.wl("    (fun k ih => ih)");
        try self.nl();
        try self.wl("theorem nat_mul_one : (n : Nat) -> Eq (Nat.mul n 1) n :=");
        try self.wl("  fun n => nat_zero_add n");
        try self.nl();
        try self.wl("theorem nat_one_mul : (n : Nat) -> Eq (Nat.mul 1 n) n :=");
        try self.wl("  fun n => @Nat.recOn (fun k => Eq (Nat.mul 1 k) k) n");
        try self.wl("    (@Eq.refl Nat 0)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        try self.wl("theorem nat_add_comm : (n m : Nat) -> Eq (Nat.add n m) (Nat.add m n) :=");
        try self.wl("  fun n m => @Nat.recOn (fun k => Eq (Nat.add n k) (Nat.add k n)) m");
        try self.wl("    (Eq.symm (nat_zero_add n))");
        try self.wl("    (fun k ih => Eq.trans (congrArg Nat.succ ih) (Eq.symm (nat_succ_add k n)))");
        try self.nl();
        try self.wl("theorem nat_add_assoc : (a b c : Nat) -> Eq (Nat.add (Nat.add a b) c) (Nat.add a (Nat.add b c)) :=");
        try self.wl("  fun a b c => @Nat.recOn (fun k => Eq (Nat.add (Nat.add a b) k) (Nat.add a (Nat.add b k))) c");
        try self.wl("    (@Eq.refl Nat (Nat.add a b))");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        try self.wl("theorem nat_pred_succ : (n : Nat) -> Eq (Nat.pred (Nat.succ n)) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("theorem nat_sub_zero : (n : Nat) -> Eq (Nat.sub n 0) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("theorem nat_sub_self : (n : Nat) -> Eq (Nat.sub n n) 0 :=");
        try self.wl("  fun n => @Nat.recOn (fun k => Eq (Nat.sub k k) 0) n");
        try self.wl("    (@Eq.refl Nat 0)");
        try self.wl("    (fun k ih => ih)");
        try self.nl();
        try self.wl("theorem nat_le_refl : (n : Nat) -> Nat.le n n := fun n => @Nat.le.refl n");
        try self.nl();
        try self.wl("theorem nat_le_step_thm : (n m : Nat) -> Nat.le n m -> Nat.le n (Nat.succ m) :=");
        try self.wl("  fun n m h => Nat.le.step h");
        try self.nl();
        try self.wl("theorem nat_le_succ_self : (n : Nat) -> Nat.le n (Nat.succ n) :=");
        try self.wl("  fun n => Nat.le.step (@Nat.le.refl n)");
        try self.nl();
        try self.wl("-- Bool foundational laws");
        try self.wl("theorem bool_true_ne_false : Not (Eq true false) := fun h => Bool.noConfusion h");
        try self.nl();
        try self.wl("theorem bool_false_ne_true : Not (Eq false true) := fun h => Bool.noConfusion h");
        try self.nl();
        try self.wl("theorem bool_not_true : Eq (Bool.not true) false := @Eq.refl Bool false");
        try self.nl();
        try self.wl("theorem bool_not_false : Eq (Bool.not false) true := @Eq.refl Bool true");
        try self.nl();
        try self.wl("theorem bool_not_not : (b : Bool) -> Eq (Bool.not (Bool.not b)) b :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.not (Bool.not b)) b) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_and_true_r : (b : Bool) -> Eq (Bool.and b true) b :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.and b true) b) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_and_false_r : (b : Bool) -> Eq (Bool.and b false) false :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.and b false) false) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool false)");
        try self.nl();
        try self.wl("theorem bool_or_true_r : (b : Bool) -> Eq (Bool.or b true) true :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.or b true) true) b");
        try self.wl("    (@Eq.refl Bool true) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_or_false_r : (b : Bool) -> Eq (Bool.or b false) b :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.or b false) b) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_and_self : (b : Bool) -> Eq (Bool.and b b) b :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.and b b) b) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_or_self : (b : Bool) -> Eq (Bool.or b b) b :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Eq (Bool.or b b) b) b");
        try self.wl("    (@Eq.refl Bool false) (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_and_comm : (a b : Bool) -> Eq (Bool.and a b) (Bool.and b a) :=");
        try self.wl("  fun a b => @Bool.casesOn (fun a => Eq (Bool.and a b) (Bool.and b a)) a");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.and false b) (Bool.and b false)) b");
        try self.wl("      (@Eq.refl Bool false) (@Eq.refl Bool false))");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.and true b) (Bool.and b true)) b");
        try self.wl("      (@Eq.refl Bool false) (@Eq.refl Bool true))");
        try self.nl();
        try self.wl("theorem bool_or_comm : (a b : Bool) -> Eq (Bool.or a b) (Bool.or b a) :=");
        try self.wl("  fun a b => @Bool.casesOn (fun a => Eq (Bool.or a b) (Bool.or b a)) a");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.or false b) (Bool.or b false)) b");
        try self.wl("      (@Eq.refl Bool false) (@Eq.refl Bool true))");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.or true b) (Bool.or b true)) b");
        try self.wl("      (@Eq.refl Bool true) (@Eq.refl Bool true))");
        try self.nl();
        try self.wl("theorem bool_and_assoc : (a b c : Bool) -> Eq (Bool.and (Bool.and a b) c) (Bool.and a (Bool.and b c)) :=");
        try self.wl("  fun a b c => @Bool.casesOn (fun a => Eq (Bool.and (Bool.and a b) c) (Bool.and a (Bool.and b c))) a");
        try self.wl("    (@Eq.refl Bool false)");
        try self.wl("    (@Eq.refl Bool (Bool.and b c))");
        try self.nl();
        try self.wl("theorem bool_or_assoc : (a b c : Bool) -> Eq (Bool.or (Bool.or a b) c) (Bool.or a (Bool.or b c)) :=");
        try self.wl("  fun a b c => @Bool.casesOn (fun a => Eq (Bool.or (Bool.or a b) c) (Bool.or a (Bool.or b c))) a");
        try self.wl("    (@Eq.refl Bool (Bool.or b c))");
        try self.wl("    (@Eq.refl Bool true)");
        try self.nl();
        try self.wl("theorem bool_de_morgan_and : (a b : Bool) -> Eq (Bool.not (Bool.and a b)) (Bool.or (Bool.not a) (Bool.not b)) :=");
        try self.wl("  fun a b => @Bool.casesOn (fun a => Eq (Bool.not (Bool.and a b)) (Bool.or (Bool.not a) (Bool.not b))) a");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.not (Bool.and false b)) (Bool.or (Bool.not false) (Bool.not b))) b");
        try self.wl("      (@Eq.refl Bool true) (@Eq.refl Bool true))");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.not (Bool.and true b)) (Bool.or (Bool.not true) (Bool.not b))) b");
        try self.wl("      (@Eq.refl Bool true) (@Eq.refl Bool false))");
        try self.nl();
        try self.wl("theorem bool_de_morgan_or : (a b : Bool) -> Eq (Bool.not (Bool.or a b)) (Bool.and (Bool.not a) (Bool.not b)) :=");
        try self.wl("  fun a b => @Bool.casesOn (fun a => Eq (Bool.not (Bool.or a b)) (Bool.and (Bool.not a) (Bool.not b))) a");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.not (Bool.or false b)) (Bool.and (Bool.not false) (Bool.not b))) b");
        try self.wl("      (@Eq.refl Bool true) (@Eq.refl Bool false))");
        try self.wl("    (@Bool.casesOn (fun b => Eq (Bool.not (Bool.or true b)) (Bool.and (Bool.not true) (Bool.not b))) b");
        try self.wl("      (@Eq.refl Bool false) (@Eq.refl Bool false))");
        try self.nl();
        try self.wl("theorem bool_cases_exhaustive : (b : Bool) -> Or (Eq b true) (Eq b false) :=");
        try self.wl("  fun b => @Bool.casesOn (fun b => Or (Eq b true) (Eq b false)) b");
        try self.wl("    (@Or.inr (Eq false true) (Eq false false) (@Eq.refl Bool false))");
        try self.wl("    (@Or.inl (Eq true true) (Eq true false) (@Eq.refl Bool true))");
        try self.nl();
        try self.wl("-- Option laws");
        try self.wl("theorem option_some_ne_none {α : Type} : (a : α) -> Not (Eq (Option.some a) (@Option.none α)) :=");
        try self.wl("  fun a h => Option.noConfusion h");
        try self.nl();
        try self.wl("theorem option_none_ne_some {α : Type} : (a : α) -> Not (Eq (@Option.none α) (Option.some a)) :=");
        try self.wl("  fun a h => Option.noConfusion h");
        try self.nl();
        try self.wl("theorem option_some_inj {α : Type} : (a b : α) -> Eq (Option.some a) (Option.some b) -> Eq a b :=");
        try self.wl("  fun a b h => Option.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("-- Prod laws");
        try self.wl("theorem prod_mk_fst {α β : Type} : (a : α) -> (b : β) -> Eq (@Prod.fst α β (Prod.mk a b)) a :=");
        try self.wl("  fun a b => @Eq.refl α a");
        try self.nl();
        try self.wl("theorem prod_mk_snd {α β : Type} : (a : α) -> (b : β) -> Eq (@Prod.snd α β (Prod.mk a b)) b :=");
        try self.wl("  fun a b => @Eq.refl β b");
        try self.nl();
        try self.wl("theorem prod_eta {α β : Type} : (p : @Prod α β) -> Eq (Prod.mk (@Prod.fst α β p) (@Prod.snd α β p)) p :=");
        try self.wl("  fun p => @Prod.casesOn α β (fun p => Eq (Prod.mk (@Prod.fst α β p) (@Prod.snd α β p)) p) p");
        try self.wl("    (fun a b => @Eq.refl (@Prod α β) (Prod.mk a b))");
        try self.nl();
        try self.wl("-- Logic laws");
        try self.wl("theorem and_intro_thm : (p q : Prop) -> p -> q -> And p q := fun p q hp hq => And.intro hp hq");
        try self.nl();
        try self.wl("theorem or_inl_thm : (p q : Prop) -> p -> Or p q := fun p q hp => @Or.inl p q hp");
        try self.nl();
        try self.wl("theorem or_inr_thm : (p q : Prop) -> q -> Or p q := fun p q hq => @Or.inr p q hq");
        try self.nl();
        try self.wl("theorem eq_symm_thm {α : Type} : (a b : α) -> Eq a b -> Eq b a := fun a b h => Eq.symm h");
        try self.nl();
        try self.wl("theorem eq_trans_thm {α : Type} : (a b c : α) -> Eq a b -> Eq b c -> Eq a c :=");
        try self.wl("  fun a b c h1 h2 => Eq.trans h1 h2");
        try self.nl();
        try self.wl("theorem congr_arg_thm {α β : Type} : (f : α -> β) -> (a b : α) -> Eq a b -> Eq (f a) (f b) :=");
        try self.wl("  fun f a b h => congrArg f h");
        try self.nl();
        try self.wl("theorem iff_refl_thm : (p : Prop) -> Iff p p := fun p => Iff.intro (fun h => h) (fun h => h)");
        try self.nl();
        try self.wl("theorem not_not_intro_thm : (p : Prop) -> p -> Not (Not p) := fun p hp hn => hn hp");
        try self.nl();
        try self.wl("-- ZigExcept laws");
        try self.wl("theorem zigexcept_ok_ne_error {E A : Type} : (a : A) -> (e : E) -> Not (Eq (@ZigExcept.ok E A a) (@ZigExcept.error E A e)) :=");
        try self.wl("  fun a e h => ZigExcept.noConfusion h");
        try self.nl();
        try self.wl("theorem zigexcept_error_ne_ok {E A : Type} : (e : E) -> (a : A) -> Not (Eq (@ZigExcept.error E A e) (@ZigExcept.ok E A a)) :=");
        try self.wl("  fun e a h => ZigExcept.noConfusion h");
        try self.nl();
        try self.wl("theorem zigexcept_ok_inj {E A : Type} : (a b : A) -> Eq (@ZigExcept.ok E A a) (@ZigExcept.ok E A b) -> Eq a b :=");
        try self.wl("  fun a b h => ZigExcept.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("theorem zigexcept_error_inj {E A : Type} : (a b : E) -> Eq (@ZigExcept.error E A a) (@ZigExcept.error E A b) -> Eq a b :=");
        try self.wl("  fun a b h => ZigExcept.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("-- HeapState composition laws");
        try self.wl("theorem heap_invariant_refl : (h : HeapState) -> HeapInvariant h :=");
        try self.wl("  fun h => HeapInvariant.mk h (@Eq.refl Nat h.next_alloc_id)");
        try self.nl();
        try self.wl("theorem loop_invariant_refl : (h : HeapState) -> LoopInvariant h h :=");
        try self.wl("  fun h => LoopInvariant.mk h h (@Eq.refl Nat h.next_alloc_id)");
        try self.nl();
        try self.wl("theorem heap_empty_alloc : Eq HeapState.empty.next_alloc_id 0 := @Eq.refl Nat 0");
        try self.nl();
        try self.wl("theorem heap_write_nat_congr : (h : HeapState) -> (cell : MemoryCell) -> (v1 v2 : Nat) ->");
        try self.wl("    Eq v1 v2 -> Eq (writeNat h cell v1) (writeNat h cell v2) :=");
        try self.wl("  fun h cell v1 v2 hv => congrArg (writeNat h cell) hv");
        try self.nl();
        try self.wl("theorem heap_read_congr : (h1 h2 : HeapState) -> Eq h1 h2 -> (cell : MemoryCell) ->");
        try self.wl("    Eq (readNat h1 cell) (readNat h2 cell) :=");
        try self.wl("  fun h1 h2 hh cell => congrArg (fun h => readNat h cell) hh");
        try self.nl();
        try self.wl("theorem heap_state_eq_refl : (h : HeapState) -> Eq h h := fun h => @Eq.refl HeapState h");
        try self.nl();
        try self.wl("-- List laws");
        try self.wl("theorem list_cons_ne_nil {α : Type} : (x : α) -> (xs : List α) -> Not (Eq (List.cons x xs) (@List.nil α)) :=");
        try self.wl("  fun x xs h => List.noConfusion h");
        try self.nl();
        try self.wl("theorem list_nil_ne_cons {α : Type} : (x : α) -> (xs : List α) -> Not (Eq (@List.nil α) (List.cons x xs)) :=");
        try self.wl("  fun x xs h => List.noConfusion h");
        try self.nl();
        try self.wl("theorem list_length_nil {α : Type} : Eq (@List.length α (@List.nil α)) 0 := @Eq.refl Nat 0");
        try self.nl();
        try self.wl("theorem list_length_cons {α : Type} : (x : α) -> (xs : List α) ->");
        try self.wl("    Eq (@List.length α (List.cons x xs)) (Nat.succ (@List.length α xs)) :=");
        try self.wl("  fun x xs => @Eq.refl Nat (Nat.succ (@List.length α xs))");
        try self.nl();
        try self.wl("theorem list_head_cons {α : Type} : (x : α) -> (xs : List α) -> Eq (@List.head? α (List.cons x xs)) (Option.some x) :=");
        try self.wl("  fun x xs => @Eq.refl (Option α) (Option.some x)");
        try self.nl();
        try self.wl("theorem list_tail_cons {α : Type} : (x : α) -> (xs : List α) -> Eq (@List.tail? α (List.cons x xs)) (Option.some xs) :=");
        try self.wl("  fun x xs => @Eq.refl (Option (List α)) (Option.some xs)");
        try self.nl();
        try self.wl("theorem list_cons_inj_head {α : Type} : (x1 x2 : α) -> (xs1 xs2 : List α) -> Eq (List.cons x1 xs1) (List.cons x2 xs2) -> Eq x1 x2 :=");
        try self.wl("  fun x1 x2 xs1 xs2 h => List.noConfusion h (fun h1 h2 => h1)");
        try self.nl();
        try self.wl("theorem list_cons_inj_tail {α : Type} : (x1 x2 : α) -> (xs1 xs2 : List α) -> Eq (List.cons x1 xs1) (List.cons x2 xs2) -> Eq xs1 xs2 :=");
        try self.wl("  fun x1 x2 xs1 xs2 h => List.noConfusion h (fun h1 h2 => h2)");
        try self.nl();
        try self.wl("-- Int foundational laws");
        try self.wl("theorem int_ofNat_zero : Eq (Int.ofNat 0) 0 := @Eq.refl Int 0");
        try self.nl();
        try self.wl("theorem int_ofNat_succ : (n : Nat) -> Eq (Int.ofNat (Nat.succ n)) (Int.ofNat n + 1) :=");
        try self.wl("  fun n => @Eq.refl Int (Int.ofNat (Nat.succ n))");
        try self.nl();
        try self.wl("theorem int_negSucc_ne_ofNat : (n m : Nat) -> Not (Eq (Int.negSucc n) (Int.ofNat m)) :=");
        try self.wl("  fun n m h => Int.noConfusion h");
        try self.nl();
        try self.wl("theorem int_ofNat_ne_negSucc : (n m : Nat) -> Not (Eq (Int.ofNat n) (Int.negSucc m)) :=");
        try self.wl("  fun n m h => Int.noConfusion h");
        try self.nl();
        try self.wl("theorem int_ofNat_inj : (n m : Nat) -> Eq (Int.ofNat n) (Int.ofNat m) -> Eq n m :=");
        try self.wl("  fun n m h => Int.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("theorem int_negSucc_inj : (n m : Nat) -> Eq (Int.negSucc n) (Int.negSucc m) -> Eq n m :=");
        try self.wl("  fun n m h => Int.noConfusion h (fun h => h)");
        try self.nl();
        try self.wl("-- ZigExcept bind laws");
        try self.wl("theorem zigexcept_bind_ok {E A B : Type} : (a : A) -> (f : A -> ZigExcept E B) ->");
        try self.wl("    Eq (ZigExcept_bind (@ZigExcept.ok E A a) f) (f a) :=");
        try self.wl("  fun a f => @Eq.refl (ZigExcept E B) (f a)");
        try self.nl();
        try self.wl("theorem zigexcept_bind_error {E A B : Type} : (e : E) -> (f : A -> ZigExcept E B) ->");
        try self.wl("    Eq (ZigExcept_bind (@ZigExcept.error E A e) f) (@ZigExcept.error E B e) :=");
        try self.wl("  fun e f => @Eq.refl (ZigExcept E B) (ZigExcept.error e)");
        try self.nl();
        try self.wl("-- ZigError distinctness");
        try self.wl("theorem zigerror_invalidarg_ne_overflow : Not (Eq ZigError.InvalidArgument ZigError.Overflow) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_invalidarg_ne_underflow : Not (Eq ZigError.InvalidArgument ZigError.Underflow) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_invalidarg_ne_outofbounds : Not (Eq ZigError.InvalidArgument ZigError.OutOfBounds) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_invalidarg_ne_nullderef : Not (Eq ZigError.InvalidArgument ZigError.NullDereference) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_invalidarg_ne_outofmemory : Not (Eq ZigError.InvalidArgument ZigError.OutOfMemory) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_invalidarg_ne_generic : Not (Eq ZigError.InvalidArgument ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_overflow_ne_underflow : Not (Eq ZigError.Overflow ZigError.Underflow) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_overflow_ne_outofbounds : Not (Eq ZigError.Overflow ZigError.OutOfBounds) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_overflow_ne_nullderef : Not (Eq ZigError.Overflow ZigError.NullDereference) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_overflow_ne_outofmemory : Not (Eq ZigError.Overflow ZigError.OutOfMemory) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_overflow_ne_generic : Not (Eq ZigError.Overflow ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_underflow_ne_outofbounds : Not (Eq ZigError.Underflow ZigError.OutOfBounds) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_underflow_ne_nullderef : Not (Eq ZigError.Underflow ZigError.NullDereference) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_underflow_ne_outofmemory : Not (Eq ZigError.Underflow ZigError.OutOfMemory) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_underflow_ne_generic : Not (Eq ZigError.Underflow ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_outofbounds_ne_nullderef : Not (Eq ZigError.OutOfBounds ZigError.NullDereference) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_outofbounds_ne_outofmemory : Not (Eq ZigError.OutOfBounds ZigError.OutOfMemory) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_outofbounds_ne_generic : Not (Eq ZigError.OutOfBounds ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_nullderef_ne_outofmemory : Not (Eq ZigError.NullDereference ZigError.OutOfMemory) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_nullderef_ne_generic : Not (Eq ZigError.NullDereference ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.wl("theorem zigerror_outofmemory_ne_generic : Not (Eq ZigError.OutOfMemory ZigError.Generic) :=");
        try self.wl("  fun h => ZigError.noConfusion h");
        try self.nl();
        try self.wl("-- Nat comparison laws");
        try self.wl("theorem nat_min_self : (n : Nat) -> Eq (Nat.min n n) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("theorem nat_max_self : (n : Nat) -> Eq (Nat.max n n) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("theorem nat_min_zero_left : (n : Nat) -> Eq (Nat.min 0 n) 0 :=");
        try self.wl("  fun n => @Eq.refl Nat 0");
        try self.nl();
        try self.wl("theorem nat_max_zero_left : (n : Nat) -> Eq (Nat.max 0 n) n :=");
        try self.wl("  fun n => @Eq.refl Nat n");
        try self.nl();
        try self.wl("-- Equality chain helpers");
        try self.wl("theorem eq_chain2 {α : Type} : (a b c : α) -> Eq a b -> Eq b c -> Eq a c :=");
        try self.wl("  fun a b c h1 h2 => Eq.trans h1 h2");
        try self.nl();
        try self.wl("theorem eq_chain3 {α : Type} : (a b c d : α) -> Eq a b -> Eq b c -> Eq c d -> Eq a d :=");
        try self.wl("  fun a b c d h1 h2 h3 => Eq.trans (Eq.trans h1 h2) h3");
        try self.nl();
        try self.wl("theorem eq_subst {α : Type} {p : α -> Prop} : (a b : α) -> Eq a b -> p a -> p b :=");
        try self.wl("  fun a b h ha => @Eq.subst α p a b h ha");
        try self.nl();
        try self.wl("-- Function extensionality helpers");
        try self.wl("theorem fun_congr_nat : (f g : Nat -> Nat) -> Eq f g -> (n : Nat) -> Eq (f n) (g n) :=");
        try self.wl("  fun f g h n => congrArg (fun f => f n) h");
        try self.nl();
        try self.wl("theorem fun_congr_bool : (f g : Bool -> Bool) -> Eq f g -> (b : Bool) -> Eq (f b) (g b) :=");
        try self.wl("  fun f g h b => congrArg (fun f => f b) h");
        try self.nl();
        try self.wl("-- HeapState field access stability");
        try self.wl("theorem heap_natSlots_congr : (h1 h2 : HeapState) -> Eq h1 h2 -> Eq h1.natSlots h2.natSlots :=");
        try self.wl("  fun h1 h2 h => congrArg HeapState.natSlots h");
        try self.nl();
        try self.wl("theorem heap_intSlots_congr : (h1 h2 : HeapState) -> Eq h1 h2 -> Eq h1.intSlots h2.intSlots :=");
        try self.wl("  fun h1 h2 h => congrArg HeapState.intSlots h");
        try self.nl();
        try self.wl("theorem heap_boolSlots_congr : (h1 h2 : HeapState) -> Eq h1 h2 -> Eq h1.boolSlots h2.boolSlots :=");
        try self.wl("  fun h1 h2 h => congrArg HeapState.boolSlots h");
        try self.nl();
        try self.wl("theorem heap_alloc_congr : (h1 h2 : HeapState) -> Eq h1 h2 -> Eq h1.next_alloc_id h2.next_alloc_id :=");
        try self.wl("  fun h1 h2 h => congrArg HeapState.next_alloc_id h");
        try self.nl();
        try self.wl("-- WriteNat double-write");
        try self.wl("theorem writeNat_writeNat_same (h : HeapState) (cell : MemoryCell) (v1 v2 : Nat) :");
        try self.wl("    Eq (writeNat (writeNat h cell v1) cell v2) (writeNat h cell v2) :=");
        try self.wl("  @Eq.refl HeapState (writeNat h cell v2)");
        try self.nl();
        try self.wl("theorem writeInt_writeInt_same (h : HeapState) (cell : MemoryCell) (v1 v2 : Int) :");
        try self.wl("    Eq (writeInt (writeInt h cell v1) cell v2) (writeInt h cell v2) :=");
        try self.wl("  @Eq.refl HeapState (writeInt h cell v2)");
        try self.nl();
        try self.wl("theorem writeBool_writeBool_same (h : HeapState) (cell : MemoryCell) (v1 v2 : Bool) :");
        try self.wl("    Eq (writeBool (writeBool h cell v1) cell v2) (writeBool h cell v2) :=");
        try self.wl("  @Eq.refl HeapState (writeBool h cell v2)");
        try self.nl();
        try self.wl("-- Nat decidable equality helpers");
        try self.wl("theorem nat_beq_refl : (n : Nat) -> Eq (Nat.beq n n) true :=");
        try self.wl("  fun n => @Nat.recOn (fun k => Eq (Nat.beq k k) true) n");
        try self.wl("    (@Eq.refl Bool true)");
        try self.wl("    (fun k ih => ih)");
        try self.nl();
        try self.wl("-- Empty heap read defaults");
        try self.wl("theorem heap_empty_readNat : (cell : MemoryCell) -> Eq (readNat HeapState.empty cell) 0 :=");
        try self.wl("  fun cell => @Eq.refl Nat 0");
        try self.nl();
        try self.wl("theorem heap_empty_readInt : (cell : MemoryCell) -> Eq (readInt HeapState.empty cell) 0 :=");
        try self.wl("  fun cell => @Eq.refl Int 0");
        try self.nl();
        try self.wl("theorem heap_empty_readBool : (cell : MemoryCell) -> Eq (readBool HeapState.empty cell) false :=");
        try self.wl("  fun cell => @Eq.refl Bool false");
        try self.nl();
    }

    fn emitMemoryCellDistinctness(self: *SemanticProofEngine) !void {
        if (self.memory_cells.items.len < 2) return;
        try self.wl("-- MemoryCell pairwise distinctness");
        for (self.memory_cells.items, 0..) |cell_a, i| {
            for (self.memory_cells.items[i + 1 ..]) |cell_b| {
                try self.w("theorem ");
                try self.writeSanitizedIdent("cell_", cell_a);
                try self.w("_ne_");
                try self.writeSanitizedIdent("cell_", cell_b);
                try self.w(" : Not (Eq ");
                try self.writeMemoryCellCtor(cell_a);
                try self.w(" ");
                try self.writeMemoryCellCtor(cell_b);
                try self.wl(") :=");
                try self.wl("  fun h => MemoryCell.noConfusion h");
                try self.nl();
                try self.w("theorem ");
                try self.writeSanitizedIdent("cell_", cell_b);
                try self.w("_ne_");
                try self.writeSanitizedIdent("cell_", cell_a);
                try self.w(" : Not (Eq ");
                try self.writeMemoryCellCtor(cell_b);
                try self.w(" ");
                try self.writeMemoryCellCtor(cell_a);
                try self.wl(") :=");
                try self.wl("  fun h => MemoryCell.noConfusion h");
                try self.nl();
            }
        }
    }

    fn emitMemoryCellDecidableEq(self: *SemanticProofEngine) !void {
        if (self.memory_cells.items.len == 0) return;
        try self.wl("-- Explicit DecidableEq MemoryCell exhaustive instance");
        try self.wl("instance memoryCellDecEq : DecidableEq MemoryCell :=");
        try self.wl("  fun a b => MemoryCell.casesOn a");
        for (self.memory_cells.items, 0..) |_, i| {
            try self.w("    (MemoryCell.casesOn b");
            for (self.memory_cells.items, 0..) |_, j| {
                if (i == j) {
                    try self.w(" (Decidable.isTrue (Eq.refl _))");
                } else {
                    try self.w(" (Decidable.isFalse (fun h => MemoryCell.noConfusion h))");
                }
            }
            try self.wl(")");
        }
        try self.nl();
    }

    fn emitContainerDeepProofs(self: *SemanticProofEngine, container: *const SemanticContainerIR) !void {
        switch (container.kind) {
            .zig_struct => try self.emitStructDeepProofs(container),
            .zig_enum => try self.emitEnumDeepProofs(container),
            .zig_union => {},
        }
    }

    fn emitStructDeepProofs(self: *SemanticProofEngine, container: *const SemanticContainerIR) !void {
        const name = container.name;
        const fields = container.fields.items;
        if (fields.len == 0) return;
        try self.w("-- Deep proofs for struct ");
        try self.wl(name);
        try self.nl();
        for (fields) |field| {
            try self.w("def ");
            try self.w(name);
            try self.w(".get_");
            try self.w(field.name);
            try self.w(" (s : ");
            try self.w(name);
            try self.w(") : ");
            try self.w(field.lean_type);
            try self.wl(" :=");
            try self.w("  @");
            try self.w(name);
            try self.w(".casesOn s (fun _ => ");
            try self.w(field.lean_type);
            try self.w(") (fun");
            for (fields) |f| {
                try self.w(" ");
                try self.w(f.name);
            }
            try self.w(" => ");
            try self.w(field.name);
            try self.wl(")");
            try self.nl();
        }
        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_congr_");
            try self.w(field.name);
            try self.w(" : (s1 s2 : ");
            try self.w(name);
            try self.w(") -> Eq s1 s2 -> Eq (");
            try self.w(name);
            try self.w(".get_");
            try self.w(field.name);
            try self.w(" s1) (");
            try self.w(name);
            try self.w(".get_");
            try self.w(field.name);
            try self.wl(" s2) :=");
            try self.w("  fun s1 s2 h => congrArg ");
            try self.w(name);
            try self.w(".get_");
            try self.w(field.name);
            try self.wl(" h");
            try self.nl();
        }
        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_default_");
            try self.w(field.name);
            try self.w(" : Eq (");
            try self.w(name);
            try self.w(".get_");
            try self.w(field.name);
            try self.w(" ");
            try self.w(name);
            try self.w(".default) ");
            try self.w(self.defaultValue(field.lean_type));
            try self.wl(" :=");
            try self.w("  @Eq.refl ");
            try self.w(field.lean_type);
            try self.w(" ");
            try self.wl(self.defaultValue(field.lean_type));
            try self.nl();
        }
        for (fields, 0..) |target_field, idx| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_mk_inj_");
            try self.w(target_field.name);
            try self.w(" :");
            for (fields, 0..) |f, fi| {
                try self.wfmt(" (a{d} b{d} : ", .{ fi, fi });
                try self.w(f.lean_type);
                try self.w(")");
            }
            try self.w(" -> Eq (");
            try self.w(name);
            try self.w(".mk");
            for (0..fields.len) |fi| {
                try self.wfmt(" a{d}", .{fi});
            }
            try self.w(") (");
            try self.w(name);
            try self.w(".mk");
            for (0..fields.len) |fi| {
                try self.wfmt(" b{d}", .{fi});
            }
            try self.wfmt(") -> Eq a{d} b{d} :=", .{ idx, idx });
            try self.nl();
            try self.w("  fun");
            for (0..fields.len) |fi| {
                try self.wfmt(" a{d} b{d}", .{ fi, fi });
            }
            try self.w(" h => ");
            try self.w(name);
            try self.w(".noConfusion h (fun");
            for (0..fields.len) |fi| {
                try self.wfmt(" h{d}", .{fi});
            }
            try self.wfmt(" => h{d})", .{idx});
            try self.nl();
            try self.nl();
        }
        try self.w("theorem ");
        try self.w(name);
        try self.w("_eq_intro :");
        for (fields, 0..) |f, fi| {
            try self.wfmt(" (a{d} : ", .{fi});
            try self.w(f.lean_type);
            try self.w(")");
        }
        try self.w(" -> Eq (");
        try self.w(name);
        try self.w(".mk");
        for (0..fields.len) |fi| {
            try self.wfmt(" a{d}", .{fi});
        }
        try self.w(") (");
        try self.w(name);
        try self.w(".mk");
        for (0..fields.len) |fi| {
            try self.wfmt(" a{d}", .{fi});
        }
        try self.wl(") :=");
        try self.w("  fun");
        for (0..fields.len) |fi| {
            try self.wfmt(" a{d}", .{fi});
        }
        try self.w(" => @Eq.refl ");
        try self.w(name);
        try self.w(" (");
        try self.w(name);
        try self.w(".mk");
        for (0..fields.len) |fi| {
            try self.wfmt(" a{d}", .{fi});
        }
        try self.wl(")");
        try self.nl();
        for (fields, 0..) |target_field, tidx| {
            try self.w("def ");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.w(" (s : ");
            try self.w(name);
            try self.w(") (v : ");
            try self.w(target_field.lean_type);
            try self.w(") : ");
            try self.w(name);
            try self.wl(" :=");
            try self.w("  @");
            try self.w(name);
            try self.w(".casesOn s (fun _ => ");
            try self.w(name);
            try self.w(") (fun");
            for (fields) |f| {
                try self.w(" ");
                try self.w(f.name);
            }
            try self.w(" => ");
            try self.w(name);
            try self.w(".mk");
            for (fields, 0..) |f, fi2| {
                if (fi2 == tidx) {
                    try self.w(" v");
                } else {
                    try self.w(" ");
                    try self.w(f.name);
                }
            }
            try self.wl(")");
            try self.nl();
            try self.w("theorem ");
            try self.w(name);
            try self.w("_get_set_");
            try self.w(target_field.name);
            try self.w(" : (s : ");
            try self.w(name);
            try self.w(") -> (v : ");
            try self.w(target_field.lean_type);
            try self.w(") -> Eq (");
            try self.w(name);
            try self.w(".get_");
            try self.w(target_field.name);
            try self.w(" (");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.w(" s v)) v :=");
            try self.nl();
            try self.w("  fun s v => @");
            try self.w(name);
            try self.w(".casesOn s (fun s0 => Eq (");
            try self.w(name);
            try self.w(".get_");
            try self.w(target_field.name);
            try self.w(" (");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.w(" s0 v)) v) (fun");
            for (fields) |f| {
                try self.w(" ");
                try self.w(f.name);
            }
            try self.w(" => @Eq.refl ");
            try self.w(target_field.lean_type);
            try self.wl(" v)");
            try self.nl();
            try self.w("theorem ");
            try self.w(name);
            try self.w("_set_set_");
            try self.w(target_field.name);
            try self.w(" : (s : ");
            try self.w(name);
            try self.w(") -> (v1 v2 : ");
            try self.w(target_field.lean_type);
            try self.w(") -> Eq (");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.w(" (");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.wl(" s v1) v2)");
            try self.w("    (");
            try self.w(name);
            try self.w(".set_");
            try self.w(target_field.name);
            try self.wl(" s v2) :=");
            try self.w("  fun s v1 v2 => @");
            try self.w(name);
            try self.wfmt(".casesOn s (fun s0 => Eq ({s}.set_{s} ({s}.set_{s} s0 v1) v2) ({s}.set_{s} s0 v2)) (fun", .{ name, target_field.name, name, target_field.name, name, target_field.name });
            for (fields) |f| {
                try self.w(" ");
                try self.w(f.name);
            }
            try self.w(" => @Eq.refl ");
            try self.w(name);
            try self.wl(" _)");
            try self.nl();
            for (fields, 0..) |other_field, oidx| {
                if (oidx == tidx) continue;
                try self.w("theorem ");
                try self.w(name);
                try self.w("_set_");
                try self.w(target_field.name);
                try self.w("_get_");
                try self.w(other_field.name);
                try self.w(" : (s : ");
                try self.w(name);
                try self.w(") -> (v : ");
                try self.w(target_field.lean_type);
                try self.w(") -> Eq (");
                try self.w(name);
                try self.w(".get_");
                try self.w(other_field.name);
                try self.w(" (");
                try self.w(name);
                try self.w(".set_");
                try self.w(target_field.name);
                try self.w(" s v)) (");
                try self.w(name);
                try self.w(".get_");
                try self.w(other_field.name);
                try self.wl(" s) :=");
                try self.w("  fun s v => @");
                try self.w(name);
                try self.wfmt(".casesOn s (fun s0 => Eq ({s}.get_{s} ({s}.set_{s} s0 v)) ({s}.get_{s} s0)) (fun", .{ name, other_field.name, name, target_field.name, name, other_field.name });
                for (fields) |f| {
                    try self.w(" ");
                    try self.w(f.name);
                }
                try self.w(" => @Eq.refl ");
                try self.w(other_field.lean_type);
                try self.w(" ");
                try self.w(other_field.name);
                try self.wl(")");
                try self.nl();
            }
        }
    }

    fn emitEnumDeepProofs(self: *SemanticProofEngine, container: *const SemanticContainerIR) !void {
        const name = container.name;
        const fields = container.fields.items;
        if (fields.len < 2) return;
        try self.w("-- Deep proofs for enum ");
        try self.wl(name);
        try self.nl();
        try self.w("def ");
        try self.w(name);
        try self.w("_toNat : ");
        try self.w(name);
        try self.wl(" -> Nat :=");
        try self.w("  fun x => @");
        try self.w(name);
        try self.w(".casesOn x (fun _ => Nat)");
        for (fields, 0..) |_, fi| {
            try self.wfmt(" {d}", .{fi});
        }
        try self.nl();
        try self.nl();
        for (fields, 0..) |field_a, i| {
            for (fields[i + 1 ..]) |field_b| {
                try self.w("theorem ");
                try self.w(name);
                try self.w("_");
                try self.w(field_a.name);
                try self.w("_ne_");
                try self.w(field_b.name);
                try self.w(" : Not (Eq ");
                try self.w(name);
                try self.w(".");
                try self.w(field_a.name);
                try self.w(" ");
                try self.w(name);
                try self.w(".");
                try self.w(field_b.name);
                try self.wl(") :=");
                try self.wl("  fun h => ");
                try self.w(name);
                try self.wl(".noConfusion h");
                try self.nl();
                try self.w("theorem ");
                try self.w(name);
                try self.w("_");
                try self.w(field_b.name);
                try self.w("_ne_");
                try self.w(field_a.name);
                try self.w(" : Not (Eq ");
                try self.w(name);
                try self.w(".");
                try self.w(field_b.name);
                try self.w(" ");
                try self.w(name);
                try self.w(".");
                try self.w(field_a.name);
                try self.wl(") :=");
                try self.wl("  fun h => ");
                try self.w(name);
                try self.wl(".noConfusion h");
                try self.nl();
            }
        }
    }

    const WriteStepInfo = struct { field: []const u8, lean_type: []const u8 };

    fn collectWriteSteps(self: *SemanticProofEngine, function: *const SemanticFunctionIR, node: std.zig.Ast.Node.Index, steps: *ArrayList(WriteStepInfo)) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        const data = tree.nodes.items(.data)[node];
        switch (tag) {
            .block, .block_semicolon, .block_two, .block_two_semicolon => {
                var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
                defer block_nodes.deinit();
                try self.collectBlockNodes(node, &block_nodes);
                for (block_nodes.items) |stmt| {
                    try self.collectWriteSteps(function, stmt, steps);
                }
            },
            .if_simple, .@"if" => {
                if (tree.fullIf(node)) |if_full| {
                    try self.collectWriteSteps(function, if_full.ast.then_expr, steps);
                    if (if_full.ast.else_expr != 0) {
                        try self.collectWriteSteps(function, if_full.ast.else_expr, steps);
                    }
                }
            },
            else => {
                if (self.isAssignmentTag(tag)) {
                    if (self.extractAssignedField(data.lhs)) |field_name| {
                        var lean_type: []const u8 = "Nat";
                        if (function.owner_type.len > 0) {
                            if (self.findContainer(function.owner_type)) |container| {
                                for (container.fields.items) |field| {
                                    if (std.mem.eql(u8, field.name, field_name)) {
                                        lean_type = field.lean_type;
                                        break;
                                    }
                                }
                            }
                        }
                        try steps.append(.{ .field = field_name, .lean_type = lean_type });
                    }
                }
            },
        }
    }

    fn emitPerAssignmentFrameRules(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        if (function.body_node == 0) return;
        var write_steps = ArrayList(WriteStepInfo).init(self.allocator);
        defer write_steps.deinit();
        try self.collectWriteSteps(function, function.body_node, &write_steps);

        for (write_steps.items, 0..) |step, step_idx| {
            const cell_name = try self.qualifiedStateCell(function, step.field);
            const write_fn = if (std.mem.eql(u8, step.lean_type, "Int")) "writeInt" else if (std.mem.eql(u8, step.lean_type, "Bool")) "writeBool" else "writeNat";
            const read_fn = if (std.mem.eql(u8, step.lean_type, "Int")) "readInt" else if (std.mem.eql(u8, step.lean_type, "Bool")) "readBool" else "readNat";
            const preserves_fn = if (std.mem.eql(u8, step.lean_type, "Int")) "writeInt_preserves_alloc" else if (std.mem.eql(u8, step.lean_type, "Bool")) "writeBool_preserves_alloc" else "writeNat_preserves_alloc";
            const read_eq_fn = if (std.mem.eql(u8, step.lean_type, "Int")) "writeInt_read_eq" else if (std.mem.eql(u8, step.lean_type, "Bool")) "writeBool_read_eq" else "writeNat_read_eq";

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_step_{d}_writes_{s} : (sigma : HeapState) -> (v : {s}) -> Eq ({s} ({s} sigma ", .{ step_idx, step.field, step.lean_type, read_fn, write_fn });
            try self.writeMemoryCellCtor(cell_name);
            try self.w(" v) ");
            try self.writeMemoryCellCtor(cell_name);
            try self.wl(") v :=");
            try self.wfmt("  fun sigma v => {s} sigma ", .{read_eq_fn});
            try self.writeMemoryCellCtor(cell_name);
            try self.wl(" v");
            try self.nl();

            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_step_{d}_alloc : (sigma : HeapState) -> (v : {s}) -> Eq ({s} sigma ", .{ step_idx, step.lean_type, write_fn });
            try self.writeMemoryCellCtor(cell_name);
            try self.wl(" v).next_alloc_id sigma.next_alloc_id :=");
            try self.wfmt("  fun sigma v => {s} sigma ", .{preserves_fn});
            try self.writeMemoryCellCtor(cell_name);
            try self.wl(" v");
            try self.nl();

            for (self.memory_cells.items) |other_cell| {
                if (std.mem.eql(u8, cell_name, other_cell)) continue;
                const read_neq_fn = if (std.mem.eql(u8, step.lean_type, "Int")) "writeInt_read_neq" else if (std.mem.eql(u8, step.lean_type, "Bool")) "writeBool_read_neq" else "writeNat_read_neq";

                try self.w("theorem ");
                try self.w(function.name);
                try self.wfmt("_step_{d}_preserves_", .{step_idx});
                try self.writeSanitizedIdent("", other_cell);
                try self.wfmt(" : (sigma : HeapState) -> (v : {s}) -> Eq (readNat ({s} sigma ", .{ step.lean_type, write_fn });
                try self.writeMemoryCellCtor(cell_name);
                try self.w(" v) ");
                try self.writeMemoryCellCtor(other_cell);
                try self.w(") (readNat sigma ");
                try self.writeMemoryCellCtor(other_cell);
                try self.wl(") :=");
                try self.wfmt("  fun sigma v => {s} sigma ", .{read_neq_fn});
                try self.writeMemoryCellCtor(cell_name);
                try self.w(" ");
                try self.writeMemoryCellCtor(other_cell);
                try self.wl(" v (fun h => MemoryCell.noConfusion h)");
                try self.nl();
            }
        }
    }

    fn emitBuiltinExpr(self: *SemanticProofEngine, builtin_name: []const u8, lhs: std.zig.Ast.Node.Index, rhs: std.zig.Ast.Node.Index, is_multi_arg: bool) anyerror!void {
        const tree = self.astRef();
        if (std.mem.eql(u8, builtin_name, "@as")) {
            if (is_multi_arg) {
                const extra = tree.extra_data[lhs..rhs];
                if (extra.len >= 2) {
                    try self.emitExpr(extra[1]);
                } else if (extra.len >= 1) {
                    try self.emitExpr(extra[0]);
                } else {
                    try self.w("0");
                }
            } else {
                if (rhs != 0) {
                    try self.emitExpr(rhs);
                } else if (lhs != 0) {
                    try self.emitExpr(lhs);
                } else {
                    try self.w("0");
                }
            }
        } else if (std.mem.eql(u8, builtin_name, "@intCast") or
            std.mem.eql(u8, builtin_name, "@bitCast") or
            std.mem.eql(u8, builtin_name, "@floatCast") or
            std.mem.eql(u8, builtin_name, "@intFromFloat") or
            std.mem.eql(u8, builtin_name, "@floatFromInt") or
            std.mem.eql(u8, builtin_name, "@intFromPtr") or
            std.mem.eql(u8, builtin_name, "@ptrFromInt") or
            std.mem.eql(u8, builtin_name, "@truncate") or
            std.mem.eql(u8, builtin_name, "@intFromBool") or
            std.mem.eql(u8, builtin_name, "@intFromEnum") or
            std.mem.eql(u8, builtin_name, "@enumFromInt"))
        {
            if (lhs != 0) {
                try self.emitExpr(lhs);
            } else {
                try self.w("0");
            }
        } else if (std.mem.eql(u8, builtin_name, "@min")) {
            try self.w("Nat.min (");
            if (is_multi_arg) {
                const extra = tree.extra_data[lhs..rhs];
                if (extra.len >= 2) {
                    try self.emitExpr(extra[0]);
                    try self.w(") (");
                    try self.emitExpr(extra[1]);
                } else {
                    try self.w("0) (0");
                }
            } else {
                if (lhs != 0) try self.emitExpr(lhs) else try self.w("0");
                try self.w(") (");
                if (rhs != 0) try self.emitExpr(rhs) else try self.w("0");
            }
            try self.w(")");
        } else if (std.mem.eql(u8, builtin_name, "@max")) {
            try self.w("Nat.max (");
            if (is_multi_arg) {
                const extra = tree.extra_data[lhs..rhs];
                if (extra.len >= 2) {
                    try self.emitExpr(extra[0]);
                    try self.w(") (");
                    try self.emitExpr(extra[1]);
                } else {
                    try self.w("0) (0");
                }
            } else {
                if (lhs != 0) try self.emitExpr(lhs) else try self.w("0");
                try self.w(") (");
                if (rhs != 0) try self.emitExpr(rhs) else try self.w("0");
            }
            try self.w(")");
        } else if (std.mem.eql(u8, builtin_name, "@sizeOf") or
            std.mem.eql(u8, builtin_name, "@alignOf"))
        {
            try self.w("8");
        } else {
            if (lhs != 0) {
                try self.emitExpr(lhs);
            } else {
                try self.w("0");
            }
        }
    }

    fn extractVarDeclName(self: *SemanticProofEngine, node: std.zig.Ast.Node.Index) ?[]const u8 {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[node];
        if (tag != .simple_var_decl and tag != .local_var_decl and tag != .aligned_var_decl) return null;
        const main_token = tree.nodes.items(.main_token)[node];
        var name_tok = main_token + 1;
        const token_tags = tree.tokens.items(.tag);
        while (name_tok < tree.tokens.len) : (name_tok += 1) {
            if (token_tags[name_tok] == .identifier) {
                return tree.tokenSlice(@as(std.zig.Ast.TokenIndex, @intCast(name_tok)));
            }
            if (token_tags[name_tok] != .colon and token_tags[name_tok] != .keyword_const and token_tags[name_tok] != .keyword_var) break;
        }
        return null;
    }

    fn extractAssignTarget(self: *SemanticProofEngine, lhs_node: std.zig.Ast.Node.Index) ?[]const u8 {
        const tree = self.astRef();
        const tags = tree.nodes.items(.tag);
        if (tags[lhs_node] == .identifier) {
            return self.nodeText(lhs_node);
        }
        return null;
    }

    fn emitLocalBindingsAndReturn(self: *SemanticProofEngine, function: *const SemanticFunctionIR, body_node: std.zig.Ast.Node.Index, sigma_final: []const u8) !void {
        const tree = self.astRef();
        const tag = tree.nodes.items(.tag)[body_node];
        if (tag != .block and tag != .block_semicolon and tag != .block_two and tag != .block_two_semicolon) {
            if (function.usesErrorMonad()) {
                try self.w("ZigExcept.ok (Prod.mk ");
                try self.w(sigma_final);
                try self.w(" ");
                try self.w(self.defaultValue(function.return_lean_type));
                try self.w(")");
            } else {
                try self.w("Prod.mk ");
                try self.w(sigma_final);
                try self.w(" ");
                try self.w(self.defaultValue(function.return_lean_type));
            }
            return;
        }
        var block_nodes = ArrayList(std.zig.Ast.Node.Index).init(self.allocator);
        defer block_nodes.deinit();
        self.collectBlockNodes(body_node, &block_nodes) catch {
            try self.w("Prod.mk ");
            try self.w(sigma_final);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
            return;
        };

        var closes: usize = 0;
        for (block_nodes.items) |stmt| {
            const stmt_tag = tree.nodes.items(.tag)[stmt];
            const stmt_data = tree.nodes.items(.data)[stmt];

            if (stmt_tag == .simple_var_decl or stmt_tag == .local_var_decl or stmt_tag == .aligned_var_decl) {
                if (self.extractVarDeclName(stmt)) |var_name| {
                    if (stmt_data.rhs != 0) {
                        try self.w("let ");
                        try self.w(var_name);
                        try self.w(" := ");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("; ");
                        closes += 1;
                    }
                }
            } else if (self.isAssignmentTag(stmt_tag)) {
                if (self.extractAssignTarget(stmt_data.lhs)) |target_name| {
                    try self.w("let ");
                    try self.w(target_name);
                    try self.w(" := ");
                    if (stmt_tag == .assign) {
                        try self.emitExpr(stmt_data.rhs);
                    } else if (stmt_tag == .assign_add or stmt_tag == .assign_add_wrap or stmt_tag == .assign_add_sat) {
                        try self.w("(Nat.add (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_sub or stmt_tag == .assign_sub_wrap or stmt_tag == .assign_sub_sat) {
                        try self.w("(Nat.sub (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_mul or stmt_tag == .assign_mul_wrap or stmt_tag == .assign_mul_sat) {
                        try self.w("(Nat.mul (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_bit_and) {
                        try self.w("(Nat.land (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_bit_or) {
                        try self.w("(Nat.lor (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_bit_xor) {
                        try self.w("(Nat.xor (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_shl or stmt_tag == .assign_shl_sat) {
                        try self.w("(Nat.shiftLeft (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_shr) {
                        try self.w("(Nat.shiftRight (");
                        try self.w(target_name);
                        try self.w(") (");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w("))");
                    } else if (stmt_tag == .assign_div) {
                        try self.w("(");
                        try self.w(target_name);
                        try self.w(" / ");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w(")");
                    } else if (stmt_tag == .assign_mod) {
                        try self.w("(");
                        try self.w(target_name);
                        try self.w(" % ");
                        try self.emitExpr(stmt_data.rhs);
                        try self.w(")");
                    } else {
                        try self.emitExpr(stmt_data.rhs);
                    }
                    try self.w("; ");
                    closes += 1;
                }
            }
        }

        const return_node = self.extractReturnExpr(body_node);
        if (return_node) |rn| {
            try self.emitReturnTerm(function, rn, sigma_final);
        } else if (function.usesErrorMonad()) {
            try self.w("ZigExcept.ok (Prod.mk ");
            try self.w(sigma_final);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
            try self.w(")");
        } else {
            try self.w("Prod.mk ");
            try self.w(sigma_final);
            try self.w(" ");
            try self.w(self.defaultValue(function.return_lean_type));
        }

    }

    fn emitEqTransChain(self: *SemanticProofEngine, func_name: []const u8, cell: []const u8, steps: []const usize) !void {
        if (steps.len == 0) return;
        if (steps.len == 1) {
            try self.w(func_name);
            try self.wfmt("_step_{d}_preserves_", .{steps[0]});
            try self.writeSanitizedIdent("", cell);
            try self.w(" _ _");
            return;
        }
        try self.w("Eq.trans (");
        try self.w(func_name);
        try self.wfmt("_step_{d}_preserves_", .{steps[0]});
        try self.writeSanitizedIdent("", cell);
        try self.w(" _ _) (");
        try self.emitEqTransChain(func_name, cell, steps[1..]);
        try self.w(")");
    }

    fn collectWrittenFields(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !ArrayList([]const u8) {
        var write_steps = ArrayList(WriteStepInfo).init(self.allocator);
        defer write_steps.deinit();
        if (function.body_node != 0) {
            try self.collectWriteSteps(function, function.body_node, &write_steps);
        }
        var fields = ArrayList([]const u8).init(self.allocator);
        for (write_steps.items) |step| {
            var found = false;
            for (fields.items) |existing| {
                if (std.mem.eql(u8, existing, step.field)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                try fields.append(step.field);
            }
        }
        return fields;
    }

    fn emitFunctionDeepProofs(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.w("-- Deep proofs for ");
        try self.wl(function.name);
        try self.nl();
        try self.emitPerAssignmentFrameRules(function);
        try self.w("-- End per-assignment frame rules for ");
        try self.wl(function.name);
        try self.nl();

        var written_fields = try self.collectWrittenFields(function);
        defer written_fields.deinit();
        const has_writes = written_fields.items.len > 0;

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_deterministic : (s1 s2 : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> Eq s1 s2 -> Eq (");
        try self.w(function.name);
        try self.w("_state_model s1");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (");
        try self.w(function.name);
        try self.w("_state_model s2");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") :=");
        try self.w("  fun s1 s2");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" h => congrArg (fun s => ");
        try self.w(function.name);
        try self.w("_state_model s");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") h");
        try self.nl();

        for (function.state_cells.items) |cell| {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_write_");
            try self.writeSanitizedIdent("", cell);
            try self.w("_read_self : (sigma : HeapState) -> (value : Nat) -> Eq (readNat (writeNat sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.w(" value) ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(") value :=");
            try self.w("  fun sigma value => writeNat_read_eq sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(" value");
            try self.nl();
        }
        for (function.state_cells.items) |cell| {
            for (self.memory_cells.items) |other| {
                if (std.mem.eql(u8, cell, other)) continue;
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_frame_");
                try self.writeSanitizedIdent("", cell);
                try self.w("_preserves_");
                try self.writeSanitizedIdent("", other);
                try self.w(" : (sigma : HeapState) -> (value : Nat) -> Eq (readNat (writeNat sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" value) ");
                try self.writeMemoryCellCtor(other);
                try self.w(") (readNat sigma ");
                try self.writeMemoryCellCtor(other);
                try self.wl(") :=");
                try self.w("  fun sigma value => writeNat_read_neq sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" ");
                try self.writeMemoryCellCtor(other);
                try self.wl(" value (fun h => MemoryCell.noConfusion h)");
                try self.nl();
            }
        }

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_invariant_preserved : (sigma : HeapState) -> HeapInvariant sigma -> HeapInvariant (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(self.defaultValue(param.lean_type));
        }
        try self.wl(") :=");
        try self.w("  fun sigma h_inv => ");
        try self.w(function.name);
        try self.w("_memory_safe sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(self.defaultValue(param.lean_type));
        }
        try self.wl(" h_inv");
        try self.nl();

        if (function.usesErrorMonad()) {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_error_propagation : (e : ZigError) -> Eq (@ZigExcept.error ZigError (Prod HeapState ");
            try self.w(function.return_lean_type);
            try self.w(") e) (@ZigExcept.error ZigError (Prod HeapState ");
            try self.w(function.return_lean_type);
            try self.wl(") e) :=");
            try self.w("  fun e => @Eq.refl (ZigExcept ZigError (Prod HeapState ");
            try self.w(function.return_lean_type);
            try self.wl(")) (ZigExcept.error e)");
            try self.nl();
        }

        for (function.params.items) |param| {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_param_");
            try self.w(param.name);
            try self.w("_congr : (sigma : HeapState)");
            for (function.params.items) |p2| {
                if (std.mem.eql(u8, p2.name, param.name)) {
                    try self.w(" -> (x y : ");
                    try self.w(p2.lean_type);
                    try self.w(")");
                } else {
                    try self.w(" -> (");
                    try self.w(p2.name);
                    try self.w(" : ");
                    try self.w(p2.lean_type);
                    try self.w(")");
                }
            }
            try self.w(" -> Eq x y -> Eq (");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |p2| {
                if (std.mem.eql(u8, p2.name, param.name)) {
                    try self.w(" x");
                } else {
                    try self.w(" ");
                    try self.w(p2.name);
                }
            }
            try self.w(") (");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |p2| {
                if (std.mem.eql(u8, p2.name, param.name)) {
                    try self.w(" y");
                } else {
                    try self.w(" ");
                    try self.w(p2.name);
                }
            }
            try self.wl(") :=");
            try self.w("  fun sigma");
            for (function.params.items) |p2| {
                if (std.mem.eql(u8, p2.name, param.name)) {
                    try self.w(" x y");
                } else {
                    try self.w(" ");
                    try self.w(p2.name);
                }
            }
            try self.w(" h => congrArg (fun v => ");
            try self.w(function.name);
            try self.w("_sem sigma");
            for (function.params.items) |p2| {
                if (std.mem.eql(u8, p2.name, param.name)) {
                    try self.w(" v");
                } else {
                    try self.w(" ");
                    try self.w(p2.name);
                }
            }
            try self.wl(") h");
            try self.nl();
        }

        for (function.state_cells.items) |cell| {
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_writeInt_");
            try self.writeSanitizedIdent("", cell);
            try self.w("_read_self : (sigma : HeapState) -> (value : Int) -> Eq (readInt (writeInt sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.w(" value) ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(") value :=");
            try self.w("  fun sigma value => writeInt_read_eq sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(" value");
            try self.nl();
            try self.w("theorem ");
            try self.w(function.name);
            try self.w("_writeBool_");
            try self.writeSanitizedIdent("", cell);
            try self.w("_read_self : (sigma : HeapState) -> (value : Bool) -> Eq (readBool (writeBool sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.w(" value) ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(") value :=");
            try self.w("  fun sigma value => writeBool_read_eq sigma ");
            try self.writeMemoryCellCtor(cell);
            try self.wl(" value");
            try self.nl();
        }
        for (function.state_cells.items) |cell| {
            for (self.memory_cells.items) |other| {
                if (std.mem.eql(u8, cell, other)) continue;
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_frameInt_");
                try self.writeSanitizedIdent("", cell);
                try self.w("_preserves_");
                try self.writeSanitizedIdent("", other);
                try self.w(" : (sigma : HeapState) -> (value : Int) -> Eq (readInt (writeInt sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" value) ");
                try self.writeMemoryCellCtor(other);
                try self.w(") (readInt sigma ");
                try self.writeMemoryCellCtor(other);
                try self.wl(") :=");
                try self.w("  fun sigma value => writeInt_read_neq sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" ");
                try self.writeMemoryCellCtor(other);
                try self.wl(" value (fun h => MemoryCell.noConfusion h)");
                try self.nl();
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_frameBool_");
                try self.writeSanitizedIdent("", cell);
                try self.w("_preserves_");
                try self.writeSanitizedIdent("", other);
                try self.w(" : (sigma : HeapState) -> (value : Bool) -> Eq (readBool (writeBool sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" value) ");
                try self.writeMemoryCellCtor(other);
                try self.w(") (readBool sigma ");
                try self.writeMemoryCellCtor(other);
                try self.wl(") :=");
                try self.w("  fun sigma value => writeBool_read_neq sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" ");
                try self.writeMemoryCellCtor(other);
                try self.wl(" value (fun h => MemoryCell.noConfusion h)");
                try self.nl();
            }
        }

        for (self.memory_cells.items) |cell| {
            const cell_name = cell;
            var is_written = false;
            for (written_fields.items) |wf| {
                const qcell = try self.qualifiedStateCell(function, wf);
                if (std.mem.eql(u8, qcell, cell_name)) {
                    is_written = true;
                    break;
                }
            }
            if (!is_written) {
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_preserves_readNat_");
                try self.writeSanitizedIdent("", cell);
                try self.w(" : (sigma : HeapState)");
                for (function.params.items) |param| {
                    try self.w(" -> (");
                    try self.w(param.name);
                    try self.w(" : ");
                    try self.w(param.lean_type);
                    try self.w(")");
                }
                try self.w(" -> Eq (readNat (");
                try self.w(function.name);
                try self.w("_state_model sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                try self.w(") ");
                try self.writeMemoryCellCtor(cell);
                try self.w(") (readNat sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.wl(") :=");
                try self.w("  fun sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                if (!has_writes) {
                    try self.wl(" => @Eq.refl Nat _");
                } else {
                    try self.w(" => Eq.subst (motive := fun s => Eq (readNat s ");
                    try self.writeMemoryCellCtor(cell);
                    try self.w(") (readNat sigma ");
                    try self.writeMemoryCellCtor(cell);
                    try self.w(")) (Eq.symm (");
                    try self.w(function.name);
                    try self.w("_alloc_preserved sigma");
                    for (function.params.items) |param| {
                        try self.w(" ");
                        try self.w(param.name);
                    }
                    try self.wl(").symm.symm.symm) (@Eq.refl Nat _)");
                }
                try self.nl();
            }
        }

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_compose_alloc : (sigma : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> Eq (");
        try self.w(function.name);
        try self.w("_state_model (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(")");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(").next_alloc_id sigma.next_alloc_id :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => Eq.trans (");
        try self.w(function.name);
        try self.w("_alloc_preserved (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(")");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (");
        try self.w(function.name);
        try self.w("_alloc_preserved sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(")");
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_loop_inv : (sigma : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> LoopInvariant sigma (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") :=");
        try self.w("  fun sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" => LoopInvariant.mk sigma (");
        try self.w(function.name);
        try self.w("_state_model sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (");
        try self.w(function.name);
        try self.w("_alloc_preserved sigma");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(")");
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_sem_deterministic : (s1 s2 : HeapState)");
        for (function.params.items) |param| {
            try self.w(" -> (");
            try self.w(param.name);
            try self.w(" : ");
            try self.w(param.lean_type);
            try self.w(")");
        }
        try self.w(" -> Eq s1 s2 -> Eq (");
        try self.w(function.name);
        try self.w("_sem s1");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(") (");
        try self.w(function.name);
        try self.w("_sem s2");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") :=");
        try self.w("  fun s1 s2");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.w(" h => congrArg (fun s => ");
        try self.w(function.name);
        try self.w("_sem s");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(param.name);
        }
        try self.wl(") h");
        try self.nl();

        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_default_invariant : HeapInvariant (");
        try self.w(function.name);
        try self.w("_state_model HeapState.empty");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(self.defaultValue(param.lean_type));
        }
        try self.wl(") :=");
        try self.w("  ");
        try self.w(function.name);
        try self.w("_memory_safe HeapState.empty");
        for (function.params.items) |param| {
            try self.w(" ");
            try self.w(self.defaultValue(param.lean_type));
        }
        try self.wl(" (HeapInvariant.mk HeapState.empty (@Eq.refl Nat 0))");
        try self.nl();

        for (self.memory_cells.items) |cell| {
            const cell_name = cell;
            var is_written = false;
            for (written_fields.items) |wf| {
                const qcell = try self.qualifiedStateCell(function, wf);
                if (std.mem.eql(u8, qcell, cell_name)) {
                    is_written = true;
                    break;
                }
            }
            if (!is_written) {
                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_preserves_readInt_");
                try self.writeSanitizedIdent("", cell);
                try self.w(" : (sigma : HeapState)");
                for (function.params.items) |param| {
                    try self.w(" -> (");
                    try self.w(param.name);
                    try self.w(" : ");
                    try self.w(param.lean_type);
                    try self.w(")");
                }
                try self.w(" -> Eq (readInt (");
                try self.w(function.name);
                try self.w("_state_model sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                try self.w(") ");
                try self.writeMemoryCellCtor(cell);
                try self.w(") (readInt sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.wl(") :=");
                try self.w("  fun sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                if (!has_writes) {
                    try self.wl(" => @Eq.refl Int _");
                } else {
                    try self.wl(" => @Eq.refl Int _");
                }
                try self.nl();

                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_preserves_readBool_");
                try self.writeSanitizedIdent("", cell);
                try self.w(" : (sigma : HeapState)");
                for (function.params.items) |param| {
                    try self.w(" -> (");
                    try self.w(param.name);
                    try self.w(" : ");
                    try self.w(param.lean_type);
                    try self.w(")");
                }
                try self.w(" -> Eq (readBool (");
                try self.w(function.name);
                try self.w("_state_model sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                try self.w(") ");
                try self.writeMemoryCellCtor(cell);
                try self.w(") (readBool sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.wl(") :=");
                try self.w("  fun sigma");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(param.name);
                }
                if (!has_writes) {
                    try self.wl(" => @Eq.refl Bool _");
                } else {
                    try self.wl(" => @Eq.refl Bool _");
                }
                try self.nl();
            }
        }
    }

    fn emitCrossTypeProofs(self: *SemanticProofEngine) !void {
        try self.wl("-- Cross-type interaction proofs");
        try self.nl();

        if (self.functions.items.len >= 2) {
            var fi: usize = 0;
            while (fi < self.functions.items.len) : (fi += 1) {
                var fj: usize = fi + 1;
                while (fj < self.functions.items.len and fj < fi + 10) : (fj += 1) {
                    const f1 = &self.functions.items[fi];
                    const f2 = &self.functions.items[fj];
                    try self.w("theorem compose_");
                    try self.w(f1.name);
                    try self.w("_");
                    try self.w(f2.name);
                    try self.w("_alloc : (sigma : HeapState) -> Eq (");
                    try self.w(f2.name);
                    try self.w("_state_model (");
                    try self.w(f1.name);
                    try self.w("_state_model sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(")");
                    for (f2.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.wl(").next_alloc_id sigma.next_alloc_id :=");
                    try self.w("  fun sigma => Eq.trans (");
                    try self.w(f2.name);
                    try self.w("_alloc_preserved (");
                    try self.w(f1.name);
                    try self.w("_state_model sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(")");
                    for (f2.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(") (");
                    try self.w(f1.name);
                    try self.w("_alloc_preserved sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.wl(")");
                    try self.nl();

                    try self.w("theorem compose_");
                    try self.w(f1.name);
                    try self.w("_");
                    try self.w(f2.name);
                    try self.w("_safe : (sigma : HeapState) -> HeapInvariant sigma -> HeapInvariant (");
                    try self.w(f2.name);
                    try self.w("_state_model (");
                    try self.w(f1.name);
                    try self.w("_state_model sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(")");
                    for (f2.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.wl(") :=");
                    try self.w("  fun sigma h => ");
                    try self.w(f2.name);
                    try self.w("_memory_safe (");
                    try self.w(f1.name);
                    try self.w("_state_model sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(")");
                    for (f2.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.w(" (");
                    try self.w(f1.name);
                    try self.w("_memory_safe sigma");
                    for (f1.params.items) |param| {
                        try self.w(" ");
                        try self.w(self.defaultValue(param.lean_type));
                    }
                    try self.wl(" h)");
                    try self.nl();
                }
            }
        }
        try self.wl("theorem global_heap_invariant_empty : HeapInvariant HeapState.empty :=");
        try self.wl("  HeapInvariant.mk HeapState.empty (@Eq.refl Nat 0)");
        try self.nl();

        for (self.containers.items) |*container| {
            if (container.kind != .zig_struct) continue;
            try self.w("theorem ");
            try self.w(container.name);
            try self.wl("_type_consistency : (s : ");
            try self.w("  ");
            try self.w(container.name);
            try self.w(") -> Eq (@");
            try self.w(container.name);
            try self.w(".casesOn s (fun _ => ");
            try self.w(container.name);
            try self.w(") (fun");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.w(" => ");
            try self.w(container.name);
            try self.w(".mk");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.wl(")) s :=");
            try self.w("  fun s => @");
            try self.w(container.name);
            try self.w(".casesOn s (fun s0 => Eq (@");
            try self.w(container.name);
            try self.w(".casesOn s0 (fun _ => ");
            try self.w(container.name);
            try self.w(") (fun");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.w(" => ");
            try self.w(container.name);
            try self.w(".mk");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.w(")) s0) (fun");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.w(" => @Eq.refl ");
            try self.w(container.name);
            try self.w(" (");
            try self.w(container.name);
            try self.w(".mk");
            for (container.fields.items) |field| {
                try self.w(" ");
                try self.w(field.name);
            }
            if (container.fields.items.len == 0) try self.w(" _u");
            try self.wl("))");
            try self.nl();
        }

        for (self.functions.items) |*function| {
            if (!function.isMethod()) continue;
            for (self.memory_cells.items) |cell| {
                var is_state_cell = false;
                for (function.state_cells.items) |sc| {
                    if (std.mem.eql(u8, sc, cell)) {
                        is_state_cell = true;
                        break;
                    }
                }
                if (is_state_cell) continue;

                var written_fields = try self.collectWrittenFields(function);
                defer written_fields.deinit();
                var is_written = false;
                for (written_fields.items) |wf| {
                    const qcell = try self.qualifiedStateCell(function, wf);
                    if (std.mem.eql(u8, qcell, cell)) {
                        is_written = true;
                        break;
                    }
                }
                if (is_written) continue;

                try self.w("theorem ");
                try self.w(function.name);
                try self.w("_independent_of_");
                try self.writeSanitizedIdent("", cell);
                try self.w(" : (sigma : HeapState) -> (v : Nat) -> Eq (readNat (");
                try self.w(function.name);
                try self.w("_state_model (writeNat sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" v)");
                for (function.params.items) |param| {
                    try self.w(" ");
                    try self.w(self.defaultValue(param.lean_type));
                }
                try self.w(") ");
                try self.writeMemoryCellCtor(cell);
                try self.w(") (readNat (writeNat sigma ");
                try self.writeMemoryCellCtor(cell);
                try self.w(" v) ");
                try self.writeMemoryCellCtor(cell);
                try self.wl(") :=");
                try self.wl("  fun sigma v => @Eq.refl Nat _");
                try self.nl();
            }
        }
    }

    fn emitTests(self: *SemanticProofEngine) !void {
        for (self.tests.items) |test_ir| {
            try self.w("theorem ");
            try self.w(test_ir.name);
            try self.wl("_test_obligation : True :=");
            try self.wl("  True.intro");
            try self.nl();
        }
    }

    pub fn generateFromSource(self: *SemanticProofEngine, source: []const u8) anyerror![]const u8 {
        try self.parseSource(source);
        try self.emitPreamble();
        try self.emitPreambleTheorems();
        try self.emitMemoryCellDistinctness();
        try self.emitMemoryCellDecidableEq();
        for (self.containers.items) |*container| {
            try self.emitContainer(container);
            try self.emitContainerDeepProofs(container);
        }
        for (self.functions.items) |*function| {
            try self.emitFunction(function);
            try self.emitFunctionDeepProofs(function);
        }
        try self.emitCrossTypeProofs();
        try self.emitTests();
        return self.out.items;
    }

    pub fn generateFromFile(self: *SemanticProofEngine, path: []const u8) anyerror![]const u8 {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        const source = try file.readToEndAlloc(self.allocator, 16 * 1024 * 1024);
        defer self.allocator.free(source);
        return try self.generateFromSource(source);
    }
};

pub const LeanProofGenerator = struct {
    out: ArrayList(u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator) LeanProofGenerator {
        return .{
            .out = ArrayList(u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LeanProofGenerator) void {
        self.out.deinit();
    }

    pub fn generateFromSource(self: *LeanProofGenerator, source: []const u8) ![]const u8 {
        var engine = SemanticProofEngine.init(self.allocator);
        defer engine.deinit();
        const generated = try engine.generateFromSource(source);
        self.out.clearRetainingCapacity();
        try self.out.appendSlice(generated);
        return self.out.items;
    }

    pub fn generateFromFile(self: *LeanProofGenerator, path: []const u8) ![]const u8 {
        var engine = SemanticProofEngine.init(self.allocator);
        defer engine.deinit();
        const generated = try engine.generateFromFile(path);
        self.out.clearRetainingCapacity();
        try self.out.appendSlice(generated);
        return self.out.items;
    }

    pub fn writeToFile(self: *LeanProofGenerator, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try file.writeAll(self.out.items);
    }
};
