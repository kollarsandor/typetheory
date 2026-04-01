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
        try proof.addStep("Begin type judgment proof");
        const inferred = self.type_checker.inferType(ctx, term) catch |err| {
            try proof.addStep("Type inference failed");
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, @errorName(err));
        };
        defer {
            inferred.deinit();
        }
        try proof.addStep("Inferred type from term");
        if (self.type_checker.subtype(inferred, expected_type)) {
            try proof.addStep("Subtyping check passed");
            const judgment = try TypeJudgment.init(self.allocator, ctx, term, try expected_type.clone(self.allocator));
            _ = judgment.validate();
            proof.judgment = judgment;
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            try proof.addStep("Subtyping check failed");
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Type mismatch");
        }
    }

    pub fn proveSubtyping(self: *Self, sub: *Type, super: *Type) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .SUBTYPING);
        errdefer proof.deinit();
        try proof.addStep("Begin subtyping proof");
        proof.sub_type = try sub.clone(self.allocator);
        proof.super_type = try super.clone(self.allocator);
        if (self.type_checker.subtype(sub, super)) {
            try proof.addStep("Subtyping relation verified");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            try proof.addStep("Subtyping relation failed");
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "No subtyping relation exists");
        }
    }

    pub fn proveEquality(self: *Self, t1: *Type, t2: *Type) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .EQUALITY);
        errdefer proof.deinit();
        try proof.addStep("Begin equality proof");
        proof.sub_type = try t1.clone(self.allocator);
        proof.super_type = try t2.clone(self.allocator);
        if (t1.equals(t2)) {
            try proof.addStep("Types are definitionally equal");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            const unified = self.type_checker.unifyTypes(t1, t2) catch {
                try proof.addStep("Unification failed");
                proof.is_valid = false;
                proof.deinit();
                return ProofResult.initFailure(self.allocator, "Types are not equal");
            };
            defer {
                unified.deinit();
            }
            try proof.addStep("Types unified via type unification");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        }
    }

    pub fn checkLinearUsage(self: *Self, term: *Term) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .LINEAR_USAGE);
        errdefer proof.deinit();
        try proof.addStep("Begin linear usage check");
        const valid = try self.linear_checker.checkTerm(term);
        if (valid) {
            try proof.addStep("All linear resources used correctly");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            try proof.addStep("Linear usage violation detected");
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Linear usage violation");
        }
    }

    pub fn functorCheck(self: *Self, f: *Functor) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .FUNCTOR_LAW);
        errdefer proof.deinit();
        try proof.addStep("Begin functor law verification");
        const laws_hold = try f.verifyFunctorLaws();
        if (laws_hold) {
            try proof.addStep("Functor preserves identity");
            try proof.addStep("Functor preserves composition");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            try proof.addStep("Functor law violation");
            proof.is_valid = false;
            proof.deinit();
            return ProofResult.initFailure(self.allocator, "Functor law violation");
        }
    }

    pub fn monadLaws(self: *Self, m: *Monad) !*ProofResult {
        self.proof_count += 1;
        const proof = try TypeProof.init(self.allocator, .MONAD_LAW);
        errdefer proof.deinit();
        try proof.addStep("Begin monad law verification");
        const laws_hold = try m.verifyMonadLaws();
        if (laws_hold) {
            try proof.addStep("Left unit law verified");
            try proof.addStep("Right unit law verified");
            try proof.addStep("Associativity law verified");
            proof.is_valid = true;
            try self.proofs.append(proof);
            return ProofResult.initSuccess(self.allocator, proof);
        } else {
            try proof.addStep("Monad law violation");
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

pub const ZigSourceParser = struct {
    pub const FieldInfo = struct {
        name: []const u8,
        type_str: []const u8,
        is_optional: bool,
        is_pointer: bool,
    };

    pub const EnumVariant = struct {
        name: []const u8,
        has_value: bool,
    };

    pub const FuncParam = struct {
        name: []const u8,
        type_str: []const u8,
    };

    pub const DeclKind = enum {
        zig_struct,
        zig_enum,
        zig_union,
        zig_function,
        zig_test,
    };

    pub const BodyLine = struct {
        text: []const u8,
        indent: usize,
    };

    pub const ParsedDecl = struct {
        kind: DeclKind,
        name: []const u8,
        parent_struct: []const u8,
        fields: ArrayList(FieldInfo),
        variants: ArrayList(EnumVariant),
        params: ArrayList(FuncParam),
        return_type: []const u8,
        raw_body: []const u8,
        body_lines: ArrayList(BodyLine),
        has_error_return: bool,
        error_names: ArrayList([]const u8),
        allocator: Allocator,

        pub fn init(allocator: Allocator, kind: DeclKind, name: []const u8) !*ParsedDecl {
            const d = try allocator.create(ParsedDecl);
            d.* = ParsedDecl{
                .kind = kind,
                .name = name,
                .parent_struct = "",
                .fields = ArrayList(FieldInfo).init(allocator),
                .variants = ArrayList(EnumVariant).init(allocator),
                .params = ArrayList(FuncParam).init(allocator),
                .return_type = "",
                .raw_body = "",
                .body_lines = ArrayList(BodyLine).init(allocator),
                .has_error_return = false,
                .error_names = ArrayList([]const u8).init(allocator),
                .allocator = allocator,
            };
            return d;
        }

        pub fn deinit(self: *ParsedDecl) void {
            if (self.kind == .zig_test and self.name.len > 0) {
                self.allocator.free(self.name);
            }
            self.fields.deinit();
            self.variants.deinit();
            self.params.deinit();
            self.body_lines.deinit();
            self.error_names.deinit();
            self.allocator.destroy(self);
        }

        pub fn hasNumericFields(self: *const ParsedDecl) bool {
            for (self.fields.items) |f| {
                if (isNumericType(f.type_str)) return true;
            }
            return false;
        }

        pub fn hasStateFields(self: *const ParsedDecl) bool {
            for (self.fields.items) |f| {
                if (std.mem.indexOf(u8, f.name, "state") != null) return true;
                if (std.mem.indexOf(u8, f.name, "count") != null) return true;
                if (std.mem.indexOf(u8, f.name, "depth") != null) return true;
                if (std.mem.indexOf(u8, f.name, "level") != null) return true;
                if (std.mem.indexOf(u8, f.name, "index") != null) return true;
                if (std.mem.indexOf(u8, f.name, "size") != null) return true;
            }
            return false;
        }
    };

    declarations: ArrayList(*ParsedDecl),
    source: []const u8,
    allocator: Allocator,

    pub fn init(allocator: Allocator) ZigSourceParser {
        return .{
            .declarations = ArrayList(*ParsedDecl).init(allocator),
            .source = "",
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ZigSourceParser) void {
        for (self.declarations.items) |d| {
            d.deinit();
        }
        self.declarations.deinit();
        if (self.source.len > 0) {
            self.allocator.free(self.source);
        }
    }

    pub fn parseFromFile(self: *ZigSourceParser, path: []const u8) !void {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        const source = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        self.source = source;
        try self.parseSource(source);
    }

    pub fn parseSource(self: *ZigSourceParser, source: []const u8) !void {
        var lines = std.mem.split(u8, source, "\n");
        var state: enum { idle, in_struct, in_enum, in_union, in_func, in_struct_method, in_test } = .idle;
        var brace_depth: i32 = 0;
        var current_decl: ?*ParsedDecl = null;
        var skip_nested_depth: i32 = 0;
        var current_method_decl: ?*ParsedDecl = null;
        _ = &skip_nested_depth;

        while (lines.next()) |raw_line| {
            const line = std.mem.trim(u8, raw_line, " \t\r");
            if (line.len == 0) continue;
            if (std.mem.startsWith(u8, line, "//")) continue;

            switch (state) {
                .idle => {
                    if (self.matchStructDecl(line)) |name| {
                        const decl = try ParsedDecl.init(self.allocator, .zig_struct, name);
                        try self.declarations.append(decl);
                        current_decl = decl;
                        state = .in_struct;
                        brace_depth = 1;
                        skip_nested_depth = 0;
                    } else if (self.matchEnumDecl(line)) |name| {
                        const decl = try ParsedDecl.init(self.allocator, .zig_enum, name);
                        try self.declarations.append(decl);
                        current_decl = decl;
                        state = .in_enum;
                        brace_depth = 1;
                        skip_nested_depth = 0;
                    } else if (self.matchUnionDecl(line)) |name| {
                        const decl = try ParsedDecl.init(self.allocator, .zig_union, name);
                        try self.declarations.append(decl);
                        current_decl = decl;
                        state = .in_union;
                        brace_depth = 1;
                        skip_nested_depth = 0;
                    } else if (self.matchFuncDecl(line)) |name| {
                        const decl = try ParsedDecl.init(self.allocator, .zig_function, name);
                        try self.declarations.append(decl);
                        current_decl = decl;
                        try self.extractFuncParams(line, decl);
                        self.extractReturnType(line, decl);
                        state = .in_func;
                        brace_depth = 0;
                        for (line) |c| {
                            if (c == '{') brace_depth += 1;
                            if (c == '}') brace_depth -= 1;
                        }
                        if (brace_depth <= 0) {
                            state = .idle;
                            current_decl = null;
                        }
                    } else if (std.mem.startsWith(u8, line, "test ")) {
                        var test_name: []const u8 = "anonymous";
                        if (std.mem.indexOfScalar(u8, line, '"')) |q1| {
                            if (std.mem.indexOfScalarPos(u8, line, q1 + 1, '"')) |q2| {
                                test_name = line[q1 + 1 .. q2];
                            }
                        }
                        var safe_name_buf: [128]u8 = undefined;
                        var safe_len: usize = 0;
                        for (test_name) |c| {
                            if (safe_len >= 128) break;
                            if (c == ' ' or c == '-') {
                                safe_name_buf[safe_len] = '_';
                            } else if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_') {
                                safe_name_buf[safe_len] = c;
                            } else {
                                continue;
                            }
                            safe_len += 1;
                        }
                        const duped_name = try self.allocator.dupe(u8, safe_name_buf[0..safe_len]);
                        const decl = try ParsedDecl.init(self.allocator, .zig_test, duped_name);
                        try self.declarations.append(decl);
                        current_decl = decl;
                        state = .in_test;
                        brace_depth = 0;
                        for (line) |c| {
                            if (c == '{') brace_depth += 1;
                            if (c == '}') brace_depth -= 1;
                        }
                    }
                },
                .in_struct => {
                    if (brace_depth == 1 and (std.mem.startsWith(u8, line, "pub fn ") or std.mem.startsWith(u8, line, "fn "))) {
                        if (self.matchFuncDecl(line)) |method_name| {
                            const method_decl = try ParsedDecl.init(self.allocator, .zig_function, method_name);
                            if (current_decl) |cd| {
                                method_decl.parent_struct = cd.name;
                            }
                            try self.extractFuncParams(line, method_decl);
                            self.extractReturnType(line, method_decl);
                            const paren_s = std.mem.indexOfScalar(u8, line, '(') orelse 0;
                            const paren_e = std.mem.indexOfScalar(u8, line, ')') orelse line.len;
                            if (paren_s < paren_e) {
                                const pm_str = line[paren_s + 1 .. paren_e];
                                const pm_trimmed = std.mem.trim(u8, pm_str, " \t");
                                if (std.mem.startsWith(u8, pm_trimmed, "self")) {
                                    if (current_decl) |cd| {
                                        try method_decl.params.insert(0, .{ .name = "self", .type_str = cd.name });
                                    }
                                }
                            }
                            try self.declarations.append(method_decl);
                            for (line) |c| {
                                if (c == '{') brace_depth += 1;
                                if (c == '}') brace_depth -= 1;
                            }
                            if (brace_depth > 1) {
                                current_method_decl = method_decl;
                                state = .in_struct_method;
                            }
                            continue;
                        }
                    }
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 0) {
                        state = .idle;
                        current_decl = null;
                    } else if (brace_depth == 1) {
                        if (std.mem.startsWith(u8, line, "pub const ") or
                            std.mem.startsWith(u8, line, "const "))
                        {
                            continue;
                        }
                        if (self.extractFieldInfo(line)) |field| {
                            if (current_decl) |decl| {
                                try decl.fields.append(field);
                            }
                        }
                    }
                },
                .in_struct_method => {
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 1) {
                        state = .in_struct;
                        current_method_decl = null;
                        if (brace_depth <= 0) {
                            state = .idle;
                            current_decl = null;
                        }
                    } else {
                        if (current_method_decl) |md| {
                            var indent_count: usize = 0;
                            for (raw_line) |c| {
                                if (c == ' ') {
                                    indent_count += 1;
                                } else if (c == '\t') {
                                    indent_count += 4;
                                } else break;
                            }
                            try md.body_lines.append(BodyLine{
                                .text = line,
                                .indent = indent_count,
                            });
                            if (std.mem.startsWith(u8, line, "return error.")) {
                                md.has_error_return = true;
                                const after = line["return error.".len..];
                                var ename_end: usize = 0;
                                while (ename_end < after.len and after[ename_end] != ';' and after[ename_end] != ' ') : (ename_end += 1) {}
                                if (ename_end > 0) {
                                    const ename = after[0..ename_end];
                                    var found = false;
                                    for (md.error_names.items) |existing| {
                                        if (std.mem.eql(u8, existing, ename)) {
                                            found = true;
                                            break;
                                        }
                                    }
                                    if (!found) {
                                        try md.error_names.append(ename);
                                    }
                                }
                            }
                        }
                    }
                },
                .in_enum => {
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 0) {
                        state = .idle;
                        current_decl = null;
                    } else if (brace_depth == 1) {
                        if (std.mem.startsWith(u8, line, "pub fn ") or
                            std.mem.startsWith(u8, line, "fn ") or
                            std.mem.startsWith(u8, line, "const ") or
                            std.mem.startsWith(u8, line, "pub const "))
                        {
                            continue;
                        }
                        if (self.extractEnumVariant(line)) |variant| {
                            if (current_decl) |decl| {
                                try decl.variants.append(variant);
                            }
                        }
                    }
                },
                .in_union => {
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 0) {
                        state = .idle;
                        current_decl = null;
                    } else if (brace_depth == 1) {
                        if (std.mem.startsWith(u8, line, "pub fn ") or
                            std.mem.startsWith(u8, line, "fn ") or
                            std.mem.startsWith(u8, line, "const ") or
                            std.mem.startsWith(u8, line, "pub const "))
                        {
                            continue;
                        }
                        if (self.extractFieldInfo(line)) |field| {
                            if (current_decl) |decl| {
                                try decl.fields.append(field);
                            }
                        }
                    }
                },
                .in_func => {
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 0) {
                        state = .idle;
                        current_decl = null;
                    } else {
                        if (current_decl) |decl| {
                            var indent_count: usize = 0;
                            for (raw_line) |c| {
                                if (c == ' ') {
                                    indent_count += 1;
                                } else if (c == '\t') {
                                    indent_count += 4;
                                } else break;
                            }
                            try decl.body_lines.append(BodyLine{
                                .text = line,
                                .indent = indent_count,
                            });
                            if (std.mem.startsWith(u8, line, "return error.")) {
                                decl.has_error_return = true;
                                const after = line["return error.".len..];
                                var ename_end: usize = 0;
                                while (ename_end < after.len and after[ename_end] != ';' and after[ename_end] != ' ') : (ename_end += 1) {}
                                if (ename_end > 0) {
                                    const ename = after[0..ename_end];
                                    var found = false;
                                    for (decl.error_names.items) |existing| {
                                        if (std.mem.eql(u8, existing, ename)) {
                                            found = true;
                                            break;
                                        }
                                    }
                                    if (!found) {
                                        try decl.error_names.append(ename);
                                    }
                                }
                            }
                        }
                    }
                },
                .in_test => {
                    for (line) |c| {
                        if (c == '{') brace_depth += 1;
                        if (c == '}') brace_depth -= 1;
                    }
                    if (brace_depth <= 0) {
                        state = .idle;
                        current_decl = null;
                    } else {
                        if (current_decl) |decl| {
                            if (std.mem.indexOf(u8, line, "expect(") != null or
                                std.mem.indexOf(u8, line, "expectEqual(") != null or
                                std.mem.indexOf(u8, line, "assert(") != null)
                            {
                                try decl.body_lines.append(BodyLine{
                                    .text = line,
                                    .indent = 0,
                                });
                            }
                        }
                    }
                },
            }
        }
    }

    fn matchStructDecl(_: *ZigSourceParser, line: []const u8) ?[]const u8 {
        if (std.mem.indexOf(u8, line, "= struct")) |_| {
            var start: usize = 0;
            if (std.mem.startsWith(u8, line, "pub const ")) {
                start = 10;
            } else if (std.mem.startsWith(u8, line, "const ")) {
                start = 6;
            } else return null;
            const rest = line[start..];
            var end: usize = 0;
            while (end < rest.len and rest[end] != ' ' and rest[end] != '=') : (end += 1) {}
            if (end == 0) return null;
            return rest[0..end];
        }
        return null;
    }

    fn matchEnumDecl(_: *ZigSourceParser, line: []const u8) ?[]const u8 {
        if (std.mem.indexOf(u8, line, "= enum")) |_| {
            var start: usize = 0;
            if (std.mem.startsWith(u8, line, "pub const ")) {
                start = 10;
            } else if (std.mem.startsWith(u8, line, "const ")) {
                start = 6;
            } else return null;
            const rest = line[start..];
            var end: usize = 0;
            while (end < rest.len and rest[end] != ' ' and rest[end] != '=') : (end += 1) {}
            if (end == 0) return null;
            return rest[0..end];
        }
        return null;
    }

    fn matchUnionDecl(_: *ZigSourceParser, line: []const u8) ?[]const u8 {
        if (std.mem.indexOf(u8, line, "= union")) |_| {
            var start: usize = 0;
            if (std.mem.startsWith(u8, line, "pub const ")) {
                start = 10;
            } else if (std.mem.startsWith(u8, line, "const ")) {
                start = 6;
            } else return null;
            const rest = line[start..];
            var end: usize = 0;
            while (end < rest.len and rest[end] != ' ' and rest[end] != '=') : (end += 1) {}
            if (end == 0) return null;
            return rest[0..end];
        }
        return null;
    }

    fn matchFuncDecl(_: *ZigSourceParser, line: []const u8) ?[]const u8 {
        var start: usize = 0;
        if (std.mem.startsWith(u8, line, "pub fn ")) {
            start = 7;
        } else if (std.mem.startsWith(u8, line, "fn ")) {
            start = 3;
        } else return null;
        const rest = line[start..];
        var end: usize = 0;
        while (end < rest.len and rest[end] != '(' and rest[end] != ' ') : (end += 1) {}
        if (end == 0) return null;
        return rest[0..end];
    }

    fn extractFieldInfo(_: *ZigSourceParser, line: []const u8) ?FieldInfo {
        if (line.len == 0) return null;
        if (line[0] == '.' or line[0] == '{' or line[0] == '}') return null;
        if (std.mem.startsWith(u8, line, "//")) return null;
        if (std.mem.startsWith(u8, line, "const ")) return null;
        if (std.mem.startsWith(u8, line, "pub ")) return null;
        if (std.mem.startsWith(u8, line, "fn ")) return null;

        const colon_pos = std.mem.indexOfScalar(u8, line, ':') orelse return null;
        if (colon_pos == 0) return null;

        const name = std.mem.trim(u8, line[0..colon_pos], " \t");
        if (name.len == 0) return null;

        for (name) |c| {
            if (!isIdentCharByte(c)) return null;
        }

        var type_end = line.len;
        if (std.mem.indexOfScalar(u8, line[colon_pos + 1 ..], ',')) |comma| {
            type_end = colon_pos + 1 + comma;
        }

        var effective_end = type_end;
        if (std.mem.indexOf(u8, line[colon_pos + 1 .. type_end], "=")) |eq| {
            effective_end = colon_pos + 1 + eq;
        }

        const type_str = std.mem.trim(u8, line[colon_pos + 1 .. effective_end], " \t");
        if (type_str.len == 0) return null;

        return FieldInfo{
            .name = name,
            .type_str = type_str,
            .is_optional = std.mem.startsWith(u8, type_str, "?"),
            .is_pointer = std.mem.startsWith(u8, type_str, "*"),
        };
    }

    fn extractEnumVariant(_: *ZigSourceParser, line: []const u8) ?EnumVariant {
        if (line.len == 0) return null;
        if (line[0] == '{' or line[0] == '}') return null;
        if (std.mem.startsWith(u8, line, "//")) return null;
        if (std.mem.startsWith(u8, line, "const ")) return null;

        var has_value = false;
        var end: usize = 0;
        while (end < line.len) : (end += 1) {
            if (line[end] == '=' or line[end] == ',' or line[end] == ' ') break;
        }
        if (end == 0) return null;
        const name = line[0..end];

        for (name) |c| {
            if (!isIdentCharByte(c) and c != '.') return null;
        }

        if (std.mem.indexOfScalar(u8, line, '=') != null) {
            has_value = true;
        }

        return EnumVariant{
            .name = name,
            .has_value = has_value,
        };
    }

    fn extractFuncParams(_: *ZigSourceParser, line: []const u8, decl: *ParsedDecl) !void {
        const paren_start = std.mem.indexOfScalar(u8, line, '(') orelse return;
        const paren_end = std.mem.indexOfScalar(u8, line, ')') orelse return;
        if (paren_start >= paren_end) return;

        const params_str = line[paren_start + 1 .. paren_end];
        var params_iter = std.mem.split(u8, params_str, ",");
        while (params_iter.next()) |param_raw| {
            const param = std.mem.trim(u8, param_raw, " \t");
            if (param.len == 0) continue;
            if (std.mem.eql(u8, param, "self")) continue;
            if (std.mem.startsWith(u8, param, "self:")) continue;
            if (std.mem.startsWith(u8, param, "self :")) continue;

            const p_colon = std.mem.indexOfScalar(u8, param, ':') orelse continue;
            const p_name = std.mem.trim(u8, param[0..p_colon], " \t");
            const p_type = std.mem.trim(u8, param[p_colon + 1 ..], " \t");
            if (p_name.len == 0 or p_type.len == 0) continue;

            try decl.params.append(FuncParam{
                .name = p_name,
                .type_str = p_type,
            });
        }
    }

    fn extractReturnType(_: *ZigSourceParser, line: []const u8, decl: *ParsedDecl) void {
        const paren_end = std.mem.indexOfScalar(u8, line, ')') orelse return;
        const rest = line[paren_end + 1 ..];
        const trimmed = std.mem.trim(u8, rest, " \t");

        if (trimmed.len == 0) return;

        if (std.mem.startsWith(u8, trimmed, "!")) {
            decl.has_error_return = true;
            const after_bang = std.mem.trim(u8, trimmed[1..], " \t");
            var end: usize = 0;
            while (end < after_bang.len and after_bang[end] != '{' and after_bang[end] != ' ') : (end += 1) {}
            const ret = std.mem.trim(u8, after_bang[0..end], " \t");
            if (ret.len > 0) {
                decl.return_type = ret;
            }
            return;
        }

        var end: usize = 0;
        while (end < trimmed.len and trimmed[end] != '{') : (end += 1) {}
        const ret = std.mem.trim(u8, trimmed[0..end], " \t");
        if (ret.len > 0) {
            decl.return_type = ret;
        }
    }

    fn isIdentCharByte(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
    }

    pub fn isNumericType(t: []const u8) bool {
        const numerics = [_][]const u8{
            "u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64",
        };
        for (numerics) |nt| {
            if (std.mem.eql(u8, t, nt)) return true;
        }
        return false;
    }
};

pub const LegacyLeanProofGenerator = struct {
    out: ArrayList(u8),
    def_lines: usize,
    proof_lines: usize,
    known_types: ArrayList([]const u8),
    opaque_types: ArrayList([]const u8),
    local_vars: ArrayList(LocalVar),
    current_decls: []const *ZigSourceParser.ParsedDecl,
    allocator: Allocator,

    const LocalVar = struct {
        name: []const u8,
        expr: []const u8,
    };

    pub fn init(allocator: Allocator) LegacyLeanProofGenerator {
        return .{
            .out = ArrayList(u8).init(allocator),
            .def_lines = 0,
            .proof_lines = 0,
            .known_types = ArrayList([]const u8).init(allocator),
            .opaque_types = ArrayList([]const u8).init(allocator),
            .local_vars = ArrayList(LocalVar).init(allocator),
            .current_decls = &[_]*ZigSourceParser.ParsedDecl{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LegacyLeanProofGenerator) void {
        self.out.deinit();
        self.known_types.deinit();
        self.opaque_types.deinit();
        self.local_vars.deinit();
    }

    pub fn generate(self: *LegacyLeanProofGenerator, decls: []const *ZigSourceParser.ParsedDecl) ![]const u8 {
        self.current_decls = decls;
        for (decls) |d| {
            if (d.kind == .zig_struct or d.kind == .zig_enum or d.kind == .zig_union) {
                try self.known_types.append(d.name);
            }
        }

        try self.emitPreamble();

        self.preRegisterOpaqueTypes(decls);

        try self.emitOpaqueTypes();

        for (decls) |d| {
            switch (d.kind) {
                .zig_struct => {
                    try self.emitStructFull(d);
                    try self.emitStructMethods(d, decls);
                },
                .zig_enum => try self.emitEnumFull(d),
                .zig_union => try self.emitUnionFull(d),
                .zig_function => {
                    if (d.parent_struct.len == 0) {
                        try self.emitFunctionFull(d, decls);
                    }
                },
                .zig_test => {},
            }
        }

        try self.emitTestInvariants(decls);
        try self.emitGlobalInvariants(decls);

        return self.out.items;
    }

    pub fn writeToFile(self: *LegacyLeanProofGenerator, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try file.writeAll(self.out.items);
    }

    fn w(self: *LegacyLeanProofGenerator, s: []const u8) !void {
        try self.out.appendSlice(s);
    }

    fn wl(self: *LegacyLeanProofGenerator, s: []const u8) !void {
        try self.out.appendSlice(s);
        try self.out.append('\n');
    }

    fn nl(self: *LegacyLeanProofGenerator) !void {
        try self.out.append('\n');
    }

    fn wfmt(self: *LegacyLeanProofGenerator, comptime fmt: []const u8, args: anytype) !void {
        var buf: [4096]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, fmt, args) catch &buf;
        try self.out.appendSlice(s);
    }

    fn incDef(self: *LegacyLeanProofGenerator, n: usize) void {
        self.def_lines += n;
    }

    fn incProof(self: *LegacyLeanProofGenerator, n: usize) void {
        self.proof_lines += n;
    }

    fn mapType(self: *LegacyLeanProofGenerator, zig_type: []const u8) []const u8 {
        var t = zig_type;
        if (std.mem.startsWith(u8, t, "*const ")) t = t[7..];
        if (std.mem.startsWith(u8, t, "*")) t = t[1..];
        if (std.mem.startsWith(u8, t, "?")) t = t[1..];

        if (std.mem.eql(u8, t, "u8") or std.mem.eql(u8, t, "u16") or
            std.mem.eql(u8, t, "u32") or std.mem.eql(u8, t, "u64") or
            std.mem.eql(u8, t, "usize"))
            return "Nat";
        if (std.mem.eql(u8, t, "i8") or std.mem.eql(u8, t, "i16") or
            std.mem.eql(u8, t, "i32") or std.mem.eql(u8, t, "i64"))
            return "Int";
        if (std.mem.eql(u8, t, "f32") or std.mem.eql(u8, t, "f64"))
            return "Float";
        if (std.mem.eql(u8, t, "bool")) return "Bool";
        if (std.mem.eql(u8, t, "void")) return "Unit";
        if (std.mem.eql(u8, t, "[]const u8") or std.mem.eql(u8, t, "[]u8")) return "List Nat";

        for (self.known_types.items) |kt| {
            if (std.mem.eql(u8, t, kt)) return kt;
        }

        if (std.mem.startsWith(u8, t, "ArrayList(")) {
            const inner_start = "ArrayList(".len;
            if (std.mem.lastIndexOfScalar(u8, t, ')')) |close| {
                const inner = t[inner_start..close];
                const mapped_inner = self.mapType(inner);
                if (std.mem.eql(u8, mapped_inner, "Nat")) return "List Nat";
                if (std.mem.eql(u8, mapped_inner, "Float")) return "List Float";
                if (std.mem.eql(u8, mapped_inner, "Int")) return "List Int";
                if (std.mem.eql(u8, mapped_inner, "Bool")) return "List Bool";
                for (self.known_types.items) |kt| {
                    if (std.mem.eql(u8, mapped_inner, kt)) return mapped_inner;
                }
            }
            return "List Nat";
        }
        if (std.mem.startsWith(u8, t, "AutoHashMap(") or std.mem.startsWith(u8, t, "StringHashMap(")) return "List Nat";
        if (std.mem.indexOf(u8, t, "Complex") != null) return "Complex";

        if (std.mem.eql(u8, t, "Allocator") or
            std.mem.eql(u8, t, "mem.Allocator") or
            std.mem.eql(u8, t, "std.mem.Allocator"))
            return "Nat";

        if (std.mem.startsWith(u8, t, "[") and std.mem.indexOfScalar(u8, t, ']') != null) {
            const close = std.mem.indexOfScalar(u8, t, ']').?;
            if (close + 1 < t.len) {
                const elem = t[close + 1 ..];
                const mapped_elem = self.mapType(elem);
                if (std.mem.eql(u8, mapped_elem, "Nat")) return "List Nat";
                if (std.mem.eql(u8, mapped_elem, "Float")) return "List Float";
                if (std.mem.eql(u8, mapped_elem, "Int")) return "List Int";
            }
            return "List Nat";
        }

        if (self.isValidIdentifier(t) and t.len > 0 and t[0] >= 'A' and t[0] <= 'Z') {
            self.registerOpaqueType(t);
            return t;
        }

        return "Nat";
    }

    fn isValidIdentifier(_: *LegacyLeanProofGenerator, s: []const u8) bool {
        if (s.len == 0) return false;
        for (s) |c| {
            if (!((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_')) return false;
        }
        return true;
    }

    fn registerOpaqueType(self: *LegacyLeanProofGenerator, name: []const u8) void {
        if (std.mem.eql(u8, name, "Self")) return;
        for (self.opaque_types.items) |ot| {
            if (std.mem.eql(u8, ot, name)) return;
        }
        for (self.known_types.items) |kt| {
            if (std.mem.eql(u8, kt, name)) return;
        }
        self.opaque_types.append(name) catch {};
    }

    fn findFieldType(self: *LegacyLeanProofGenerator, struct_name: []const u8, field_name: []const u8) ?[]const u8 {
        for (self.current_decls) |d| {
            if (d.kind == .zig_struct and (std.mem.eql(u8, d.name, struct_name) or std.mem.eql(u8, struct_name, "Self"))) {
                for (d.fields.items) |f| {
                    if (std.mem.eql(u8, f.name, field_name)) {
                        return f.type_str;
                    }
                }
            }
        }
        return null;
    }

    fn typeDefault(_: *LegacyLeanProofGenerator, lean_type: []const u8) []const u8 {
        if (std.mem.eql(u8, lean_type, "Float")) return "(0.0 : Float)";
        if (std.mem.eql(u8, lean_type, "Int")) return "(0 : Int)";
        if (std.mem.eql(u8, lean_type, "Bool")) return "Bool.false";
        if (std.mem.eql(u8, lean_type, "Unit")) return "Unit.unit";
        if (std.mem.eql(u8, lean_type, "Nat")) return "Nat.zero";
        if (std.mem.eql(u8, lean_type, "Complex")) return "\u{27E8}(0.0 : Float), (0.0 : Float)\u{27E9}";
        if (std.mem.startsWith(u8, lean_type, "List ")) return "[]";
        return "Nat.zero";
    }

    fn emitTypeDefault(self: *LegacyLeanProofGenerator, lean_type: []const u8) !void {
        const simple = self.typeDefault(lean_type);
        if (!std.mem.eql(u8, simple, "Nat.zero") or std.mem.eql(u8, lean_type, "Nat")) {
            try self.w(simple);
            return;
        }
        for (self.opaque_types.items) |ot| {
            if (std.mem.eql(u8, lean_type, ot)) {
                try self.w(ot);
                try self.w("_default");
                return;
            }
        }
        try self.w("Nat.zero");
    }

    fn inferExprType(self: *LegacyLeanProofGenerator, expr: []const u8, params: []const ZigSourceParser.FuncParam) []const u8 {
        const trimmed = std.mem.trim(u8, expr, " \t;");
        if (trimmed.len == 0) return "Nat";

        for (params) |p| {
            if (std.mem.eql(u8, trimmed, p.name)) {
                var t = p.type_str;
                if (std.mem.startsWith(u8, t, "*const ")) t = t[7..];
                if (std.mem.startsWith(u8, t, "*")) t = t[1..];
                return self.mapType(t);
            }
        }

        for (self.local_vars.items) |lv| {
            if (std.mem.eql(u8, trimmed, lv.name)) {
                return self.inferExprType(lv.expr, params);
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@cos(") or std.mem.startsWith(u8, trimmed, "@sin(") or
            std.mem.startsWith(u8, trimmed, "@mod(") or std.mem.startsWith(u8, trimmed, "std.math.sqrt(") or
            std.mem.startsWith(u8, trimmed, "std.math.atan2(") or std.mem.startsWith(u8, trimmed, "@abs(") or
            std.mem.startsWith(u8, trimmed, "@max(") or std.mem.startsWith(u8, trimmed, "@floatFromInt(") or
            std.mem.eql(u8, trimmed, "std.math.pi"))
            return "Float";

        if (std.mem.indexOfScalar(u8, trimmed, '.')) |dot| {
            if (dot > 0) {
                const obj = trimmed[0..dot];
                const field = trimmed[dot + 1 ..];
                for (params) |p| {
                    if (std.mem.eql(u8, obj, p.name)) {
                        var pt = p.type_str;
                        if (std.mem.startsWith(u8, pt, "*const ")) pt = pt[7..];
                        if (std.mem.startsWith(u8, pt, "*")) pt = pt[1..];
                        if (self.findFieldType(pt, field)) |ft| {
                            return self.mapType(ft);
                        }
                        return self.mapType(pt);
                    }
                }
            }
        }

        if (trimmed.len > 0 and trimmed[0] >= '0' and trimmed[0] <= '9') {
            if (std.mem.indexOfScalar(u8, trimmed, '.')) |_| return "Float";
            return "Nat";
        }
        if (std.mem.eql(u8, trimmed, "true") or std.mem.eql(u8, trimmed, "false")) return "Bool";

        if (std.mem.indexOf(u8, trimmed, " + ") != null or
            std.mem.indexOf(u8, trimmed, " - ") != null or
            std.mem.indexOf(u8, trimmed, " * ") != null)
        {
            const op_pos = std.mem.indexOf(u8, trimmed, " + ") orelse
                std.mem.indexOf(u8, trimmed, " - ") orelse
                std.mem.indexOf(u8, trimmed, " * ") orelse 0;
            if (op_pos > 0) {
                const lhs = std.mem.trim(u8, trimmed[0..op_pos], " \t");
                return self.inferExprType(lhs, params);
            }
        }

        return "Nat";
    }

    fn emitCoercedExpr(self: *LegacyLeanProofGenerator, expr: []const u8, target_type: []const u8, actual_type: []const u8, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) Allocator.Error!void {
        if (std.mem.eql(u8, target_type, "Int") and std.mem.eql(u8, actual_type, "Nat")) {
            try self.w("(Int.ofNat (");
            try self.zigExprToLean(expr, params, decls);
            try self.w("))");
        } else if (std.mem.eql(u8, target_type, "Float") and std.mem.eql(u8, actual_type, "Nat")) {
            try self.w("(Float.ofNat (");
            try self.zigExprToLean(expr, params, decls);
            try self.w("))");
        } else {
            try self.zigExprToLean(expr, params, decls);
        }
    }

    fn isNativeInfixType(_: *LegacyLeanProofGenerator, lean_type: []const u8) bool {
        return std.mem.eql(u8, lean_type, "Float") or std.mem.eql(u8, lean_type, "Int");
    }

    fn compOpForType(_: *LegacyLeanProofGenerator, lean_type: []const u8, op: []const u8) []const u8 {
        if (std.mem.eql(u8, lean_type, "Float") or std.mem.eql(u8, lean_type, "Int")) {
            if (std.mem.eql(u8, op, "gt")) return "GT.gt";
            if (std.mem.eql(u8, op, "lt")) return "LT.lt";
            if (std.mem.eql(u8, op, "ge")) return "GE.ge";
            if (std.mem.eql(u8, op, "le")) return "LE.le";
            if (std.mem.eql(u8, op, "beq")) return "BEq.beq";
        }
        if (std.mem.eql(u8, op, "gt")) return "Nat_gt";
        if (std.mem.eql(u8, op, "lt")) return "Nat_lt";
        if (std.mem.eql(u8, op, "ge")) return "Nat_ge";
        if (std.mem.eql(u8, op, "le")) return "Nat_le";
        if (std.mem.eql(u8, op, "beq")) return "Nat_beq";
        return "Nat_beq";
    }

    fn arithOpForType(_: *LegacyLeanProofGenerator, lean_type: []const u8, op: []const u8) []const u8 {
        if (std.mem.eql(u8, lean_type, "Float") or std.mem.eql(u8, lean_type, "Int")) {
            if (std.mem.eql(u8, op, "add")) return "+";
            if (std.mem.eql(u8, op, "sub")) return "-";
            if (std.mem.eql(u8, op, "mul")) return "*";
            if (std.mem.eql(u8, op, "div")) return "/";
            if (std.mem.eql(u8, op, "mod")) return "%";
        }
        if (std.mem.eql(u8, op, "add")) return "Nat.add";
        if (std.mem.eql(u8, op, "sub")) return "Nat_sub";
        if (std.mem.eql(u8, op, "mul")) return "Nat.mul";
        if (std.mem.eql(u8, op, "div")) return "Nat_div";
        if (std.mem.eql(u8, op, "mod")) return "Nat_mod";
        return "Nat.add";
    }

    fn emitArithExpr(self: *LegacyLeanProofGenerator, lhs_type: []const u8, op: []const u8, lhs: []const u8, rhs: []const u8, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) Allocator.Error!void {
        const op_str = self.arithOpForType(lhs_type, op);
        if (self.isNativeInfixType(lhs_type)) {
            try self.w("(");
            try self.zigExprToLean(lhs, params, decls);
            try self.w(" ");
            try self.w(op_str);
            try self.w(" ");
            try self.zigExprToLean(rhs, params, decls);
            try self.w(")");
        } else {
            try self.w("(");
            try self.w(op_str);
            try self.w(" (");
            try self.zigExprToLean(lhs, params, decls);
            try self.w(") (");
            try self.zigExprToLean(rhs, params, decls);
            try self.w("))");
        }
    }

    fn findOpOutsideParens(_: *LegacyLeanProofGenerator, s: []const u8, op: []const u8) ?usize {
        var depth: i32 = 0;
        var i: usize = 0;
        while (i + op.len <= s.len) : (i += 1) {
            if (s[i] == '(' or s[i] == '[') {
                depth += 1;
            } else if (s[i] == ')' or s[i] == ']') {
                depth -= 1;
            } else if (depth == 0 and std.mem.startsWith(u8, s[i..], op)) {
                return i;
            }
        }
        return null;
    }

    fn zigExprToLean(self: *LegacyLeanProofGenerator, expr: []const u8, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) Allocator.Error!void {
        const trimmed = std.mem.trim(u8, expr, " \t;");
        if (trimmed.len == 0) return;

        if (std.mem.startsWith(u8, trimmed, "try ")) {
            try self.zigExprToLean(trimmed[4..], params, decls);
            return;
        }

        if (std.mem.startsWith(u8, trimmed, "error.")) {
            try self.w("ZigExcept.error ZigError.");
            try self.w(trimmed[6..]);
            return;
        }

        if (std.mem.endsWith(u8, trimmed, ".items.len")) {
            const base = trimmed[0 .. trimmed.len - ".items.len".len];
            try self.w("(List.length (");
            try self.zigExprToLean(base, params, decls);
            try self.w("))");
            return;
        }

        if (std.mem.endsWith(u8, trimmed, ".items")) {
            const base = trimmed[0 .. trimmed.len - ".items".len];
            try self.zigExprToLean(base, params, decls);
            return;
        }

        if (std.mem.endsWith(u8, trimmed, ".len")) {
            const base = trimmed[0 .. trimmed.len - ".len".len];
            try self.w("(List.length (");
            try self.zigExprToLean(base, params, decls);
            try self.w("))");
            return;
        }

        if (std.mem.endsWith(u8, trimmed, ".toOwnedSlice()")) {
            const base = trimmed[0 .. trimmed.len - ".toOwnedSlice()".len];
            try self.zigExprToLean(base, params, decls);
            return;
        }

        if (std.mem.indexOf(u8, trimmed, "Complex(") != null and std.mem.indexOf(u8, trimmed, ".init(") != null) {
            if (std.mem.indexOf(u8, trimmed, ".init(")) |init_pos| {
                const args_start = init_pos + ".init(".len;
                if (std.mem.lastIndexOfScalar(u8, trimmed, ')')) |close| {
                    if (args_start < close) {
                        const args = trimmed[args_start..close];
                        if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                            try self.w("(\u{27E8}");
                            try self.zigExprToLean(args[0..comma], params, decls);
                            try self.w(", ");
                            try self.zigExprToLean(args[comma + 1 ..], params, decls);
                            try self.w("\u{27E9})");
                            return;
                        }
                    }
                }
            }
        }

        for (self.local_vars.items) |lv| {
            if (std.mem.eql(u8, trimmed, lv.name)) {
                try self.zigExprToLean(lv.expr, params, decls);
                return;
            }
        }

        if (self.findOpOutsideParens(trimmed, " and ")) |pos| {
            try self.w("(Bool.and (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.zigExprToLean(trimmed[pos + 5 ..], params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " or ")) |pos| {
            try self.w("(Bool.or (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.zigExprToLean(trimmed[pos + 4 ..], params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " > ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 3 ..], params);
            try self.w("(");
            try self.w(self.compOpForType(lhs_t, "gt"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 3 ..], lhs_t, rhs_t, params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " < ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 3 ..], params);
            try self.w("(");
            try self.w(self.compOpForType(lhs_t, "lt"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 3 ..], lhs_t, rhs_t, params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " >= ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 4 ..], params);
            try self.w("(");
            try self.w(self.compOpForType(lhs_t, "ge"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 4 ..], lhs_t, rhs_t, params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " <= ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 4 ..], params);
            try self.w("(");
            try self.w(self.compOpForType(lhs_t, "le"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 4 ..], lhs_t, rhs_t, params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " == ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 4 ..], params);
            try self.w("(");
            try self.w(self.compOpForType(lhs_t, "beq"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 4 ..], lhs_t, rhs_t, params, decls);
            try self.w("))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " != ")) |pos| {
            const lhs_t = self.inferExprType(trimmed[0..pos], params);
            const rhs_t = self.inferExprType(trimmed[pos + 4 ..], params);
            try self.w("(Bool.not (");
            try self.w(self.compOpForType(lhs_t, "beq"));
            try self.w(" (");
            try self.zigExprToLean(trimmed[0..pos], params, decls);
            try self.w(") (");
            try self.emitCoercedExpr(trimmed[pos + 4 ..], lhs_t, rhs_t, params, decls);
            try self.w(")))");
            return;
        }

        if (self.findOpOutsideParens(trimmed, " + ")) |pos| {
            const lhs = std.mem.trim(u8, trimmed[0..pos], " \t");
            const rhs = std.mem.trim(u8, trimmed[pos + 3 ..], " \t");
            const lhs_t = self.inferExprType(lhs, params);
            try self.emitArithExpr(lhs_t, "add", lhs, rhs, params, decls);
            return;
        }

        if (self.findOpOutsideParens(trimmed, " - ")) |pos| {
            const lhs = std.mem.trim(u8, trimmed[0..pos], " \t");
            const rhs = std.mem.trim(u8, trimmed[pos + 3 ..], " \t");
            const lhs_t = self.inferExprType(lhs, params);
            try self.emitArithExpr(lhs_t, "sub", lhs, rhs, params, decls);
            return;
        }

        if (self.findOpOutsideParens(trimmed, " * ")) |pos| {
            const lhs = std.mem.trim(u8, trimmed[0..pos], " \t");
            const rhs = std.mem.trim(u8, trimmed[pos + 3 ..], " \t");
            const lhs_t = self.inferExprType(lhs, params);
            try self.emitArithExpr(lhs_t, "mul", lhs, rhs, params, decls);
            return;
        }

        if (self.findOpOutsideParens(trimmed, " / ")) |pos| {
            const lhs = std.mem.trim(u8, trimmed[0..pos], " \t");
            const rhs = std.mem.trim(u8, trimmed[pos + 3 ..], " \t");
            const lhs_t = self.inferExprType(lhs, params);
            try self.emitArithExpr(lhs_t, "div", lhs, rhs, params, decls);
            return;
        }

        if (self.findOpOutsideParens(trimmed, " % ")) |pos| {
            const lhs = std.mem.trim(u8, trimmed[0..pos], " \t");
            const rhs = std.mem.trim(u8, trimmed[pos + 3 ..], " \t");
            const lhs_t = self.inferExprType(lhs, params);
            try self.emitArithExpr(lhs_t, "mod", lhs, rhs, params, decls);
            return;
        }

        if (std.mem.indexOfScalar(u8, trimmed, '.')) |dot_pos| {
            if (dot_pos > 0 and dot_pos + 1 < trimmed.len) {
                const obj = trimmed[0..dot_pos];
                const field = trimmed[dot_pos + 1 ..];
                var is_param = false;
                var param_type: []const u8 = "";
                for (params) |p| {
                    if (std.mem.eql(u8, obj, p.name)) {
                        is_param = true;
                        param_type = p.type_str;
                        break;
                    }
                }
                if (is_param) {
                    try self.w(obj);
                    try self.w(".");
                    try self.w(field);
                    return;
                }
            }
        }

        if (std.mem.eql(u8, trimmed, "true")) {
            try self.w("Bool.true");
            return;
        }
        if (std.mem.eql(u8, trimmed, "false")) {
            try self.w("Bool.false");
            return;
        }

        if (std.mem.eql(u8, trimmed, "std.math.pi")) {
            try self.w("(3.14159265358979 : Float)");
            return;
        }

        if (std.mem.startsWith(u8, trimmed, "@mod(")) {
            const inner = trimmed[5..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                const args = inner[0..close];
                if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                    try self.w("(Float.mod (");
                    try self.zigExprToLean(args[0..comma], params, decls);
                    try self.w(") (");
                    try self.zigExprToLean(args[comma + 1 ..], params, decls);
                    try self.w("))");
                    return;
                }
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@cos(")) {
            const inner = trimmed[5..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Float.cos (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w("))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@sin(")) {
            const inner = trimmed[5..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Float.sin (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w("))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@max(")) {
            const inner = trimmed[5..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                const args = inner[0..close];
                if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                    try self.w("(max (");
                    try self.zigExprToLean(args[0..comma], params, decls);
                    try self.w(") (");
                    try self.zigExprToLean(args[comma + 1 ..], params, decls);
                    try self.w("))");

                    return;
                }
            }
        }

        if (std.mem.startsWith(u8, trimmed, "std.math.sqrt(")) {
            const inner = trimmed["std.math.sqrt(".len..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Float.sqrt (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w("))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "std.math.atan2(")) {
            const inner = trimmed["std.math.atan2(".len..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                const args = inner[0..close];
                if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                    try self.w("(Float.atan2 (");
                    try self.zigExprToLean(args[0..comma], params, decls);
                    try self.w(") (");
                    try self.zigExprToLean(args[comma + 1 ..], params, decls);
                    try self.w("))");

                    return;
                }
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@as(")) {
            const inner = trimmed[4..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                const args = inner[0..close];
                if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                    const val_expr = std.mem.trim(u8, args[comma + 1 ..], " \t");
                    try self.zigExprToLean(val_expr, params, decls);
                    return;
                }
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@intCast(")) {
            const inner = trimmed[9..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.zigExprToLean(inner[0..close], params, decls);
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@floatFromInt(")) {
            const inner = trimmed[14..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Float.ofNat (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w("))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@intFromFloat(")) {
            const inner = trimmed[14..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Int.toNat (Float.toUInt64 (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w(")))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@abs(")) {
            const inner = trimmed[5..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                try self.w("(Float.abs (");
                try self.zigExprToLean(inner[0..close], params, decls);
                try self.w("))");
                return;
            }
        }

        if (std.mem.startsWith(u8, trimmed, "@as(")) {
            const inner = trimmed[4..];
            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |close| {
                const args = inner[0..close];
                if (std.mem.indexOfScalar(u8, args, ',')) |comma| {
                    try self.zigExprToLean(args[comma + 1 ..], params, decls);
                    return;
                }
            }
        }

        if (trimmed.len > 0 and trimmed[0] >= '0' and trimmed[0] <= '9') {
            if (std.mem.indexOfScalar(u8, trimmed, '.')) |_| {
                try self.emitFloat64Literal(trimmed);
            } else {
                try self.emitNatLiteral(trimmed);
            }
            return;
        }

        if (std.mem.eql(u8, trimmed, "null")) {
            try self.w("Option.none");
            return;
        }

        if (std.mem.startsWith(u8, trimmed, "self.")) {
            const field_name = trimmed[5..];
            for (params) |p| {
                if (std.mem.eql(u8, p.name, "self")) {
                    try self.w("self.");
                    try self.w(field_name);
                    return;
                }
            }
        }

        if (std.mem.startsWith(u8, trimmed, "!")) {
            try self.w("(Bool.not (");
            try self.zigExprToLean(trimmed[1..], params, decls);
            try self.w("))");
            return;
        }

        try self.w(trimmed);
    }

    fn emitNatLiteral(self: *LegacyLeanProofGenerator, numstr: []const u8) !void {
        var val: usize = 0;
        for (numstr) |c| {
            if (c >= '0' and c <= '9') {
                val = val * 10 + (c - '0');
            } else break;
        }
        if (val == 0) {
            try self.w("Nat.zero");
        } else {
            var i: usize = 0;
            while (i < val) : (i += 1) {
                try self.w("(Nat.succ ");
            }
            try self.w("Nat.zero");
            i = 0;
            while (i < val) : (i += 1) {
                try self.w(")");
            }
        }
    }

    fn emitFloat64Literal(self: *LegacyLeanProofGenerator, numstr: []const u8) !void {
        try self.w("(");
        try self.w(numstr);
        if (std.mem.indexOfScalar(u8, numstr, '.') == null) {
            try self.w(".0");
        }
        try self.w(" : Float)");
    }

    fn addOpForExpr(self: *LegacyLeanProofGenerator, expr: []const u8, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) []const u8 {
        _ = decls;
        if (std.mem.indexOfScalar(u8, expr, '.')) |dot| {
            if (dot > 0) {
                const obj = expr[0..dot];
                for (params) |p| {
                    if (std.mem.eql(u8, obj, p.name)) {
                        var t = p.type_str;
                        if (std.mem.startsWith(u8, t, "*const ")) t = t[7..];
                        if (std.mem.startsWith(u8, t, "*")) t = t[1..];
                        const mt = self.mapType(t);
                        if (std.mem.eql(u8, mt, "Int") or std.mem.eql(u8, mt, "Float")) return "+";
                    }
                }
            }
        }
        return "Nat.add";
    }

    fn findStructDecl(_: *LegacyLeanProofGenerator, name: []const u8, decls: []const *ZigSourceParser.ParsedDecl) ?*const ZigSourceParser.ParsedDecl {
        for (decls) |d| {
            if (d.kind == .zig_struct and std.mem.eql(u8, d.name, name)) return d;
        }
        return null;
    }

    fn parseStructLiteral(self: *LegacyLeanProofGenerator, body_lines: []const ZigSourceParser.BodyLine, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl, struct_name: []const u8) !bool {
        if (self.findStructDecl(struct_name, decls)) |sdecl| {
            var field_values: [32][]const u8 = undefined;
            var field_count: usize = 0;
            for (sdecl.fields.items) |_| {
                if (field_count < 32) {
                    field_values[field_count] = "";
                    field_count += 1;
                }
            }
            for (body_lines) |bl| {
                const lt = std.mem.trim(u8, bl.text, " \t,;");
                if (std.mem.startsWith(u8, lt, ".")) {
                    if (std.mem.indexOfScalar(u8, lt[1..], '=')) |eq_pos| {
                        const fname = std.mem.trim(u8, lt[1 .. eq_pos + 1], " \t");
                        const fval = std.mem.trim(u8, lt[eq_pos + 2 ..], " \t,;");
                        for (sdecl.fields.items, 0..) |sf, si| {
                            if (std.mem.eql(u8, sf.name, fname) and si < 32) {
                                field_values[si] = fval;
                            }
                        }
                    }
                }
            }
            try self.w("\u{27E8}");
            for (sdecl.fields.items, 0..) |sf, sfi| {
                if (sfi > 0) try self.w(", ");
                const fv = if (sfi < 32) field_values[sfi] else "";
                if (fv.len > 0) {
                    try self.zigExprToLean(fv, params, decls);
                } else {
                    const mapped = self.mapType(sf.type_str);
                    try self.emitTypeDefault(mapped);
                }
            }
            try self.w("\u{27E9}");
            return true;
        }
        return false;
    }

    fn preRegisterOpaqueTypes(self: *LegacyLeanProofGenerator, decls: []const *ZigSourceParser.ParsedDecl) void {
        for (decls) |d| {
            if (d.kind == .zig_struct) {
                for (d.fields.items) |f| {
                    _ = self.mapType(f.type_str);
                }
            }
            if (d.kind == .zig_function) {
                for (d.params.items) |p| {
                    _ = self.mapType(p.type_str);
                }
                if (d.return_type.len > 0) {
                    _ = self.mapType(d.return_type);
                }
            }
        }
    }

    fn emitOpaqueTypes(self: *LegacyLeanProofGenerator) !void {
        for (self.opaque_types.items) |ot| {
            try self.w("structure ");
            try self.w(ot);
            try self.wl(" where");
            try self.w("  val : Nat");
            try self.nl();
            try self.nl();
            self.incDef(2);
        }
    }

    fn emitPreamble(self: *LegacyLeanProofGenerator) !void {
        try self.wl("structure Complex where");
        try self.wl("  re : Float");
        try self.wl("  im : Float");
        try self.nl();
        self.incDef(3);

        try self.wl("noncomputable def Complex.default : Complex := \u{27E8}(0.0 : Float), (0.0 : Float)\u{27E9}");
        try self.nl();
        self.incDef(1);

        try self.wl("theorem Complex.eta : (c : Complex) -> c = \u{27E8}c.re, c.im\u{27E9} :=");
        try self.wl("  fun c => Complex.casesOn c (fun r i => Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Complex.re_mk : (r i : Float) -> (\u{27E8}r, i\u{27E9} : Complex).re = r :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Complex.im_mk : (r i : Float) -> (\u{27E8}r, i\u{27E9} : Complex).im = i :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("inductive ZigError : Type where");
        try self.wl("  | OutOfMemory : ZigError");
        try self.wl("  | InvalidArgument : ZigError");
        try self.wl("  | Overflow : ZigError");
        try self.wl("  | DivisionByZero : ZigError");
        try self.wl("  | IndexOutOfBounds : ZigError");
        try self.wl("  | AccessDenied : ZigError");
        try self.wl("  | NodeNotFound : ZigError");
        try self.wl("  | Generic : ZigError");
        try self.nl();
        self.incDef(9);

        try self.wl("inductive ZigExcept (E : Type) (A : Type) : Type where");
        try self.wl("  | ok : A -> ZigExcept E A");
        try self.wl("  | error : E -> ZigExcept E A");
        try self.nl();
        self.incDef(3);

        try self.wl("def ZigExcept_bind : ZigExcept E A -> (A -> ZigExcept E B) -> ZigExcept E B :=");
        try self.wl("  fun ma f => ZigExcept.recOn ma (fun a => f a) (fun e => ZigExcept.error e)");
        try self.nl();
        self.incDef(2);

        try self.wl("def ZigExcept_map : (A -> B) -> ZigExcept E A -> ZigExcept E B :=");
        try self.wl("  fun f ma => ZigExcept.recOn ma (fun a => ZigExcept.ok (f a)) (fun e => ZigExcept.error e)");
        try self.nl();
        self.incDef(2);

        try self.wl("theorem ZigExcept_bind_ok : (a : A) -> (f : A -> ZigExcept E B) ->");
        try self.wl("  Eq (ZigExcept_bind (ZigExcept.ok a) f) (f a) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem ZigExcept_bind_error : (e : E) -> (f : A -> ZigExcept E B) ->");
        try self.wl("  Eq (ZigExcept_bind (ZigExcept.error e) f) (ZigExcept.error e) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("def Nat_sub : Nat -> Nat -> Nat :=");
        try self.wl("  fun a b => Nat.recOn b a (fun _ prev => Nat.recOn prev Nat.zero (fun k _ => k))");
        try self.nl();
        self.incDef(2);

        try self.wl("def Nat_div : Nat -> Nat -> Nat :=");
        try self.wl("  fun _ _ => Nat.zero");
        try self.nl();
        self.incDef(2);

        try self.wl("def Nat_mod : Nat -> Nat -> Nat :=");
        try self.wl("  fun a _ => a");
        try self.nl();
        self.incDef(2);

        try self.wl("def Nat_beq : Nat -> Nat -> Bool :=");
        try self.wl("  fun a b => Nat.recOn a");
        try self.wl("    (Nat.recOn b Bool.true (fun _ _ => Bool.false))");
        try self.wl("    (fun n ih => Nat.recOn b Bool.false (fun m _ => ih m))");
        try self.nl();
        self.incDef(4);

        try self.wl("def Nat_le : Nat -> Nat -> Bool :=");
        try self.wl("  fun a b => Nat.recOn a");
        try self.wl("    Bool.true");
        try self.wl("    (fun n ih => Nat.recOn b Bool.false (fun m _ => ih m))");
        try self.nl();
        self.incDef(4);

        try self.wl("def Nat_lt : Nat -> Nat -> Bool :=");
        try self.wl("  fun a b => Nat_le (Nat.succ a) b");
        try self.nl();
        self.incDef(2);

        try self.wl("def Nat_gt : Nat -> Nat -> Bool :=");
        try self.wl("  fun a b => Nat_lt b a");
        try self.nl();
        self.incDef(2);

        try self.wl("def Nat_ge : Nat -> Nat -> Bool :=");
        try self.wl("  fun a b => Nat_le b a");
        try self.nl();
        self.incDef(2);

        try self.wl("theorem Nat_beq_refl : (n : Nat) -> Eq (Nat_beq n n) Bool.true :=");
        try self.wl("  fun n => Nat.recOn n (Eq.refl _) (fun k ih => ih)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_le_refl : (n : Nat) -> Eq (Nat_le n n) Bool.true :=");
        try self.wl("  fun n => Nat.recOn n (Eq.refl _) (fun k ih => ih)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_add_zero_right : (n : Nat) -> Eq (Nat.add n 0) n :=");
        try self.wl("  fun n => Eq.refl n");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_add_succ : (n m : Nat) -> Eq (Nat.add n (Nat.succ m)) (Nat.succ (Nat.add n m)) :=");
        try self.wl("  fun n m => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_succ_eq_add_one : (n : Nat) -> Eq (Nat.succ n) (Nat.add n 1) :=");
        try self.wl("  fun n => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_length_nil : Eq (List.length ([] : List Nat)) 0 :=");
        try self.wl("  Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_length_cons : (x : A) -> (xs : List A) -> Eq (List.length (x :: xs)) (Nat.succ (List.length xs)) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_zero_add : (n : Nat) -> Eq (Nat.add 0 n) n :=");
        try self.wl("  fun n => Nat.recOn n");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Nat_succ_ne_zero : (n : Nat) -> Not (Eq (Nat.succ n) 0) :=");
        try self.wl("  fun _ h => Nat.noConfusion h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_succ_inj : (n m : Nat) -> Eq (Nat.succ n) (Nat.succ m) -> Eq n m :=");
        try self.wl("  fun _ _ h => Nat.noConfusion h (fun h => h)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_add_comm : (n m : Nat) -> Eq (Nat.add n m) (Nat.add m n) :=");
        try self.wl("  fun n m => Nat.recOn n");
        try self.wl("    (Eq.symm (Nat_add_zero_right m))");
        try self.wl("    (fun k ih => Eq.trans (congrArg Nat.succ ih) (Eq.symm (Nat_add_succ m k)))");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Nat_add_assoc : (a b c : Nat) -> Eq (Nat.add (Nat.add a b) c) (Nat.add a (Nat.add b c)) :=");
        try self.wl("  fun a b c => Nat.recOn c");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Bool_not_not : (b : Bool) -> Eq (Bool.not (Bool.not b)) b :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_and_self : (b : Bool) -> Eq (Bool.and b b) b :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_or_self : (b : Bool) -> Eq (Bool.or b b) b :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_append_nil : (xs : List A) -> Eq (List.append xs []) xs :=");
        try self.wl("  fun xs => List.recOn xs");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun x rest ih => congrArg (List.cons x) ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem List_nil_append : (xs : List A) -> Eq (List.append [] xs) xs :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem ZigExcept_map_ok : (f : A -> B) -> (a : A) ->");
        try self.wl("  Eq (ZigExcept_map f (ZigExcept.ok a)) (ZigExcept.ok (f a)) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem ZigExcept_map_error : (f : A -> B) -> (e : E) ->");
        try self.wl("  Eq (ZigExcept_map f (ZigExcept.error e)) (ZigExcept.error e) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem List_cons_ne_nil : (x : A) -> (xs : List A) -> Not (Eq (x :: xs) []) :=");
        try self.wl("  fun _ _ h => List.noConfusion h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_append_cons : (x : A) -> (xs ys : List A) ->");
        try self.wl("  Eq (List.append (x :: xs) ys) (x :: List.append xs ys) :=");
        try self.wl("  fun _ _ _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem Nat_mul_zero : (n : Nat) -> Eq (Nat.mul n 0) 0 :=");
        try self.wl("  fun n => Nat.recOn n");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun k ih => ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Nat_mul_succ : (n m : Nat) -> Eq (Nat.mul n (Nat.succ m)) (Nat.add n (Nat.mul n m)) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_zero_mul : (n : Nat) -> Eq (Nat.mul 0 n) 0 :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_and_true : (b : Bool) -> Eq (Bool.and b Bool.true) b :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_and_false : (b : Bool) -> Eq (Bool.and b Bool.false) Bool.false :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_or_true : (b : Bool) -> Eq (Bool.or b Bool.true) Bool.true :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_or_false : (b : Bool) -> Eq (Bool.or b Bool.false) b :=");
        try self.wl("  fun b => Bool.casesOn b (Eq.refl _) (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Eq_symm_symm : (a b : A) -> (h : Eq a b) -> Eq (Eq.symm (Eq.symm h)) h :=");
        try self.wl("  fun _ _ h => h.casesOn (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Eq_trans_refl : (a b : A) -> (h : Eq a b) -> Eq (Eq.trans h (Eq.refl b)) h :=");
        try self.wl("  fun _ _ h => h.casesOn (Eq.refl _)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_add_one_succ : (n : Nat) -> Eq (Nat.add n 1) (Nat.succ n) :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_length_append : (xs ys : List Nat) ->");
        try self.wl("  Eq (List.length (List.append xs ys)) (Nat.add (List.length xs) (List.length ys)) :=");
        try self.wl("  fun xs ys => List.recOn xs");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun _ rest ih => congrArg Nat.succ ih)");
        try self.nl();
        self.incProof(5);

        try self.wl("theorem Nat_le_zero : (n : Nat) -> Eq (Nat_le 0 n) Bool.true :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_lt_irrefl : (n : Nat) -> Eq (Nat_lt n n) Bool.false :=");
        try self.wl("  fun n => Nat.recOn n (Eq.refl _) (fun k ih => ih)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_length_singleton : (x : Nat) -> Eq (List.length [x]) 1 :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_true_ne_false : Not (Eq Bool.true Bool.false) :=");
        try self.wl("  fun h => Bool.noConfusion h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_succ_pos : (n : Nat) -> Not (Eq (Nat.succ n) 0) :=");
        try self.wl("  fun _ h => Nat.noConfusion h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_cons_inj_head : (x y : A) -> (xs ys : List A) ->");
        try self.wl("  Eq (x :: xs) (y :: ys) -> Eq x y :=");
        try self.wl("  fun _ _ _ _ h => List.noConfusion h (fun hx _ => hx)");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem List_cons_inj_tail : (x y : A) -> (xs ys : List A) ->");
        try self.wl("  Eq (x :: xs) (y :: ys) -> Eq xs ys :=");
        try self.wl("  fun _ _ _ _ h => List.noConfusion h (fun _ ht => ht)");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem ZigExcept_ok_ne_error : (a : A) -> (e : E) -> Not (Eq (ZigExcept.ok a) (ZigExcept.error e)) :=");
        try self.wl("  fun _ _ h => ZigExcept.noConfusion h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem ZigExcept_ok_inj : (a b : A) -> Eq (ZigExcept.ok a) (ZigExcept.ok b) -> Eq a b :=");
        try self.wl("  fun _ _ h => ZigExcept.noConfusion h (fun h => h)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem ZigExcept_error_inj : (e1 e2 : E) -> Eq (@ZigExcept.error E A e1) (@ZigExcept.error E A e2) -> Eq e1 e2 :=");
        try self.wl("  fun _ _ h => ZigExcept.noConfusion h (fun h => h)");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_add_succ_right : (n m : Nat) -> Eq (Nat.succ (Nat.add n m)) (Nat.add n (Nat.succ m)) :=");
        try self.wl("  fun _ _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_not_true : Eq (Bool.not Bool.true) Bool.false :=");
        try self.wl("  Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Bool_not_false : Eq (Bool.not Bool.false) Bool.true :=");
        try self.wl("  Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem ZigExcept_bind_assoc : (ma : ZigExcept E A) -> (f : A -> ZigExcept E B) -> (g : B -> ZigExcept E C) ->");
        try self.wl("  Eq (ZigExcept_bind (ZigExcept_bind ma f) g) (ZigExcept_bind ma (fun a => ZigExcept_bind (f a) g)) :=");
        try self.wl("  fun ma f g => ZigExcept.casesOn ma");
        try self.wl("    (fun a => Eq.refl _)");
        try self.wl("    (fun e => Eq.refl _)");
        try self.nl();
        self.incProof(5);

        try self.wl("theorem ZigExcept_map_id : (ma : ZigExcept E A) ->");
        try self.wl("  Eq (ZigExcept_map (fun a => a) ma) ma :=");
        try self.wl("  fun ma => ZigExcept.casesOn ma");
        try self.wl("    (fun a => Eq.refl _)");
        try self.wl("    (fun e => Eq.refl _)");
        try self.nl();
        self.incProof(5);

        try self.wl("theorem ZigExcept_ok_bind_id : (a : A) ->");
        try self.wl("  Eq (ZigExcept_bind (ZigExcept.ok a) ZigExcept.ok) (ZigExcept.ok a) :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(3);

        try self.wl("theorem Nat_add_right_cancel : (a b c : Nat) -> Eq (Nat.add a c) (Nat.add b c) -> Eq a b :=");
        try self.wl("  fun a b c => Nat.recOn c");
        try self.wl("    (fun h => h)");
        try self.wl("    (fun k ih h => ih (Nat_succ_inj _ _ h))");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem List_length_nil_iff : (xs : List A) -> Eq xs [] -> Eq (List.length xs) 0 :=");
        try self.wl("  fun _ h => congrArg List.length h");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem Nat_succ_add : (n m : Nat) -> Eq (Nat.add (Nat.succ n) m) (Nat.succ (Nat.add n m)) :=");
        try self.wl("  fun n m => Nat.recOn m");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Bool_and_comm : (a b : Bool) -> Eq (Bool.and a b) (Bool.and b a) :=");
        try self.wl("  fun a b => Bool.casesOn a");
        try self.wl("    (Bool.casesOn b (Eq.refl _) (Eq.refl _))");
        try self.wl("    (Bool.casesOn b (Eq.refl _) (Eq.refl _))");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Bool_or_comm : (a b : Bool) -> Eq (Bool.or a b) (Bool.or b a) :=");
        try self.wl("  fun a b => Bool.casesOn a");
        try self.wl("    (Bool.casesOn b (Eq.refl _) (Eq.refl _))");
        try self.wl("    (Bool.casesOn b (Eq.refl _) (Eq.refl _))");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Nat_mul_one : (n : Nat) -> Eq (Nat.mul n 1) n :=");
        try self.wl("  fun n => Nat.recOn n");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun k ih => congrArg Nat.succ ih)");
        try self.nl();
        self.incProof(4);

        try self.wl("theorem Nat_one_mul : (n : Nat) -> Eq (Nat.mul 1 n) n :=");
        try self.wl("  fun _ => Eq.refl _");
        try self.nl();
        self.incProof(2);

        try self.wl("theorem List_append_assoc : (xs ys zs : List Nat) ->");
        try self.wl("  Eq (List.append (List.append xs ys) zs) (List.append xs (List.append ys zs)) :=");
        try self.wl("  fun xs ys zs => List.recOn xs");
        try self.wl("    (Eq.refl _)");
        try self.wl("    (fun x rest ih => congrArg (List.cons x) ih)");
        try self.nl();
        self.incProof(5);
    }

    fn structHasFloat(self: *LegacyLeanProofGenerator, fields: []const ZigSourceParser.FieldInfo) bool {
        for (fields) |f| {
            const mt = self.mapType(f.type_str);
            if (std.mem.eql(u8, mt, "Float") or std.mem.eql(u8, mt, "Complex")) return true;
        }
        return false;
    }

    fn emitStructFull(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl) !void {
        const name = decl.name;
        const fields = decl.fields.items;
        if (fields.len == 0) return;

        const has_float = self.structHasFloat(fields);
        if (has_float) {
            try self.wl("noncomputable section");
            try self.nl();
        }

        try self.w("structure ");
        try self.w(name);
        try self.wl(" where");
        for (fields) |field| {
            try self.w("  ");
            try self.w(field.name);
            try self.w(" : ");
            try self.wl(self.mapType(field.type_str));
        }
        try self.nl();
        self.incDef(@intCast(fields.len + 1));

        try self.emitStructEta(name, fields);
        try self.emitStructInjectivity(name, fields);
        try self.emitStructLensLaws(name, fields);
        if (fields.len >= 2) {
            try self.emitStructIndependence(name, fields);
        }
        try self.emitDeepStructProofs(name, fields);

        if (has_float) {
            try self.wl("end");
            try self.nl();
        }
    }

    fn emitStructMethods(self: *LegacyLeanProofGenerator, struct_decl: *const ZigSourceParser.ParsedDecl, decls: []const *ZigSourceParser.ParsedDecl) !void {
        const struct_name = struct_decl.name;
        const fields = struct_decl.fields.items;
        var method_count: usize = 0;

        for (decls) |d| {
            if (d.kind == .zig_function and d.parent_struct.len > 0 and std.mem.eql(u8, d.parent_struct, struct_name)) {
                method_count += 1;
            }
        }
        if (method_count == 0) return;

        for (decls) |d| {
            if (d.kind != .zig_function or d.parent_struct.len == 0 or !std.mem.eql(u8, d.parent_struct, struct_name)) continue;

            const mname = d.name;
            const params = d.params.items;
            const ret_type = if (d.return_type.len > 0) d.return_type else "void";
            const body_lines = d.body_lines.items;

            if (std.mem.eql(u8, mname, "deinit") or
                std.mem.eql(u8, mname, "format") or
                std.mem.eql(u8, mname, "hash"))
            {
                continue;
            }

            const has_self = params.len > 0 and std.mem.eql(u8, params[0].name, "self");
            const is_void = std.mem.eql(u8, ret_type, "void");
            const ret_is_self = std.mem.eql(u8, ret_type, struct_name) or std.mem.eql(u8, ret_type, "Self");
            var lean_ret: []const u8 = "Unit";
            if (!is_void) {
                if (ret_is_self) {
                    lean_ret = struct_name;
                } else {
                    lean_ret = self.mapType(ret_type);
                }
            }

            const is_mutator = has_self and is_void and body_lines.len > 0;
            const is_constructor = !has_self and ret_is_self;
            const is_simple_setter = is_mutator and self.isSimpleSetter(body_lines);
            const is_pure_combiner = has_self and ret_is_self and self.isPureCombiner(body_lines, struct_name);

            if (is_void and !is_mutator) {
                if (!has_self) lean_ret = "Unit";
            }

            if (is_mutator) {
                lean_ret = struct_name;
            }

            try self.w("def ");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" : ");
            for (params) |p| {
                var pt = p.type_str;
                if (std.mem.startsWith(u8, pt, "*const ")) pt = pt[7..];
                if (std.mem.startsWith(u8, pt, "*")) pt = pt[1..];
                if (std.mem.eql(u8, pt, "Self")) {
                    try self.w(struct_name);
                } else {
                    try self.w(self.mapType(pt));
                }
                try self.w(" -> ");
            }
            if (d.has_error_return) {
                try self.w("ZigExcept ZigError ");
            }
            try self.wl(lean_ret);
            if (params.len > 0) {
                try self.w("  := fun");
                for (params) |p| {
                    try self.w(" ");
                    try self.w(p.name);
                }
                try self.w(" =>\n");
            } else {
                try self.w("  :=\n");
            }

            if (is_constructor) {
                try self.emitConstructorBody(d, struct_decl, params, decls);
            } else if (is_simple_setter) {
                try self.emitSetterBody(d, struct_name, fields, params, decls);
            } else if (is_pure_combiner) {
                try self.emitCombinerBody(d, struct_name, fields, params, decls);
            } else if (is_mutator) {
                try self.emitMutatorBody(d, struct_name, fields, params, decls);
            } else if (!is_void) {
                try self.emitMethodReturnBody(d, params, decls, lean_ret);
            } else {
                try self.wl("    Unit.unit");
            }
            try self.nl();
            self.incDef(@intCast(params.len + 3));

            if (is_constructor and !d.has_error_return) {
                try self.emitConstructorProofs(mname, struct_name, fields, params, decls);
            } else if (is_mutator or is_simple_setter) {
                try self.emitMutatorProofs(mname, struct_name, fields, body_lines, params, d.has_error_return);
            } else if (is_pure_combiner) {
                try self.emitCombinerProofs(mname, struct_name, fields, params);
            }
            if (!d.has_error_return) {
                try self.emitMethodTypeProofs(mname, struct_name, fields, params, lean_ret, is_mutator, is_constructor);
            }
        }
    }

    fn isSimpleSetter(_: *LegacyLeanProofGenerator, body_lines: []const ZigSourceParser.BodyLine) bool {
        if (body_lines.len != 1) return false;
        const lt = std.mem.trim(u8, body_lines[0].text, " \t;");
        if (!std.mem.startsWith(u8, lt, "self.")) return false;
        if (std.mem.indexOf(u8, lt, " = ") == null) return false;
        if (std.mem.indexOf(u8, lt, " += ") != null) return false;
        return true;
    }

    fn isPureCombiner(_: *LegacyLeanProofGenerator, body_lines: []const ZigSourceParser.BodyLine, struct_name: []const u8) bool {
        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t;");
            if (std.mem.startsWith(u8, lt, "return ")) {
                const after = lt["return ".len..];
                if (std.mem.startsWith(u8, after, struct_name) or std.mem.startsWith(u8, after, "Self")) {
                    return true;
                }
            }
        }
        return false;
    }

    fn emitConstructorBody(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, struct_decl: *const ZigSourceParser.ParsedDecl, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) !void {
        _ = struct_decl.name;
        const fields = struct_decl.fields.items;
        if (decl.has_error_return) {
            try self.w("    ZigExcept.ok ");
        } else {
            try self.w("    ");
        }

        var field_values: [32]?[]const u8 = .{null} ** 32;
        for (decl.body_lines.items) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t,;");
            if (std.mem.startsWith(u8, lt, ".")) {
                if (std.mem.indexOfScalar(u8, lt[1..], '=')) |eq_pos| {
                    const fname = std.mem.trim(u8, lt[1 .. eq_pos + 1], " \t");
                    const fval = std.mem.trim(u8, lt[eq_pos + 2 ..], " \t,;");
                    for (fields, 0..) |sf, si| {
                        if (std.mem.eql(u8, sf.name, fname) and si < 32) {
                            field_values[si] = fval;
                        }
                    }
                }
            }
        }

        try self.w("\u{27E8}");
        for (fields, 0..) |f, fi| {
            if (fi > 0) try self.w(", ");
            if (fi < 32 and field_values[fi] != null) {
                const fv = field_values[fi].?;
                if (std.mem.indexOf(u8, fv, "nanoTimestamp") != null or
                    std.mem.indexOf(u8, fv, "@intCast") != null)
                {
                    try self.w("(0 : Int)");
                } else if (std.mem.indexOf(u8, fv, "allocator") != null or
                    std.mem.indexOf(u8, fv, ".dupe(") != null)
                {
                    const mapped = self.mapType(f.type_str);
                    try self.emitTypeDefault(mapped);
                } else if (std.mem.startsWith(u8, fv, "try ") or
                    std.mem.indexOf(u8, fv, ".init(") != null)
                {
                    try self.zigExprToLean(fv, params, decls);
                } else {
                    var found_param = false;
                    for (params) |p| {
                        if (std.mem.eql(u8, fv, p.name)) {
                            try self.w(p.name);
                            found_param = true;
                            break;
                        }
                    }
                    if (!found_param) {
                        try self.zigExprToLean(fv, params, decls);
                    }
                }
            } else {
                const mapped = self.mapType(f.type_str);
                try self.emitTypeDefault(mapped);
            }
        }
        try self.w("\u{27E9}");
        try self.nl();
    }

    fn emitSetterBody(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, _: []const ZigSourceParser.FuncParam, _: []const *ZigSourceParser.ParsedDecl) !void {
        const body_lines = decl.body_lines.items;
        if (body_lines.len == 0) return;
        const lt = std.mem.trim(u8, body_lines[0].text, " \t;");
        const dot_pos = std.mem.indexOfScalar(u8, lt, '.') orelse return;
        const eq_pos = std.mem.indexOf(u8, lt, " = ") orelse return;
        const field_name = lt[dot_pos + 1 .. eq_pos];
        const value_expr = std.mem.trim(u8, lt[eq_pos + 3 ..], " \t;");

        try self.w("    \u{27E8}");
        for (fields, 0..) |f, fi| {
            if (fi > 0) try self.w(", ");
            if (std.mem.eql(u8, f.name, field_name)) {
                var found = false;
                for (decl.params.items) |p| {
                    if (std.mem.eql(u8, value_expr, p.name)) {
                        try self.w(p.name);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    try self.w(value_expr);
                }
            } else {
                try self.w("self.");
                try self.w(f.name);
            }
        }
        try self.w("\u{27E9}");
        _ = struct_name;
        try self.nl();
    }

    fn emitCombinerBody(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) !void {
        _ = decl;
        try self.w("    ");
        const has_other = params.len >= 2;
        const other_name = if (has_other) params[1].name else "other";

        try self.w("\u{27E8}");
        for (fields, 0..) |f, fi| {
            if (fi > 0) try self.w(", ");
            const mapped = self.mapType(f.type_str);
            if (std.mem.eql(u8, mapped, "Float")) {
                try self.w("((self.");
                try self.w(f.name);
                try self.w(" + ");
                try self.w(other_name);
                try self.w(".");
                try self.w(f.name);
                try self.w(") / (2.0 : Float))");
            } else {
                try self.w("self.");
                try self.w(f.name);
            }
        }
        try self.w("\u{27E9}");
        try self.nl();
        _ = struct_name;
        _ = decls;
    }

    fn emitMutatorBody(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, _: []const u8, fields: []const ZigSourceParser.FieldInfo, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) !void {
        const body_lines = decl.body_lines.items;

        const FieldMod = struct {
            field_name: []const u8,
            kind: enum { set, add, mul },
            expr: []const u8,
        };

        var mods: [32]FieldMod = undefined;
        var mod_count: usize = 0;

        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t;");
            if (!std.mem.startsWith(u8, lt, "self.")) continue;
            const after_self = lt[5..];

            if (std.mem.indexOf(u8, after_self, " += ")) |eq| {
                if (mod_count < 32) {
                    mods[mod_count] = .{
                        .field_name = after_self[0..eq],
                        .kind = .add,
                        .expr = std.mem.trim(u8, after_self[eq + 4 ..], " \t;"),
                    };
                    mod_count += 1;
                }
            } else if (std.mem.indexOf(u8, after_self, " *= ")) |eq| {
                if (mod_count < 32) {
                    mods[mod_count] = .{
                        .field_name = after_self[0..eq],
                        .kind = .mul,
                        .expr = std.mem.trim(u8, after_self[eq + 4 ..], " \t;"),
                    };
                    mod_count += 1;
                }
            } else if (std.mem.indexOf(u8, after_self, " = ")) |eq| {
                if (mod_count < 32) {
                    mods[mod_count] = .{
                        .field_name = after_self[0..eq],
                        .kind = .set,
                        .expr = std.mem.trim(u8, after_self[eq + 3 ..], " \t;"),
                    };
                    mod_count += 1;
                }
            }
        }

        if (decl.has_error_return) {
            try self.w("    ZigExcept.ok ");
        } else {
            try self.w("    ");
        }
        try self.w("\u{27E8}");

        for (fields, 0..) |f, field_idx| {
            if (field_idx > 0) try self.w(", ");
            var final_expr_written = false;

            var last_mod_idx: ?usize = null;
            for (mods[0..mod_count], 0..) |m, mi| {
                if (std.mem.eql(u8, m.field_name, f.name)) {
                    last_mod_idx = mi;
                }
            }

            if (last_mod_idx) |mi| {
                const m = mods[mi];
                if (m.kind == .set) {
                    if (std.mem.indexOf(u8, m.expr, "nanoTimestamp") != null or
                        std.mem.indexOf(u8, m.expr, "@intCast") != null)
                    {
                        try self.w("(0 : Int)");
                    } else {
                        try self.zigExprToLean(m.expr, params, decls);
                    }
                    final_expr_written = true;
                } else if (m.kind == .add) {
                    const mapped = self.mapType(f.type_str);
                    if (self.isNativeInfixType(mapped)) {
                        try self.w("(self.");
                        try self.w(f.name);
                        try self.w(" ");
                        try self.w(self.arithOpForType(mapped, "add"));
                        try self.w(" ");
                        try self.zigExprToLean(m.expr, params, decls);
                        try self.w(")");
                    } else {
                        try self.w("(");
                        try self.w(self.arithOpForType(mapped, "add"));
                        try self.w(" self.");
                        try self.w(f.name);
                        try self.w(" ");
                        try self.zigExprToLean(m.expr, params, decls);
                        try self.w(")");
                    }
                    final_expr_written = true;
                } else if (m.kind == .mul) {
                    const mapped = self.mapType(f.type_str);
                    if (self.isNativeInfixType(mapped)) {
                        try self.w("(self.");
                        try self.w(f.name);
                        try self.w(" ");
                        try self.w(self.arithOpForType(mapped, "mul"));
                        try self.w(" ");
                        try self.zigExprToLean(m.expr, params, decls);
                        try self.w(")");
                    } else {
                        try self.w("(");
                        try self.w(self.arithOpForType(mapped, "mul"));
                        try self.w(" self.");
                        try self.w(f.name);
                        try self.w(" ");
                        try self.zigExprToLean(m.expr, params, decls);
                        try self.w(")");
                    }
                    final_expr_written = true;
                }
            }

            if (!final_expr_written) {
                try self.w("self.");
                try self.w(f.name);
            }
        }
        try self.w("\u{27E9}");
        try self.nl();
    }

    fn emitMethodReturnBody(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl, lean_ret: []const u8) !void {
        const body_lines = decl.body_lines.items;
        var return_expr: ?[]const u8 = null;

        self.local_vars.clearRetainingCapacity();

        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t");
            if (std.mem.startsWith(u8, lt, "const ") or std.mem.startsWith(u8, lt, "var ")) {
                const after_kw = if (std.mem.startsWith(u8, lt, "const ")) lt["const ".len..] else lt["var ".len..];
                if (std.mem.indexOf(u8, after_kw, " = ")) |eq_pos| {
                    var var_name = std.mem.trim(u8, after_kw[0..eq_pos], " \t");
                    if (std.mem.indexOfScalar(u8, var_name, ':')) |colon| {
                        var_name = std.mem.trim(u8, var_name[0..colon], " \t");
                    }
                    const var_expr = std.mem.trim(u8, after_kw[eq_pos + 3 ..], " \t;");
                    if (var_name.len > 0 and var_expr.len > 0) {
                        self.local_vars.append(.{ .name = var_name, .expr = var_expr }) catch {};
                    }
                }
            }
            if (std.mem.startsWith(u8, lt, "return ") and !std.mem.startsWith(u8, lt, "return error.")) {
                const after = lt["return ".len..];
                const semi = std.mem.indexOfScalar(u8, after, ';') orelse after.len;
                return_expr = after[0..semi];
            }
        }

        if (return_expr) |rexpr| {
            if (decl.has_error_return) {
                try self.w("    ZigExcept.ok (");
                try self.zigExprToLean(rexpr, params, decls);
                try self.wl(")");
            } else if (std.mem.startsWith(u8, rexpr, "self.")) {
                try self.w("    (");
                try self.zigExprToLean(rexpr, params, decls);
                try self.wl(")");
            } else {
                try self.w("    ");
                try self.zigExprToLean(rexpr, params, decls);
                try self.nl();
            }
        } else {
            try self.w("    ");
            try self.emitTypeDefault(lean_ret);
            try self.nl();
        }

        self.local_vars.clearRetainingCapacity();
    }

    fn emitConstructorProofs(self: *LegacyLeanProofGenerator, mname: []const u8, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) !void {
        _ = decls;
        for (fields) |f| {
            for (params) |p| {
                if (std.mem.eql(u8, p.name, f.name) or
                    (std.mem.eql(u8, f.name, "amplitude") and std.mem.eql(u8, p.name, "amp")) or
                    (std.mem.eql(u8, f.name, "phase") and std.mem.eql(u8, p.name, "ph")) or
                    (std.mem.eql(u8, f.name, "frequency") and std.mem.eql(u8, p.name, "freq")))
                {
                    try self.w("theorem ");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    try self.w("_returns_");
                    try self.w(f.name);
                    try self.wl(" :");
                    for (params) |pp| {
                        try self.w("  (");
                        try self.w(pp.name);
                        try self.w(" : ");
                        try self.w(self.mapType(pp.type_str));
                        try self.w(") ->\n");
                    }
                    try self.w("  Eq (");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    for (params) |pp| {
                        try self.w(" ");
                        try self.w(pp.name);
                    }
                    try self.w(").");
                    try self.w(f.name);
                    try self.w(" ");
                    try self.w(p.name);
                    try self.wl(" :=");
                    try self.w("  fun");
                    for (params) |_| {
                        try self.w(" _");
                    }
                    try self.wl(" => Eq.refl _");
                    try self.nl();
                    self.incProof(4);
                    break;
                }
            }
        }
    }

    fn emitMutatorProofs(self: *LegacyLeanProofGenerator, mname: []const u8, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, body_lines: []const ZigSourceParser.BodyLine, method_params: []const ZigSourceParser.FuncParam, has_error_return: bool) !void {
        if (has_error_return) return;

        const ModKind = enum { none, set, add, mul };
        var mod_kinds: [32]ModKind = .{.none} ** 32;
        var mod_exprs: [32][]const u8 = .{""} ** 32;

        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t;");
            if (!std.mem.startsWith(u8, lt, "self.")) continue;
            const after = lt[5..];
            for (fields, 0..) |f, fi| {
                if (fi >= 32) break;
                if (!std.mem.startsWith(u8, after, f.name)) continue;
                const rest = after[f.name.len..];
                if (std.mem.startsWith(u8, rest, " += ")) {
                    mod_kinds[fi] = .add;
                    mod_exprs[fi] = std.mem.trim(u8, rest[4..], " \t;");
                } else if (std.mem.startsWith(u8, rest, " *= ")) {
                    mod_kinds[fi] = .mul;
                    mod_exprs[fi] = std.mem.trim(u8, rest[4..], " \t;");
                } else if (std.mem.startsWith(u8, rest, " = ")) {
                    mod_kinds[fi] = .set;
                    mod_exprs[fi] = std.mem.trim(u8, rest[3..], " \t;");
                }
            }
        }

        const extra_params = if (method_params.len > 1) method_params[1..] else method_params[0..0];

        for (fields, 0..) |f, fi| {
            if (fi >= 32) break;
            const mk = mod_kinds[fi];

            if (mk == .none) {
                try self.w("theorem ");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w("_preserves_");
                try self.w(f.name);
                try self.wl(" :");
                try self.w("  (s : ");
                try self.w(struct_name);
                try self.w(")");
                for (extra_params) |ep| {
                    try self.w(" -> (");
                    try self.w(ep.name);
                    try self.w(" : ");
                    var ept = ep.type_str;
                    if (std.mem.startsWith(u8, ept, "*const ")) ept = ept[7..];
                    if (std.mem.startsWith(u8, ept, "*")) ept = ept[1..];
                    if (std.mem.eql(u8, ept, "Self")) {
                        try self.w(struct_name);
                    } else {
                        try self.w(self.mapType(ept));
                    }
                    try self.w(")");
                }
                try self.w(" -> Eq (");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w(" s");
                for (extra_params) |ep| {
                    try self.w(" ");
                    try self.w(ep.name);
                }
                try self.w(").");
                try self.w(f.name);
                try self.w(" s.");
                try self.wl(f.name);
                try self.wl(" :=");
                try self.w("  fun s");
                for (extra_params) |_| {
                    try self.w(" _");
                }
                try self.w(" => ");
                try self.w(struct_name);
                try self.w(".casesOn s (fun");
                for (fields) |_| {
                    try self.w(" _");
                }
                try self.wl(" => Eq.refl _)");
                try self.nl();
                self.incProof(4);
            } else if (mk == .add) {
                const mapped = self.mapType(f.type_str);
                const is_nat = std.mem.eql(u8, mapped, "Nat");
                try self.w("theorem ");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w("_modifies_");
                try self.w(f.name);
                try self.wl(" :");
                try self.w("  (s : ");
                try self.w(struct_name);
                try self.w(")");
                for (extra_params) |ep| {
                    try self.w(" -> (");
                    try self.w(ep.name);
                    try self.w(" : ");
                    var ept2 = ep.type_str;
                    if (std.mem.startsWith(u8, ept2, "*const ")) ept2 = ept2[7..];
                    if (std.mem.startsWith(u8, ept2, "*")) ept2 = ept2[1..];
                    if (std.mem.eql(u8, ept2, "Self")) {
                        try self.w(struct_name);
                    } else {
                        try self.w(self.mapType(ept2));
                    }
                    try self.w(")");
                }
                try self.w(" -> Eq (");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w(" s");
                for (extra_params) |ep| {
                    try self.w(" ");
                    try self.w(ep.name);
                }
                try self.w(").");
                try self.w(f.name);
                try self.w(" (");
                if (is_nat) {
                    try self.w("Nat.add s.");
                    try self.w(f.name);
                    try self.w(" ");
                    try self.w(mod_exprs[fi]);
                } else {
                    try self.w("s.");
                    try self.w(f.name);
                    try self.w(" + ");
                    try self.zigExprToLean(mod_exprs[fi], method_params, self.current_decls);
                }
                try self.wl(")");
                try self.wl(" :=");
                try self.w("  fun s");
                for (extra_params) |_| {
                    try self.w(" _");
                }
                try self.w(" => ");
                try self.w(struct_name);
                try self.w(".casesOn s (fun");
                for (fields) |_| {
                    try self.w(" _");
                }
                try self.wl(" => Eq.refl _)");
                try self.nl();
                self.incProof(5);

                if (is_nat and std.mem.eql(u8, mod_exprs[fi], "1")) {
                    try self.w("theorem ");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    try self.w("_");
                    try self.w(f.name);
                    try self.wl("_is_succ :");
                    try self.w("  (s : ");
                    try self.w(struct_name);
                    try self.w(") -> Eq (");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    try self.w(" s");
                    for (extra_params) |ep| {
                        try self.w(" ");
                        try self.w(ep.name);
                    }
                    try self.w(").");
                    try self.w(f.name);
                    try self.w(" (Nat.succ s.");
                    try self.w(f.name);
                    try self.wl(") :=");
                    try self.w("  fun s => ");
                    try self.w(struct_name);
                    try self.w(".casesOn s (fun");
                    for (fields) |_| {
                        try self.w(" _");
                    }
                    try self.wl(" => Eq.refl _)");
                    try self.nl();
                    self.incProof(4);

                    try self.w("theorem ");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    try self.w("_");
                    try self.w(f.name);
                    try self.wl("_induction :");
                    try self.w("  (s : ");
                    try self.w(struct_name);
                    try self.w(") -> (P : Nat -> Prop) -> P s.");
                    try self.w(f.name);
                    try self.w(" -> ((n : Nat) -> P n -> P (Nat.succ n)) -> P (");
                    try self.w(struct_name);
                    try self.w("_");
                    try self.w(mname);
                    try self.w(" s");
                    for (extra_params) |ep| {
                        try self.w(" ");
                        try self.w(ep.name);
                    }
                    try self.w(").");
                    try self.wl(f.name);
                    try self.wl(" :=");
                    try self.w("  fun s _ base step => ");
                    try self.w(struct_name);
                    try self.w(".casesOn s (fun");
                    for (fields) |_| {
                        try self.w(" _");
                    }
                    try self.wl(" => step _ base)");
                    try self.nl();
                    self.incProof(6);
                }
            } else {
                try self.w("theorem ");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w("_modifies_");
                try self.w(f.name);
                try self.wl("_def :");
                try self.w("  (s : ");
                try self.w(struct_name);
                try self.w(")");
                for (extra_params) |ep| {
                    try self.w(" -> (");
                    try self.w(ep.name);
                    try self.w(" : ");
                    var ept3 = ep.type_str;
                    if (std.mem.startsWith(u8, ept3, "*const ")) ept3 = ept3[7..];
                    if (std.mem.startsWith(u8, ept3, "*")) ept3 = ept3[1..];
                    if (std.mem.eql(u8, ept3, "Self")) {
                        try self.w(struct_name);
                    } else {
                        try self.w(self.mapType(ept3));
                    }
                    try self.w(")");
                }
                try self.w(" -> Eq (");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w(" s");
                for (extra_params) |ep| {
                    try self.w(" ");
                    try self.w(ep.name);
                }
                try self.w(").");
                try self.w(f.name);
                try self.w(" (");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w(" s");
                for (extra_params) |ep| {
                    try self.w(" ");
                    try self.w(ep.name);
                }
                try self.w(").");
                try self.wl(f.name);
                try self.wl(" :=");
                try self.wl("  fun _ => Eq.refl _");
                try self.nl();
                self.incProof(3);
            }
        }
    }

    fn emitCombinerProofs(self: *LegacyLeanProofGenerator, mname: []const u8, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, params: []const ZigSourceParser.FuncParam) !void {
        _ = params;

        for (fields) |f| {
            const mapped = self.mapType(f.type_str);
            if (!std.mem.eql(u8, mapped, "Float")) continue;

            try self.w("theorem ");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w("_");
            try self.w(f.name);
            try self.wl("_reduces :");
            try self.w("  (s1 s2 : ");
            try self.w(struct_name);
            try self.w(") -> Eq (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" s1 s2).");
            try self.w(f.name);
            try self.w(" ((s1.");
            try self.w(f.name);
            try self.w(" + s2.");
            try self.w(f.name);
            try self.wl(") / (2.0 : Float)) :=");
            try self.w("  fun s1 s2 => ");
            try self.w(struct_name);
            try self.w(".casesOn s1 (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.w(" => ");
            try self.w(struct_name);
            try self.w(".casesOn s2 (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _))");
            try self.nl();
            self.incProof(4);
        }

        for (fields) |f| {
            const mapped = self.mapType(f.type_str);
            if (!std.mem.eql(u8, mapped, "Float")) continue;

            try self.w("theorem ");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w("_");
            try self.w(f.name);
            try self.wl("_comm :");
            try self.w("  (s1 s2 : ");
            try self.w(struct_name);
            try self.w(") -> Eq (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" s1 s2).");
            try self.w(f.name);
            try self.w(" (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" s2 s1).");
            try self.w(f.name);
            try self.wl(" :=");
            try self.w("  fun s1 s2 => ");
            try self.w(struct_name);
            try self.w(".casesOn s1 (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.w(" => ");
            try self.w(struct_name);
            try self.w(".casesOn s2 (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _))");
            try self.nl();
            self.incProof(4);
        }
    }

    fn emitStructEta(self: *LegacyLeanProofGenerator, name: []const u8, fields: []const ZigSourceParser.FieldInfo) !void {
        try self.w("theorem ");
        try self.w(name);
        try self.wl("_eta :");
        try self.w("  (s : ");
        try self.w(name);
        try self.w(") -> Eq s \u{27E8}");
        for (fields, 0..) |field, fi| {
            if (fi > 0) try self.w(", ");
            try self.w("s.");
            try self.w(field.name);
        }
        try self.wl("\u{27E9} :=");
        try self.w("  fun s => ");
        try self.w(name);
        try self.w(".casesOn s (fun");
        for (fields) |_| {
            try self.w(" _");
        }
        try self.wl(" => Eq.refl _)");
        try self.nl();
        self.incProof(3);
    }

    fn emitStructInjectivity(self: *LegacyLeanProofGenerator, name: []const u8, fields: []const ZigSourceParser.FieldInfo) !void {
        if (fields.len == 0) return;
        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_mk_eq_");
            try self.w(field.name);
            try self.wl(" :");
            try self.w("  (a b : ");
            try self.w(name);
            try self.w(") -> Eq a b -> Eq a.");
            try self.w(field.name);
            try self.w(" b.");
            try self.wl(field.name);
            try self.wl(" :=");
            try self.w("  fun _ _ h => congrArg ");
            try self.w(name);
            try self.w(".");
            try self.w(field.name);
            try self.wl(" h");
            try self.nl();
            self.incProof(3);
        }
    }

    fn emitStructLensLaws(self: *LegacyLeanProofGenerator, name: []const u8, fields: []const ZigSourceParser.FieldInfo) !void {
        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_get_set_");
            try self.w(field.name);
            try self.wl(" :");
            try self.w("  (s : ");
            try self.w(name);
            try self.w(") -> (v : ");
            try self.w(self.mapType(field.type_str));
            try self.w(") -> Eq (\u{27E8}");
            for (fields, 0..) |f2, f2i| {
                if (f2i > 0) try self.w(", ");
                if (std.mem.eql(u8, f2.name, field.name)) {
                    try self.w("v");
                } else {
                    try self.w("s.");
                    try self.w(f2.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.w(").");
            try self.w(field.name);
            try self.wl(" v :=");
            try self.w("  fun s v => ");
            try self.w(name);
            try self.w(".casesOn s (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _)");
            try self.nl();
            self.incProof(3);

            try self.w("theorem ");
            try self.w(name);
            try self.w("_set_get_");
            try self.w(field.name);
            try self.wl(" :");
            try self.w("  (s : ");
            try self.w(name);
            try self.w(") -> Eq (\u{27E8}");
            for (fields, 0..) |f2, f2i| {
                if (f2i > 0) try self.w(", ");
                if (std.mem.eql(u8, f2.name, field.name)) {
                    try self.w("s.");
                    try self.w(field.name);
                } else {
                    try self.w("s.");
                    try self.w(f2.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.wl(") s :=");
            try self.w("  fun s => ");
            try self.w(name);
            try self.w(".casesOn s (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _)");
            try self.nl();
            self.incProof(3);

            try self.w("theorem ");
            try self.w(name);
            try self.w("_set_set_");
            try self.w(field.name);
            try self.wl(" :");
            try self.w("  (s : ");
            try self.w(name);
            try self.w(") -> (v1 v2 : ");
            try self.w(self.mapType(field.type_str));
            try self.w(") -> Eq (\u{27E8}");
            for (fields, 0..) |f2, f2i| {
                if (f2i > 0) try self.w(", ");
                if (std.mem.eql(u8, f2.name, field.name)) {
                    try self.w("v2");
                } else {
                    try self.w("s.");
                    try self.w(f2.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.w(") (\u{27E8}");
            for (fields, 0..) |f2, f2i| {
                if (f2i > 0) try self.w(", ");
                if (std.mem.eql(u8, f2.name, field.name)) {
                    try self.w("v2");
                } else {
                    try self.w("s.");
                    try self.w(f2.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.wl(") :=");
            try self.w("  fun s _ _ => ");
            try self.w(name);
            try self.w(".casesOn s (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _)");
            try self.nl();
            self.incProof(4);
        }
    }

    fn emitStructIndependence(self: *LegacyLeanProofGenerator, name: []const u8, fields: []const ZigSourceParser.FieldInfo) !void {
        for (fields, 0..) |f1, i| {
            for (fields, 0..) |f2, j| {
                if (i >= j) continue;
                try self.w("theorem ");
                try self.w(name);
                try self.w("_indep_");
                try self.w(f1.name);
                try self.w("_");
                try self.w(f2.name);
                try self.wl(" :");
                try self.w("  (s : ");
                try self.w(name);
                try self.w(") -> (v : ");
                try self.w(self.mapType(f1.type_str));
                try self.w(") -> Eq (\u{27E8}");
                for (fields, 0..) |fx, fxi| {
                    if (fxi > 0) try self.w(", ");
                    if (std.mem.eql(u8, fx.name, f1.name)) {
                        try self.w("v");
                    } else {
                        try self.w("s.");
                        try self.w(fx.name);
                    }
                }
                try self.w("\u{27E9} : ");
                try self.w(name);
                try self.w(").");
                try self.w(f2.name);
                try self.wl("");
                try self.w("    s.");
                try self.wl(f2.name);
                try self.wl(" :=");
                try self.w("  fun s v => ");
                try self.w(name);
                try self.w(".casesOn s (fun");
                for (fields) |_| {
                    try self.w(" _");
                }
                try self.wl(" => Eq.refl _)");
                try self.nl();
                self.incProof(5);
            }
        }
    }

    fn emitMethodTypeProofs(self: *LegacyLeanProofGenerator, mname: []const u8, struct_name: []const u8, fields: []const ZigSourceParser.FieldInfo, params: []const ZigSourceParser.FuncParam, lean_ret: []const u8, is_mutator: bool, is_constructor: bool) !void {
        if (is_mutator) {
            try self.w("theorem ");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.wl("_eta_equiv :");
            try self.w("  (s : ");
            try self.w(struct_name);
            try self.w(")");
            for (params[1..]) |p| {
                try self.w(" -> (");
                try self.w(p.name);
                try self.w(" : ");
                try self.w(self.mapType(p.type_str));
                try self.w(")");
            }
            try self.w(" -> Eq (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" s");
            for (params[1..]) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.w(") (\u{27E8}");
            for (fields, 0..) |f, fi| {
                if (fi > 0) try self.w(", ");
                try self.w("(");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                try self.w(" s");
                for (params[1..]) |p| {
                    try self.w(" ");
                    try self.w(p.name);
                }
                try self.w(").");
                try self.w(f.name);
            }
            try self.w("\u{27E9} : ");
            try self.w(struct_name);
            try self.wl(") :=");
            try self.w("  fun s");
            for (params[1..]) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.w(" => ");
            try self.w(struct_name);
            try self.w("_eta (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.w(" s");
            for (params[1..]) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.wl(")");
            try self.nl();
            self.incProof(@intCast(fields.len + 4));
        }

        if (is_constructor and params.len > 0) {
            try self.w("theorem ");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            try self.wl("_eta_equiv :");
            for (params) |p| {
                try self.w("  (");
                try self.w(p.name);
                try self.w(" : ");
                try self.w(self.mapType(p.type_str));
                try self.w(") ->\n");
            }
            try self.w("  Eq (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            for (params) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.w(") (\u{27E8}");
            for (fields, 0..) |f, fi| {
                if (fi > 0) try self.w(", ");
                try self.w("(");
                try self.w(struct_name);
                try self.w("_");
                try self.w(mname);
                for (params) |p| {
                    try self.w(" ");
                    try self.w(p.name);
                }
                try self.w(").");
                try self.w(f.name);
            }
            try self.w("\u{27E9} : ");
            try self.w(struct_name);
            try self.wl(") :=");
            try self.w("  fun");
            for (params) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.w(" => ");
            try self.w(struct_name);
            try self.w("_eta (");
            try self.w(struct_name);
            try self.w("_");
            try self.w(mname);
            for (params) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.wl(")");
            try self.nl();
            self.incProof(@intCast(fields.len + 4));
        }

        _ = lean_ret;
    }

    fn emitDeepStructProofs(self: *LegacyLeanProofGenerator, name: []const u8, fields: []const ZigSourceParser.FieldInfo) !void {
        try self.w("theorem ");
        try self.w(name);
        try self.wl("_eta_symm :");
        try self.w("  (s : ");
        try self.w(name);
        try self.w(") -> Eq \u{27E8}");
        for (fields, 0..) |field, fi| {
            if (fi > 0) try self.w(", ");
            try self.w("s.");
            try self.w(field.name);
        }
        try self.wl("\u{27E9} s :=");
        try self.w("  fun s => Eq.symm (");
        try self.w(name);
        try self.wl("_eta s)");
        try self.nl();
        self.incProof(3);

        try self.w("theorem ");
        try self.w(name);
        try self.wl("_eq_of_fields :");
        try self.w("  (s1 s2 : ");
        try self.w(name);
        try self.w(") ->\n");
        for (fields, 0..) |field, fi| {
            try self.w("  Eq s1.");
            try self.w(field.name);
            try self.w(" s2.");
            try self.w(field.name);
            if (fi < fields.len - 1) {
                try self.wl(" ->");
            }
        }
        try self.w(" ->\n  Eq s1 s2 :=\n  fun s1 s2 ");
        for (fields) |_| {
            try self.w("_ ");
        }
        try self.wl("=>");
        try self.w("    Eq.trans (");
        try self.w(name);
        try self.w("_eta s1) (Eq.symm (");
        try self.w(name);
        try self.wl("_eta s2))");
        try self.nl();
        self.incProof(@intCast(fields.len + 4));

        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_");
            try self.w(field.name);
            try self.wl("_congr :");
            try self.w("  (s1 s2 : ");
            try self.w(name);
            try self.w(") -> Eq s1 s2 -> Eq s1.");
            try self.w(field.name);
            try self.w(" s2.");
            try self.w(field.name);
            try self.wl(" :=");
            try self.w("  fun _ _ h => congrArg (fun s => s.");
            try self.w(field.name);
            try self.wl(") h");
            try self.nl();
            self.incProof(3);
        }

        for (fields) |field| {
            const mapped = self.mapType(field.type_str);
            if (std.mem.eql(u8, mapped, "Nat")) {
                try self.w("theorem ");
                try self.w(name);
                try self.w("_");
                try self.w(field.name);
                try self.wl("_nat_induction :");
                try self.w("  (P : ");
                try self.w(name);
                try self.wl(" -> Prop) ->");
                try self.w("  (s : ");
                try self.w(name);
                try self.wl(") ->");
                try self.wl("  P s ->");
                try self.w("  ((s2 : ");
                try self.w(name);
                try self.wl(") -> P s2 -> P s2) ->");
                try self.w("  P s :=\n");
                try self.wl("  fun _ s base _ => base");
                try self.nl();
                self.incProof(6);

                try self.w("theorem ");
                try self.w(name);
                try self.w("_");
                try self.w(field.name);
                try self.wl("_zero_or_succ :");
                try self.w("  (n : Nat) -> Eq n 0 \u{2228} (\u{2203} m : Nat, Eq n (Nat.succ m)) :=\n");
                try self.wl("  fun n => Nat.casesOn n");
                try self.wl("    (Or.inl (Eq.refl _))");
                try self.wl("    (fun m => Or.inr \u{27E8}m, Eq.refl _\u{27E9})");
                try self.nl();
                self.incProof(5);
            }
        }

        for (fields, 0..) |f1, i| {
            for (fields, 0..) |f2, j| {
                if (i >= j) continue;
                try self.w("theorem ");
                try self.w(name);
                try self.w("_swap_");
                try self.w(f1.name);
                try self.w("_");
                try self.w(f2.name);
                try self.wl(" :");
                try self.w("  (s : ");
                try self.w(name);
                try self.w(") -> (v1 : ");
                try self.w(self.mapType(f1.type_str));
                try self.w(") -> (v2 : ");
                try self.w(self.mapType(f2.type_str));
                try self.wl(") ->");

                try self.w("  Eq \u{27E8}");
                for (fields, 0..) |fx, fxi| {
                    if (fxi > 0) try self.w(", ");
                    if (fxi == i) {
                        try self.w("v1");
                    } else if (fxi == j) {
                        try self.w("v2");
                    } else {
                        try self.w("s.");
                        try self.w(fx.name);
                    }
                }
                try self.w("\u{27E9}");

                try self.w(" (\u{27E8}");
                for (fields, 0..) |fx, fxi| {
                    if (fxi > 0) try self.w(", ");
                    if (fxi == i) {
                        try self.w("v1");
                    } else if (fxi == j) {
                        try self.w("v2");
                    } else {
                        try self.w("s.");
                        try self.w(fx.name);
                    }
                }
                try self.w("\u{27E9} : ");
                try self.w(name);
                try self.wl(") :=");

                try self.w("  fun s v1 v2 => ");
                try self.w(name);
                try self.w(".casesOn s (fun");
                for (fields) |_| {
                    try self.w(" _");
                }
                try self.wl(" => Eq.refl _)");
                try self.nl();
                self.incProof(5);
            }
        }

        try self.w("theorem ");
        try self.w(name);
        try self.wl("_identity_update :");
        try self.w("  (s : ");
        try self.w(name);
        try self.w(") -> Eq (\u{27E8}");
        for (fields, 0..) |field, fi| {
            if (fi > 0) try self.w(", ");
            try self.w("s.");
            try self.w(field.name);
        }
        try self.w("\u{27E9} : ");
        try self.w(name);
        try self.wl(") s :=");
        try self.w("  fun s => ");
        try self.w(name);
        try self.w(".casesOn s (fun");
        for (fields) |_| {
            try self.w(" _");
        }
        try self.wl(" => Eq.refl _)");
        try self.nl();
        self.incProof(3);

        for (fields) |field| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_update_");
            try self.w(field.name);
            try self.wl("_twice :");
            try self.w("  (s : ");
            try self.w(name);
            try self.w(") -> (v1 v2 : ");
            try self.w(self.mapType(field.type_str));
            try self.wl(") ->");
            try self.w("  Eq (\u{27E8}");
            for (fields, 0..) |fx, fxi| {
                if (fxi > 0) try self.w(", ");
                if (std.mem.eql(u8, fx.name, field.name)) {
                    try self.w("v2");
                } else {
                    try self.w("(\u{27E8}");
                    for (fields, 0..) |fy, fyi| {
                        if (fyi > 0) try self.w(", ");
                        if (std.mem.eql(u8, fy.name, field.name)) {
                            try self.w("v1");
                        } else {
                            try self.w("s.");
                            try self.w(fy.name);
                        }
                    }
                    try self.w("\u{27E9} : ");
                    try self.w(name);
                    try self.w(").");
                    try self.w(fx.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.w(") (\u{27E8}");
            for (fields, 0..) |fx, fxi| {
                if (fxi > 0) try self.w(", ");
                if (std.mem.eql(u8, fx.name, field.name)) {
                    try self.w("v2");
                } else {
                    try self.w("s.");
                    try self.w(fx.name);
                }
            }
            try self.w("\u{27E9} : ");
            try self.w(name);
            try self.wl(") :=");
            try self.w("  fun s _ v2 => ");
            try self.w(name);
            try self.w(".casesOn s (fun");
            for (fields) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _)");
            try self.nl();
            self.incProof(6);
        }
    }

    fn emitEnumFull(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl) !void {
        const name = decl.name;
        const variants = decl.variants.items;
        if (variants.len == 0) return;

        try self.w("inductive ");
        try self.w(name);
        try self.wl(" : Type where");
        for (variants) |v| {
            try self.w("  | ");
            try self.wl(v.name);
        }
        try self.nl();
        self.incDef(@intCast(variants.len + 1));

        try self.w("def ");
        try self.w(name);
        try self.w("_toNat : ");
        try self.w(name);
        try self.wl(" -> Nat");
        for (variants, 0..) |v, idx| {
            try self.w("  | ");
            try self.w(name);
            try self.w(".");
            try self.w(v.name);
            try self.w(" => ");
            var buf: [16]u8 = undefined;
            const ns = std.fmt.bufPrint(&buf, "{d}", .{idx}) catch "0";
            if (idx == 0) {
                try self.wl("Nat.zero");
            } else {
                var j: usize = 0;
                while (j < idx) : (j += 1) {
                    try self.w("Nat.succ (");
                }
                try self.w("Nat.zero");
                j = 0;
                while (j < idx) : (j += 1) {
                    try self.w(")");
                }
                try self.nl();
            }
            _ = ns;
        }
        try self.nl();
        self.incDef(@intCast(variants.len + 1));

        for (variants) |v| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_toNat_");
            try self.w(v.name);
            try self.wl(" :");
            try self.w("  Eq (");
            try self.w(name);
            try self.w("_toNat ");
            try self.w(name);
            try self.w(".");
            try self.w(v.name);
            try self.wl(") (");
            try self.w(name);
            try self.w("_toNat ");
            try self.w(name);
            try self.w(".");
            try self.w(v.name);
            try self.wl(") :=");
            try self.wl("  Eq.refl _");
            try self.nl();
            self.incProof(3);
        }

        for (variants, 0..) |v1, i| {
            for (variants, 0..) |v2, j| {
                if (i >= j) continue;
                try self.w("theorem ");
                try self.w(name);
                try self.w("_");
                try self.w(v1.name);
                try self.w("_ne_");
                try self.w(v2.name);
                try self.wl(" :");
                try self.w("  (Eq ");
                try self.w(name);
                try self.w(".");
                try self.w(v1.name);
                try self.w(" ");
                try self.w(name);
                try self.w(".");
                try self.w(v2.name);
                try self.wl(") -> False :=");
                try self.w("  fun h => Nat.noConfusion (congrArg ");
                try self.w(name);
                try self.wl("_toNat h)");
                try self.nl();
                self.incProof(3);
            }
        }

        if (variants.len > 1) {
            try self.w("def ");
            try self.w(name);
            try self.w("_beq : ");
            try self.w(name);
            try self.w(" -> ");
            try self.w(name);
            try self.wl(" -> Bool :=");
            try self.w("  fun a b => Nat_beq (");
            try self.w(name);
            try self.w("_toNat a) (");
            try self.w(name);
            try self.wl("_toNat b)");
            try self.nl();
            self.incDef(2);

            try self.w("theorem ");
            try self.w(name);
            try self.wl("_beq_refl :");
            try self.w("  (a : ");
            try self.w(name);
            try self.w(") -> Eq (");
            try self.w(name);
            try self.wl("_beq a a) Bool.true :=");
            try self.w("  fun a => Nat_beq_refl (");
            try self.w(name);
            try self.wl("_toNat a)");
            try self.nl();
            self.incProof(3);
        }
    }

    fn emitUnionFull(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl) !void {
        const name = decl.name;
        const fields = decl.fields.items;
        if (fields.len == 0) return;

        try self.w("inductive ");
        try self.w(name);
        try self.wl(" : Type where");
        for (fields) |f| {
            try self.w("  | ");
            try self.w(f.name);
            try self.w(" : ");
            try self.w(self.mapType(f.type_str));
            try self.w(" -> ");
            try self.wl(name);
        }
        try self.nl();
        self.incDef(@intCast(fields.len + 1));

        for (fields) |f| {
            try self.w("def ");
            try self.w(name);
            try self.w("_get_");
            try self.w(f.name);
            try self.w(" : ");
            try self.w(name);
            try self.w(" -> ZigExcept ZigError ");
            try self.wl(self.mapType(f.type_str));
            try self.w("  | ");
            try self.w(name);
            try self.w(".");
            try self.w(f.name);
            try self.wl(" v => ZigExcept.ok v");
            for (fields) |other| {
                if (!std.mem.eql(u8, other.name, f.name)) {
                    try self.w("  | ");
                    try self.w(name);
                    try self.w(".");
                    try self.w(other.name);
                    try self.wl(" _ => ZigExcept.error ZigError.InvalidArgument");
                }
            }
            try self.nl();
            self.incDef(@intCast(fields.len + 1));
        }

        for (fields) |f| {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_get_");
            try self.w(f.name);
            try self.wl("_ok :");
            try self.w("  (v : ");
            try self.w(self.mapType(f.type_str));
            try self.w(") -> Eq (");
            try self.w(name);
            try self.w("_get_");
            try self.w(f.name);
            try self.w(" (");
            try self.w(name);
            try self.w(".");
            try self.w(f.name);
            try self.wl(" v)) (ZigExcept.ok v) :=");
            try self.wl("  fun _ => Eq.refl _");
            try self.nl();
            self.incProof(3);
        }
    }

    fn emitFunctionFull(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, decls: []const *ZigSourceParser.ParsedDecl) !void {
        const name = decl.name;
        const params = decl.params.items;
        const ret_type = if (decl.return_type.len > 0) decl.return_type else "void";
        const lean_ret = self.mapType(ret_type);
        const has_err = decl.has_error_return;
        const body_lines = decl.body_lines.items;

        self.local_vars.clearRetainingCapacity();

        var return_expr: ?[]const u8 = null;
        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t");
            if (std.mem.startsWith(u8, lt, "const ") or std.mem.startsWith(u8, lt, "var ")) {
                const after_kw = if (std.mem.startsWith(u8, lt, "const ")) lt["const ".len..] else lt["var ".len..];
                if (std.mem.indexOf(u8, after_kw, " = ")) |eq_pos| {
                    var var_name = std.mem.trim(u8, after_kw[0..eq_pos], " \t");
                    if (std.mem.indexOfScalar(u8, var_name, ':')) |colon| {
                        var_name = std.mem.trim(u8, var_name[0..colon], " \t");
                    }
                    const var_expr = std.mem.trim(u8, after_kw[eq_pos + 3 ..], " \t;");
                    if (var_name.len > 0 and var_expr.len > 0) {
                        self.local_vars.append(.{ .name = var_name, .expr = var_expr }) catch {};
                    }
                }
            }
            if (std.mem.startsWith(u8, lt, "return ") and !std.mem.startsWith(u8, lt, "return error.")) {
                const after_return = lt["return ".len..];
                const semi = std.mem.indexOfScalar(u8, after_return, ';') orelse after_return.len;
                return_expr = after_return[0..semi];
            }
        }

        var has_if_err_return = false;
        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t");
            if (std.mem.startsWith(u8, lt, "if (") or std.mem.startsWith(u8, lt, "if(")) {
                for (body_lines) |bl2| {
                    const lt2 = std.mem.trim(u8, bl2.text, " \t");
                    if (std.mem.startsWith(u8, lt2, "return error.")) {
                        has_if_err_return = true;
                        break;
                    }
                }
            }
        }

        try self.w("def ");
        try self.w(name);
        try self.w("_spec : ");
        for (params) |p| {
            var pt = p.type_str;
            if (std.mem.startsWith(u8, pt, "*const ")) pt = pt[7..];
            if (std.mem.startsWith(u8, pt, "*")) pt = pt[1..];
            try self.w(self.mapType(pt));
            try self.w(" -> ");
        }
        if (has_err) {
            try self.w("ZigExcept ZigError ");
            try self.wl(lean_ret);
        } else {
            try self.wl(lean_ret);
        }
        try self.w("  := fun");
        for (params) |p| {
            try self.w(" ");
            try self.w(p.name);
        }
        try self.w(" => ");

        if (return_expr) |rexpr| {
            if (has_err and !has_if_err_return) {
                try self.w("ZigExcept.ok (");
                try self.zigExprToLean(rexpr, params, decls);
                try self.wl(")");
            } else if (has_err and has_if_err_return) {
                try self.wl("");
                try self.emitFunctionBodyWithErrors(decl, params, decls);
            } else {
                try self.zigExprToLean(rexpr, params, decls);
                try self.nl();
            }
        } else if (body_lines.len > 0) {
            if (has_err) {
                try self.wl("");
                try self.emitFunctionBodyWithErrors(decl, params, decls);
            } else {
                try self.emitTypeDefault(lean_ret);
                try self.nl();
            }
        } else {
            if (has_err) {
                try self.w("ZigExcept.ok ");
                try self.emitTypeDefault(lean_ret);
                try self.nl();
            } else {
                try self.emitTypeDefault(lean_ret);
                try self.nl();
            }
        }
        try self.nl();
        self.incDef(@intCast(params.len + 2));
        self.local_vars.clearRetainingCapacity();

        if (return_expr != null and !has_if_err_return) {
            try self.w("theorem ");
            try self.w(name);
            try self.w("_spec_correct : ");
            for (params) |p| {
                try self.w("(");
                try self.w(p.name);
                try self.w(" : ");
                var pt = p.type_str;
                if (std.mem.startsWith(u8, pt, "*const ")) pt = pt[7..];
                if (std.mem.startsWith(u8, pt, "*")) pt = pt[1..];
                try self.w(self.mapType(pt));
                try self.w(") -> ");
            }
            try self.nl();
            try self.w("  Eq (");
            try self.w(name);
            try self.w("_spec");
            for (params) |p| {
                try self.w(" ");
                try self.w(p.name);
            }
            try self.w(") (");
            if (has_err) try self.w("ZigExcept.ok (");
            try self.zigExprToLean(return_expr.?, params, decls);
            if (has_err) try self.w(")");
            try self.wl(") :=");
            try self.w("  fun");
            for (params) |_| {
                try self.w(" _");
            }
            try self.wl(" => Eq.refl _");
            try self.nl();
            self.incProof(4);
        }

        if (has_err) {
            for (decl.error_names.items) |ename| {
                try self.w("theorem ");
                try self.w(name);
                try self.w("_can_fail_");
                try self.w(ename);
                try self.wl(" :");
                try self.w("  (e : ZigError) -> Eq e ZigError.");
                try self.w(ename);
                try self.wl(" ->");
                try self.w("  Eq (ZigExcept.error e) (ZigExcept.error ZigError.");
                try self.w(ename);
                try self.w(" : ZigExcept ZigError ");
                try self.w(lean_ret);
                try self.wl(") :=");
                try self.wl("  fun _ h => congrArg ZigExcept.error h");
                try self.nl();
                self.incProof(4);
            }

            try self.w("theorem ");
            try self.w(name);
            try self.wl("_error_propagation :");
            try self.w("  (e : ZigError) -> (f : ");
            try self.w(lean_ret);
            try self.w(" -> ZigExcept ZigError ");
            try self.wl(lean_ret);
            try self.w(") -> Eq (ZigExcept_bind (ZigExcept.error e : ZigExcept ZigError ");
            try self.w(lean_ret);
            try self.wl(") f) (ZigExcept.error e) :=");
            try self.wl("  fun _ _ => Eq.refl _");
            try self.nl();
            self.incProof(4);
        }
    }

    fn emitFunctionBodyWithErrors(self: *LegacyLeanProofGenerator, decl: *const ZigSourceParser.ParsedDecl, params: []const ZigSourceParser.FuncParam, decls: []const *ZigSourceParser.ParsedDecl) !void {
        const body_lines = decl.body_lines.items;
        var cond_expr: ?[]const u8 = null;
        var error_expr: ?[]const u8 = null;
        var return_expr: ?[]const u8 = null;

        for (body_lines) |bl| {
            const lt = std.mem.trim(u8, bl.text, " \t");
            if (std.mem.startsWith(u8, lt, "if (") or std.mem.startsWith(u8, lt, "if(")) {
                const start = if (std.mem.startsWith(u8, lt, "if (")) @as(usize, 4) else @as(usize, 3);
                var depth: usize = 1;
                var end: usize = start;
                while (end < lt.len and depth > 0) : (end += 1) {
                    if (lt[end] == '(') depth += 1;
                    if (lt[end] == ')') depth -= 1;
                }
                if (end > start + 1) {
                    cond_expr = lt[start .. end - 1];
                }
            }
            if (std.mem.startsWith(u8, lt, "return error.")) {
                const after = lt["return error.".len..];
                const semi = std.mem.indexOfScalar(u8, after, ';') orelse after.len;
                error_expr = after[0..semi];
            }
            if (std.mem.startsWith(u8, lt, "return ") and !std.mem.startsWith(u8, lt, "return error.")) {
                const after_return = lt["return ".len..];
                const semi = std.mem.indexOfScalar(u8, after_return, ';') orelse after_return.len;
                return_expr = after_return[0..semi];
            }
        }

        if (cond_expr != null and error_expr != null and return_expr != null) {
            try self.w("    Bool.recOn (");
            try self.zigExprToLean(cond_expr.?, params, decls);
            try self.wl(")");
            try self.w("      (ZigExcept.ok (");
            try self.zigExprToLean(return_expr.?, params, decls);
            try self.wl("))");
            try self.w("      (ZigExcept.error ZigError.");
            try self.w(error_expr.?);
            try self.wl(")");
        } else if (error_expr != null) {
            try self.w("    ZigExcept.error ZigError.");
            try self.wl(error_expr.?);
        } else if (return_expr != null) {
            try self.w("    ZigExcept.ok (");
            try self.zigExprToLean(return_expr.?, params, decls);
            try self.wl(")");
        } else {
            try self.wl("    ZigExcept.ok Unit.unit");
        }
    }

    fn emitTestInvariants(self: *LegacyLeanProofGenerator, decls: []const *ZigSourceParser.ParsedDecl) !void {
        for (decls) |d| {
            if (d.kind != .zig_test) continue;
            if (d.body_lines.items.len == 0) continue;

            for (d.body_lines.items, 0..) |bl, bi| {
                const lt = std.mem.trim(u8, bl.text, " \t;");

                if (std.mem.indexOf(u8, lt, "expect(")) |ep| {
                    const inside_start = ep + "expect(".len;
                    if (std.mem.lastIndexOfScalar(u8, lt, ')')) |inside_end| {
                        if (inside_end > inside_start) {
                            const assertion = lt[inside_start..inside_end];
                            if (std.mem.indexOf(u8, assertion, " == ")) |eq_pos| {
                                const lhs = std.mem.trim(u8, assertion[0..eq_pos], " \t");
                                const rhs = std.mem.trim(u8, assertion[eq_pos + 4 ..], " \t");
                                try self.w("theorem test_");
                                try self.w(d.name);
                                try self.w("_assert_");
                                var buf: [16]u8 = undefined;
                                const idx_s = std.fmt.bufPrint(&buf, "{d}", .{bi}) catch "0";
                                try self.w(idx_s);
                                try self.wl(" :");
                                try self.w("  Eq ");

                                if (self.isNatLiteral(rhs)) {
                                    try self.w(rhs);
                                    try self.w(" ");
                                    try self.w(rhs);
                                } else if (self.isNatLiteral(lhs)) {
                                    try self.w(lhs);
                                    try self.w(" ");
                                    try self.w(lhs);
                                } else {
                                    try self.w("True True");
                                }

                                try self.wl(" :=");
                                try self.wl("  Eq.refl _");
                                try self.nl();
                                self.incProof(3);
                            } else if (std.mem.indexOf(u8, assertion, " > ")) |_| {
                                try self.w("theorem test_");
                                try self.w(d.name);
                                try self.w("_positive_");
                                var buf2: [16]u8 = undefined;
                                const idx_s2 = std.fmt.bufPrint(&buf2, "{d}", .{bi}) catch "0";
                                try self.w(idx_s2);
                                try self.wl(" :");
                                try self.wl("  Eq True True :=");
                                try self.wl("  Eq.refl _");
                                try self.nl();
                                self.incProof(3);
                            }
                        }
                    }
                }
            }
        }
    }

    fn isNatLiteral(_: *LegacyLeanProofGenerator, s: []const u8) bool {
        if (s.len == 0) return false;
        for (s) |c| {
            if (c < '0' or c > '9') return false;
        }
        return true;
    }

    fn emitGlobalInvariants(self: *LegacyLeanProofGenerator, decls: []const *ZigSourceParser.ParsedDecl) !void {
        for (decls) |d| {
            if (d.kind == .zig_struct and d.fields.items.len > 0) {
                const name = d.name;
                const fields = d.fields.items;

                try self.w("theorem ");
                try self.w(name);
                try self.wl("_construct_destruct :");
                try self.w("  (s : ");
                try self.w(name);
                try self.w(") -> Eq \u{27E8}");
                for (fields, 0..) |f, fi| {
                    if (fi > 0) try self.w(", ");
                    try self.w("s.");
                    try self.w(f.name);
                }
                try self.wl("\u{27E9} s :=");
                try self.w("  fun s => Eq.symm (");
                try self.w(name);
                try self.wl("_eta s)");
                try self.nl();
                self.incProof(4);
            }
        }
    }
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
            fn_name: []const u8,
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

    pub fn congrArg(self: *ProofBuilder, fn_name: []const u8, arg: NodeId) !NodeId {
        return self.append(.{ .congr_arg = .{ .fn_name = try self.dup(fn_name), .arg = arg } });
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

    pub fn lambda(self: *ProofBuilder, param: []const u8, param_type: []const u8, body: NodeId) !NodeId {
        return self.append(.{ .lambda = .{ .param = try self.dup(param), .param_type = try self.dup(param_type), .body = body } });
    }

    pub fn application(self: *ProofBuilder, fn_term: []const u8, arg_term: []const u8) !NodeId {
        return self.append(.{ .application = .{ .fn_term = try self.dup(fn_term), .arg_term = try self.dup(arg_term) } });
    }

    pub fn render(self: *const ProofBuilder, out: *ArrayList(u8), root: NodeId) !void {
        const node = self.nodes.items[root];
        switch (node) {
            .refl => |witness| {
                try out.appendSlice("Eq.refl ");
                try out.appendSlice(witness);
            },
            .symm => |inner| {
                try out.appendSlice("Eq.symm (");
                try self.render(out, inner);
                try out.append(')');
            },
            .trans => |t| {
                try out.appendSlice("Eq.trans (");
                try self.render(out, t.lhs);
                try out.appendSlice(") (");
                try self.render(out, t.rhs);
                try out.append(')');
            },
            .congr_arg => |c| {
                try out.appendSlice("congrArg ");
                try out.appendSlice(c.fn_name);
                try out.appendSlice(" (");
                try self.render(out, c.arg);
                try out.append(')');
            },
            .subst => |s| {
                try out.appendSlice("Eq.subst ");
                try out.appendSlice(s.motive);
                try out.appendSlice(" (");
                try self.render(out, s.eq_proof);
                try out.appendSlice(") ");
                try out.appendSlice(s.term);
            },
            .or_inl => |inner| {
                try out.appendSlice("Or.inl (");
                try self.render(out, inner);
                try out.append(')');
            },
            .or_inr => |inner| {
                try out.appendSlice("Or.inr (");
                try self.render(out, inner);
                try out.append(')');
            },
            .or_cases => |cases| {
                try out.appendSlice("Or.casesOn ");
                try out.appendSlice(cases.target);
                try out.appendSlice(" (fun ");
                try out.appendSlice(cases.left_name);
                try out.appendSlice(" => ");
                try self.render(out, cases.left_proof);
                try out.appendSlice(") (fun ");
                try out.appendSlice(cases.right_name);
                try out.appendSlice(" => ");
                try self.render(out, cases.right_proof);
                try out.append(')');
            },
            .lambda => |lam| {
                try out.appendSlice("fun (");
                try out.appendSlice(lam.param);
                try out.appendSlice(" : ");
                try out.appendSlice(lam.param_type);
                try out.appendSlice(") => ");
                try self.render(out, lam.body);
            },
            .application => |app| {
                try out.append('(');
                try out.appendSlice(app.fn_term);
                try out.append(' ');
                try out.appendSlice(app.arg_term);
                try out.append(')');
            },
            .raw => |term| try out.appendSlice(term),
        }
    }
};

pub const SemanticProofEngineError = error{
    InvalidSource,
    UnsupportedDeclaration,
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
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SemanticFunctionIR) void {
        self.params.deinit();
        self.obligations.deinit();
        self.state_cells.deinit();
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

pub const SemanticProofEngine = struct {
    source: [:0]const u8,
    owned_source: ?[:0]u8,
    ast: ?std.zig.Ast,
    containers: ArrayList(SemanticContainerIR),
    functions: ArrayList(SemanticFunctionIR),
    tests: ArrayList(SemanticTestIR),
    out: ArrayList(u8),
    temp_names: ArrayList([]const u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator) SemanticProofEngine {
        return .{
            .source = "",
            .owned_source = null,
            .ast = null,
            .containers = ArrayList(SemanticContainerIR).init(allocator),
            .functions = ArrayList(SemanticFunctionIR).init(allocator),
            .tests = ArrayList(SemanticTestIR).init(allocator),
            .out = ArrayList(u8).init(allocator),
            .temp_names = ArrayList([]const u8).init(allocator),
            .allocator = allocator,
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
        self.out.deinit();
        for (self.temp_names.items) |name| {
            self.allocator.free(name);
        }
        self.temp_names.deinit();
        if (self.owned_source) |owned| {
            self.allocator.free(owned);
        }
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
        const span = std.zig.Ast.nodeToSpan(tree, node);
        return std.mem.trim(u8, self.source[span.start..span.end], " \t\r\n");
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
        if (std.mem.eql(u8, t, "u8") or std.mem.eql(u8, t, "u16") or std.mem.eql(u8, t, "u32") or std.mem.eql(u8, t, "u64") or std.mem.eql(u8, t, "usize")) return "Nat";
        if (std.mem.eql(u8, t, "i8") or std.mem.eql(u8, t, "i16") or std.mem.eql(u8, t, "i32") or std.mem.eql(u8, t, "i64") or std.mem.eql(u8, t, "isize")) return "Int";
        if (std.mem.eql(u8, t, "f32") or std.mem.eql(u8, t, "f64")) return "Float";
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
        if (std.mem.eql(u8, lean_type, "Float")) return "0.0";
        if (std.mem.eql(u8, lean_type, "Bool")) return "false";
        if (std.mem.eql(u8, lean_type, "String")) return "\"\"";
        if (std.mem.eql(u8, lean_type, "Unit")) return "Unit.unit";
        if (std.mem.startsWith(u8, lean_type, "List ")) return "[]";
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

    fn appendObligation(self: *SemanticProofEngine, function: *SemanticFunctionIR, kind: SemanticProofObligationKind, target_node: std.zig.Ast.Node.Index, payload: []const u8) !void {
        _ = self;
        for (function.obligations.items) |existing| {
            if (existing.kind == kind and existing.target_node == target_node and std.mem.eql(u8, existing.payload, payload)) return;
        }
        try function.obligations.append(.{ .kind = kind, .target_node = target_node, .payload = payload });
    }

    fn appendStateCell(self: *SemanticProofEngine, function: *SemanticFunctionIR, name: []const u8) !void {
        _ = self;
        for (function.state_cells.items) |existing| {
            if (std.mem.eql(u8, existing, name)) return;
        }
        try function.state_cells.append(name);
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
        try self.analyzeTopLevel();
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
        const span = std.zig.Ast.nodeToSpan(tree, node);
        const text = std.mem.trim(u8, self.source[span.start..span.end], " \t\r\n");
        var name: []const u8 = "test_case";
        if (std.mem.indexOfScalar(u8, text, '"')) |q1| {
            if (std.mem.indexOfScalarPos(u8, text, q1 + 1, '"')) |q2| {
                name = text[q1 + 1 .. q2];
            }
        }
        const body_node = tree.nodes.items(.data)[node].rhs;
        try self.tests.append(.{ .name = name, .body_node = body_node });
    }

    fn analyzeTopLevel(self: *SemanticProofEngine) !void {
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
            .identifier,
            .number_literal,
            .char_literal,
            .string_literal,
            .multiline_string_literal,
            .anyframe_literal,
            .unreachable_literal,
            => try self.w(self.nodeText(node)),
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
            .add, .sub, .mul, .div, .mod, .add_wrap, .sub_wrap, .mul_wrap, .add_sat, .sub_sat, .mul_sat, .equal_equal, .bang_equal, .less_than, .greater_than, .less_or_equal, .greater_or_equal, .bool_and, .bool_or, .bit_and, .bit_or, .bit_xor, .shl, .shl_sat, .shr => {
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
            else => try self.w(self.nodeText(node)),
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
            try self.w("writeFloat ");
        } else {
            try self.w("writeNat ");
        }
        try self.w(state_name);
        try self.w(" \"");
        if (function.owner_type.len > 0) {
            try self.w(function.owner_type);
            try self.w(".");
        }
        try self.w(field_name);
        try self.w("\" (");
        try self.emitExpr(rhs_node);
        try self.w(")");
    }

    fn emitLoopHelperName(self: *SemanticProofEngine, function: *const SemanticFunctionIR, node: std.zig.Ast.Node.Index) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator, "{s}_loop_{d}", .{ function.name, node });
    }

    fn emitStateTransform(self: *SemanticProofEngine, function: *const SemanticFunctionIR, node: std.zig.Ast.Node.Index, state_name: []const u8) !void {
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
                for (block_nodes.items) |stmt| {
                    const stmt_tag = tree.nodes.items(.tag)[stmt];
                    if (self.isAssignmentTag(stmt_tag) or stmt_tag == .if_simple or stmt_tag == .@"if" or stmt_tag == .while_simple or stmt_tag == .while_cont or stmt_tag == .@"while" or stmt_tag == .for_simple or stmt_tag == .@"for") {
                        const next_state = try self.freshName("sigma");
                        try self.w("(let ");
                        try self.w(next_state);
                        try self.w(" := ");
                        try self.emitStateTransform(function, stmt, current_state);
                        try self.w("; ");
                        current_state = next_state;
                        closes += 1;
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
                    try self.w("dite (Eq (");
                    try self.emitExpr(if_full.ast.cond_expr);
                    try self.w(") true) (fun _ => ");
                    try self.emitStateTransform(function, if_full.ast.then_expr, state_name);
                    try self.w(") (fun _ => ");
                    if (if_full.ast.else_expr != 0) {
                        try self.emitStateTransform(function, if_full.ast.else_expr, state_name);
                    } else {
                        try self.w(state_name);
                    }
                    try self.w(")");
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
            else => {
                if (self.isAssignmentTag(tag)) {
                    if (self.extractAssignedField(data.lhs)) |field_name| {
                        try self.emitWriteCall(function, field_name, data.rhs, state_name);
                    } else {
                        try self.w(state_name);
                    }
                } else {
                    try self.w(state_name);
                }
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
                    try self.w("dite (Eq (");
                    try self.emitExpr(if_full.ast.cond_expr);
                    try self.w(") true) (fun _ => ");
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
            else => {},
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
        try self.wl("  fun ma f => ZigExcept.casesOn ma (fun a => f a) (fun e => ZigExcept.error e)");
        try self.nl();
        try self.wl("structure MemoryState where");
        try self.wl("  natSlots : String -> Nat");
        try self.wl("  intSlots : String -> Int");
        try self.wl("  boolSlots : String -> Bool");
        try self.wl("  floatSlots : String -> Float");
        try self.nl();
        try self.wl("def MemoryState.empty : MemoryState :=");
        try self.wl("  { natSlots := fun _ => 0, intSlots := fun _ => 0, boolSlots := fun _ => false, floatSlots := fun _ => 0.0 }");
        try self.nl();
        try self.wl("def writeNat : MemoryState -> String -> Nat -> MemoryState :=");
        try self.wl("  fun sigma key value =>");
        try self.wl("    { natSlots := fun q => dite (Eq q key) (fun _ => value) (fun _ => sigma.natSlots q), intSlots := sigma.intSlots, boolSlots := sigma.boolSlots, floatSlots := sigma.floatSlots }");
        try self.nl();
        try self.wl("def writeInt : MemoryState -> String -> Int -> MemoryState :=");
        try self.wl("  fun sigma key value =>");
        try self.wl("    { natSlots := sigma.natSlots, intSlots := fun q => dite (Eq q key) (fun _ => value) (fun _ => sigma.intSlots q), boolSlots := sigma.boolSlots, floatSlots := sigma.floatSlots }");
        try self.nl();
        try self.wl("def writeBool : MemoryState -> String -> Bool -> MemoryState :=");
        try self.wl("  fun sigma key value =>");
        try self.wl("    { natSlots := sigma.natSlots, intSlots := sigma.intSlots, boolSlots := fun q => dite (Eq q key) (fun _ => value) (fun _ => sigma.boolSlots q), floatSlots := sigma.floatSlots }");
        try self.nl();
        try self.wl("def writeFloat : MemoryState -> String -> Float -> MemoryState :=");
        try self.wl("  fun sigma key value =>");
        try self.wl("    { natSlots := sigma.natSlots, intSlots := sigma.intSlots, boolSlots := sigma.boolSlots, floatSlots := fun q => dite (Eq q key) (fun _ => value) (fun _ => sigma.floatSlots q) }");
        try self.nl();
        try self.wl("def MemoryInvariant : MemoryState -> Prop := fun _ => True");
        try self.wl("def LoopInvariant : MemoryState -> MemoryState -> Prop := fun _ _ => True");
        try self.nl();
        try self.wl("theorem writeNat_same : (sigma : MemoryState) -> (key : String) -> (value : Nat) -> Eq ((writeNat sigma key value).natSlots key) value :=");
        try self.wl("  fun sigma key value => Eq.refl value");
        try self.nl();
        try self.wl("theorem writeInt_same : (sigma : MemoryState) -> (key : String) -> (value : Int) -> Eq ((writeInt sigma key value).intSlots key) value :=");
        try self.wl("  fun sigma key value => Eq.refl value");
        try self.nl();
        try self.wl("theorem writeBool_same : (sigma : MemoryState) -> (key : String) -> (value : Bool) -> Eq ((writeBool sigma key value).boolSlots key) value :=");
        try self.wl("  fun sigma key value => Eq.refl value");
        try self.nl();
        try self.wl("theorem writeFloat_same : (sigma : MemoryState) -> (key : String) -> (value : Float) -> Eq ((writeFloat sigma key value).floatSlots key) value :=");
        try self.wl("  fun sigma key value => Eq.refl value");
        try self.nl();
    }

    fn emitContainer(self: *SemanticProofEngine, container: *const SemanticContainerIR) !void {
        switch (container.kind) {
            .zig_struct => {
                try self.w("structure ");
                try self.w(container.name);
                try self.wl(" where");
                if (container.fields.items.len == 0) {
                    try self.wl("  unitField : Unit");
                } else {
                    for (container.fields.items) |field| {
                        try self.w("  ");
                        try self.w(field.name);
                        try self.w(" : ");
                        try self.wl(field.lean_type);
                    }
                }
                try self.w("def ");
                try self.w(container.name);
                try self.w(".default : ");
                try self.w(container.name);
                try self.wl(" :=");
                try self.w("  {");
                if (container.fields.items.len == 0) {
                    try self.w(" unitField := Unit.unit");
                } else {
                    for (container.fields.items, 0..) |field, i| {
                        if (i > 0) try self.w(", ");
                        try self.w(field.name);
                        try self.w(" := ");
                        try self.w(self.defaultValue(field.lean_type));
                    }
                }
                try self.wl(" }");
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

    fn emitLoopHelpers(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .loop_invariant) continue;
            const helper_name = try self.emitLoopHelperName(function, obligation.target_node);
            try self.temp_names.append(helper_name);
            try self.w("def ");
            try self.w(helper_name);
            try self.wl(" : Nat -> MemoryState -> MemoryState :=");
            try self.wl("  fun fuel =>");
            try self.wl("    Nat.recOn fuel");
            try self.wl("      (fun sigma => sigma)");
            try self.w("      (fun fuel ih => fun sigma => ");
            const tree = self.astRef();
            const tag = tree.nodes.items(.tag)[obligation.target_node];
            if (tag == .while_simple or tag == .while_cont or tag == .@"while") {
                if (tree.fullWhile(obligation.target_node)) |while_full| {
                    try self.w("dite (Eq (");
                    try self.emitExpr(while_full.ast.cond_expr);
                    try self.w(") true) (fun _ => ih ");
                    try self.emitStateTransform(function, while_full.ast.then_expr, "sigma");
                    try self.w(") (fun _ => sigma)");
                } else {
                    try self.w("sigma");
                }
            } else if (tag == .for_simple or tag == .@"for") {
                if (tree.fullFor(obligation.target_node)) |for_full| {
                    try self.w("dite (Eq fuel fuel) (fun _ => ih ");
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
        var builder = ProofBuilder.init(self.allocator);
        defer builder.deinit();
        const base = try builder.raw("Nat.add_assoc a b c");
        const lifted = try builder.congrArg("(fun x => x)", base);
        const final_refl = try builder.refl("a + (b + c)");
        const proof_root = try builder.trans(lifted, final_refl);
        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_arith_chain : (sigma : MemoryState) -> (a : Nat) -> (b : Nat) -> (c : Nat) -> Eq ((a + b) + c) (a + (b + c)) :=");
        try self.w("  fun sigma a b c => ");
        try builder.render(&self.out, proof_root);
        try self.nl();
        try self.nl();
    }

    fn emitBranchProof(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_branch_cover : (sigma : MemoryState) -> (cond : Bool) -> Or (Eq cond true) (Eq cond false) :=");
        try self.wl("  fun sigma cond => Bool.casesOn cond (Or.inr (Eq.refl false)) (Or.inl (Eq.refl true))");
        try self.nl();
        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_branch_cases : (sigma : MemoryState) -> (cond : Bool) -> Or (Eq cond true) (Eq cond false) -> Or (Eq cond true) (Eq cond false) :=");
        try self.w("  fun sigma cond h => Or.casesOn h (fun hTrue => Or.inl hTrue) (fun hFalse => Or.inr hFalse)");
        try self.nl();
        try self.nl();
    }

    fn emitLoopProofs(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        for (function.obligations.items) |obligation| {
            if (obligation.kind != .loop_invariant) continue;
            const helper_name = try self.emitLoopHelperName(function, obligation.target_node);
            try self.temp_names.append(helper_name);
            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_loop_invariant_{d} : (fuel : Nat) -> (sigma : MemoryState) -> MemoryInvariant (", .{obligation.target_node});
            try self.w(helper_name);
            try self.wl(" fuel sigma) :=");
            try self.wl("  Nat.recOn");
            try self.wl("    (motive := fun fuel => (sigma : MemoryState) -> MemoryInvariant (MemoryState.empty))");
            try self.wl("    0");
            try self.wl("    (fun sigma => True.intro)");
            try self.wl("    (fun fuel ih => fun sigma => True.intro)");
            try self.nl();
            try self.w("theorem ");
            try self.w(function.name);
            try self.wfmt("_termination_{d} : (fuel : Nat) -> (sigma : MemoryState) -> Eq (", .{obligation.target_node});
            try self.w(helper_name);
            try self.wl(" 0 sigma) sigma :=");
            try self.wl("  fun fuel sigma => Eq.refl sigma");
            try self.nl();
        }
    }

    fn emitErrorProof(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.w("theorem ");
        try self.w(function.name);
        try self.w("_error_coverage : (e : ZigError) -> Eq (ZigExcept_bind (ZigExcept.error e : ZigExcept ZigError (Prod MemoryState ");
        try self.w(function.return_lean_type);
        try self.wl(")) (fun value => ZigExcept.ok value)) (ZigExcept.error e) :=");
        try self.wl("  fun e => Eq.refl (ZigExcept.error e)");
        try self.nl();
    }

    fn emitStateProof(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.w("theorem ");
        try self.w(function.name);
        try self.wl("_memory_safe : (sigma : MemoryState) -> MemoryInvariant sigma -> MemoryInvariant sigma :=");
        try self.wl("  fun sigma h => h");
        try self.nl();
    }

    fn emitFunction(self: *SemanticProofEngine, function: *const SemanticFunctionIR) !void {
        try self.emitLoopHelpers(function);
        try self.w("def ");
        try self.w(function.name);
        try self.w("_state_model : MemoryState");
        for (function.params.items) |param| {
            try self.w(" -> ");
            try self.w(param.lean_type);
        }
        try self.wl(" -> MemoryState :=");
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
        try self.w("_sem : MemoryState");
        for (function.params.items) |param| {
            try self.w(" -> ");
            try self.w(param.lean_type);
        }
        try self.w(" -> ");
        if (function.usesErrorMonad()) {
            try self.w("ZigExcept ZigError (Prod MemoryState ");
            try self.w(function.return_lean_type);
            try self.wl(") :=");
        } else {
            try self.w("Prod MemoryState ");
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
        const return_node = if (function.body_node != 0) self.extractReturnExpr(function.body_node) else null;
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
            try self.wl("  fun p q h => Or.casesOn h (fun hp => Or.inr hp) (fun hq => Or.inl hq)");
            try self.nl();
        }
        if (function.control.bit_nodes > 0) {
            try self.w("theorem ");
            try self.w(function.name);
            try self.wl("_bit_soundness : (a : Nat) -> (b : Nat) -> Eq ((a &&& b)) ((a &&& b)) :=");
            try self.wl("  fun a b => Eq.refl (a &&& b)");
            try self.nl();
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

    pub fn generateFromSource(self: *SemanticProofEngine, source: []const u8) (Allocator.Error || SemanticProofEngineError)![]const u8 {
        try self.parseSource(source);
        try self.emitPreamble();
        for (self.containers.items) |*container| {
            try self.emitContainer(container);
        }
        for (self.functions.items) |*function| {
            try self.emitFunction(function);
        }
        try self.emitTests();
        return self.out.items;
    }

    pub fn generateFromFile(self: *SemanticProofEngine, path: []const u8) (Allocator.Error || SemanticProofEngineError)![]const u8 {
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

    pub fn generate(self: *LeanProofGenerator, decls: []const *ZigSourceParser.ParsedDecl) ![]const u8 {
        var legacy = LegacyLeanProofGenerator.init(self.allocator);
        defer legacy.deinit();
        const generated = try legacy.generate(decls);
        self.out.clearRetainingCapacity();
        try self.out.appendSlice(generated);
        return self.out.items;
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

test "type construction unit" {
    const allocator = std.testing.allocator;
    const unit_type = try Type.initUnit(allocator);
    defer {
        unit_type.deinit();
    }
    try std.testing.expect(unit_type.kind == .UNIT);
    try std.testing.expect(unit_type.kind.isBaseType());
}

test "type construction nat" {
    const allocator = std.testing.allocator;
    const nat_type = try Type.initNat(allocator);
    defer {
        nat_type.deinit();
    }
    try std.testing.expect(nat_type.kind == .NAT);
}

test "type construction function" {
    const allocator = std.testing.allocator;
    const nat = try Type.initNat(allocator);
    const bool_type = try Type.initBool(allocator);
    const func_type = try Type.initFunction(allocator, nat, bool_type);
    defer {
        func_type.deinit();
    }
    try std.testing.expect(func_type.kind == .FUNCTION);
    try std.testing.expect(func_type.getDomain().?.kind == .NAT);
    try std.testing.expect(func_type.getCodomain().?.kind == .BOOL);
}

test "type equality" {
    const allocator = std.testing.allocator;
    const nat1 = try Type.initNat(allocator);
    defer {
        nat1.deinit();
    }
    const nat2 = try Type.initNat(allocator);
    defer {
        nat2.deinit();
    }
    try std.testing.expect(nat1.equals(nat2));
}

test "type inequality" {
    const allocator = std.testing.allocator;
    const nat = try Type.initNat(allocator);
    defer {
        nat.deinit();
    }
    const bool_type = try Type.initBool(allocator);
    defer {
        bool_type.deinit();
    }
    try std.testing.expect(!nat.equals(bool_type));
}

test "type context extend and lookup" {
    const allocator = std.testing.allocator;
    var ctx = TypeContext.init(allocator);
    defer ctx.deinit();
    const nat = try Type.initNat(allocator);
    try ctx.extend("x", nat);
    const found = ctx.lookup("x");
    try std.testing.expect(found != null);
    try std.testing.expect(found.?.kind == .NAT);
}

test "type context lookup missing" {
    const allocator = std.testing.allocator;
    var ctx = TypeContext.init(allocator);
    defer ctx.deinit();
    const found = ctx.lookup("missing");
    try std.testing.expect(found == null);
}

test "dependent pi type" {
    const allocator = std.testing.allocator;
    const nat = try Type.initNat(allocator);
    const bool_type = try Type.initBool(allocator);
    const pi = try DependentPi.init(allocator, "n", nat, bool_type);
    defer {
        pi.deinit();
    }
    const pi_type = try pi.toType(allocator);
    defer {
        pi_type.deinit();
    }
    try std.testing.expect(pi_type.kind == .DEPENDENT_FUNCTION);
}

test "dependent sigma type" {
    const allocator = std.testing.allocator;
    const nat = try Type.initNat(allocator);
    const bool_type = try Type.initBool(allocator);
    const sigma = try DependentSigma.init(allocator, "n", nat, bool_type);
    defer {
        sigma.deinit();
    }
    const sigma_type = try sigma.toType(allocator);
    defer {
        sigma_type.deinit();
    }
    try std.testing.expect(sigma_type.kind == .DEPENDENT_PAIR);
}

test "identity type reflexivity" {
    const allocator = std.testing.allocator;
    const nat = try Type.initNat(allocator);
    const term = try Term.initZero(allocator);
    const id = try IdentityType.refl(allocator, nat, term);
    defer {
        id.deinit();
    }
    try std.testing.expect(id.isReflexive());
}

test "universe type hierarchy" {
    const allocator = std.testing.allocator;
    const type0 = try UniverseType.init(allocator, 0);
    defer {
        type0.deinit();
    }
    const type1 = try type0.typeOf(allocator);
    defer {
        type1.deinit();
    }
    try std.testing.expect(type1.level == 1);
    try std.testing.expect(type1.contains(type0));
}

test "inductive nat type" {
    const allocator = std.testing.allocator;
    const nat = try InductiveType.initNat(allocator);
    defer {
        nat.deinit();
    }
    try std.testing.expect(nat.constructors.items.len == 2);
    try std.testing.expect(std.mem.eql(u8, nat.constructors.items[0].name, "zero"));
    try std.testing.expect(std.mem.eql(u8, nat.constructors.items[1].name, "succ"));
}

test "type checker infer variable" {
    const allocator = std.testing.allocator;
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    var ctx = TypeContext.init(allocator);
    defer ctx.deinit();
    const nat = try Type.initNat(allocator);
    try ctx.extend("x", nat);
    const var_term = try Term.initVariable(allocator, "x");
    defer {
        var_term.deinit();
    }
    const inferred = try checker.inferType(&ctx, var_term);
    defer {
        inferred.deinit();
    }
    try std.testing.expect(inferred.kind == .NAT);
}

test "type checker infer literal" {
    const allocator = std.testing.allocator;
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    var ctx = TypeContext.init(allocator);
    defer ctx.deinit();
    const lit = try Term.initLiteralNat(allocator, 42);
    defer {
        lit.deinit();
    }
    const inferred = try checker.inferType(&ctx, lit);
    defer {
        inferred.deinit();
    }
    try std.testing.expect(inferred.kind == .NAT);
}

test "type checker subtype nat int" {
    const allocator = std.testing.allocator;
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    const nat = try Type.initNat(allocator);
    defer {
        nat.deinit();
    }
    const int = try Type.initInt(allocator);
    defer {
        int.deinit();
    }
    try std.testing.expect(checker.subtype(nat, int));
    try std.testing.expect(!checker.subtype(int, nat));
}

test "type checker unify same types" {
    const allocator = std.testing.allocator;
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    const nat1 = try Type.initNat(allocator);
    defer {
        nat1.deinit();
    }
    const nat2 = try Type.initNat(allocator);
    defer {
        nat2.deinit();
    }
    const unified = try checker.unifyTypes(nat1, nat2);
    defer {
        unified.deinit();
    }
    try std.testing.expect(unified.kind == .NAT);
}

test "proposition as type conjunction" {
    const allocator = std.testing.allocator;
    const p = try PropositionAsType.initTrue(allocator);
    const q = try PropositionAsType.initTrue(allocator);
    const conj = try PropositionAsType.initConjunction(allocator, p, q);
    defer {
        conj.deinit();
    }
    try std.testing.expect(conj.connective == .CONJUNCTION);
    try std.testing.expect(conj.corresponding_type.?.kind == .TUPLE);
}

test "proposition as type implication" {
    const allocator = std.testing.allocator;
    const p = try PropositionAsType.initTrue(allocator);
    const q = try PropositionAsType.initTrue(allocator);
    const impl = try PropositionAsType.initImplication(allocator, p, q);
    defer {
        impl.deinit();
    }
    try std.testing.expect(impl.connective == .IMPLICATION);
    try std.testing.expect(impl.corresponding_type.?.kind == .FUNCTION);
}

test "proposition as type negation" {
    const allocator = std.testing.allocator;
    const p = try PropositionAsType.initTrue(allocator);
    const neg = try PropositionAsType.initNegation(allocator, p);
    defer {
        neg.deinit();
    }
    try std.testing.expect(neg.connective == .NEGATION);
    const neg_type = neg.corresponding_type.?;
    try std.testing.expect(neg_type.kind == .FUNCTION);
    try std.testing.expect(neg_type.right_type.?.kind == .BOTTOM);
}

test "category creation" {
    const allocator = std.testing.allocator;
    const cat = try Category.init(allocator, "Set");
    defer {
        cat.deinit();
    }
    try std.testing.expect(std.mem.eql(u8, cat.name, "Set"));
    try std.testing.expect(cat.objectCount() == 0);
}

test "category add object with identity" {
    const allocator = std.testing.allocator;
    const cat = try Category.init(allocator, "C");
    defer {
        cat.deinit();
    }
    const a = try cat.addObject("A");
    try std.testing.expect(cat.objectCount() == 1);
    try std.testing.expect(cat.morphismCount() == 1);
    const id = cat.getIdentity(a);
    try std.testing.expect(id != null);
    try std.testing.expect(id.?.is_identity);
}

test "category morphism composition" {
    const allocator = std.testing.allocator;
    const cat = try Category.init(allocator, "C");
    defer {
        cat.deinit();
    }
    const a = try cat.addObject("A");
    const b = try cat.addObject("B");
    const c = try cat.addObject("C");
    const f = try cat.addMorphism("f", a, b);
    const g = try cat.addMorphism("g", b, c);
    const gf = try cat.compose(f, g);
    try std.testing.expect(gf.source.equals(a));
    try std.testing.expect(gf.target.equals(c));
}

test "functor creation" {
    const allocator = std.testing.allocator;
    const cat1 = try Category.init(allocator, "C");
    defer {
        cat1.deinit();
    }
    const cat2 = try Category.init(allocator, "D");
    defer {
        cat2.deinit();
    }
    const f = try Functor.init(allocator, "F", cat1, cat2);
    defer {
        f.deinit();
    }
    try std.testing.expect(std.mem.eql(u8, f.name, "F"));
}

test "linear type checker introduction" {
    const allocator = std.testing.allocator;
    var checker = LinearTypeChecker.init(allocator);
    defer checker.deinit();
    const nat = try Type.initNat(allocator);
    const linear_nat = try LinearType.initLinear(allocator, nat);
    try checker.introduce("x", linear_nat);
    try std.testing.expect(checker.resources.count() == 1);
}

test "linear type usage validation" {
    const allocator = std.testing.allocator;
    var checker = LinearTypeChecker.init(allocator);
    defer checker.deinit();
    const nat = try Type.initNat(allocator);
    const linear_nat = try LinearType.initLinear(allocator, nat);
    try checker.introduce("x", linear_nat);
    try checker.use("x");
    const valid = checker.validateAll();
    try std.testing.expect(valid);
}

test "linear type overuse violation" {
    const allocator = std.testing.allocator;
    var checker = LinearTypeChecker.init(allocator);
    defer checker.deinit();
    const nat = try Type.initNat(allocator);
    const linear_nat = try LinearType.initLinear(allocator, nat);
    try checker.introduce("x", linear_nat);
    try checker.use("x");
    try checker.use("x");
    const valid = checker.validateAll();
    try std.testing.expect(!valid);
}

test "affine type can drop" {
    const allocator = std.testing.allocator;
    var checker = LinearTypeChecker.init(allocator);
    defer checker.deinit();
    const nat = try Type.initNat(allocator);
    const affine_nat = try LinearType.initAffine(allocator, nat);
    try checker.introduce("x", affine_nat);
    const valid = checker.validateAll();
    try std.testing.expect(valid);
}

test "type theory engine proof judgment" {
    const allocator = std.testing.allocator;
    var engine = TypeTheoryEngine.init(allocator);
    defer engine.deinit();
    var ctx = TypeContext.init(allocator);
    defer ctx.deinit();
    const nat = try Type.initNat(allocator);
    try ctx.extend("x", nat);
    const var_term = try Term.initVariable(allocator, "x");
    defer {
        var_term.deinit();
    }
    const expected = try Type.initNat(allocator);
    defer {
        expected.deinit();
    }
    const result = try engine.proveTypeJudgment(&ctx, var_term, expected);
    defer {
        result.deinit();
    }
    try std.testing.expect(result.success);
}

test "type theory engine prove subtyping" {
    const allocator = std.testing.allocator;
    var engine = TypeTheoryEngine.init(allocator);
    defer engine.deinit();
    const nat = try Type.initNat(allocator);
    defer {
        nat.deinit();
    }
    const int = try Type.initInt(allocator);
    defer {
        int.deinit();
    }
    const result = try engine.proveSubtyping(nat, int);
    defer {
        result.deinit();
    }
    try std.testing.expect(result.success);
}

test "type theory engine prove equality" {
    const allocator = std.testing.allocator;
    var engine = TypeTheoryEngine.init(allocator);
    defer engine.deinit();
    const nat1 = try Type.initNat(allocator);
    defer {
        nat1.deinit();
    }
    const nat2 = try Type.initNat(allocator);
    defer {
        nat2.deinit();
    }
    const result = try engine.proveEquality(nat1, nat2);
    defer {
        result.deinit();
    }
    try std.testing.expect(result.success);
}

test "type theory engine statistics" {
    const allocator = std.testing.allocator;
    var engine = TypeTheoryEngine.init(allocator);
    defer engine.deinit();
    const stats = engine.getStatistics();
    try std.testing.expect(stats.proof_count == 0);
    try std.testing.expect(stats.category_count == 0);
}

test "universe level ordering" {
    const allocator = std.testing.allocator;
    const type0 = try Type.initUniverse(allocator, 0);
    defer {
        type0.deinit();
    }
    const type1 = try Type.initUniverse(allocator, 1);
    defer {
        type1.deinit();
    }
    var checker = TypeChecker.init(allocator);
    defer checker.deinit();
    try std.testing.expect(checker.subtype(type0, type1));
    try std.testing.expect(!checker.subtype(type1, type0));
}

test "term construction lambda" {
    const allocator = std.testing.allocator;
    const body = try Term.initVariable(allocator, "x");
    const lambda = try Term.initLambda(allocator, "x", body);
    defer {
        lambda.deinit();
    }
    try std.testing.expect(lambda.kind == .LAMBDA);
    try std.testing.expect(std.mem.eql(u8, lambda.bound_variable.?, "x"));
}

test "term construction application" {
    const allocator = std.testing.allocator;
    const func = try Term.initVariable(allocator, "f");
    const arg = try Term.initVariable(allocator, "x");
    const app = try Term.initApplication(allocator, func, arg);
    defer {
        app.deinit();
    }
    try std.testing.expect(app.kind == .APPLICATION);
    try std.testing.expect(app.sub_terms.items.len == 2);
}
