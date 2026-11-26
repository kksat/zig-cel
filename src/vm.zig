//! Bytecode compiler and stack VM for CEL.
//!
//! `compile` lowers an arena AST to a flat instruction stream with:
//! - interned function ids (no name dispatch at eval time),
//! - a constant pool with compile-time constant folding (arithmetic on
//!   literals, literal list/map construction, string concatenation),
//! - precompiled regexes for constant `matches()` patterns,
//! - lexical slots for comprehension variables (no name lookup in loops),
//! - precomputed qualified-name chains for select fallbacks.
//!
//! `run` executes with a stack machine. Errors are values on the operand
//! stack (cel-go's model), so the commutative `&&`/`||` error absorption and
//! macro semantics compose naturally; comprehension predicates run as
//! bounded sub-executions on the same stack.
//!
//! The operand stack, comprehension locals, call-argument buffer, and
//! resolved-variable slots are owned by the Program and reused across runs,
//! so steady-state evaluation of an expression that constructs no
//! lists/maps/strings performs zero heap allocations. This makes `run` on a
//! single Program unsafe to call concurrently (compile one per thread).

const std = @import("std");
const ast = @import("ast.zig");
const value_mod = @import("value.zig");
const stdlib = @import("stdlib.zig");
const regex = @import("regex.zig");
const ext = @import("ext.zig");

pub const Value = value_mod.Value;
pub const Map = value_mod.Map;
pub const Binding = stdlib.Binding;

/// Implementation of a host-provided function, mirroring cel-go's
/// function-level `cel.FunctionBinding` (which receives every argument and
/// is responsible for its own arity/type dispatch): `args` are the call's
/// operands, `ctx` carries the run's arena (for constructing result
/// values) and the error-reporting helpers. Signal evaluation errors with
/// `ctx.fail` / `ctx.noOverloadNamed` (they become absorbable CEL error
/// values, like standard-function errors). Host functions are host-trusted
/// code: unlike expressions they are not covered by RunOptions budgets or
/// cancellation, and must terminate on their own.
pub const HostFunc = fn (ctx: *stdlib.Ctx, args: []const Value) stdlib.Error!Value;

/// A host-provided function registered under a name (like cel-go's
/// `cel.Function("double", ..., cel.FunctionBinding(...))`), resolved for
/// call names the standard library does not define. The struct is borrowed
/// by compiled programs and must outlive them.
pub const HostFn = struct {
    name: []const u8,
    func: *const HostFunc,
};

pub const Error = error{ Compile, OutOfMemory };

/// Errors from `Program.run`: allocation failure, exceeding the optional
/// instruction budget, or cooperative cancellation.
pub const RunError = error{ OutOfMemory, BudgetExceeded, Canceled };

/// Errors internal to the exec loop (error *values* ride the operand stack
/// as `RunResult.err`; these are control-flow failures that abort the run).
const ExecError = error{ Eval, OutOfMemory, BudgetExceeded, Canceled };

/// Cooperative cancellation for `Program.run`. Polling happens at
/// instruction granularity, so a run aborts promptly even inside a long
/// comprehension.
pub const Cancel = struct {
    canceled: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    /// Request cancellation; safe to call from any thread.
    pub fn request(c: *Cancel) void {
        c.canceled.store(true, .release);
    }

    pub fn isCanceled(c: *const Cancel) bool {
        return c.canceled.load(.acquire);
    }
};

/// Options for `Program.run`.
pub const RunOptions = struct {
    /// Instruction budget: the maximum number of VM instructions a run may
    /// execute (comprehension predicate/transform sub-runs included)
    /// before aborting with `error.BudgetExceeded`. Counted conservatively:
    /// every entered instruction range is charged its full length, so
    /// short-circuited branches count their untaken side. `null` (the
    /// default) means unlimited. Bounds untrusted expressions that would
    /// otherwise run long (e.g. a macro over a large range list).
    max_steps: ?u64 = null,
    /// Cancellation flag polled whenever an instruction range begins (per
    /// comprehension iteration at worst); call `request()` on it (from
    /// another thread or a signal handler) to abort the run with
    /// `error.Canceled`.
    cancel: ?*Cancel = null,
};

pub const RunResult = union(enum) {
    value: Value,
    err: []const u8,
};

pub const Op = enum(u8) {
    /// a: const index.
    push_const,
    /// a: local (comprehension variable) slot.
    push_local,
    /// a: name index.
    push_var,
    /// a: const index (field name). Pops operand; map select.
    select,
    /// a: const index (field name). Pops operand; presence test (has()).
    select_test,
    /// a: chain index. Pops operand; when it is an unbound-name error,
    /// resolve the (possibly container-qualified) chain against bindings
    /// and type names; otherwise push the operand back.
    select_qualified,
    /// a: function id, b: arity. Pops args; propagates error args.
    call,
    /// a: host function index, b: arity. Pops args; propagates error
    /// args; invokes the registered host function.
    call_host,
    /// a: regex pool index. Pops input string.
    matches_const,
    /// a: target.
    jump,
    /// Ternary: pops condition. a: then, b: else, c: end (error path).
    cond_jump,
    /// a: end target. Peeks; jumps when the operand is the absorbing bool.
    and_check,
    or_check,
    /// Pops rhs and lhs, pushes the combined result.
    and_merge,
    or_merge,
    /// Pops, pushes logical not.
    not,
    /// Pops, pushes arithmetic negation.
    neg,
    /// a: macro index. Pops range; loops the predicate sub-program.
    macro_all,
    macro_exists,
    macro_exists_one,
    macro_filter,
    macro_map,
    macro_map3,
    /// a: const index (pre-formatted message), b: arity to pop. Pushes an
    /// error value.
    fail,
    /// a: element count; pops that many values, pushes the list.
    make_list,
    /// a: entry count; pops 2n values (key, value...), pushes the map.
    make_map,
    /// End of program / sub-program.
    ret,
};

pub const Inst = struct {
    op: Op,
    a: u32 = 0,
    b: u32 = 0,
    c: u32 = 0,
};

pub const ErrInfo = struct {
    msg: []const u8,
    unbound: bool,
};

/// Internal evaluation value: a CEL value or a propagated error.
pub const StackVal = union(enum) {
    val: Value,
    err: ErrInfo,
};

const MacroInfo = struct {
    var_slot: u32,
    pred_start: u32,
    pred_end: u32,
    transform_start: u32 = 0,
    transform_end: u32 = 0,
};

pub const Program = struct {
    /// Allocator that owns the program (and run-state that persists across
    /// runs, e.g. expanded name candidates).
    backing: std.mem.Allocator,
    /// Enabled extensions for dispatch.
    ext: ext.Extensions = .{},
    /// Host-provided functions (borrowed from CompileOptions).
    host: []const HostFn = &.{},
    insts: []Inst,
    consts: []Value,
    names: [][]const u8,
    name_abs: []bool,
    chains: [][]const u8,
    chain_abs: []bool,
    macros: []MacroInfo,
    regexes: []*regex.Regex,
    n_locals: usize,
    max_arity: usize,

    // Run state (reused across runs; see module docs).
    stack: []StackVal,
    locals: []Value,
    args_buf: []Value,
    resolved_vars: []StackVal,
    /// Candidates per chain for the current run (container-expanded).
    run_candidates: [][]const []const u8,
    /// Bindings of the current run.
    run_vars: []const Binding,

    /// Evaluate the program. `alloc` backs values constructed during the
    /// run (lists, maps, strings) and error messages.
    pub fn run(
        prog: *Program,
        alloc: std.mem.Allocator,
        vars: []const Binding,
        container: []const u8,
        opts: RunOptions,
    ) RunError!RunResult {
        var vm = Vm{
            .prog = prog,
            .ctx = stdlib.Ctx.init(alloc),
            .sp = 0,
            .limited = opts.max_steps != null or opts.cancel != null,
            .max_steps = opts.max_steps,
            .cancel = opts.cancel,
        };
        vm.ctx.regexes = prog.regexes;
        vm.ctx.ext = prog.ext;
        prog.run_vars = vars;
        vm.prepareChains(container) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            else => unreachable,
        };
        _ = prog.backing;
        vm.resolveVars(container) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            else => unreachable,
        };
        const r = vm.exec(0, @intCast(prog.insts.len)) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Eval => return .{ .err = vm.ctx.err_msg },
            error.BudgetExceeded => return error.BudgetExceeded,
            error.Canceled => return error.Canceled,
        };
        return .{ .value = r };
    }

    pub fn deinit(prog: *Program, alloc: std.mem.Allocator) void {
        alloc.free(prog.insts);
        alloc.free(prog.consts);
        alloc.free(prog.names);
        alloc.free(prog.name_abs);
        for (prog.chains) |ch| alloc.free(ch);
        alloc.free(prog.chains);
        alloc.free(prog.chain_abs);
        alloc.free(prog.macros);
        for (prog.regexes) |re| {
            re.deinit(alloc);
            alloc.destroy(re);
        }
        alloc.free(prog.regexes);
        alloc.free(prog.stack);
        alloc.free(prog.locals);
        alloc.free(prog.args_buf);
        alloc.free(prog.resolved_vars);
        for (prog.run_candidates) |list| alloc.free(list);
        alloc.free(prog.run_candidates);
        alloc.destroy(prog);
    }
};

const Vm = struct {
    prog: *Program,
    ctx: stdlib.Ctx,
    sp: usize,
    /// Run limits (RunOptions); enforced at exec entry (see exec docs).
    limited: bool = false,
    steps: u64 = 0,
    max_steps: ?u64 = null,
    cancel: ?*Cancel = null,

    fn push(vm: *Vm, v: StackVal) void {
        vm.prog.stack[vm.sp] = v;
        vm.sp += 1;
    }

    fn pop(vm: *Vm) StackVal {
        vm.sp -= 1;
        return vm.prog.stack[vm.sp];
    }

    fn peek(vm: *Vm) StackVal {
        return vm.prog.stack[vm.sp - 1];
    }

    fn pushErr(vm: *Vm, comptime fmt: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(vm.ctx.alloc, fmt, args) catch "out of memory";
        vm.push(.{ .err = .{ .msg = msg, .unbound = false } });
    }

    fn pushCtxError(vm: *Vm) void {
        vm.push(.{ .err = .{ .msg = vm.ctx.err_msg, .unbound = vm.ctx.unbound } });
    }

    /// Expand chain candidates with container prefixes. With an empty
    /// container (the common case) each candidate list is just the chain
    /// itself, pre-built at compile time.
    fn prepareChains(vm: *Vm, container: []const u8) stdlib.Error!void {
        const prog = vm.prog;
        const backing = prog.backing;
        for (prog.chains, 0..) |chain, i| {
            // With an empty (or absolute) chain scope the candidates are
            // just the chain itself; built once and reused.
            if (container.len == 0 or prog.chain_abs[i]) {
                const list = prog.run_candidates[i];
                if (list.len == 1 and std.mem.eql(u8, list[0], chain)) continue;
                if (list.len > 0) backing.free(list);
                const buf = backing.alloc([]const u8, 1) catch return error.OutOfMemory;
                buf[0] = chain;
                prog.run_candidates[i] = buf;
                continue;
            }
            // Container-qualified: rebuild (previous list freed; the strings
            // themselves live in the run arena).
            const old = prog.run_candidates[i];
            var list: std.ArrayList([]const u8) = .empty;
            errdefer list.deinit(backing);
            var end = container.len;
            while (true) {
                const q = std.fmt.allocPrint(vm.ctx.alloc, "{s}.{s}", .{ container[0..end], chain }) catch return error.OutOfMemory;
                try list.append(backing, q);
                const dot = std.mem.lastIndexOfScalar(u8, container[0..end], '.') orelse break;
                end = dot;
            }
            try list.append(backing, chain);
            prog.run_candidates[i] = list.toOwnedSlice(backing) catch return error.OutOfMemory;
            if (old.len > 0) backing.free(old);
        }
    }

    /// Resolve referenced variable names against the bindings.
    fn resolveVars(vm: *Vm, container: []const u8) stdlib.Error!void {
        const prog = vm.prog;
        for (prog.names, 0..) |name, i| {
            prog.resolved_vars[i] = resolveName(vm, name, prog.run_vars, container, prog.name_abs[i]);
        }
    }

    fn resolveName(vm: *Vm, name: []const u8, vars: []const Binding, container: []const u8, abs: bool) StackVal {
        if (abs) {
            for (vars) |v| {
                if (std.mem.eql(u8, v.name, name)) return .{ .val = v.value };
            }
        } else {
            if (container.len > 0) {
                var end = container.len;
                while (true) {
                    const q = std.fmt.allocPrint(vm.ctx.alloc, "{s}.{s}", .{ container[0..end], name }) catch return errOf("out of memory");
                    for (vars) |v| {
                        if (std.mem.eql(u8, v.name, q)) return .{ .val = v.value };
                    }
                    const dot = std.mem.lastIndexOfScalar(u8, container[0..end], '.') orelse break;
                    end = dot;
                }
            }
            for (vars) |v| {
                if (std.mem.eql(u8, v.name, name)) return .{ .val = v.value };
            }
        }
        // Bare type denotations (int, string, list, ...). `dyn`,
        // `duration`, and `timestamp` are functions, not denotations.
        if (stdlib.bareTypeDenotation(name)) |t| {
            return .{ .val = .{ .type_t = t } };
        }
        const msg = std.fmt.allocPrint(vm.ctx.alloc, "unknown variable: undeclared reference to '{s}' (in container '')", .{name}) catch return errOf("out of memory");
        return .{ .err = .{ .msg = msg, .unbound = true } };
    }

    fn errOf(msg: []const u8) StackVal {
        return .{ .err = .{ .msg = msg, .unbound = false } };
    }

    /// Execute instructions in [start, stop). Sub-programs (predicate and
    /// transform ranges) run on the same stack; each leaves exactly one
    /// value.
    ///
    /// Run limits are enforced here rather than per instruction: within one
    /// exec invocation the program counter only moves forward through
    /// [start, stop) (all jumps are forward), so the invocation executes at
    /// most `stop - start` instructions. Charging that range on entry makes
    /// `max_steps` an exact count for straight-line code and a conservative
    /// upper bound when `&&`/`||`/`?:` short-circuit, while keeping the
    /// dispatch loop free of per-instruction bookkeeping (measured ~10%
    /// regression on macro-heavy expressions otherwise). Comprehension
    /// iterations each enter exec once (via subrun), so cancellation and
    /// budget aborts happen at iteration granularity.
    fn exec(vm: *Vm, start: u32, stop: u32) ExecError!Value {
        if (vm.limited) {
            if (vm.max_steps) |max| {
                vm.steps += stop - start;
                if (vm.steps > max) return error.BudgetExceeded;
            }
            if (vm.cancel) |c| {
                if (c.isCanceled()) return error.Canceled;
            }
        }
        var pc = start;
        while (pc < stop) {
            const inst = vm.prog.insts[pc];
            switch (inst.op) {
                .push_const => {
                    vm.push(.{ .val = vm.prog.consts[inst.a] });
                    pc += 1;
                },
                .push_local => {
                    vm.push(.{ .val = vm.prog.locals[inst.a] });
                    pc += 1;
                },
                .push_var => {
                    vm.push(vm.prog.resolved_vars[inst.a]);
                    pc += 1;
                },
                .select, .select_test => {
                    const operand = vm.pop();
                    try vm.doSelect(inst, operand);
                    pc += 1;
                },
                .select_qualified => {
                    const operand = vm.pop();
                    vm.push(try vm.resolveQualified(inst, operand));
                    pc += 1;
                },
                .call => {
                    try vm.doCall(inst);
                    pc += 1;
                },
                .call_host => {
                    try vm.doHostCall(inst);
                    pc += 1;
                },
                .matches_const => {
                    const input = vm.pop();
                    switch (input) {
                        .err => vm.push(input),
                        .val => |v| {
                            if (v != .string_t) {
                                _ = vm.ctx.noOverload(.matches, &.{v}) catch {};
                                vm.pushCtxError();
                            } else {
                                const re = vm.prog.regexes[inst.a];
                                vm.push(.{ .val = .{ .bool_t = re.matches(v.string_t) } });
                            }
                        },
                    }
                    pc += 1;
                },
                .jump => pc = inst.a,
                .cond_jump => {
                    const cond = vm.pop();
                    switch (cond) {
                        .err => {
                            vm.push(cond);
                            pc = inst.c;
                        },
                        .val => |v| {
                            if (v == .bool_t) {
                                pc = if (v.bool_t) inst.a else inst.b;
                            } else {
                                _ = vm.ctx.noOverload(.ternary, &.{v}) catch {};
                                vm.pushCtxError();
                                pc = inst.c;
                            }
                        },
                    }
                },
                .and_check, .or_check => {
                    const absorbing = inst.op == .or_check;
                    const top = vm.peek();
                    if (top == .val and top.val == .bool_t and top.val.bool_t == absorbing) {
                        pc = inst.a;
                    } else {
                        pc += 1;
                    }
                },
                .and_merge, .or_merge => {
                    const rhs = vm.pop();
                    const lhs = vm.pop();
                    vm.push(try vm.mergeLogical(inst.op == .or_merge, lhs, rhs));
                    pc += 1;
                },
                .not => {
                    const v = vm.pop();
                    switch (v) {
                        .err => vm.push(v),
                        .val => |x| {
                            if (x == .bool_t) {
                                vm.push(.{ .val = .{ .bool_t = !x.bool_t } });
                            } else {
                                _ = vm.ctx.noOverload(.not, &.{x}) catch {};
                                vm.pushCtxError();
                            }
                        },
                    }
                    pc += 1;
                },
                .neg => {
                    const v = vm.pop();
                    switch (v) {
                        .err => vm.push(v),
                        .val => |x| {
                            switch (x) {
                                .int_t => |i| {
                                    if (i == std.math.minInt(i64)) {
                                        vm.pushErr("return error for overflow: negation of min int", .{});
                                    } else {
                                        vm.push(.{ .val = .{ .int_t = -i } });
                                    }
                                },
                                .double_t => |d| vm.push(.{ .val = .{ .double_t = -d } }),
                                else => {
                                    _ = vm.ctx.noOverload(.neg, &.{x}) catch {};
                                    vm.pushCtxError();
                                },
                            }
                        },
                    }
                    pc += 1;
                },
                .macro_all, .macro_exists, .macro_exists_one, .macro_filter, .macro_map, .macro_map3 => {
                    try vm.doMacro(inst);
                    pc += 1;
                },
                .fail => {
                    vm.sp -= inst.b;
                    const msg = vm.prog.consts[inst.a].string_t;
                    vm.push(.{ .err = .{ .msg = msg, .unbound = false } });
                    pc += 1;
                },
                .make_list => {
                    const n: usize = inst.a;
                    var failed: ?StackVal = null;
                    for (0..n) |i| {
                        const sv = vm.prog.stack[vm.sp - n + i];
                        if (sv == .err and failed == null) failed = sv;
                    }
                    if (failed) |f| {
                        vm.sp -= n;
                        vm.push(f);
                        pc += 1;
                        continue;
                    }
                    const items = vm.ctx.alloc.alloc(Value, n) catch {
                        vm.sp -= n;
                        vm.push(errOf("out of memory"));
                        pc += 1;
                        continue;
                    };
                    for (0..n) |i| items[i] = vm.prog.stack[vm.sp - n + i].val;
                    vm.sp -= n;
                    vm.push(.{ .val = .{ .list_t = items } });
                    pc += 1;
                },
                .make_map => {
                    const n: usize = inst.a;
                    var failed: ?StackVal = null;
                    var bad_key = false;
                    for (0..n) |i| {
                        const k = vm.prog.stack[vm.sp - 2 * n + 2 * i];
                        const v = vm.prog.stack[vm.sp - 2 * n + 2 * i + 1];
                        if (failed == null) {
                            if (k == .err) {
                                failed = k;
                            } else if (v == .err) {
                                failed = v;
                            } else switch (k.val) {
                                .int_t, .uint_t, .bool_t, .string_t => {},
                                else => bad_key = true,
                            }
                        }
                    }
                    if (failed) |f| {
                        vm.sp -= 2 * n;
                        vm.push(f);
                        pc += 1;
                        continue;
                    }
                    if (bad_key) {
                        vm.sp -= 2 * n;
                        vm.pushErr("bad key type: map key must be int, uint, bool, or string", .{});
                        pc += 1;
                        continue;
                    }
                    const entries = vm.ctx.alloc.alloc(Map.Entry, n) catch {
                        vm.sp -= 2 * n;
                        vm.push(errOf("out of memory"));
                        pc += 1;
                        continue;
                    };
                    for (0..n) |i| {
                        entries[i] = .{
                            .key = vm.prog.stack[vm.sp - 2 * n + 2 * i].val,
                            .value = vm.prog.stack[vm.sp - 2 * n + 2 * i + 1].val,
                        };
                    }
                    vm.sp -= 2 * n;
                    const m = Map.init(vm.ctx.alloc, entries) catch |e| switch (e) {
                        error.OutOfMemory => {
                            vm.push(errOf("out of memory"));
                            pc += 1;
                            continue;
                        },
                        error.RepeatedKey => {
                            vm.pushErr("Failed with repeated key", .{});
                            pc += 1;
                            continue;
                        },
                    };
                    vm.push(.{ .val = .{ .map_t = m } });
                    pc += 1;
                },
                .ret => break,
            }
        }
        const result = vm.pop();
        switch (result) {
            .val => |v| return v,
            .err => |e| {
                vm.ctx.err_msg = e.msg;
                vm.ctx.unbound = e.unbound;
                return error.Eval;
            },
        }
    }

    fn mergeLogical(vm: *Vm, is_or: bool, lhs: StackVal, rhs: StackVal) ExecError!StackVal {
        const absorbing = is_or;
        if (rhs == .val and rhs.val == .bool_t and rhs.val.bool_t == absorbing) {
            return .{ .val = .{ .bool_t = absorbing } };
        }
        if (lhs == .val and lhs.val == .bool_t and lhs.val.bool_t == absorbing) {
            return .{ .val = .{ .bool_t = absorbing } };
        }
        if (lhs == .err) return lhs;
        if (rhs == .err) return rhs;
        const id: stdlib.FnId = if (is_or) .or_msg else .and_msg;
        if (lhs.val != .bool_t) {
            _ = vm.ctx.noOverload(id, &.{lhs.val}) catch {};
            return .{ .err = .{ .msg = vm.ctx.err_msg, .unbound = false } };
        }
        if (rhs.val != .bool_t) {
            _ = vm.ctx.noOverload(id, &.{rhs.val}) catch {};
            return .{ .err = .{ .msg = vm.ctx.err_msg, .unbound = false } };
        }
        return .{ .val = .{ .bool_t = !absorbing } };
    }

    fn doSelect(vm: *Vm, inst: Inst, operand: StackVal) ExecError!void {
        const field = vm.prog.consts[inst.a].string_t;
        switch (operand) {
            .err => vm.push(operand),
            .val => |v| {
                switch (v) {
                    .map_t => |m| {
                        const key = Value{ .string_t = field };
                        if (inst.op == .select_test) {
                            vm.push(.{ .val = .{ .bool_t = m.has(key) } });
                        } else if (m.get(key)) |val| {
                            vm.push(.{ .val = val });
                        } else {
                            vm.pushErr("no such key: '{s}'", .{field});
                        }
                    },
                    else => {
                        if (inst.op == .select_test) {
                            vm.pushErr("no such field: has() is only supported on maps", .{});
                        } else {
                            vm.pushErr("no such field: cannot select field '{s}' of {s}", .{ field, v.typeOf().name() });
                        }
                    },
                }
            },
        }
    }

    fn resolveQualified(vm: *Vm, inst: Inst, operand: StackVal) stdlib.Error!StackVal {
        switch (operand) {
            .err => |e| {
                if (!e.unbound) return operand;
                const candidates = vm.prog.run_candidates[inst.a];
                for (candidates) |cand| {
                    for (vm.prog.run_vars) |v| {
                        if (std.mem.eql(u8, v.name, cand)) {
                            return .{ .val = v.value };
                        }
                    }
                }
                for (candidates) |cand| {
                    if (value_mod.Type.fromName(cand)) |t| {
                        return .{ .val = .{ .type_t = t } };
                    }
                }
                return operand;
            },
            .val => return operand,
        }
    }

    fn doCall(vm: *Vm, inst: Inst) ExecError!void {
        const arity: usize = inst.b;
        const fid: stdlib.FnId = @fromBackingInt(@intCast(inst.a));
        // Fast paths for the common operators: equality and int ordering.
        if (arity == 2) {
            if (fid == .eq or fid == .ne) {
                const b = vm.pop();
                const a = vm.pop();
                if (a == .err) {
                    vm.push(a);
                    return;
                }
                if (b == .err) {
                    vm.push(b);
                    return;
                }
                const eq = Value.eql(a.val, b.val);
                vm.push(.{ .val = .{ .bool_t = if (fid == .eq) eq else !eq } });
                return;
            }
            if ((fid == .lt or fid == .le or fid == .gt or fid == .ge) and
                vm.prog.stack[vm.sp - 1] == .val and vm.prog.stack[vm.sp - 2] == .val and
                vm.prog.stack[vm.sp - 1].val == .int_t and vm.prog.stack[vm.sp - 2].val == .int_t)
            {
                const y = vm.prog.stack[vm.sp - 1].val.int_t;
                const x = vm.prog.stack[vm.sp - 2].val.int_t;
                vm.sp -= 2;
                const result = switch (fid) {
                    .lt => x < y,
                    .le => x <= y,
                    .gt => x > y,
                    .ge => x >= y,
                    else => unreachable,
                };
                vm.push(.{ .val = .{ .bool_t = result } });
                return;
            }
            // String function fast paths.
            if (fid == .contains or fid == .starts_with or fid == .ends_with) {
                const b = vm.pop();
                const a = vm.pop();
                if (a == .err) {
                    vm.push(a);
                    return;
                }
                if (b == .err) {
                    vm.push(b);
                    return;
                }
                if (a.val == .string_t and b.val == .string_t) {
                    const hay = a.val.string_t;
                    const needle = b.val.string_t;
                    const result = switch (fid) {
                        .contains => needle.len == 0 or (needle.len <= hay.len and std.mem.indexOf(u8, hay, needle) != null),
                        .starts_with => std.mem.startsWith(u8, hay, needle),
                        .ends_with => std.mem.endsWith(u8, hay, needle),
                        else => unreachable,
                    };
                    vm.push(.{ .val = .{ .bool_t = result } });
                    return;
                }
                // Non-string: fall through to the slow path for the proper
                // error, with operands restored.
                vm.push(a);
                vm.push(b);
            } else if (fid == .size and vm.sp >= 1 and vm.prog.stack[vm.sp - 1] == .val) {
                const v = vm.prog.stack[vm.sp - 1].val;
                const n: ?i64 = switch (v) {
                    .list_t => |l| @intCast(l.len),
                    .bytes_t => |s| @intCast(s.len),
                    .string_t => |s| blk: {
                        // Code point count.
                        var count: i64 = 0;
                        var i: usize = 0;
                        while (i < s.len) {
                            const l = std.unicode.utf8ByteSequenceLength(s[i]) catch break :blk null;
                            i += l;
                            count += 1;
                        }
                        break :blk count;
                    },
                    else => null,
                };
                if (n) |count| {
                    vm.sp -= 1;
                    vm.push(.{ .val = .{ .int_t = count } });
                    return;
                }
            } else if (fid == .in and vm.prog.stack[vm.sp - 1] == .val and vm.prog.stack[vm.sp - 2] == .val) {
                // List membership fast path.
                const container = vm.prog.stack[vm.sp - 1].val;
                if (container == .list_t) {
                    const needle = vm.prog.stack[vm.sp - 2].val;
                    var found = false;
                    for (container.list_t) |item| {
                        if (Value.eql(needle, item)) {
                            found = true;
                            break;
                        }
                    }
                    vm.sp -= 2;
                    vm.push(.{ .val = .{ .bool_t = found } });
                    return;
                }
            }
        }
        var failed: ?StackVal = null;
        for (0..arity) |i| {
            const sv = vm.prog.stack[vm.sp - arity + i];
            if (sv == .err) {
                if (failed == null) failed = sv;
            } else {
                vm.prog.args_buf[i] = sv.val;
            }
        }
        vm.sp -= arity;
        if (failed) |f| {
            vm.push(f);
            return;
        }
        const args = vm.prog.args_buf[0..arity];
        const result = stdlib.call(&vm.ctx, fid, args) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Eval => {
                vm.push(.{ .err = .{ .msg = vm.ctx.err_msg, .unbound = vm.ctx.unbound } });
                return;
            },
        };
        vm.push(.{ .val = result });
    }

    /// Invoke a host-provided function. Error-argument propagation and
    /// error-value semantics mirror the standard call path: host errors
    /// (error.Eval via ctx.fail/noOverloadNamed) become absorbable error
    /// values.
    fn doHostCall(vm: *Vm, inst: Inst) ExecError!void {
        const arity: usize = inst.b;
        var failed: ?StackVal = null;
        for (0..arity) |i| {
            const sv = vm.prog.stack[vm.sp - arity + i];
            if (sv == .err) {
                if (failed == null) failed = sv;
            } else {
                vm.prog.args_buf[i] = sv.val;
            }
        }
        vm.sp -= arity;
        if (failed) |f| {
            vm.push(f);
            return;
        }
        const hf = vm.prog.host[inst.a];
        const result = hf.func(&vm.ctx, vm.prog.args_buf[0..arity]) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Eval => {
                vm.push(.{ .err = .{ .msg = vm.ctx.err_msg, .unbound = vm.ctx.unbound } });
                return;
            },
        };
        vm.push(.{ .val = result });
    }

    fn doMacro(vm: *Vm, inst: Inst) ExecError!void {
        const info = vm.prog.macros[inst.a];
        const range_val = vm.pop();
        const range: []const Value = switch (range_val) {
            .err => {
                vm.push(range_val);
                return;
            },
            .val => |v| switch (v) {
                .list_t => |l| l,
                .map_t => |m| blk: {
                    const keys = vm.ctx.alloc.alloc(Value, m.entries.len) catch return error.OutOfMemory;
                    for (m.entries, 0..) |entry, i| keys[i] = entry.key;
                    break :blk keys;
                },
                else => {
                    vm.pushErr("no_matching_overload: cannot range over {s}", .{v.typeOf().name()});
                    return;
                },
            },
        };

        switch (inst.op) {
            .macro_all, .macro_exists => {
                const is_all = inst.op == .macro_all;
                var saw_error: ?StackVal = null;
                for (range) |item| {
                    vm.prog.locals[info.var_slot] = item;
                    const p = try vm.subrun(info.pred_start, info.pred_end);
                    switch (p) {
                        .val => |v| {
                            if (v == .bool_t) {
                                if (v.bool_t == !is_all) {
                                    vm.push(.{ .val = .{ .bool_t = !is_all } });
                                    return;
                                }
                            } else {
                                _ = vm.ctx.noOverload(if (is_all) .macro_all else .macro_exists, &.{v}) catch {};
                                vm.pushCtxError();
                                return;
                            }
                        },
                        .err => saw_error = p,
                    }
                }
                if (saw_error) |e| {
                    vm.push(e);
                } else {
                    vm.push(.{ .val = .{ .bool_t = is_all } });
                }
            },
            .macro_exists_one => {
                var count: usize = 0;
                for (range) |item| {
                    vm.prog.locals[info.var_slot] = item;
                    const p = try vm.subrun(info.pred_start, info.pred_end);
                    switch (p) {
                        .val => |v| {
                            if (v != .bool_t) {
                                _ = vm.ctx.noOverload(.macro_exists_one, &.{v}) catch {};
                                vm.pushCtxError();
                                return;
                            }
                            if (v.bool_t) count += 1;
                        },
                        .err => {
                            vm.push(p);
                            return;
                        },
                    }
                }
                vm.push(.{ .val = .{ .bool_t = count == 1 } });
            },
            .macro_filter => {
                // At most range.len elements survive; the over-allocation
                // lives in the run arena, so no shrink-copy is needed.
                const items = vm.ctx.alloc.alloc(Value, range.len) catch return error.OutOfMemory;
                var n: usize = 0;
                for (range) |item| {
                    vm.prog.locals[info.var_slot] = item;
                    const p = try vm.subrun(info.pred_start, info.pred_end);
                    switch (p) {
                        .val => |v| {
                            if (v != .bool_t) {
                                _ = vm.ctx.noOverload(.macro_filter, &.{v}) catch {};
                                vm.pushCtxError();
                                return;
                            }
                            if (v.bool_t) {
                                items[n] = item;
                                n += 1;
                            }
                        },
                        .err => {
                            vm.push(p);
                            return;
                        },
                    }
                }
                vm.push(.{ .val = .{ .list_t = items[0..n] } });
            },
            .macro_map, .macro_map3 => {
                // The two-argument map uses the "predicate" range as the
                // transform; the three-argument form filters first. At most
                // range.len results; the over-allocation lives in the run
                // arena.
                const out = vm.ctx.alloc.alloc(Value, range.len) catch return error.OutOfMemory;
                var n: usize = 0;
                // The two-argument map uses the "predicate" range as the
                // transform; the three-argument form filters first.
                const t_start = if (inst.op == .macro_map3) info.transform_start else info.pred_start;
                const t_end = if (inst.op == .macro_map3) info.transform_end else info.pred_end;
                for (range) |item| {
                    vm.prog.locals[info.var_slot] = item;
                    if (inst.op == .macro_map3) {
                        const p = try vm.subrun(info.pred_start, info.pred_end);
                        switch (p) {
                            .val => |v| {
                                if (v != .bool_t) {
                                    _ = vm.ctx.noOverload(.macro_map, &.{v}) catch {};
                                    vm.pushCtxError();
                                    return;
                                }
                                if (!v.bool_t) continue;
                            },
                            .err => {
                                vm.push(p);
                                return;
                            },
                        }
                    }
                    const t = try vm.subrun(t_start, t_end);
                    switch (t) {
                        .val => |v| {
                            out[n] = v;
                            n += 1;
                        },
                        .err => {
                            vm.push(t);
                            return;
                        },
                    }
                }
                vm.push(.{ .val = .{ .list_t = out[0..n] } });
            },
            else => unreachable,
        }
    }

    /// Run a sub-program leaving its single result value. An error result
    /// is returned as an error *value* so the macro's error-absorption
    /// rules can apply. Budget/cancel aborts are host-level control flow,
    /// not CEL error values: they propagate out of the whole run (the
    /// `else` arm, together with OutOfMemory).
    fn subrun(vm: *Vm, start: u32, end: u32) ExecError!StackVal {
        const result = vm.exec(start, end) catch |e| switch (e) {
            error.Eval => return .{ .err = .{ .msg = vm.ctx.err_msg, .unbound = vm.ctx.unbound } },
            else => return e,
        };
        return .{ .val = result };
    }
};

// ---- compiler ----

const ScopeEntry = struct {
    name: []const u8,
    slot: u32,
};

pub const CompileOptions = struct {
    /// Enabled extensions; dispatched functions are gated on this set.
    ext: ext.Extensions = .{},
    /// Host-provided functions (see HostFn), resolved for call names the
    /// standard library does not define; standard names take precedence
    /// and cannot be overridden. The slice is borrowed: it must outlive
    /// the compiled Program.
    host: []const HostFn = &.{},
};

pub const Compiler = struct {
    alloc: std.mem.Allocator,
    tree: ast.Ast,
    options: CompileOptions = .{},
    insts: std.ArrayList(Inst),
    consts: std.ArrayList(Value),
    names: std.ArrayList([]const u8),
    name_abs: std.ArrayList(bool),
    chains: std.ArrayList([]const u8),
    chain_abs: std.ArrayList(bool),
    macros: std.ArrayList(MacroInfo),
    regexes: std.ArrayList(*regex.Regex),
    scope: std.ArrayList(ScopeEntry),
    n_locals: usize = 0,
    max_stack: usize = 0,
    stack_depth: usize = 0,
    max_arity: usize = 0,
    fold_ctx: stdlib.Ctx,

    /// Compile an AST into a heap-allocated Program (call `deinit` on the
    /// program when done). `alloc` must outlive the program.
    pub fn compile(alloc: std.mem.Allocator, tree: ast.Ast, options: CompileOptions) Error!*Program {
        var comp = Compiler{
            .alloc = alloc,
            .tree = tree,
            .options = options,
            .insts = .empty,
            .consts = .empty,
            .names = .empty,
            .name_abs = .empty,
            .chains = .empty,
            .chain_abs = .empty,
            .macros = .empty,
            .regexes = .empty,
            .scope = .empty,
            .fold_ctx = stdlib.Ctx.init(alloc),
        };
        try comp.emitNode(tree.root);
        try comp.emit(.{ .op = .ret });
        return comp.finish();
    }

    fn emit(c: *Compiler, inst: Inst) Error!void {
        try c.insts.append(c.alloc, inst);
    }

    fn here(c: *Compiler) u32 {
        return @intCast(c.insts.items.len);
    }

    fn addConst(c: *Compiler, v: Value) Error!u32 {
        // Dedup scalar kinds to keep the pool small.
        switch (v) {
            .int_t, .uint_t, .double_t, .bool_t, .null_t, .type_t => {
                for (c.consts.items, 0..) |k, i| {
                    if (std.meta.activeTag(k) == std.meta.activeTag(v) and Value.eql(k, v)) {
                        return @intCast(i);
                    }
                }
            },
            else => {},
        }
        try c.consts.append(c.alloc, v);
        return @intCast(c.consts.items.len - 1);
    }

    fn addName(c: *Compiler, name: []const u8, abs: bool) Error!u32 {
        for (c.names.items, 0..) |n, i| {
            if (std.mem.eql(u8, n, name) and c.name_abs.items[i] == abs) return @intCast(i);
        }
        try c.names.append(c.alloc, name);
        try c.name_abs.append(c.alloc, abs);
        return @intCast(c.names.items.len - 1);
    }

    fn addChain(c: *Compiler, chain: []const u8, abs: bool) Error!u32 {
        try c.chains.append(c.alloc, chain);
        try c.chain_abs.append(c.alloc, abs);
        return @intCast(c.chains.items.len - 1);
    }

    /// Index of a host function by name, if registered.
    fn hostIndexOf(c: *Compiler, name: []const u8) ?u32 {
        for (c.options.host, 0..) |hf, i| {
            if (std.mem.eql(u8, hf.name, name)) return @intCast(i);
        }
        return null;
    }

    fn addRegex(c: *Compiler, pattern: []const u8) Error!?u32 {
        for (c.regexes.items, 0..) |re, i| {
            if (std.mem.eql(u8, re.pattern, pattern)) return @intCast(i);
        }
        const re = c.alloc.create(regex.Regex) catch return error.OutOfMemory;
        re.* = regex.Regex.compile(c.alloc, pattern) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return null, // invalid pattern: runtime reports it
        };
        try c.regexes.append(c.alloc, re);
        return @intCast(c.regexes.items.len - 1);
    }

    fn pushDepth(c: *Compiler) void {
        c.stack_depth += 1;
        if (c.stack_depth > c.max_stack) c.max_stack = c.stack_depth;
    }

    fn popDepth(c: *Compiler, n: usize) void {
        c.stack_depth -= n;
    }

    /// Fold a node to a compile-time constant when possible.
    fn tryFold(c: *Compiler, node: ast.NodeIndex) Error!?Value {
        const n = c.tree.nodes[node];
        switch (n.kind) {
            .int, .uint, .double, .string, .bytes, .bool_, .null_ => {
                return valueOfLiteral(n);
            },
            .list => {
                const kids = c.tree.children(node);
                const items = c.alloc.alloc(Value, kids.len) catch return error.OutOfMemory;
                for (kids, 0..) |k, i| {
                    items[i] = (try c.tryFold(k)) orelse return null;
                }
                return .{ .list_t = items };
            },
            .map => {
                const kids = c.tree.children(node);
                const count = kids.len / 2;
                const entries = c.alloc.alloc(Map.Entry, count) catch return error.OutOfMemory;
                for (0..count) |i| {
                    const k = (try c.tryFold(kids[2 * i])) orelse return null;
                    switch (k) {
                        .int_t, .uint_t, .bool_t, .string_t => {},
                        else => return null,
                    }
                    entries[i] = .{ .key = k, .value = (try c.tryFold(kids[2 * i + 1])) orelse return null };
                }
                const m = Map.init(c.alloc, entries) catch return null;
                return .{ .map_t = m };
            },
            .call => {
                const kids = c.tree.children(node);
                if (kids.len == 0 or kids.len > 8) return null;
                const fid = stdlib.fnIdFromName(n.payload.str);
                if (fid == .unknown or fid == .matches) return null;
                var args: [8]Value = undefined;
                for (kids, 0..) |k, i| {
                    args[i] = (try c.tryFold(k)) orelse return null;
                }
                c.fold_ctx.err_msg = "";
                return stdlib.call(&c.fold_ctx, fid, args[0..kids.len]) catch null;
            },
            .neg, .not => {
                const operand = c.tree.children(node)[0];
                const v = (try c.tryFold(operand)) orelse return null;
                c.fold_ctx.err_msg = "";
                const fid: stdlib.FnId = if (n.kind == .neg) .neg else .not;
                return stdlib.call(&c.fold_ctx, fid, &.{v}) catch null;
            },
            else => return null,
        }
    }

    fn valueOfLiteral(n: ast.Node) Value {
        return switch (n.kind) {
            .int => .{ .int_t = n.payload.int },
            .uint => .{ .uint_t = n.payload.uint },
            .double => .{ .double_t = n.payload.double },
            .string => .{ .string_t = n.payload.str },
            .bytes => .{ .bytes_t = n.payload.str },
            .bool_ => .{ .bool_t = n.payload.int != 0 },
            .null_ => .{ .null_t = {} },
            else => unreachable,
        };
    }

    fn emitNode(c: *Compiler, node: ast.NodeIndex) Error!void {
        const n = c.tree.nodes[node];

        // Constant folding.
        if (try c.tryFold(node)) |v| {
            const idx = try c.addConst(v);
            try c.emit(.{ .op = .push_const, .a = idx });
            c.pushDepth();
            return;
        }

        switch (n.kind) {
            .int, .uint, .double, .string, .bytes, .bool_, .null_ => unreachable,

            .ident => {
                // Comprehension variables shadow outer names.
                if (!n.flags.abs) {
                    var i = c.scope.items.len;
                    while (i > 0) {
                        i -= 1;
                        if (std.mem.eql(u8, c.scope.items[i].name, n.payload.str)) {
                            try c.emit(.{ .op = .push_local, .a = c.scope.items[i].slot });
                            c.pushDepth();
                            return;
                        }
                    }
                }
                const idx = try c.addName(n.payload.str, n.flags.abs);
                try c.emit(.{ .op = .push_var, .a = idx });
                c.pushDepth();
            },

            .select => {
                const operand = c.tree.children(node)[0];
                try c.emitNode(operand);
                const field_idx = try c.addConst(.{ .string_t = n.payload.str });
                const op: Op = if (n.flags.test_only) .select_test else .select;
                try c.emit(.{ .op = op, .a = field_idx });
                // Qualified-name fallback for pure chains rooted at an ident.
                if (c.chainOf(node)) |chain| {
                    const chain_idx = try c.addChain(chain.name, chain.abs);
                    try c.emit(.{ .op = .select_qualified, .a = chain_idx });
                }
            },

            .call => {
                const kids = c.tree.children(node);
                const fid = stdlib.fnIdFromName(n.payload.str);
                if (fid == .matches and kids.len == 2) {
                    if (c.constStringOf(kids[1])) |pattern| {
                        if (try c.addRegex(pattern)) |re_idx| {
                            try c.emitNode(kids[0]);
                            try c.emit(.{ .op = .matches_const, .a = re_idx });
                            c.popDepth(1);
                            c.pushDepth();
                            return;
                        }
                        // Invalid pattern: fall through to the runtime path.
                    }
                }
                for (kids) |k| try c.emitNode(k);
                c.popDepth(kids.len);
                if (kids.len > c.max_arity) c.max_arity = kids.len;
                if (fid == .unknown) {
                    // Host-provided function: dispatch through the host
                    // table (interned at compile time; no name lookup at
                    // eval time). Not constant-folded: host functions are
                    // not assumed pure.
                    if (c.hostIndexOf(n.payload.str)) |hi| {
                        try c.emit(.{ .op = .call_host, .a = hi, .b = @intCast(kids.len) });
                        c.pushDepth();
                        return;
                    }
                    // Unknown function: runtime error carrying the name.
                    const msg = std.fmt.allocPrint(c.alloc, "unbound function: '{s}' is not defined", .{n.payload.str}) catch return error.OutOfMemory;
                    const msg_idx = try c.addConst(.{ .string_t = msg });
                    try c.emit(.{ .op = .fail, .a = msg_idx, .b = @intCast(kids.len) });
                    c.pushDepth();
                    return;
                }
                try c.emit(.{ .op = .call, .a = @backingInt(fid), .b = @intCast(kids.len) });
                c.pushDepth();
            },

            .list => {
                const kids = c.tree.children(node);
                for (kids) |k| try c.emitNode(k);
                c.popDepth(kids.len);
                try c.emit(.{ .op = .make_list, .a = @intCast(kids.len) });
                c.pushDepth();
            },

            .map => {
                const kids = c.tree.children(node);
                for (kids) |k| try c.emitNode(k);
                c.popDepth(kids.len);
                try c.emit(.{ .op = .make_map, .a = @intCast(kids.len / 2) });
                c.pushDepth();
            },

            .struct_ => {
                const msg = std.fmt.allocPrint(c.alloc, "unsupported: message literal '{s}' (protobuf messages are not implemented)", .{n.payload.str}) catch return error.OutOfMemory;
                const msg_idx = try c.addConst(.{ .string_t = msg });
                try c.emit(.{ .op = .fail, .a = msg_idx, .b = 0 });
                c.pushDepth();
            },

            .not, .neg => {
                try c.emitNode(c.tree.children(node)[0]);
                try c.emit(.{ .op = if (n.kind == .not) .not else .neg });
            },

            .and_, .or_ => {
                const kids = c.tree.children(node);
                const is_or = n.kind == .or_;
                try c.emitNode(kids[0]);
                const check_pc = c.here();
                try c.emit(.{ .op = if (is_or) .or_check else .and_check, .a = 0 });
                try c.emitNode(kids[1]);
                c.popDepth(1); // rhs consumed by merge
                try c.emit(.{ .op = if (is_or) .or_merge else .and_merge });
                const end = c.here();
                c.insts.items[check_pc].a = end;
            },

            .ternary => {
                const kids = c.tree.children(node);
                try c.emitNode(kids[0]);
                c.popDepth(1);
                const cj = c.here();
                try c.emit(.{ .op = .cond_jump, .a = 0, .b = 0, .c = 0 });
                const then_pc = c.here();
                try c.emitNode(kids[1]);
                c.popDepth(1);
                const j = c.here();
                try c.emit(.{ .op = .jump, .a = 0 });
                const else_pc = c.here();
                try c.emitNode(kids[2]);
                c.popDepth(1);
                const end = c.here();
                c.insts.items[cj].a = then_pc;
                c.insts.items[cj].b = else_pc;
                c.insts.items[cj].c = end;
                c.insts.items[j].a = end;
                c.pushDepth();
            },

            .macro => try c.emitMacro(node),
        }
    }

    fn constStringOf(c: *Compiler, node: ast.NodeIndex) ?[]const u8 {
        const n = c.tree.nodes[node];
        if (n.kind == .string) return n.payload.str;
        return null;
    }

    const Chain = struct { name: []const u8, abs: bool };

    /// The dotted name of a pure select chain rooted at an ident, if this
    /// node is (the tail of) one.
    fn chainOf(c: *Compiler, node: ast.NodeIndex) ?Chain {
        var parts: std.ArrayList([]const u8) = .empty;
        defer parts.deinit(c.alloc);
        var abs = false;
        var cur = node;
        while (true) {
            const n = c.tree.nodes[cur];
            switch (n.kind) {
                .ident => {
                    abs = n.flags.abs;
                    parts.append(c.alloc, n.payload.str) catch return null;
                    break;
                },
                .select => {
                    if (n.flags.test_only) return null;
                    parts.append(c.alloc, n.payload.str) catch return null;
                    cur = c.tree.children(cur)[0];
                },
                else => return null,
            }
        }
        std.mem.reverse([]const u8, parts.items);
        var out: std.ArrayList(u8) = .empty;
        defer out.deinit(c.alloc);
        for (parts.items, 0..) |p, i| {
            if (i > 0) out.append(c.alloc, '.') catch return null;
            out.appendSlice(c.alloc, p) catch return null;
        }
        return .{ .name = out.toOwnedSlice(c.alloc) catch return null, .abs = abs };
    }

    fn emitMacro(c: *Compiler, node: ast.NodeIndex) Error!void {
        const n = c.tree.nodes[node];
        const kids = c.tree.children(node);
        const var_name = c.tree.nodes[kids[0]].payload.str;

        // Range expression.
        try c.emitNode(kids[1]);
        c.popDepth(1);

        // Jump over the predicate code (emitted inline).
        const skip = c.here();
        try c.emit(.{ .op = .jump, .a = 0 });

        // Allocate a local slot for the comprehension variable.
        const slot: u32 = @intCast(c.n_locals);
        c.n_locals += 1;
        try c.scope.append(c.alloc, .{ .name = var_name, .slot = slot });

        const pred_start = c.here();
        try c.emitNode(kids[2]);
        try c.emit(.{ .op = .ret });
        const pred_end = c.here();

        var transform_start: u32 = 0;
        var transform_end: u32 = 0;
        if (kids.len > 3) {
            const skip_t = c.here();
            try c.emit(.{ .op = .jump, .a = 0 });
            transform_start = c.here();
            try c.emitNode(kids[3]);
            try c.emit(.{ .op = .ret });
            transform_end = c.here();
            const after_t = c.here();
            c.insts.items[skip_t].a = after_t;
        }

        _ = c.scope.pop();

        const after = c.here();
        c.insts.items[skip].a = after;

        try c.macros.append(c.alloc, .{
            .var_slot = slot,
            .pred_start = pred_start,
            .pred_end = pred_end,
            .transform_start = transform_start,
            .transform_end = transform_end,
        });
        const macro_idx: u32 = @intCast(c.macros.items.len - 1);
        const op: Op = switch (n.payload.macro) {
            .all => .macro_all,
            .exists => .macro_exists,
            .exists_one => .macro_exists_one,
            .filter => .macro_filter,
            .map => .macro_map,
            .map_filter => .macro_map3,
        };
        try c.emit(.{ .op = op, .a = macro_idx });
        c.pushDepth();
    }

    fn finish(c: *Compiler) Error!*Program {
        const alloc = c.alloc;
        const prog = alloc.create(Program) catch return error.OutOfMemory;
        errdefer alloc.destroy(prog);

        // Sizes must be captured before the lists are drained by
        // toOwnedSlice.
        const n_names = c.names.items.len;
        const n_chains = c.chains.items.len;

        prog.backing = alloc;
        prog.ext = c.options.ext;
        prog.host = c.options.host;
        prog.insts = c.insts.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.consts = c.consts.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.names = c.names.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.name_abs = c.name_abs.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.chains = c.chains.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.chain_abs = c.chain_abs.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.macros = c.macros.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.regexes = c.regexes.toOwnedSlice(alloc) catch return error.OutOfMemory;
        prog.n_locals = c.n_locals;
        prog.max_arity = @max(c.max_arity, 2);

        // Run state.
        prog.stack = alloc.alloc(StackVal, @max(c.max_stack, 1) + 64) catch return error.OutOfMemory;
        prog.locals = alloc.alloc(Value, @max(c.n_locals, 1)) catch return error.OutOfMemory;
        prog.args_buf = alloc.alloc(Value, prog.max_arity) catch return error.OutOfMemory;
        prog.resolved_vars = alloc.alloc(StackVal, @max(n_names, 1)) catch return error.OutOfMemory;
        prog.run_candidates = alloc.alloc([]const []const u8, @max(n_chains, 1)) catch return error.OutOfMemory;
        for (prog.run_candidates) |*list| list.* = &.{};
        prog.run_vars = &.{};
        return prog;
    }
};

test {
    _ = tests;
}

const tests = struct {
    const testing = std.testing;
    const parser = @import("parser.zig");

    /// Parse, compile, run, and format; returns the display form (copied
    /// `twice` binding on int. test memory) or null on any error. The arena is released before
    /// returning.
    fn evalOpts(src: []const u8, vars: []const Binding, opts: CompileOptions) !?[]const u8 {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const r = try parser.parseSrc(arena.allocator(), src, .{});
        const tree = switch (r) {
            .ok => |t| t,
            .err => {
                std.debug.print("parse error (expr: {s})\n", .{src});
                return null;
            },
        };
        const prog = try Compiler.compile(arena.allocator(), tree, opts);
        const result = try prog.run(arena.allocator(), vars, "", .{});
        const v = switch (result) {
            .value => |v| v,
            .err => |e| {
                std.debug.print("vm error: {s} (expr: {s})\n", .{ e, src });
                return null;
            },
        };
        var aw = std.Io.Writer.Allocating.init(arena.allocator());
        try v.format(&aw.writer);
        return try testing.allocator.dupe(u8, aw.written());
    }

    fn evalToDisplay(src: []const u8, vars: []const Binding) !?[]const u8 {
        return evalOpts(src, vars, .{});
    }

    fn check(expr: []const u8, want: []const u8, vars: []const Binding) !void {
        const got = (try evalToDisplay(expr, vars)) orelse return error.TestUnexpectedResult;
        defer testing.allocator.free(got);
        try testing.expectEqualStrings(want, got);
    }

    fn checkHost(expr: []const u8, want: []const u8, vars: []const Binding, host: []const HostFn) !void {
        const got = (try evalOpts(expr, vars, .{ .host = host })) orelse return error.TestUnexpectedResult;
        defer testing.allocator.free(got);
        try testing.expectEqualStrings(want, got);
    }

    /// cel-go's canonical custom-function example, on a non-stdlib name: a
    /// `twice` binding on int.
    fn hostTwice(ctx: *stdlib.Ctx, args: []const Value) stdlib.Error!Value {
        if (args.len != 1 or args[0] != .int_t) return ctx.noOverloadNamed("twice", args);
        return .{ .int_t = args[0].int_t * 2 };
    }

    /// A host function that signals an evaluation error.
    fn hostFail(ctx: *stdlib.Ctx, args: []const Value) stdlib.Error!Value {
        _ = args;
        return ctx.fail("host boom", .{});
    }

    /// A host function constructing a value in the run arena.
    fn hostMakePair(ctx: *stdlib.Ctx, args: []const Value) stdlib.Error!Value {
        if (args.len != 2) return ctx.noOverloadNamed("makePair", args);
        const items = try ctx.alloc.alloc(Value, 2);
        items[0] = args[0];
        items[1] = args[1];
        return .{ .list_t = items };
    }

    test "vm basics" {
        try check("1 + 2 * 3 == 7", "true", &.{});
        try check("'a' + 'b' == 'ab'", "true", &.{});
        try check("1.0 == 1", "true", &.{});
        try check("size([1, 2, 3]) == 3", "true", &.{});
        try check("{1: 'a'}[1] == 'a'", "true", &.{});
        try check("true ? 1 : 2", "1", &.{});
        try check("false ? 1 : 2", "2", &.{});
        try check("!false && (true || false)", "true", &.{});
    }

    test "vm vars and selects" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const entries = try arena.allocator().alloc(Map.Entry, 1);
        entries[0] = .{ .key = .{ .string_t = "user" }, .value = .{ .string_t = "alice" } };
        const m = try Map.init(arena.allocator(), entries);
        const vars = [_]Binding{
            .{ .name = "x", .value = .{ .int_t = 42 } },
            .{ .name = "payload", .value = .{ .map_t = m } },
        };
        try check("x * 2", "84", &vars);
        try check("payload.user == 'alice'", "true", &vars);
        try check("has(payload.user)", "true", &vars);
        try check("has(payload.nope)", "false", &vars);
        try check("payload['user'] == 'alice'", "true", &vars);
    }

    test "vm unbound and errors" {
        try check("(1 / 0 == 1) || true", "true", &.{});
        try check("(1 / 0 == 1) && false", "false", &.{});
        try check("'horses' && false", "false", &.{});
        // Expected eval errors.
        try testing.expect((try evalToDisplay("y", &.{})) == null);
        try testing.expect((try evalToDisplay("1 / 0", &.{})) == null);
    }

    test "vm macros" {
        const items = [_]Value{ .{ .int_t = 1 }, .{ .int_t = 2 }, .{ .int_t = 3 } };
        const vars = [_]Binding{.{ .name = "items", .value = .{ .list_t = &items } }};
        try check("items.all(x, x > 0)", "true", &vars);
        try check("items.all(x, x > 1)", "false", &vars);
        try check("items.exists(x, x == 2)", "true", &vars);
        try check("items.exists_one(x, x > 2)", "true", &vars);
        try check("items.filter(x, x % 2 == 1).size() == 2", "true", &vars);
        try check("items.map(x, x * x).size() == 3", "true", &vars);
        try check("[1, 2, 3, 4].map(x, x % 2 == 0, x * 2).size() == 2", "true", &vars);
        // Nested macros with shadowing.
        try check("items.all(x, [1, 2].exists(y, x + y > 0))", "true", &vars);
        // Comprehension var shadows binding.
        try check("[10].exists(x, x == 10)", "true", &vars);
    }

    test "vm qualified names" {
        try check("type(duration('1s')) == google.protobuf.Duration", "true", &.{});
        try check("google.protobuf.Timestamp == google.protobuf.Timestamp", "true", &.{});
        try check("type(1) == int", "true", &.{});
    }

    test "vm regex" {
        try check("'banana'.matches('ba(na)*')", "true", &.{});
        try check("'banana'.matches('^cherry$')", "false", &.{});
    }

    test "vm constant folding" {
        // Folded expressions produce a single push_const.
        try check("1 + 2 * 3", "7", &.{});
        try check("'foo' + 'bar'", "foobar", &.{});
        try check("[1, 2] + [3]", "[1, 2, 3]", &.{});
        try check("size([1, 2, 3])", "3", &.{});
        try check("{1: 2}[1]", "2", &.{});
    }

    test "vm host functions" {
        const host = [_]HostFn{
            .{ .name = "twice", .func = &hostTwice },
            .{ .name = "hostFail", .func = &hostFail },
            .{ .name = "makePair", .func = &hostMakePair },
        };
        const vars = [_]Binding{.{ .name = "x", .value = .{ .int_t = 5 } }};

        // Global and receiver-style calls (e.f() is sugar for f(e)).
        try checkHost("twice(21)", "42", &.{}, &host);
        try checkHost("x.twice()", "10", &vars, &host);
        // Usable everywhere a standard function is, including macros and
        // operator expressions.
        try checkHost("[1, 2, 3].map(x, twice(x))", "[2, 4, 6]", &.{}, &host);
        try checkHost("twice(x) == 10", "true", &vars, &host);
        // Host-constructed values live in the run arena like any other.
        try checkHost("size(makePair(1, 2))", "2", &.{}, &host);

        // Host errors are CEL error values: absorbed by && / || like
        // standard-function errors.
        try checkHost("hostFail() && false", "false", &.{}, &host);
        try checkHost("hostFail() || true", "true", &.{}, &host);
        try testing.expect((try evalOpts("hostFail()", &.{}, .{ .host = &host })) == null);

        // Wrong arity: no_matching_overload, shaped like the standard
        // dispatcher's error.
        {
            var arena = std.heap.ArenaAllocator.init(testing.allocator);
            defer arena.deinit();
            const a = arena.allocator();
            const r = try parser.parseSrc(a, "twice(1, 2)", .{});
            const prog = try Compiler.compile(a, r.ok, .{ .host = &host });
            const result = try prog.run(a, &.{}, "", .{});
            switch (result) {
                .err => |e| try testing.expect(std.mem.indexOf(u8, e, "no_matching_overload: found no matching overload for 'twice' applied to '(int, int)'") != null),
                .value => return error.TestUnexpectedResult,
            }
        }

        // Standard names take precedence and cannot be overridden.
        try checkHost("size([1, 2])", "2", &.{}, &.{.{ .name = "size", .func = &hostTwice }});
        // Unregistered names still fail as unbound functions.
        try testing.expect((try evalOpts("nosuch(1)", &.{}, .{ .host = &host })) == null);
        // Without registration, `twice` is unbound too.
        try testing.expect((try evalOpts("twice(21)", &.{}, .{})) == null);
    }

    test "vm run budget and cancellation" {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const a = arena.allocator();
        const parsed = try parser.parseSrc(a, "x + 1", .{});
        const tree = switch (parsed) {
            .ok => |t| t,
            .err => return error.TestUnexpectedResult,
        };
        const prog = try Compiler.compile(a, tree, .{});
        // push_var, push_const, call, ret.
        try testing.expectEqual(@as(usize, 4), prog.insts.len);
        const vars = [_]Binding{.{ .name = "x", .value = .{ .int_t = 1 } }};

        // Unlimited (default): fine.
        const ok = try prog.run(a, &vars, "", .{});
        try testing.expect(ok == .value);

        // Budget exactly covering the program: fine.
        const exact = try prog.run(a, &vars, "", .{ .max_steps = 4 });
        try testing.expect(exact == .value);

        // One short: distinct error.
        try testing.expectError(error.BudgetExceeded, prog.run(a, &vars, "", .{ .max_steps = 3 }));

        // Cancellation flag polled in the exec loop.
        var cancel = Cancel{};
        cancel.request();
        try testing.expectError(error.Canceled, prog.run(a, &vars, "", .{ .cancel = &cancel }));
        cancel = Cancel{};
        const uncancelled = try prog.run(a, &vars, "", .{ .cancel = &cancel });
        try testing.expect(uncancelled == .value);

        // Budget spans comprehension sub-runs: a macro over 10 elements
        // needs one predicate execution per element.
        const items = try a.alloc(Value, 10);
        for (items, 0..) |*v, i| v.* = .{ .int_t = @intCast(i) };
        const ivars = [_]Binding{.{ .name = "items", .value = .{ .list_t = items } }};
        const parsed_m = try parser.parseSrc(a, "items.all(x, x >= 0)", .{});
        const prog_m = try Compiler.compile(a, switch (parsed_m) {
            .ok => |t| t,
            .err => return error.TestUnexpectedResult,
        }, .{});
        const all = try prog_m.run(a, &ivars, "", .{});
        try testing.expect(all == .value);
        // Budget charges each entered instruction range its full length:
        // 10 iterations of a 4-instruction predicate cannot fit in 10.
        try testing.expectError(error.BudgetExceeded, prog_m.run(a, &ivars, "", .{ .max_steps = 10 }));
    }
};
