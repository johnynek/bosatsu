# Phase 7: BosatsuPermissions - PR Guide

## Overview

Phase 7 implements a shared permission layer for Bosatsu applications, ported from BurritoScript's permission system. This provides:

- Permission declaration types with scopes
- Static analysis of required permissions from Matchless IR
- Call graph permission propagation
- Runtime permission checking with hierarchy support

## Files Created

### Core Permission Types

| File | Description |
|------|-------------|
| `core/src/main/scala/dev/bosatsu/permissions/Permission.scala` | Permission types: Simple, Scoped, Template, Config, Manifest |

### Permission Analysis

| File | Description |
|------|-------------|
| `core/src/main/scala/dev/bosatsu/permissions/PermissionAnalyzer.scala` | Static analysis of Matchless IR for permissions |
| `core/src/main/scala/dev/bosatsu/permissions/CallGraphPermissions.scala` | Permission propagation through call graphs |

### Permission Checking

| File | Description |
|------|-------------|
| `core/src/main/scala/dev/bosatsu/permissions/PermissionChecker.scala` | Runtime permission checking with three levels |

### Tests

| File | Description |
|------|-------------|
| `core/src/test/scala/dev/bosatsu/permissions/PermissionGen.scala` | ScalaCheck generators |
| `core/src/test/scala/dev/bosatsu/permissions/PermissionAnalyzerTest.scala` | Analyzer property tests |
| `core/src/test/scala/dev/bosatsu/permissions/PermissionCheckerTest.scala` | Checker property tests |

## Key Concepts

### Permission Format

Permissions follow the format: `resource:action` or `resource:action:scope`

Examples:
- `users:read` - read any user
- `users:read:own` - read only own user
- `budget:update:team` - update team budget

### Scopes

| Scope | Description |
|-------|-------------|
| `own` | Only resources owned by the current user |
| `any` | All resources (if permitted) |
| `team` | Resources within the user's team |
| `org` | Resources within the user's organization |
| `{placeholder}` | Template resolved at runtime |
| `specific-id` | Specific resource ID |

### Three-Level Checking

1. **Static (build time)** - Verify permission annotations are complete
2. **Early (request entry)** - Check non-scoped permissions upfront
3. **Late (after I/O)** - Check scoped permissions after data is loaded

### Permission Hierarchy

Broader permissions imply narrower ones:
- `users:*` implies `users:read`
- `users:read:any` implies `users:read:own`
- `users:read:org` implies `users:read:team` implies `users:read:own`

## Usage Examples

### Defining Permission Annotations

```scala
val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
  (PackageName.parts("MyApp", "Users"), Identifier.Name("getUser")) ->
    PermissionConfig.require(Permission.scoped("users", "read", Scope.Own)),
  (PackageName.parts("MyApp", "Users"), Identifier.Name("deleteUser")) ->
    PermissionConfig.require(Permission.scoped("users", "delete", Scope.Own))
)
```

### Analyzing Expressions

```scala
val result = PermissionAnalyzer.analyze(expr, annotations)
// result.staticRequirements: List[Permission]
// result.dynamicRequirements: List[PermissionTemplate]
// result.calledFunctions: Set[(PackageName, Bindable)]
```

### Call Graph Propagation

```scala
val callGraph = CallGraphPermissions.buildCallGraph(packages, annotations)
val propagated = CallGraphPermissions.propagatePermissions(callGraph)
// propagated.get(pkg, name): List[Permission] - all permissions transitively required
```

### Runtime Checking

```scala
val user = UserContext(
  userId = "user-123",
  teamId = Some("team-456"),
  orgId = Some("org-789"),
  permissions = Set("users:read:own", "posts:read")
)

val result = PermissionChecker.check(
  Permission.scoped("users", "read", Scope.Own),
  user
)

result match {
  case CheckResult.Allowed => // Proceed
  case CheckResult.Denied(perm, reason) => // Handle denial
  case CheckResult.NeedsLateCheck(template, reason) => // Defer to late check
}
```

### Scoped Permission Checking

```scala
val resource = ResourceContext(
  ownerId = Some("user-123"),
  teamId = Some("team-456"),
  orgId = Some("org-789")
)

PermissionChecker.checkScoped(
  Scoped("users", "read", Scope.Own),
  user,
  Some(resource)
) // CheckResult.Allowed if user owns resource
```

## Architecture

```
                    ┌─────────────────────────────────────────┐
                    │           Permission.scala              │
                    │  Types: Simple, Scoped, Template, etc.  │
                    └──────────────────┬──────────────────────┘
                                       │
        ┌──────────────────────────────┼──────────────────────────────┐
        │                              │                              │
        ▼                              ▼                              ▼
┌───────────────────┐    ┌───────────────────────┐    ┌───────────────────┐
│PermissionAnalyzer │    │CallGraphPermissions   │    │PermissionChecker  │
│                   │    │                       │    │                   │
│ Static analysis   │───▶│ Build call graph      │    │ Runtime checking  │
│ of Matchless IR   │    │ Propagate permissions │    │ Three-level model │
│                   │    │ Find entry points     │    │ Scope resolution  │
└───────────────────┘    └───────────────────────┘    └───────────────────┘
        │                              │                              │
        └──────────────────────────────┼──────────────────────────────┘
                                       │
                                       ▼
                              ┌────────────────┐
                              │PermissionManifest│
                              │ Per-function     │
                              │ permission info  │
                              └────────────────┘
```

## Test Results

Run tests with:
```bash
nix-shell --run "sbt 'coreJVM/testOnly dev.bosatsu.permissions.*'"
```

## Integration with Other Phases

- **Phase 6 (Simulation Applets)**: Use permissions to control who can edit simulations
- **Phase 11 (BosatsuService)**: Check permissions before handler execution
- **Phase 12 (BosatsuExplorer)**: Display permission requirements in explorer UI

## Verification

1. Build: `nix-shell --run "sbt coreJVM/compile"`
2. Test: `nix-shell --run "sbt 'coreJVM/testOnly dev.bosatsu.permissions.*'"`
3. Coverage: Verify all permission paths are tested

## Security Considerations

- Permission strings are case-sensitive
- Template placeholders must be explicitly resolved
- Scoped permissions require resource context for full verification
- Wildcards (`*`) should be used sparingly in production
