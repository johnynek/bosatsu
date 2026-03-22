---
github.base_url=
---

# `Bosatsu/Example/Json/Github/Workflows/Util`

private package

source code:
- [`test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu)

public dependencies: [`Bosatsu/Json`](../../../../Json.html)

## Index

- Types: [`FetchJavaWith`](#type-fetchjavawith), [`JavaStrategy`](#type-javastrategy),
[`JavaStrategyMatrix`](#type-javastrategymatrix), [`JavaWith`](#type-javawith),
[`OnPush`](#type-onpush), [`OnPushBranches`](#type-onpushbranches), [`Step`](#type-step),
[`StepEnv`](#type-stepenv)
- Values: [`action_cache`](#value-action-cache), [`action_cache_v4`](#value-action-cache-v4),
[`action_checkout_v2`](#value-action-checkout-v2),
[`action_checkout_v4`](#value-action-checkout-v4), [`action_codecov`](#value-action-codecov),
[`action_setup_java`](#value-action-setup-java),
[`action_setup_python`](#value-action-setup-python), [`action_setup_sbt`](#value-action-setup-sbt),
[`action_upload_artifact`](#value-action-upload-artifact), [`cache_step`](#value-cache-step),
[`cat_lines`](#value-cat-lines),
[`checkout_v2_fetch_depth_0_step`](#value-checkout-v2-fetch-depth-0-step),
[`checkout_v2_step`](#value-checkout-v2-step),
[`checkout_v4_fetch_depth_0_step`](#value-checkout-v4-fetch-depth-0-step),
[`checkout_v4_step`](#value-checkout-v4-step),
[`codecov_token_secret`](#value-codecov-token-secret),
[`github_token_secret`](#value-github-token-secret), [`graalvm_java_23`](#value-graalvm-java-23),
[`graalvm_version_23_0_2`](#value-graalvm-version-23-0-2),
[`install_b3sum_step`](#value-install-b3sum-step),
[`install_sbt_step_name`](#value-install-sbt-step-name),
[`install_sbt_step_name_title`](#value-install-sbt-step-name-title),
[`java17_title_fetch_step`](#value-java17-title-fetch-step),
[`java_17_setup_name`](#value-java-17-setup-name),
[`java_17_setup_name_title`](#value-java-17-setup-name-title),
[`java_distribution_temurin`](#value-java-distribution-temurin),
[`java_matrix_setup_name`](#value-java-matrix-setup-name),
[`java_strategy_17`](#value-java-strategy-17), [`java_version_17`](#value-java-version-17),
[`java_version_matrix`](#value-java-version-matrix), [`main_branch`](#value-main-branch),
[`matrix_java_fetch_step`](#value-matrix-java-fetch-step),
[`matrix_java_step`](#value-matrix-java-step), [`needs_prepare`](#value-needs-prepare),
[`needs_test`](#value-needs-test), [`needs_test_c`](#value-needs-test-c),
[`node_lib_eval_run_create_mode_cmd`](#value-node-lib-eval-run-create-mode-cmd),
[`node_tool_eval_run_create_mode_cmd`](#value-node-tool-eval-run-create-mode-cmd),
[`permission_write`](#value-permission-write), [`python_version_3_9`](#value-python-version-3-9),
[`python_version_matrix`](#value-python-version-matrix),
[`run_c_runtime_ci`](#value-run-c-runtime-ci), [`run_lib_build_c`](#value-run-lib-build-c),
[`run_lib_build_node_c`](#value-run-lib-build-node-c), [`run_lib_test_c`](#value-run-lib-test-c),
[`run_publish_bosatsu_libs`](#value-run-publish-bosatsu-libs),
[`run_python_tests_cmd`](#value-run-python-tests-cmd), [`runner_macos_14`](#value-runner-macos-14),
[`runner_macos_latest`](#value-runner-macos-latest),
[`runner_ubuntu_latest`](#value-runner-ubuntu-latest),
[`sbt_cache_fetch_step`](#value-sbt-cache-fetch-step), [`setup_java_step`](#value-setup-java-step),
[`setup_sbt_step`](#value-setup-sbt-step), [`setup_sbt_title_step`](#value-setup-sbt-title-step),
[`timeout_minutes_default`](#value-timeout-minutes-default),
[`tool_test_workspace_cmd`](#value-tool-test-workspace-cmd),
[`tool_test_workspace_node_cmd`](#value-tool-test-workspace-node-cmd),
[`transpile_c_cmd`](#value-transpile-c-cmd), [`transpile_python_cmd`](#value-transpile-python-cmd),
[`upload_artifact_step`](#value-upload-artifact-step),
[`verify_graalvm_native_image_pin_cmd`](#value-verify-graalvm-native-image-pin-cmd)

## Types

<a id="type-fetchjavawith"></a>

### `FetchJavaWith`

```bosatsu
type FetchJavaWith
```

#### Constructors

- FetchJavaWith(
      `fetch-depth`: Bosatsu/Json::Optional[Int],
      distribution: Bosatsu/Json::Optional[String],
      `java-version`: Bosatsu/Json::Optional[String],
      `python-version`: Bosatsu/Json::Optional[String],
      token: Bosatsu/Json::Optional[String],
      override_branch: Bosatsu/Json::Optional[String],
      name: Bosatsu/Json::Optional[String],
      path: Bosatsu/Json::Optional[String],
      key: Bosatsu/Json::Optional[String],
      `restore-keys`: Bosatsu/Json::Optional[String]
  )

<a id="type-javastrategy"></a>

### `JavaStrategy`

```bosatsu
type JavaStrategy
```

#### Constructors

- `JavaStrategy(matrix: JavaStrategyMatrix)`

<a id="type-javastrategymatrix"></a>

### `JavaStrategyMatrix`

```bosatsu
type JavaStrategyMatrix
```

#### Constructors

- `JavaStrategyMatrix(java: List[String])`

<a id="type-javawith"></a>

### `JavaWith`

```bosatsu
type JavaWith
```

#### Constructors

- `JavaWith(distribution: String, \`java-version\`: String)`

<a id="type-onpush"></a>

### `OnPush`

```bosatsu
type OnPush
```

#### Constructors

- `OnPush(push: OnPushBranches)`

<a id="type-onpushbranches"></a>

### `OnPushBranches`

```bosatsu
type OnPushBranches
```

#### Constructors

- `OnPushBranches(branches: List[String])`

<a id="type-step"></a>

### `Step[with_t]`

```bosatsu
type Step[with_t: +*]
```

#### Constructors

- Step(
      uses: Bosatsu/Json::Optional[String],
      name: Bosatsu/Json::Optional[String],
      with: Bosatsu/Json::Optional[with_t],
      run: Bosatsu/Json::Optional[String],
      env: Bosatsu/Json::Optional[StepEnv],
      id: Bosatsu/Json::Optional[String]
  )

<a id="type-stepenv"></a>

### `StepEnv`

```bosatsu
type StepEnv
```

#### Constructors

- StepEnv(
      `TAG_NAME`: Bosatsu/Json::Optional[String],
      `BOSATSU_C_RUNTIME_HASH`: Bosatsu/Json::Optional[String],
      `BOSATSU_STATIC_NATIVE_IMAGE`: Bosatsu/Json::Optional[String],
      `BOSATSU_NATIVE_IMAGE_LIBC`: Bosatsu/Json::Optional[String],
      `BOSATSU_NATIVE_IMAGE_CLIB_PATH`: Bosatsu/Json::Optional[String],
      `CC`: Bosatsu/Json::Optional[String],
      `PGP_PASSPHRASE`: Bosatsu/Json::Optional[String],
      `PGP_SECRET`: Bosatsu/Json::Optional[String],
      `SONATYPE_PASSWORD`: Bosatsu/Json::Optional[String],
      `SONATYPE_USERNAME`: Bosatsu/Json::Optional[String],
      `REPO_ROOT`: Bosatsu/Json::Optional[String],
      `OUTDIR`: Bosatsu/Json::Optional[String],
      `GIT_SHA`: Bosatsu/Json::Optional[String],
      `URI_BASE`: Bosatsu/Json::Optional[String],
      `PUBLISH_DRY_RUN`: Bosatsu/Json::Optional[String],
      `C_RUNTIME_ARCHIVE`: Bosatsu/Json::Optional[String],
      `GITHUB_TOKEN`: Bosatsu/Json::Optional[String],
      `GITHUB_REF_NAME`: Bosatsu/Json::Optional[String],
      `GH_TOKEN`: Bosatsu/Json::Optional[String]
  )

## Values

<a id="value-action-cache"></a>

### `action_cache`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_cache: String
```

<a id="value-action-cache-v4"></a>

### `action_cache_v4`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_cache_v4: String
```

<a id="value-action-checkout-v2"></a>

### `action_checkout_v2`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_checkout_v2: String
```

<a id="value-action-checkout-v4"></a>

### `action_checkout_v4`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_checkout_v4: String
```

<a id="value-action-codecov"></a>

### `action_codecov`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_codecov: String
```

<a id="value-action-setup-java"></a>

### `action_setup_java`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_setup_java: String
```

<a id="value-action-setup-python"></a>

### `action_setup_python`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_setup_python: String
```

<a id="value-action-setup-sbt"></a>

### `action_setup_sbt`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_setup_sbt: String
```

<a id="value-action-upload-artifact"></a>

### `action_upload_artifact`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
action_upload_artifact: String
```

<a id="value-cache-step"></a>

### `cache_step`

references: [`Step`](#type-step)

```bosatsu
cache_step: forall a: *. Step[a]
```

<a id="value-cat-lines"></a>

### `cat_lines`

references: [`List`](../../../../Predef.html#type-list), [`String`](../../../../Predef.html#type-string)

```bosatsu
def cat_lines(lines: List[String]) -> String
```

<a id="value-checkout-v2-fetch-depth-0-step"></a>

### `checkout_v2_fetch_depth_0_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step)

```bosatsu
checkout_v2_fetch_depth_0_step: Step[FetchJavaWith]
```

<a id="value-checkout-v2-step"></a>

### `checkout_v2_step`

references: [`Step`](#type-step)

```bosatsu
checkout_v2_step: forall a: *. Step[a]
```

<a id="value-checkout-v4-fetch-depth-0-step"></a>

### `checkout_v4_fetch_depth_0_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step)

```bosatsu
checkout_v4_fetch_depth_0_step: Step[FetchJavaWith]
```

<a id="value-checkout-v4-step"></a>

### `checkout_v4_step`

references: [`Step`](#type-step)

```bosatsu
checkout_v4_step: forall a: *. Step[a]
```

<a id="value-codecov-token-secret"></a>

### `codecov_token_secret`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
codecov_token_secret: String
```

<a id="value-github-token-secret"></a>

### `github_token_secret`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
github_token_secret: String
```

<a id="value-graalvm-java-23"></a>

### `graalvm_java_23`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
graalvm_java_23: String
```

<a id="value-graalvm-version-23-0-2"></a>

### `graalvm_version_23_0_2`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
graalvm_version_23_0_2: String
```

<a id="value-install-b3sum-step"></a>

### `install_b3sum_step`

references: [`Step`](#type-step)

```bosatsu
install_b3sum_step: forall a: *. Step[a]
```

<a id="value-install-sbt-step-name"></a>

### `install_sbt_step_name`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
install_sbt_step_name: String
```

<a id="value-install-sbt-step-name-title"></a>

### `install_sbt_step_name_title`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
install_sbt_step_name_title: String
```

<a id="value-java17-title-fetch-step"></a>

### `java17_title_fetch_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step)

```bosatsu
java17_title_fetch_step: Step[FetchJavaWith]
```

<a id="value-java-17-setup-name"></a>

### `java_17_setup_name`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_17_setup_name: String
```

<a id="value-java-17-setup-name-title"></a>

### `java_17_setup_name_title`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_17_setup_name_title: String
```

<a id="value-java-distribution-temurin"></a>

### `java_distribution_temurin`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_distribution_temurin: String
```

<a id="value-java-matrix-setup-name"></a>

### `java_matrix_setup_name`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_matrix_setup_name: String
```

<a id="value-java-strategy-17"></a>

### `java_strategy_17`

references: [`JavaStrategy`](#type-javastrategy)

```bosatsu
java_strategy_17: JavaStrategy
```

<a id="value-java-version-17"></a>

### `java_version_17`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_version_17: String
```

<a id="value-java-version-matrix"></a>

### `java_version_matrix`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
java_version_matrix: String
```

<a id="value-main-branch"></a>

### `main_branch`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
main_branch: String
```

<a id="value-matrix-java-fetch-step"></a>

### `matrix_java_fetch_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step)

```bosatsu
matrix_java_fetch_step: Step[FetchJavaWith]
```

<a id="value-matrix-java-step"></a>

### `matrix_java_step`

references: [`JavaWith`](#type-javawith), [`Step`](#type-step)

```bosatsu
matrix_java_step: Step[JavaWith]
```

<a id="value-needs-prepare"></a>

### `needs_prepare`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
needs_prepare: String
```

<a id="value-needs-test"></a>

### `needs_test`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
needs_test: String
```

<a id="value-needs-test-c"></a>

### `needs_test_c`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
needs_test_c: String
```

<a id="value-node-lib-eval-run-create-mode-cmd"></a>

### `node_lib_eval_run_create_mode_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
node_lib_eval_run_create_mode_cmd: String
```

<a id="value-node-tool-eval-run-create-mode-cmd"></a>

### `node_tool_eval_run_create_mode_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
node_tool_eval_run_create_mode_cmd: String
```

<a id="value-permission-write"></a>

### `permission_write`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
permission_write: String
```

<a id="value-python-version-3-9"></a>

### `python_version_3_9`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
python_version_3_9: String
```

<a id="value-python-version-matrix"></a>

### `python_version_matrix`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
python_version_matrix: String
```

<a id="value-run-c-runtime-ci"></a>

### `run_c_runtime_ci`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_c_runtime_ci: String
```

<a id="value-run-lib-build-c"></a>

### `run_lib_build_c`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_lib_build_c: String
```

<a id="value-run-lib-build-node-c"></a>

### `run_lib_build_node_c`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_lib_build_node_c: String
```

<a id="value-run-lib-test-c"></a>

### `run_lib_test_c`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_lib_test_c: String
```

<a id="value-run-publish-bosatsu-libs"></a>

### `run_publish_bosatsu_libs`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_publish_bosatsu_libs: String
```

<a id="value-run-python-tests-cmd"></a>

### `run_python_tests_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
run_python_tests_cmd: String
```

<a id="value-runner-macos-14"></a>

### `runner_macos_14`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
runner_macos_14: String
```

<a id="value-runner-macos-latest"></a>

### `runner_macos_latest`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
runner_macos_latest: String
```

<a id="value-runner-ubuntu-latest"></a>

### `runner_ubuntu_latest`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
runner_ubuntu_latest: String
```

<a id="value-sbt-cache-fetch-step"></a>

### `sbt_cache_fetch_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step)

```bosatsu
sbt_cache_fetch_step: Step[FetchJavaWith]
```

<a id="value-setup-java-step"></a>

### `setup_java_step`

references: [`Step`](#type-step), [`String`](../../../../Predef.html#type-string)

```bosatsu
def setup_java_step[a](step_name: String, with: a) -> Step[a]
```

<a id="value-setup-sbt-step"></a>

### `setup_sbt_step`

references: [`Step`](#type-step)

```bosatsu
setup_sbt_step: forall a: *. Step[a]
```

<a id="value-setup-sbt-title-step"></a>

### `setup_sbt_title_step`

references: [`Step`](#type-step)

```bosatsu
setup_sbt_title_step: forall a: *. Step[a]
```

<a id="value-timeout-minutes-default"></a>

### `timeout_minutes_default`

references: [`Int`](../../../../Predef.html#type-int)

```bosatsu
timeout_minutes_default: Int
```

<a id="value-tool-test-workspace-cmd"></a>

### `tool_test_workspace_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
tool_test_workspace_cmd: String
```

<a id="value-tool-test-workspace-node-cmd"></a>

### `tool_test_workspace_node_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
tool_test_workspace_node_cmd: String
```

<a id="value-transpile-c-cmd"></a>

### `transpile_c_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
transpile_c_cmd: String
```

<a id="value-transpile-python-cmd"></a>

### `transpile_python_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
transpile_python_cmd: String
```

<a id="value-upload-artifact-step"></a>

### `upload_artifact_step`

references: [`FetchJavaWith`](#type-fetchjavawith), [`Step`](#type-step), [`String`](../../../../Predef.html#type-string)

```bosatsu
def upload_artifact_step(step_name: String, artifact_name: String, path: String) -> Step[FetchJavaWith]
```

<a id="value-verify-graalvm-native-image-pin-cmd"></a>

### `verify_graalvm_native_image_pin_cmd`

references: [`String`](../../../../Predef.html#type-string)

```bosatsu
verify_graalvm_native_image_pin_cmd: String
```