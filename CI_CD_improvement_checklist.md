# CI/CD Workflow Improvements Checklist

## Version Updates

- [ ] Update `docker/build-push-action` from v5 to v6 in docker.yml
- [x] Pin `neurogenomics/rworkflows` to specific version instead of @master
- [ ] Check if `pre-commit-hooks` can be updated from v5.0.0 to newer version
- [ ] Verify all other actions are using latest stable versions

## Concurrency Controls

- [ ] Add concurrency key to rworkflows.yml to prevent duplicate runs
- [ ] Add concurrency key to docker.yml to prevent duplicate runs  
- [ ] Add concurrency key to pkgdown.yml to prevent duplicate runs
- [ ] Configure cancel-in-progress appropriately for each workflow

## Security Enhancements

- [ ] Enable SARIF upload for Trivy security scanning results
- [ ] Integrate security scan results with GitHub Security tab
- [ ] Configure security alerts and notifications
- [ ] Review and optimize security scan coverage

## Runner Image Improvements

- [ ] Pin ubuntu-latest to ubuntu-22.04 in rworkflows.yml
- [ ] Pin ubuntu-latest to ubuntu-22.04 in docker.yml
- [ ] Pin ubuntu-latest to ubuntu-22.04 in pkgdown.yml
- [ ] Pin macos-latest to specific version in rworkflows.yml
- [ ] Pin windows-latest to specific version in rworkflows.yml

## Workflow Enhancements

- [ ] Add dependabot configuration for automated dependency updates
- [ ] Add workflow status badges to README
- [x] Create workflow documentation
- [ ] Implement quarterly review process for pinned versions

## Optional Improvements

- [ ] Consider different concurrency strategies for main vs feature branches
- [ ] Add more detailed artifact retention policies
- [ ] Optimize Docker layer caching strategy
- [ ] Add workflow performance monitoring
- [ ] Consider adding workflow dispatch triggers for manual runs
- [ ] Review and optimize matrix strategy configurations
- [ ] Add workflow failure notifications
- [ ] Consider adding workflow success/failure metrics collection

- [ ] Upgrade `docker/build-push-action` from **v5** to **v6** and update workflow inputs where syntax changed.
- [ ] Run the full matrix on a test branch to validate `docker/build-push-action@v6` before merging to *main*.
- [ ] Pin `aquasecurity/trivy-action` to the latest stable semver tag (e.g., `v0.19.0`) instead of using `master`.
- [x] Pin `neurogenomics/rworkflows` to a stable tag (e.g., `v1`) or a specific commit SHA for reproducibility.
- [ ] Add a `concurrency` block to each workflow to cancel duplicate runs on the same branch/PR.
- [ ] Replace generic runner labels (`ubuntu-latest`, `macos-latest`, `windows-latest`) with version-pinned runners (e.g., `ubuntu-22.04`, `macos-14`, `windows-2022`).
- [ ] Add `dependabot.yml` to automatically track and propose updates for GitHub Actions and Docker images.
- [ ] Introduce a GitHub **CodeQL** security-analysis workflow to scan source, Dockerfiles, and workflows.
- [ ] Ensure every third-party action in all workflows is pinned to a version tag or commit SHA.
- [ ] Keep the monthly autoupdate schedule for **pre-commit** hooks and monitor for a future `pre-commit-hooks v6` release. 