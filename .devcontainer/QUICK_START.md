# Quick Start: Modern DevContainer

## 🚀 Get Started in 30 Seconds

### For VS Code Users

1. **Switch to modern devcontainer**:
   ```bash
   ./.devcontainer/switch-devcontainer.sh modern
   ```

2. **Reopen in container**:
   - Press `Cmd/Ctrl+Shift+P`
   - Type "Reopen in Container"
   - Press Enter

3. **Wait for build** (5-10 min first time, 30 sec after)

4. **Start coding**:
   - VS Code: R extension ready
   - RStudio: http://localhost:8787
   - Graphics: http://localhost:8080

### For RStudio Users

Same steps, then open http://localhost:8787 in your browser!

## 🎯 What You Get

- **R 4.5.0** - Latest version (April 2025)
- **Both IDEs** - VS Code + RStudio Server
- **Modern tools** - radian console, httpgd graphics
- **Pre-installed** - All hetid dependencies
- **Python included** - For pre-commit hooks
- **Faster on ARM64** - Native Apple Silicon support

## 💡 Tips

### Check Current Setup
```bash
./.devcontainer/switch-devcontainer.sh status
```

### Switch Back to Traditional
```bash
./.devcontainer/switch-devcontainer.sh traditional
```

### Run Package Tests
```bash
# In the container terminal
make test
```

### Use Enhanced R Console
```bash
# Instead of 'R', try:
radian
```

## 🔧 Troubleshooting

### "Port 8787 already in use"
- VS Code auto-forwards to a different port
- Check the "Ports" tab in VS Code

### "Container build failed"
- Ensure Docker is running
- Try: `docker system prune` to free space

### "RStudio not loading"
```bash
sudo rstudio-server restart
```

## 📚 More Info

- Full guide: [README-modern.md](README-modern.md)
- Traditional setup: Run `switch-devcontainer.sh traditional`
- Local Docker: `make dev-start`
