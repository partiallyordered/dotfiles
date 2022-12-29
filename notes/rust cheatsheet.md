#### Turn on crate features in LanguageClient-neovim rust-analyzer per-project
Put the following file in `.vim/settings.json` relative to your `Cargo.toml`:
```json
{
    "rust-analyzer": {
        "cargo": {
            "features": [
                "reqwest"
            ]
        }
    }
}
```
More config options: https://rust-analyzer.github.io/manual.html#configuration
