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

#### Expand a macro
```sh
nix-shell -p cargo-expand --command 'cargo expand path::to::module::or::function'
```
Example expanding `systemd_manager::UnitRemoved`:
```sh
nix-shell -p cargo-expand --command 'cargo expand systemd_manager::UnitRemoved'
```
