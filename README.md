# home-manager

Configuracion de Home Manager con varios perfiles de trabajo.

## Perfiles

- `antonio`: perfil base con Git, Emacs, Zsh y Chrome.
- `ubuntu`: perfil general de desarrollo en Ubuntu.
- `haskell96`: extiende `ubuntu` con toolchain de Haskell (GHC 9.6).
- `contribute-clippy`: perfil para contribuir en Rust/Clippy.
- `rare-evo-2025`: perfil con herramientas de Cardano.

## Estructura

- `flake.nix`: entradas y salidas del flake.
- `home.nix`: perfil base principal.
- `ubuntu.nix`, `haskell96.nix`, `contribute-clippy.nix`, `rare-evo-2025.nix`: perfiles por contexto.
- `sh/zsh.nix`, `git/git.nix`, `emacs/*.nix`, `google-chrome/google-chrome.nix`: modulos reutilizables.
- `modules/terminal/alacritty-system.nix`: configuracion compartida de Alacritty para perfiles Linux no-NixOS.

## Comandos utiles

```bash
# Activar un perfil
home-manager switch --flake .#antonio
home-manager switch --flake .#ubuntu
home-manager switch --flake .#haskell96
home-manager switch --flake .#contribute-clippy
home-manager switch --flake .#rare-evo-2025

# Actualizar lock file
nix flake update
```

## Convenciones

- Mantener modulos pequenos y composables.
- Evitar duplicar bloques grandes entre perfiles.
- Evitar dependencias impuras salvo que sean estrictamente necesarias.

## Historial de cambios

- Ver `CHANGELOG.md` para el registro detallado de cambios del proyecto.
