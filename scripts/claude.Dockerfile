# Build (--no-cache to get the newest Claude Code; rebuild after updating the host):
#   podman build --no-cache -t claude-sandbox -f claude.Dockerfile .
FROM docker.io/manjarolinux/base

# Same distro as the host so host-built binaries and system libraries match.
# rustup provides the cargo/rustc proxies in /usr/bin; the toolchains come from
# the host ~/.rustup mounted by claude-podman, which also sets RUSTUP_HOME and
# CARGO_HOME. The container runs as the host uid (--userns=keep-id).
RUN pacman -Syu --noconfirm --needed \
       base-devel git ripgrep emacs-nox openssl rustup nodejs-lts-jod npm \
    && pacman -Scc --noconfirm \
    && npm install -g @anthropic-ai/claude-code \
    && node "$(npm root -g)/@anthropic-ai/claude-code/install.cjs"

WORKDIR /workspace
