
# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Load default guix profile
GUIX_PROFILE="/home/justin/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Load extra task-oriented guix profiles
GUIX_EXTRA_PROFILES="$HOME/.guix-extra-profiles"
for i in $GUIX_EXTRA_PROFILES/*; do
  profile=$i/$(basename "$i")
  if [ -f "$profile"/etc/profile ]; then
    GUIX_PROFILE="$profile"
    . "$GUIX_PROFILE"/etc/profile
  fi
  unset profile
done

# Reverse scrolling on the Lenovo
# xinput set-prop 15 316 -29 -29
# Reverse scrolling on the Lenovo with ideapad_laptop blacklisted
xinput set-prop 14 316 -29 -29
# Enable tap
xinput set-prop 14 323 1, 1, 1, 1, 1, 0, 0

# Load Nix environment
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi
# source /gnu/store/7y5kmal6zr9pmvhkzk8q28813xsb4xzs-nix-2.3.13/etc/profile.d/nix.sh

export EDITOR=emacs

# Ensure that font folders are loaded correctly
xset +fp $(dirname $(readlink -f ~/.guix-extra-profiles/desktop/desktop/share/fonts/truetype/fonts.dir))

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc

# FIXME Try to set this for gnome-keyring to work right with nextcloud_client
# export GNOME_KEYRING_CONTROL=/run/user/1000/keyring/control
# export $(gnome-keyring-daemon -r -d)

# Add my bash scripts to PATH
export PATH="$HOME/.bin:$PATH"

# To upgrade pip with --user, python needs to have the .local in its path
# Just install other packages in env
export PYTHONPATH=.local/lib/python3.8/site-packages:$PYTHONPATH
# Tensorflow can't find libstc++
# this could be altered to help
# GCC_TOOLCHAIN="gcc-toolchain@10.3"
# GCC_LIB_PATH="$(grep -oE "[^\"]*gcc-10[^\"]*-lib" $(grep -oE "[^\"]*gcc-10[^\"]*drv" $(guix build -d ${GCC_TOOLCHAIN})) | head -n 1)"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/gnu/store/3h7xd0d47a286b6r9qhz4ybi5iaxkfwi-gcc-11.1.0-lib/lib
