
# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

GUIX_PROFILE="/home/justin/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

xinput set-prop 15 316 -29 -29

# Load Nix environment
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi
# source /gnu/store/7y5kmal6zr9pmvhkzk8q28813xsb4xzs-nix-2.3.13/etc/profile.d/nix.sh

export EDITOR=emacs
