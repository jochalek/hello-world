# # Honor per-interactive-shell startup file
# if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# GUIX_PROFILE="/home/justin/.guix-profile"
# . "$GUIX_PROFILE/etc/profile"

# xinput set-prop 15 316 -29 -29

# source /gnu/store/7y5kmal6zr9pmvhkzk8q28813xsb4xzs-nix-2.3.13/etc/profile.d/nix.sh

# Propigate .profile into Bash
if [ -f ~/.profile ]; then . ~/.profile; fi
