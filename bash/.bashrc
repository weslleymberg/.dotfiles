# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

if [ -v TOOLBOX_PATH ];
then
    PS1="\[$(tput setaf 1)\]; \[$(tput sgr0)\]"
else
    PS1="; "
fi


# Plan 9 Port path
PLAN9=/var/home/weslleymberg/.local/plan9 export PLAN9
PATH=$PATH:$PLAN9/bin export PATH
