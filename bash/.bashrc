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

# Go path
export PATH=$PATH:~/.local/go/bin:~/go/bin
