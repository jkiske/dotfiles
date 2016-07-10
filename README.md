# Git
<pre>
sudo add-apt-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git
</pre>

# Emacs
<pre>
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs24 emacs24-el emacs24-common-non-dfsg
</pre>

## Python
<pre>
sudo apt-get install python-pip3 python-pip
sudo -H pip install jedi virtualenv
emacs --batch -l ~/.emacs --eval "(jedi:install-server)"
</pre>

# oh-my-zsh
<pre>
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
</pre>

# Hack font
<pre>
wget https://github.com/powerline/fonts/archive/master.zip
unzip master.zip
cd fonts-master/Hack
mkdir ~/.fonts
mv *.ttf ~/.fonts
sudo fc-cache -f -v
</pre>
