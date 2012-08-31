cd ~

# Git config
git config --global alias.s status
git config --global alias.ci commit
git config --global alias.co checkout
git config --global alias.b branch
git config --global alias.a add
git config --global alias.p push
git config --global user.name "Florian Margaine"
git config --global user.email florian@margaine.com
git config --global color.branch auto
git config --global color.diff auto
git config --global color.interactive auto
git config --global color.status auto

# Symbolic links
ln -s config-files/vimrc .vimrc
ln -s config-files/vim .vim
ln -s config-files/tmux.conf .tmux.conf
ln -s config-files/jshintrc .jshintrc

# Dependencies
sudo apt-get install curl tmux mercurial python-dev ruby-dev linux-headers-$(uname -r) g++ make

# Download and install vim
hg clone https://vim.googlecode.com/hg/ vim
cd vim/
./configure --with-features=huge --enable-pythoninterp --enable-rubyinterp --enable-cscope
make
sudo make install
cd ~
rm -rf vim/

# Download and install node.js
git clone https://github.com/joyent/node.git
cd node/
git checkout origin/v0.8.8-release
./configure
make
sudo make install
cd ~
rm -rf node/

