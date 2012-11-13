#!/bin/sh

cd ~

# Symbolic links
ln -s .dotfiles/vimrc .vimrc
ln -s .dotfiles/vim .vim
ln -s .dotfiles/tmux.conf .tmux.conf
ln -s .dotfiles/jshintrc .jshintrc
ln -s .dotfiles/gitconfig .gitconfig
sudo ln -s .dotfiles/drupal_ctags /usr/bin/drupal_ctags
sudo chmod +x /usr/bin/drupal_ctags
sudo chmod 777 /usr/bin/drupal_ctags

# Dependencies
sudo apt-get install curl tmux mercurial python-dev ruby-dev g++ make

# Vim
hg clone https://vim.googlecode.com/hg/ vim
cd vim/
./configure --with-features=huge --enable-pythoninterp --enable-rubyinterp --enable-cscope
make
sudo make install
cd ~
rm -rf vim/

# Node.js
git clone https://github.com/joyent/node.git
cd node/
git checkout origin/v0.8.8-release
./configure
make
sudo make install
cd ~
rm -rf node/

# Npm modules
npm install -g jshint browserify uglify-js node-gyp

