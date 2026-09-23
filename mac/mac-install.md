# ことえり設定
¥キーで入力する文字をバックスラッシュ (\) に変更する

# emacs

emacs-macがtahoeで動作しなくなったのでmac-appを使う
```
brea install mac-app
```

以下未使用
```
brew tap railwaycat/emacsmacport
brew install emacs-mac
```

## font インストール
https://github.com/yuru7/HackGen/releases
からHackGen-Bold HackGen-Regular をインストール

## 起動
emacsで起動するとキー入力を受け付けないので以下の設定を.zshrcに記載
```
emacs() {
    /opt/homebrew/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs "$@" &!
}
```

# rg
```
brew install ripgrep
```

# spell
```
brew install hunspell
```

下記よりファイル取得
https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic

en_US.aff en_US.dic を ~/Library/Spelling へコピー

# node
https://qiita.com/mame_daifuku/items/373daf5f49ee585ea498
https://qiita.com/wagi0716/items/94193a80502f9d81a9e0
nodeはnodebrewを使う

```
brew install nodebrew
# 以下を.zshrcへ追加
export PATH=$HOME/.nodebrew/current/bin:$PATH

nodebrew setup
nodebrew install-binary stable
nodebrew use stable
```

## 現在のnodeバージョン
nodebrew list

## nodeのアップデート
nodebrew install-binary stable
nodebrew use stable

# textlint
https://qiita.com/yukibe/items/bae442fa6314bd8f9d7a
https://mako-note.com/ja/textlint-emacs/
https://www.if-blog.site/posts/emacs/text-lint/
rule https://github.com/textlint/textlint/wiki/Collection-of-textlint-rule

``` 
brew install nodebrew
echo "export PATH=$HOME/.nodebrew/current/bin:$PATH" >> ~/.zshrc
nodebrew setup
nodebrew install-binary stable
nodebrew use stable
npm install -g textlint
npm install -g textlint-rule-preset-ja-technical-writing
npm install -g textlint-rule-preset-ja-spacing
npm install -g textlint-rule-unexpanded-acronym
npm install -g textlint-rule-write-good
npm install -g textlint-rule-ginger
npm install -g textlint-rule-alex
npm install -g textlint-rule-common-misspellings
npm install -g textlint-rule-en-max-word-count
```

## ~/.textlintrc に以下を記載、各ディレクトリごとに作成して設定上書きが可能
{
  "rules": {
    "preset-ja-technical-writing": true,
    "preset-ja-spacing": true,
    "unexpanded-acronym": true,
    "alex": true,
    "common-misspellings": true,
    "ginger": true,
    "write-good": true,
    "en-max-word-count": false,
  }
}

## npm でパッケージ確認
npm list -g

## textlintのアップデート
## nodeのバージョンが変わるので再インストール
npm install -g textlint
npm install -g textlint-rule-preset-ja-technical-writing
npm install -g textlint-rule-preset-ja-spacing
npm install -g textlint-rule-unexpanded-acronym
npm install -g textlint-rule-write-good
npm install -g textlint-rule-ginger
npm install -g textlint-rule-alex
npm install -g textlint-rule-common-misspellings
npm install -g textlint-rule-en-max-word-count

最新にしたら動かなくなった・・・
```
masami [~/work/kaggle] % nodebrew list
v18.10.0
v20.12.2

current: v20.12.2
masami [~/work/kaggle] % npm list -g
/Users/masami/.nodebrew/node/v20.12.2/lib
├── corepack@0.25.2
├── npm@10.5.0
├── textlint-rule-alex@5.0.0
├── textlint-rule-common-misspellings@1.0.1
├── textlint-rule-en-max-word-count@2.0.1
├── textlint-rule-ginger@2.2.1
├── textlint-rule-preset-ja-spacing@2.3.1
├── textlint-rule-preset-ja-technical-writing@10.0.1
├── textlint-rule-unexpanded-acronym@1.2.4
├── textlint-rule-write-good@2.0.0
└── textlint@14.0.4

masami [~/work/kaggle] %
```

# python

環境構築の方針
- 環境切り替えはpyenv virtualenv を使う
- condaの場合は以下参照
- condaの仮想環境(conda create)は使用しない（使用しても良いが毎回conda activateする必要があるのでめんどくさい）
- pyenvの仮想環境(pyenv virtualenv anaconda3-2024.02-1 kaggle)は使用しない
（anacondaのベース環境を引き継いだ環境でないため、anacondaで入っているライブラリなどを結局インストールの必要がある）
- condaで作成した仮想環境をpyenv local anaconda3-2024.02-1/envs/kaggle などのようにしても上記同様anacondaのベース環境が使えない状態になる
- 上記の通りanacondaの仮想環境は諦める、anacondaはバージョン毎に１つの環境とする
- インストールは conda install -c conda-forge hoge に統一する

```
brew install pyenv
brew install pyenv-virtualenv
```
以下を.zshrcへ記載
```
eval "$(pyenv init -)"
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
```

## pythonのインストール
利用可能なバージョンは
```
pyenv install --list
```
インストールは
```
pyenv install <python-version>
```

## pyenv-virtualenvで各ディレクトリごとに環境を構築する
特定のバージョンで環境作成
```
pyenv virtualenv <python-version> <env-name>
```
カレントディレクトリで作成した環境を使う
```
pyenv local <env-name>
```


# rust
brewは使用せずにrustupで入れる
https://rust-lang.org/ja/learn/get-started/
```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

要シェルの再起動

```
rustup component add rust-src rust-analysis rust-analyzer
## rust update
rustup update
```

__
brew install rust しないこと（２箇所にインストールされて管理が面倒になる）
rust-toolchainのあるディレクトリ下ではrust-toolchainで指定されたバージョンが（ダウンロードされて）使用される 
このようなディレクトリではrust のlsp起動がうまくいかないかもしれない、その場合はrust-toolchainのあるディレクトリで上記 rustup component add .... を行う
__

## cargo のサブコマンドにadd upgradeなどを追加する
https://zenn.dev/shinyay/articles/hello-rust-day070

```
cargo install cargo-edit
```

# Hammerspoon
https://qiita.com/y-sakata/items/0006ba2a838355da7b5e
```
brew install hammerspoon --cask
```


# ollama
https://blog.tomoya.dev/posts/emacs-on-local-llm/
```
brew install ollama
# 別 window で
ollama serve
# llama3.1 を起動
ollama run llama3.1
```

# el-get update
M-x el-get-update-all
M-x copilot-install-server

