# foreign-regexp のローカル cl-lib 移植版

ELPA の foreign-regexp-20200325.50 を元にした Emacs 30.2 向け修正版。
元の ELPA ディレクトリは変更していない。
`inits/search-center.el` がこのディレクトリを load-path の先頭へ追加する。
パッケージを再インストールしても、この修正は維持される。
通常の Emacs を再起動すると有効になる。稼働中のセッションは変更していない。

新しい外部依存は追加していない。検索・置換は従来どおりローカルの Perl を使う。
既存のキーバインド、プリセット、Perl の検索・置換構文は変更していない。

## 来歴とライセンス

- 原本: k-talo/foreign-regexp.el のコミット
  e2dd47f2160cadc194eb156e7c76c3c869e6706e。
- 原本のハッシュ: upstream.sha256。
- 本体の原本からの差分: cl-lib.patch。起動時には適用しない。
- 追加補助ファイル: foreign-regexp-closures.el。
- ライセンス: GPL-3.0-or-later。原作者の表示を維持し、COPYING に本文を同梱。
- ローカル移植日: 2026-10-17。

## 変更の判断

- require、defmacro*、defun*、case、copy-list、member-if、position、
  reduce、remove-if、progv を cl-lib の対応する名前へ移行。
- 本体は lexical-binding: nil を維持。全体の束縛方式は変更しない。
- 検索関数の一時差し替えは cl-letf と apply-partially を使用。
  対象文字列の判定は従来どおり eq。正常終了・エラー時とも元の関数へ戻す。
- 置換ループ内の flet は関数セルの一時束縛へ移行。
- オプションのコールバックと isearch の一度だけ実行するフックは、
  補助ファイルの小さな lexical-binding: t の関数に閉じ込める。
  元バッファ・検索文字列・自己参照を保持し、再帰呼び出しを避ける。
- 遅延するコマンド移動はタイマー引数で検索文字列とコマンドを渡す。
  RE-Builder からの移動は、捕捉した文字列をフックへ明示的に渡す。
  これにより lexical-let と内部 alambda マクロは不要になった。
- 非増分検索の繰り返しにある2つの常時有効な旧 advice を advice-add へ移行。
  他の一時有効化・無効化を伴う advice の大規模な作り直しは行わない。
- 生成されるコードのコンパイル前に regexp-type の初期値 nil を宣言。
  原本末尾の byte-compile-warnings 抑制指定は引き継がない。

## 検証

リポジトリのルートで、通常の Emacs とは別プロセスで実行する。

```bash
emacs --batch -Q \
  -l tests/foreign-regexp-cl-lib-test.el \
  -f ert-run-tests-batch-and-exit
emacs --batch -Q \
  -l tests/anything-startup-test.el \
  -f ert-run-tests-batch-and-exit
```

foreign-regexp の16件では旧 cl/cl-compat の require を禁止し、実際の Perl で
前方・後方検索、キャプチャ、日本語、件数・範囲指定、検索失敗、大小文字等の
オプション、領域内置換、確認付き置換、ゼロ幅置換、quote-meta を検証する。
検索関数の復元、コールバックの文字列保持・自己解除、オプション変更時の
再帰防止、RE-Builder/isearch の受け渡し、繰り返し検索、search-center の
領域置換も確認する。実データは使用せず、一時バッファと一時ファイルを使う。

コンパイルしたコピーでも同じ16件を確認できる。

```bash
foreign_check_dir=$(mktemp -d /tmp/foreign-regexp-check.XXXXXX)
cp vendor/foreign-regexp/*.el "$foreign_check_dir/"
emacs --batch -Q \
  -L "$foreign_check_dir" \
  -f batch-byte-compile "$foreign_check_dir"/*.el
FOREIGN_REGEXP_TEST_LIBRARY="$foreign_check_dir" \
  emacs --batch -Q \
  -l tests/foreign-regexp-cl-lib-test.el \
  -f ert-run-tests-batch-and-exit
```

元のELPA版との比較は FOREIGN_REGEXP_TEST_ORIGINAL=1 で実行する。
共通する11件が成功し、移植固有の5件は明示的にスキップする。
原本が手元に残っている場合だけ使用できる。

通常設定全体も、設定ディレクトリ・履歴保存先とサーバー起動を分離した
バッチ環境で確認。Anything と foreign-regexp の修正版が選ばれ、
旧 cl は未ロードで、起動時の警告も出ないことを確認した。
バイトコンパイル自体には、残した旧 advice 等の既存警告がある。
警告の非表示設定は追加していない。

## 手動確認が必要なこと

通常の Emacs を再起動し、普段の検索窓で前後検索・置換・プリセット・
日本語入力を確認する。isearch/RE-Builder間の実画面での操作も未確認。
Wayland の普段の画面に干渉するGUI操作は実施していない。
Perl 以外の Ruby/JavaScript/Python バックエンドは今回検証していない。
