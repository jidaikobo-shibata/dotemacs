# Anything のローカル cl-lib 移植版

既存の `elpa/anything-20170125.1710/` から、通常設定が使う次の4ファイルを
複製・修正したもの。対象は Emacs 30.2。ELPA 配下の原本は変更していない。

- anything.el
- anything-config.el
- anything-match-plugin.el
- anything-gtags.el

`inits/anything.init.el` が、このディレクトリを load-path の先頭へ追加する。
ELPA で Anything を再インストールしても、この修正版は維持される。
反映は Emacs の再起動で行う。起動中の Anything の一括再読み込みは行わない。
ELPA 版は他の未移植拡張のためにも残すが、未使用の同梱拡張までは移植していない。
新しい外部依存は追加していない。

## 来歴・変更の追跡

原本の SHA-256 は `upstream.sha256`、原本との変更差分は `cl-lib.patch` にある。
これらと修正版ソースを Git 管理する。patch は比較・確認用で、起動時には適用しない。
原本を更新したときは、保存したハッシュとの相違を確認してから変更を取り込む。

各ソースの著作権表示・ライセンス表示を維持した。
本体・マッチング・gtags は GPL-2.0-or-later、config は GPL-3.0-or-later。
ライセンス本文は COPYING と COPYING.GPL-3 に同梱した。
ローカル移植日: 2026-10-17。

## 移植の判断

- 旧 cl の require と関数・マクロを cl-lib へ移行。
- 旧 cl は dolist/dotimes に暗黙の block を追加していたため、cl-dolist/
  cl-dotimes を使い、入れ子やマクロ展開先の cl-return による途中終了を維持。
- flet は cl-letf による関数セルの一時束縛へ移行。引用された action-fn や
  symlink-file、補助関数間の動的な呼び出しも維持する。
- lexical-let の遅延処理は、名前付き関数と apply-partially により値を保持。
  古い Emacs 向け apply-partially の互換定義は削除し、標準実装を使用する。
- 全体の動的束縛を維持し、先頭に lexical-binding: nil を明示。
- special 宣言は cl-declare に移行。旧 cl の declare advice に依存しない。
- cl-case の引用付きキーは明示的なキー一覧へ変換し、旧来の一致対象を維持。
- ソース読込で表面化する minor-mode 定義の空の旧位置引数を除去。
- 子 Emacs に渡す非同期コピー用プログラム文字列も cl-lib へ移行。

## 検証

通常のセッションとは別の Emacs --batch -Q で実施する。

```bash
emacs --batch -Q \
  -l tests/anything-cl-lib-test.el \
  -f ert-run-tests-batch-and-exit
emacs --batch -Q \
  -l tests/anything-startup-test.el \
  -f ert-run-tests-batch-and-exit
```

前者11件は旧 cl/cl-compat の require を禁止し、候補の重複除去・件数制限、
複数正規表現、引用アクション、補助関数、遅延処理、gtags分類、バッファ操作、
子 Emacs でのコピー・上書きを検証する。元の ELPA 版も
ANYTHING_TEST_ORIGINAL=1 を指定して同じ振る舞いを比較できる。
コンパイル済みコピーは ANYTHING_TEST_LIBRARY にディレクトリを指定して検証できる。
後者3件は package-initialize 後の読み込み先、キーと候補構成、独自候補を確認する。
foreign-regexp のローカル移植後は、後者の検証でも実際に読み込み、
両者を同時に使って旧 cl を要求しないことを確認する。
SSH/FTP は架空の設定を使い、接続や実際の認証情報の読み出しは行わない。

バイトコンパイルは出力先を /tmp 以下に指定して実施済み。
4ファイルすべて成功し、旧 cl は未ロード、旧 cl の非推奨警告はゼロ。
未使用の外部機能や point-at-bol/eol、defadvice 等の既存警告は対象外として残る。
通常設定全体の隔離読込でも、この修正版が選ばれることを確認済み。

## 残る警告と手動確認

当初残っていた ace-jump-mode の旧 cl 依存は2026年版への更新で解消した。
foreign-regexp も別ディレクトリで cl-lib へ移植した
（../foreign-regexp/README.md を参照）。
現在の通常設定の隔離起動では旧 cl は読み込まれず、起動時の警告も出ない。

GUIでのキー入力・描画・フォーカス・日本語入力は自動確認していない。
再起動後、C-; の候補表示、絞り込み、Tab のアクション、C-d/s-w のバッファ削除、
日本語入力、M-. のタグジャンプ、C-^ の文字コード選択を普段の操作で確認する。
SSH/FTP 接続先候補の実データ・実接続も未検証。
