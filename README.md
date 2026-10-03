# dotemacs

jidaikobo-shibata's dotemacs.

## Ubuntu / macOS で共有する設定

対象は Emacs 30.2。初回に個人設定と依存関係を整えた後は、共有変更を
pull して Emacs を再起動する。起動中の一括再読み込みは、フックやタイマーの
重複を避けるため推奨しない。パッケージは起動時に自動取得・更新しない。

読み込み順:

1. `inits/settings.init.el`: 設定項目と既定値。
2. `local/settings.el`: 初期化前の個人設定値（存在する場合）。
3. `inits/platform.init.el` と既存モジュール: OS対応と共通機能。
4. `local/overrides.el`: 個人のキーバインド・追加設定（存在する場合）。

`local/` は Git 管理外。各自の設定はここに置き、共有ファイルへ直接書かない。
個人ファイルの読込エラーは警告として表示する。遅延読込されるパッケージの
キーを変更するときは、`overrides.el` 内で `with-eval-after-load` を使う。
Customize の保存先は従来どおり一時ファイルなので、永続化したい設定は
個人ファイルに記述する。

### 初回の移行

まず既存の設定・履歴を別の場所へバックアップし、未コミットの変更を確認する。
共有ファイルにある個人変更を抽出してから pull する。バックアップやメモの
移動・削除はこの設定からは行わない。

macOSでは `examples/macos-settings.el` を `local/settings.el` へコピーする。
`local/` がなければ作成する。既存の個人ファイルがあれば上書きせず内容を追記・調整する。
この例は標準IMEを使い、CommandをSuper、OptionをMetaとして扱い、
新規メモを `~/Tasks/_tmp/` に保存して自動削除しない。

キー移動も変更したい人だけ、`examples/macos-overrides.el` を
`local/overrides.el` へコピーする。macOSでもこの例は自動適用されない。
既定のSuper＋左右は従来どおり単語・記号単位の移動。

Ubuntuでは個人ファイルを作らなくても従来の設定値を使用する。
フォントは MyricaM M を優先し、未導入なら候補から選択する。
MozcのLispと `mozc_emacs_helper` がなければMozc連携を無効化する。
macOSで標準IMEを利用するときはOS側の入力切替を使用する。

### 保存先と保持期間

- `my/junk-directory`: Super＋N（macOSではCommand＋N）のメモ保存先。
- `my/junk-auto-delete`: メモの期限切れ削除を有効にするか。既定は `t`。
- `my/tmp-retention-days`: 管理対象の保持日数。既定は30日。
- メモを削除対象にするのは、保存先が管理用 `.tmp/` の配下にあり、
  `my/junk-auto-delete` が有効な場合のみ。外部の保存先は自動削除しない。
- バックアップ・自動保存・自動保存一覧の保持はメモとは独立する。

### 依存機能

`init.el` の `package-selected-packages` は利用パッケージの一覧であり、
自動インストールの指定ではない。必要なパッケージは `M-x list-packages` で
手動導入する。未導入の補助機能は無効になり、警告またはMessagesに理由が出る。

`elisp/` は従来どおりGit管理外。次の追加コードは各環境に導入済みなら使用する。

- `elisp/dired-explorer/`: 未導入なら標準Diredを使用。
- `elisp/focus-on-editable-buffers/`: 未導入ならAnythingの標準バッファ一覧を使用。
- `gtags` / `anything-gtags`: 未導入ならランチャーの他機能は継続する。

これらの配布方法・ライセンスの整理は継続事項。未導入環境でも起動できるが、
両環境で全機能が同一になることはまだ保証しない。

### 確認

隔離した一時設定でのロードと回帰テスト:

```bash
emacs -Q --batch -l tests/shared-settings-test.el
```

実機では起動、テーマ、ランチャー、フォント、日本語入力、キー操作、
emacsclient、新規メモ、Diredへのドラッグ＆ドロップを確認する。
TRAMPを使う場合はリモート転送も確認する。macOSの実操作は別途検証が必要。
