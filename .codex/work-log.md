# Work Log

## 2026-04-08

- 何をしたか: `inits/mozc.init.el` から、旧来の `isearch` 直結コード（`my/isearch-resync-input-method`、`isearch-use-input-method`、`isearch` 中 `<henkan>/<muhenkan>` 制御）を削除し、`C-s <henkan>` の橋渡しは `search-center.el` 側の責務だと明示した。あわせてミニバッファ各種 map では `<muhenkan>` を `ignore` にして、終了確認などで英数キーのつもりで押したときに `Quit` にならないようにした。
- なぜそうしたか: 現在は `isearch + Mozc` を直接成立させる設計ではなく、`search-center` へ橋渡しする粗結合構成に変わっており、`mozc.init.el` に旧 `isearch` 救済コードを残す意味が薄くなっていたため。また、ミニバッファ中の `<muhenkan>` が予期せず `Quit` を起こすのはストレスが大きいため。
- 未完了の事項: `my/deactivate-input-method-command` の直接状態操作（`current-input-method` などの `setq`）は今回そのまま残している。通常バッファ・Anything・ミニバッファでの `<muhenkan>` の最終方針は、しばらく運用してから再評価してよい。
- 次にやるとよいこと: `s-q` の終了確認ミニバッファで `<muhenkan>` が無害化されたか確認する。あわせて通常バッファでの `<henkan>` / `<muhenkan>` 操作感が従来どおりか軽く見る。

- 何をしたか: `C-s <henkan>` の日本語検索は `isearch` を直接日本語対応させるのではなく、`search-center.el` 側へ寄せる方針に切り替えた。`search-center.el` に `sc/is-use-mozc-search-bridge`、`sc/bridge-isearch-to-mozc-search`、`sc/set-search-string` などを追加し、`C-s <henkan>` で `isearch` を中断して `Mozc search:` に入り、確定文字列を `sc/search-str-buffer` と `sc/previous-searched-str` に格納してから `sc/search-replace "next"` を1回実行する流れにした。`Mozc search:` 中の `<muhenkan>` はキャンセル扱いにして、元の `isearch` へ戻す経路も追加した。あわせて `mozc.init.el` から `my-mozc-isearch` の読込を外し、`search-center` ブリッジ使用時は旧 `<henkan>` 直結設定をスキップするようにした。
- なぜそうしたか: 既存の `s-g` / `s-G` による next/prev は `search-center.el` が担っており、そこを別実装に分散させるより、Mozc 側は検索語を `search-center` へ渡す入口だけに絞る方が運用に合っていて安全だから。
- 未完了の事項: GUI 実機で `C-s <henkan>` -> `Mozc search:` -> 日本語確定 -> 初回前方検索 -> `s-g` / `s-G` 巡回、そして `<muhenkan>` による `isearch` 復帰がまだ未確認。
- 次にやるとよいこと: Emacs を再起動し、まず `C-s <henkan>` 後に `Mozc search:` が出ること、確定文字列で1回検索が走ること、`s-g` / `s-G` がそのまま効くこと、`<muhenkan>` で `isearch` に戻れることを順に確認する。

- 何をしたか: `C-s` の `isearch` 中に `I-search [[Mozc]]:` と表示されるのに日本語入力できない件を調査し、`current-input-method="japanese-mozc"` / `mozc-mode=t` に対して `input-method-function=nil` になる半壊状態を確認した。対策として `inits/mozc.init.el` に `my/isearch-resync-input-method` を追加し、`isearch-mode-hook` で Mozc 表示だけ残って実体が欠けている場合に入力メソッドを再同期するようにした。あわせて `.gitignore` に `*.elc` と `session.*` を追加した。
- 何をしたか: `isearch` 中の Mozc 日本語入力は既存の `activate-input-method` 連携だけでは安定しないと判断し、`elisp/my-mozc-isearch.el` を新規追加した。`<henkan>` で `isearch` 本体へ IME を直接載せる代わりに、一時的にミニバッファで Mozc 入力し、確定文字列を `isearch-yank-string` で検索文字列へ戻す方針に切り替えた。`inits/mozc.init.el` からは新ファイルを `require` するだけに留めた。
- 何をしたか: `my-mozc-isearch` が読まれていても `inits/mozc.init.el` 側の既存 `isearch` 設定が `<henkan>` と `isearch-use-input-method` を上書きしていたため、`featurep 'my-mozc-isearch` のときは従来の `isearch` 直結設定をスキップする分岐を追加した。
- 何をしたか: `inits/modes.init.el` で `php-mode` / `html-mode` / `lisp-mode` / `emacs-lisp-mode` への `flycheck-mode` 自動有効化をコメントアウトし、実験的に手動有効化運用へ切り替えた旨のコメントを残した。
- なぜそうしたか: `.el` 編集中に `lexical-binding` 警告などのノイズが多く、今回の `mozc` 調査中の切り分けを阻害していたため。必要なら `M-x flycheck-mode` で個別に再有効化できる状態は維持するため。
- 未完了の事項: PHP や HTML 編集中に Flycheck が自動では動かなくなるため、必要時は手動有効化が必要。
- 次にやるとよいこと: しばらくこの運用でノイズが減るかを見る。必要なら後で `emacs-lisp-mode` だけ無効、他は自動有効に戻すなど、モード別に再調整する。
- なぜそうしたか: 新しい別ファイル方式を試しても `C-s <henkan>` が旧実装のまま動いており、`my-mozc-isearch` の実験経路に入れていなかったため。
- 未完了の事項: 分岐修正後の GUI 実機確認は未実施。`C-s <henkan>` で本当にミニバッファ prompt が出るか、Mozc 入力できるかは要確認。
- 次にやるとよいこと: Emacs を再起動して `C-s <henkan>` を試し、`Mozc search:` ミニバッファに入るかをまず確認する。入らないならキーイベント名の違い（`<henkan>` と `[henkan]`）を疑う。
- なぜそうしたか: 既存の `mozc.init.el` には通常時の Mozc 制御、`muhenkan` の独自回避、`isearch` 連携が混在しており、そこへさらに実験コードを重ねると切り戻しと切り分けが難しくなるため。
- 未完了の事項: 新しい `my-mozc-isearch.el` の実機確認は未実施。`C-s` -> `<henkan>` -> ミニバッファ入力 -> `isearch` 復帰の体感、`DEL`、`C-s` 続行、`C-g` 中断の自然さは要確認。
- 次にやるとよいこと: GUI Emacs で新方式を試し、少なくとも `C-s` / `<henkan>` / 日本語確定 / `DEL` / `C-s` / `<muhenkan>` が破綻しないかを見る。必要なら trigger key や prompt 文言を微調整する。
- なぜそうしたか: `my/deactivate-input-method-command` は過去の試行錯誤の結果で変更影響が読みにくいため、そこには触れず、症状が出る `isearch` 開始時だけ局所的に整合を取り直す方が安全だと判断したため。
- 未完了の事項: GUI 実運用で `C-s` 開始直後、`<henkan>` 後、`<muhenkan>` 後の各ケースで Mozc 入力が安定するかは未確認。`my/deactivate-input-method-command` 自体が状態不整合の起点かどうかも未確定。
- 次にやるとよいこと: 実際に `C-s` で `I-search [[Mozc]]:` になったケースを数回試し、以前のような `input-method-function=nil` の再発があるかを確認する。再発するなら `activate-input-method` / `deactivate-input-method` / `my/deactivate-input-method-command` 周辺のログを追加して壊れる経路をさらに絞る。

## 2026-03-04

- 何をしたか: このプロジェクトの主たる目的を記録した。対象は `init.el`、`inits/`、`themes/`、`elisp/dired-explorer/`、`elisp/focus-on-editable-buffers/` の改善。
- なぜそうしたか: 今後の作業で優先対象を明確にし、判断基準をぶらさないため。
- 未完了の事項: 各対象ディレクトリ・ファイルごとの具体的な改善方針、優先順位、完了条件は未整理。
- 次にやるとよいこと: 各対象について、現状の課題、改善したい点、優先順位を簡潔に追記する。

- 何をしたか: `inits/` 配下を静的レビューし、目立つ不具合、バグ候補、セキュリティ上の懸念を確認した。
- なぜそうしたか: 初期化処理は起動時に広く影響し、シェル実行やファイル保存フックの不備が使い勝手や安全性に直結するため。
- 主な指摘:
  - `auto-complete.init.el` の `git commit` 実行と `filer.init.el` の `msgfmt` 実行が `shell-command` の文字列連結になっており、空白やメタ文字を含むパスで壊れうる。場合によっては意図しないシェル解釈の余地がある。
  - `auto-complete.init.el` で辞書文字列を `re-search-forward` にそのまま入れており、正規表現メタ文字を含む文字列で誤判定や `invalid-regexp` の可能性がある。
  - `filer.init.el` の `.po` 保存後処理がグローバル `after-save-hook` で動いており、`msgfmt` 未導入時、TRAMP、特殊な保存経路で不安定になりやすい。
  - `log.el` は読み込まれると `isearch` 中の打鍵を平文で `~/Desktop/emacs-log.txt` に記録しうるため、機密情報を含む検索語の漏えいリスクがある。
  - `modes.init.el` の `flycheck` 用 `autoload` 記述はシンボル名とファイル名が不正確で、現状は `require` 済みのため顕在化しにくいが設定として壊れている。
- 未完了の事項: 上記の指摘は未修正。実行時の再現確認や、パッケージの存在条件を含むロード試験は未実施。
- 次にやるとよいこと: `shell-command` を `call-process` 系へ置換し、正規表現入力の `regexp-quote`、`.po` フックの限定、必要なら `log.el` の無効化または用途限定を進める。

- 何をしたか: `inits/log.el` を削除した。
- なぜそうしたか: 現状の `init.el` からは読み込まれておらず未使用で、読み込まれた場合は `isearch` 中の打鍵内容を平文ログに残すため、保守対象から外してリスクを減らす方が妥当だった。
- 未完了の事項: 同等の開発用ログが将来必要になった場合は、常時フックではなく明示的に有効化する形で再実装する。
- 次にやるとよいこと: 次は `auto-complete.init.el` と `filer.init.el` の `shell-command` 周りを安全なプロセス起動に置き換える。

- 何をしたか: `filer.init.el` の `.po` 保存後処理を、グローバル `after-save-hook` から `.po` バッファ限定のローカルフックへ変更し、`shell-command` を `process-file` に置き換えた。
- なぜそうしたか: 通常の保存処理へ不要に干渉しないようにしつつ、`msgfmt` 未導入時や TRAMP 上での不安定さ、シェル文字列連結のリスクを避けるため。
- 未完了の事項: `po-mode` 以外で `.po` を開く運用がある場合、そのケースでは自動コンパイルされない。
- 次にやるとよいこと: 必要なら `msgfmt` 失敗時の標準エラーを専用バッファに出すなど、失敗原因を見やすくする。

- 何をしたか: `auto-complete.init.el` で、辞書更新後の `git commit` を `shell-command` から `process-file` ベースへ置き換え、辞書内検索時に `regexp-quote` を追加した。
- なぜそうしたか: パス中の空白やシェルメタ文字でコマンドが壊れる問題を避けつつ、辞書文字列に正規表現メタ文字が含まれても誤判定や `invalid-regexp` が起きないようにするため。
- 未完了の事項: `git commit` 失敗時はメッセージ表示のみで、詳細な失敗理由の表示まではしていない。
- 次にやるとよいこと: 必要なら `git` 実行結果を専用バッファへ出すか、コミット要否を確認して空コミット相当の失敗を静かに扱う。

- 何をしたか: `modes.init.el` の `flycheck` 設定から、不整合な `autoload` 記述を削除した。
- なぜそうしたか: 直前で `require 'flycheck` しているため `autoload` は不要であり、かつ元の `autoload` はシンボル名とライブラリ名が不正確で、将来の整理時に不具合の原因になりうるため。
- 未完了の事項: `flycheck` 自体の読み込み失敗時の代替動作までは追加していない。
- 次にやるとよいこと: 必要なら `require` を `require ... nil t` にして、未導入環境でも起動を止めにくくするか検討する。

- 何をしたか: `modes.init.el` の `flycheck` を起動時必須ロードから外し、導入済みのときだけ有効化するヘルパー経由のフックへ変更した。
- なぜそうしたか: `flycheck` 未導入環境でも起動を止めず、導入済み環境では従来どおり対象モードで有効になるようにするため。
- 未完了の事項: `flycheck` が未導入のときは、当然ながらエラーチェック機能は無効のままになる。
- 次にやるとよいこと: 必要なら未導入時に一度だけ通知するか、`package-selected-packages` との整合を見直す。

- 何をしたか: `search-center.el` で、検索文字列が空文字列のときは未設定扱いにするよう変更し、`string-empty-p` のために `subr-x` を読み込むようにした。
- なぜそうしたか: `\"\"` を検索に渡すと、どこでも一致するような不安定な挙動になりやすいため。一方で、置換文字列の空は削除用途として有効なので、その挙動は維持するため。
- 未完了の事項: 空の検索バッファ時に前回の検索文字列へフォールバックする既存仕様は維持しているため、常に即エラーにしたい場合は追加調整が必要。
- 次にやるとよいこと: 次は `sc/target-buffer` をバッファ名文字列ではなくバッファオブジェクトで持つように整理すると、リネーム耐性が上がる。

- 何をしたか: `search-center.el` の `sc/target-buffer` をバッファ名文字列ではなくバッファオブジェクトで保持するよう変更し、参照時に `buffer-live-p` で生存確認するヘルパーを追加した。
- なぜそうしたか: バッファ名変更や同名バッファ発生時でも、検索対象の実体を取り違えにくくし、kill 済みバッファ参照で壊れにくくするため。
- 未完了の事項: `sc/target-window` 側は従来どおりなので、対象ウィンドウが消えた場合の扱いまでは今回変えていない。
- 次にやるとよいこと: 必要なら `sc/target-window` も `window-live-p` で補強し、`s-h` や `sc/quit-str-windows` の復帰先を安定させる。

- 何をしたか: `search-center.el` に `sc/select-target-window` を追加し、`sc/target-window` を直接 `select-window` していた箇所を `window-live-p` 付きの復帰処理へ置き換えた。
- なぜそうしたか: 対象ウィンドウが閉じられたあとでも、`s-h`、`sc/quit-str-windows`、検索、全置換が死んだウィンドウ参照で壊れにくくするため。
- 未完了の事項: 既存の「どのウィンドウを target とみなすか」の更新方針自体は変えていないため、より賢い復帰先選択まではしていない。
- 次にやるとよいこと: 必要なら常駐 idle timer の見直しや、`ignore-errors` の縮小でデバッグしやすさを上げる。

- 何をしたか: `sc/select-target-window` に `interactive` を追加し、`s-h` から呼べる対話コマンドにした。
- なぜそうしたか: `global-set-key` に割り当てた関数がコマンドでないと `Wrong type argument: commandp` で失敗するため。
- 未完了の事項: `s-h` 自体が現行運用でどの程度使われているかは未確認。
- 次にやるとよいこと: 実際に `s-h` を押して、対象ウィンドウが生きている場合と閉じた場合の両方を軽く確認する。

- 何をしたか: `sc/grep` で、無拡張子バッファ時の既定拡張子を `*` に変更し、空の検索語を弾き、無視ディレクトリを呼び出し中だけ有効なローカル設定にした。
- なぜそうしたか: `*.` のような分かりにくい初期値を避け、空検索による曖昧な実行を防ぎ、`grep-find-ignored-directories` へ一時的な設定が蓄積し続ける副作用を止めるため。
- 未完了の事項: `*grep*` バッファを毎回消すか確認する運用はそのまま残っている。
- 次にやるとよいこと: 必要なら次は `rgrep` 結果を検索ごとに別バッファ名へ分ける整理を検討する。

- 何をしたか: `rgrep` 前の `*grep*` 削除確認 advice をやめ、`rgrep` 後に検索語・対象ディレクトリ・拡張子を含むバッファ名へリネームするよう変更した。
- なぜそうしたか: 古い grep 結果を消さずに複数保持できるようにし、毎回の削除確認による操作の引っかかりをなくすため。
- 未完了の事項: バッファ名は簡潔さ優先で検索語を 30 文字までに切り詰めているため、長い検索語では末尾が省略される。
- 次にやるとよいこと: 実際に `sc/grep` を2回以上実行して、結果バッファが `*grep: ...*` として並ぶことを確認する。

- 何をしたか: `search-center.el` の `sc/replace-all` から `ignore-errors` を外した。
- なぜそうしたか: 無効な regexp や読み取り専用バッファなどで置換に失敗したとき、原因が見えるようにするため。
- 未完了の事項: エラーメッセージをより親切に整形する処理は追加していないため、失敗時は Emacs 標準のエラー表示になる。
- 次にやるとよいこと: 実際に不正な regexp を入れて一度失敗させ、原因が見えることを確認する。

- 何をしたか: `search-center.el` で、`sc/target-buffer` / `sc/target-window` のフォールバック先が `*search string*` / `*replace string*` にならないよう、補助バッファを除外して live な通常ウィンドウを探すようにした。また、`rgrep` 後のバッファ名変更 advice は `sc/grep` 実行中だけ有効にした。
- なぜそうしたか: 対象バッファや対象ウィンドウが消えたあとに、検索・置換処理が補助バッファ上で誤って動くのを防ぎ、`search-center` 以外から呼ばれた `rgrep` まで勝手にリネームしてしまう副作用を避けるため。
- 未完了の事項: 通常ウィンドウが1つも残っていない特殊な状態では `user-error` にしているため、そのときに新規ウィンドウを自動で選び直すような積極的な救済まではしていない。
- 次にやるとよいこと: `*search string*` 側で target を kill したあとに検索を実行し、補助バッファではなく別の通常バッファへ正しく復帰することと、通常の `M-x rgrep` では `*grep*` 名のまま動くことを確認する。

- 何をしたか: `search-center.el` のモードライン色同期を、`0.03` 秒ごとの `idle timer` 監視から、`search-center-re-mode` の切替時とウィンドウ/バッファ切替時のイベント駆動へ変更した。
- なぜそうしたか: 正規表現モードかどうかの見た目を維持しつつ、常時ポーリングによる無駄な負荷と、timer オブジェクト管理の分かりにくさを減らすため。
- 未完了の事項: 検索・置換ウィンドウを自動で閉じる側の `idle timer` は今回そのままなので、必要なら別途同じ方針でイベント駆動へ寄せられる。
- 次にやるとよいこと: 通常検索と正規表現検索を切り替えつつ、別バッファへ移動したときにモードライン色が追従することを確認する。

- 何をしたか: モードライン色同期の初期化時に、設定ファイル読込直後の `(sc/toggle-mode-line)` を呼ばないように戻した。
- なぜそうしたか: テーマ適用前の `mode-line` 色を「通常時の保存色」として拾ってしまい、その後に通常モードへ戻るたび灰色系の初期色へ戻る不具合を避けるため。
- 未完了の事項: テーマを後から切り替えた場合に、保存済みの通常色を再取得する仕組みまでは入れていない。
- 次にやるとよいこと: Emacs 起動後に通常時のモードライン色がテーマどおりのままで、正規表現モードの ON/OFF 時だけ色が切り替わることを確認する。

- 何をしたか: `load-theme` / `enable-theme` / `disable-theme` の後で、`search-center` が保持している通常時の `mode-line` 色キャッシュを破棄し、テーマ適用後の色を取り直すようにした。
- なぜそうしたか: `search-center` がテーマ適用前の色を先に記憶してしまうと、通常モードへ戻るたびにテーマの `mode-line` 色ではなく古い灰色系を再適用してしまうため。
- 未完了の事項: 今回は `search-center` 側で吸収したため、`init.el` の読込順自体は変更していない。
- 次にやるとよいこと: `jidaikobo-dark` 読込後に通常時の `mode-line` がテーマ色になり、テーマ切替後でも正規表現モードの ON/OFF が正しく追従することを確認する。

- 何をしたか: 検索・置換ウィンドウを自動で閉じる仕組みを、`0.03` 秒ごとの `idle timer` から、ウィンドウ選択や表示バッファが変わった時だけ判定するイベント駆動へ置き換えた。あわせて、片方の補助ウィンドウだけ残っている状態でも個別に安全に閉じるようにした。
- なぜそうしたか: 常時ポーリングをやめて負荷と追跡しにくさを減らしつつ、従来どおり「補助ウィンドウから離れたら閉じる」挙動を保つため。
- 未完了の事項: `sc/ignore-delete-window-hook` は元から実質未使用のまま残っているため、不要なら別途整理できる。
- 次にやるとよいこと: `s-f` で補助ウィンドウを開き、他ウィンドウへ移動した時にすぐ閉じることと、補助ウィンドウ上での編集中は閉じないことを確認する。

- 何をしたか: `search-center.el` から、参照されていない `sc/ignore-delete-window-hook` の定義と代入を削除した。
- なぜそうしたか: 使われていない状態変数を残すと、将来「削除抑止に効いている」と誤認しやすく、コード追跡のノイズになるため。
- 未完了の事項: 補助ウィンドウまわりには古い命名やコメントがまだ残っているため、必要なら次に軽いリネーム整理はできる。
- 次にやるとよいこと: 現状挙動に変化がないことを前提に、未使用変数や古いコメントを同じ粒度で少しずつ掃除していく。

- 何をしたか: `search-center.el` 内の表記ゆれと古いコメントを整理し、`regrex` / `wiondow` / `repalce` / `controll` / `corsor` などの typo と、`idle timer` 前提だった説明を現在の実装に合わせて直した。
- なぜそうしたか: 動作確認後の軽い保守として、コメントと実装の不一致や typo を減らし、あとで読み返した時の引っかかりを減らすため。
- 未完了の事項: 変数名や関数名そのものには古い綴りや命名方針の揺れがまだ残るが、今回は挙動影響を避けてコメント整理のみに留めた。
- 次にやるとよいこと: 必要なら次は、シンボル名の整理を互換性を崩さない範囲で段階的に進める。

- 何をしたか: `elisp/codex-pane.el` を新規追加し、上下2分割（上: 出力 / 下: 入力）で `C-c C-c` 送信できる最小の Codex 連携 UI を実装した。あわせて `init.el` に `(require 'codex-pane)` を追加して起動時に読み込むようにした。
- なぜそうしたか: 要望の「下で入力してキーバインド送信し、上に返答を表示する」をすぐ使える形で満たすため。`make-process` を使ってシェル経由を避け、引数注入リスクを下げつつ非同期で応答を表示できる構成にした。
- 未完了の事項: `codex-pane-cli-command` の実引数は環境ごとに異なる可能性があるため、必要に応じてユーザー環境の Codex CLI 仕様に合わせた調整が必要。
- 次にやるとよいこと: `M-x codex-pane-open` で起動し、`*codex-input*` で `C-c C-c` を押して実際に応答が `*codex-output*` に出るかを確認する。必要なら送信時にバッファ全体ではなくリージョン送信を追加する。

- 何をしたか: `codex-pane-open` の入力バッファ初期化を修正し、`text-mode` と `codex-pane-input-mode` を `*codex-input*` バッファに対して確実に有効化するようにした。
- なぜそうしたか: これまで `set-window-buffer` 後に current buffer が切り替わる前提で処理していたため、`C-c C-c` が入力バッファで未定義になることがあったため。
- 未完了の事項: 送信コマンド自体の CLI 引数最適化は未調整のまま。
- 次にやるとよいこと: `M-x codex-pane-open` 後、`*codex-input*` で `C-c C-c` を押して送信できることを実機で確認する。

- 何をしたか: `codex-pane--append-output` を更新し、出力追記後に `*codex-output*` を表示している全ウィンドウの `window-point` を末尾へ移動する自動スクロールを追加した。
- なぜそうしたか: 応答ストリームが伸びるたびに手動スクロールしなくても、上ペインが常に最新出力を追従できるようにするため。
- 未完了の事項: ユーザーが途中を読んでいるときも追従する挙動なので、「手動スクロール中は追従停止」の配慮は未実装。
- 次にやるとよいこと: 必要なら「下端付近にいる時だけ自動追従する」条件を追加し、閲覧中の意図しないジャンプを防ぐ。

- 何をしたか: 実験用に追加していた `elisp/codex-pane.el` を削除し、`init.el` から `(require 'codex-pane)` を除去した。
- なぜそうしたか: 実験終了に伴い、不要な読み込みと関連ファイルを残さないため。
- 未完了の事項: なし。
- 次にやるとよいこと: Emacs 再起動後に起動エラーが出ないことだけ確認する。

## 2026-03-05

- 何をしたか: 起動時の `anything-config.el` 警告を調査し、`elpa/anything-20170125.1710/anything-config.elc` が欠けていたため、`byte-compile-file` で `anything-config.elc` を生成した。`require 'anything-config` の再確認で、`letf` / `loop` 由来の警告が再現しないことを確認した。
- なぜそうしたか: 古い `anything` 実装自体は残っていても、`.el` 直読みを避けて `.elc` を使うことで、起動時のノイズを減らせるため。
- 未完了の事項: `anything` 本体が古いこと自体は未解決で、将来の Emacs 互換性リスクは残る。
- 次にやるとよいこと: 長期的には `helm` / `consult` への段階移行を検討する。

- 何をしたか: `emacs-mozc` の警告について、APT 上の版を確認し、Ubuntu noble では `2.28.4715.102+dfsg-2.2build7` が installed/candidate 同一であることを確認した。`inits/mozc.init.el` 側で `warning-minimum-level` や `byte-compile-warnings` の一時変更による抑制も試したが、起動時ログの `mozc.el` 警告には実効が薄いことを確認した。
- なぜそうしたか: まず設定側で安全に抑制できるかを見極め、システムファイル改変の必要性を判断するため。
- 未完了の事項: `inits/mozc.init.el` に警告抑制の試行コードが残っており、現状の解決手段（`.elc` 生成）には必須ではない。
- 次にやるとよいこと: `mozc.init.el` の抑制コードは、不要なら通常の `require` へ戻して簡素化する。

- 何をしたか: ユーザー実施の `sudo emacs --batch ... (byte-compile-file \"/usr/share/emacs/site-lisp/emacs-mozc/mozc.el\")` により、`mozc.el` の起動時 warning が消えることを確認した。
- なぜそうしたか: システム側 `mozc.el` がソース読込されることで出ていた obsolete 警告を、事前コンパイルで抑えるため。
- 未完了の事項: パッケージ更新時に `mozc.elc` が消える・再生成が必要になる可能性は残る（更新頻度は低い見込み）。
- 次にやるとよいこと: 更新後に警告が再発した場合は同じコマンドで再生成し、異常時は `mozc.elc` 削除でロールバックする。

- 何をしたか: `inits/` 全体を静的レビューし、バグ・無効コード・不安定要因を優先度付きで抽出した。重点確認は `modes.init.el`、`search-center.el`、`window.init.el`、`util.init.el`、`anything.init.el`。
- なぜそうしたか: 自作 elisp 改善を進めるにあたり、まず「起動停止や不安定化に直結する箇所」を先に特定して、修正順序を明確化するため。
- 主な指摘:
  - `modes.init.el` の `php-mode-hook` 内で `c-special-indent-hook` へ `add-hook` しており、ローカル指定なしのためバッファを開くたびに重複登録されうる。
  - `modes.init.el` の `unindent-closure` が `delete-char` で固定量を削除しており、条件次第で意図しない削除が起きる余地がある。
  - `search-center.el` で `sc/search` 実行中に `sc/move-region` を `defun` しており、都度のグローバル再定義で保守性が下がる。
  - `modes.init.el` / `window.init.el` / `util.init.el` / `anything.init.el` の `require` が強制で、依存欠落時に起動失敗しやすい（環境差に弱い）。
  - `window.init.el` の `0.03` 秒 idle timer 更新は常時負荷とチラつき要因になりうる。
  - `util.init.el` の `path:point` 保存形式は `:` を含むパス（例: TRAMP）で復元時に壊れうる。
- 未完了の事項: まだコード修正は未着手。レビュー結果のうち、どこまで互換性維持で直すか（特に `search-center` と `window` の挙動変更範囲）は未決定。
- 次にやるとよいこと: まず `modes.init.el` の hook 増殖と `unindent-closure` の安全化を先行し、次に依存 `require` を `require ... nil t` 系へ段階的に切り替える。

- 何をしたか: `modes.init.el` の最優先2件を修正した。`php-mode-hook` 内の `c-special-indent-hook` 追加をバッファローカル化（`add-hook ... nil t`）し、`unindent-closure` を「行頭空白のみ削除」する実装へ変更した。
- なぜそうしたか: `php-mode` バッファを開くたびの hook 増殖を防ぎ、`delete-char` による想定外削除（非空白文字や行境界の破壊）リスクを下げるため。
- 未完了の事項: `unindent-closure` の実挙動を実際の PHP closure 例で手動確認（インデント期待値との一致）は未実施。
- 次にやるとよいこと: 依存 `require` の段階的ソフト化（`php-mode` / `cursor-chg` / `google-translate` から）を進める。

- 何をしたか: 依存 `require` の段階的ソフト化を3箇所に適用した。`window.init.el` の `cursor-chg`、`util.init.el` の `google-translate`、`modes.init.el` の `php-mode` を `require ... nil t` 化し、未導入時は起動を止めないようにした。あわせて `google-translate` 実行時は未導入なら `user-error` を返すようにした。
- なぜそうしたか: パッケージ未導入・一時欠落・環境差がある場合でも Emacs の起動自体は継続させ、依存機能のみを限定的に無効化するため。
- 未完了の事項: `php-mode` 未導入時は `.php` の `auto-mode-alist` 追加を行わないため、代替モード（`php-ts-mode` など）への自動フォールバックは未実装。
- 次にやるとよいこと: 必要なら `.php` を `php-ts-mode` へフォールバックする条件分岐を追加し、未導入時の編集体験を維持する。

- 何をしたか: `search-center.el` の `sc/search-replace` 内で実行時にグローバル `defun` されていた `sc/move-region` を、`cl-labels` のローカル関数 `move-region` に置換した。あわせて `cl-lib` の `require` を追加した。
- なぜそうしたか: 検索実行のたびにグローバル関数を再定義する状態をやめ、挙動を保ったまま保守性と予測可能性を上げるため。
- 未完了の事項: `sc/search-replace` のローカル変数に未使用のものが残っており、byte-compile warning はまだ一部残る。
- 次にやるとよいこと: 未使用ローカル変数の整理（`beg` / `end` / `target-str` 等）を行って警告ノイズを減らす。

- 何をしたか: `search-center.el` の `foreign-regexp` 判定を `package-installed-p` ベースから `require ... nil t` ベースへ変更し、古い `search-center.elc` を再コンパイルして反映した。
- なぜそうしたか: パッケージ情報だけ存在して実体が無い環境で `require 'foreign-regexp` が失敗し、起動停止する不安定要因を避けるため。
- 未完了の事項: バッチ起動では次の停止要因として `editing.init.el` の `multiple-cursors` 強制 `require` が残っている。
- 次にやるとよいこと: `editing.init.el` の `multiple-cursors` も `require ... nil t` 化し、関連キーバインド定義を導入時だけ有効化する。

- 何をしたか: `editing.init.el` の `multiple-cursors` / `smartrep` をソフト `require` 化し、関連キーバインドは導入済み時のみ設定するように変更した。あわせて `web-beautify` もソフト `require` 化した。
- なぜそうしたか: 依存パッケージ欠落時に `editing.init.el` 読込で起動停止するのを防ぎ、機能単位で劣化運用できるようにするため。
- 未完了の事項: `multiple-cursors` 未導入時は該当キーバインド（`C-M-c` / `C-M-r` / `C-t` smartrep 系）は無効のまま。
- 次にやるとよいこと: 必要なら未導入時だけ一度通知する仕組みを追加する。

- 何をしたか: `filer.init.el` の `zlc` と `dired-async` をソフト `require` 化した。
- なぜそうしたか: 補助パッケージの未導入で Dired 初期化時に落ちないようにするため。
- 未完了の事項: `zlc` 未導入時はミニバッファ補完の操作感が変わる。
- 次にやるとよいこと: 必要なら標準の `fido-mode` / `icomplete` と併用する代替設定を検討する。

- 何をしたか: `anything.init.el` を `anything` 利用可能時のみ設定を実行する構造に変更し、`init.el` 側は任意モジュール読込用の `my/require-optional` を追加して `anything.init` / `ace-jump-mode.init` / `auto-complete.init` / `gtags.init` の読込を安全化した。
- なぜそうしたか: 任意パッケージ欠落で1モジュールが失敗しても Emacs 全体の起動を継続させるため。
- 未完了の事項: 任意モジュール内の実装不具合も `my/require-optional` で起動継続できる反面、見逃される可能性がある。
- 次にやるとよいこと: 開発時は `my-dev-mode-on` で必須モジュールだけ読み込む運用に加え、任意モジュールは個別にロードテストして問題を早期検知する。

- 何をしたか: `window.init.el` の `split-window` 用 `defadvice` を `advice-add` へ置換し、能動的な分割コマンド（`split-window-vertically` / `split-window-horizontally`）時に新規ウィンドウへフォーカスする実装へ更新した。さらに `hl-line` の `0.03` 秒 idle timer を廃止し、標準の `global-hl-line-mode` 有効化へ切り替えた。
- なぜそうしたか: 旧 advice 機構依存と常時ポーリングを減らし、将来互換性と安定性を上げるため。
- 未完了の事項: `global-hl-line` の見え方が従来の timer 駆動と微妙に異なる可能性はある。
- 次にやるとよいこと: 実運用でハイライトの追従感に違和感がないか軽く確認する。

- 何をしたか: `editing.init.el` の `indent-for-tab-command` / `align-regexp` に対する `defadvice` を `advice-add` の around 関数へ置換した。
- なぜそうしたか: 旧 `defadvice` 依存を減らし、将来の Emacs で壊れにくい実装へ寄せるため。
- 未完了の事項: 既存ロジックは維持しているため、挙動差は最小だが、端ケース（リージョン端の再配置など）は未実機確認。
- 次にやるとよいこと: `indent-for-tab-command` と `align-regexp` を普段の操作で1回ずつ実行し、期待どおりかを確認する。

- 何をしたか: `util.init.el` の「終了時バッファ履歴」保存形式を `path:point` テキストから Lisp データ（`((PATH . POINT) ...)`）へ変更し、読込側は新形式優先・旧形式フォールバック（後方互換）にした。保存前に履歴ディレクトリが無ければ作成する処理も追加した。
- なぜそうしたか: `:` を含むパス（TRAMP 等）で復元が壊れる問題を避けるため。
- 未完了の事項: 旧形式データを一度新形式で保存し直すまでは、起動時にフォールバック経路を通る。
- 次にやるとよいこと: 一度 Emacs を通常終了して `~/.emacs.d/histories/last-files` が Lisp 形式で更新されることを確認する。

- 何をしたか: `elisp/` 配下をレビューし、優先修正として `dired-explorer.el` の `shell-command-to-string` を `process-lines` ベースに置換した。`osascript` 呼び出しは引数分離（`on run argv`）へ変更し、パスの直接埋め込みをやめた。
- なぜそうしたか: パス文字列をシェル文字列連結していたため、クォート崩れやコマンド注入余地があったため。
- 未完了の事項: macOS 専用分岐のため、Darwin 実機で alias 解決が従来通りかは未確認。
- 次にやるとよいこと: macOS 環境で alias ファイルに対して `dired-mac-alias-path` が正しく実パスを返すか確認する。

- 何をしたか: `elisp/focus-on-editable-buffers/focus-on-editable-buffers.el` の `delete-window` 用 `defadvice` を `advice-add :after` へ置換した。
- なぜそうしたか: 旧 advice 機構依存を減らし、将来の Emacs 互換性を上げるため。
- 未完了の事項: `foeb/is-use-advice-delete-window` が `nil` のときは advice を追加しない既存仕様を維持している。
- 次にやるとよいこと: `foeb/is-use-advice-delete-window` を `t` にした運用で、ウィンドウ削除後の遷移先が期待通りか確認する。

- 何をしたか: `elisp/mozc-isearch.el` の `defadvice` 4箇所（`mozc-handle-event` / `mozc-send-key-event` / `mozc-mode` / `isearch-process-search-multibyte-characters`）を `advice-add` へ置換した。
- なぜそうしたか: 旧 advice 機構依存を解消し、同等挙動を保ったまま保守性と将来互換性を上げるため。
- 未完了の事項: 実機での日本語 isearch（mozc preedit を伴う入力）に対する手動回帰確認は未実施。
- 次にやるとよいこと: `isearch` 中に mozc で入力し、確定・削除・IME ON/OFF が従来通り動くかを確認する。

- 何をしたか: `mozc-isearch` は実運用で効いていない認識に合わせ、`inits/mozc.init.el` から `require 'mozc-isearch` と `mozc-isearch-setup` 呼び出しを削除した。
- なぜそうしたか: 効いていない補助レイヤーを外して isearch まわりの挙動を単純化し、切り分けしやすくするため。
- 未完了の事項: `elisp/mozc-isearch.el` ファイル自体は残っているが、設定上は未使用になった。
- 次にやるとよいこと: 必要なければ `elisp/mozc-isearch.el` の管理方針（残置/退避/削除）を決める。

- 何をしたか: `dired-explorer` のキーバインド挙動を確認し、現状は「modifier + alphabet」ではなく、`dired-explorer-mode` 有効時に素の `a-z0-9` が `dired-explorer-isearch` に割り当てられていることを確認した。加えて `inits/filer.init.el` で `dired-mode-hook` 時に `dired-explorer-mode` が自動有効化されていることを確認した。
- なぜそうしたか: 「デフォルトでは無害（通常キーを奪わない）」方針との整合を次回修正で取るため、現状の事実を明確化しておくため。
- 未完了の事項: キーバインドの方針変更（modifier 必須化、あるいは自動有効化の撤廃）は未着手。
- 次にやるとよいこと: 次回は `dired-explorer` 側を `M-a..` など modifier 前提へ変更するか、`filer.init.el` 側の自動有効化をやめて `:` で手動有効化するかを決めて実装する。

- 何をしたか: `muhenkan`（ENG）キー不安定の切り分けを行い、グローバル束縛が `my-confirm-and-deactivate-input-method` になっていることを確認した上で、`inits/mozc.init.el` の `<muhenkan>` 束縛を `deactivate-input-method` へ変更した（global / `mozc-mode-map` / `anything-map`）。
- なぜそうしたか: 独自関数経由での `mozc-handle-event` 呼び出しが状態依存で失敗しうるため、キー動作を単純化して安定させるため。
- 未完了の事項: `deactivate-input-method` はこの環境で `commandp` が `nil` のため、キーに直接割り当てると `Wrong type argument: commandp` が出ることが判明。
- 次にやるとよいこと: `interactive` ラッパーを介して `muhenkan` を再設定する。

- 何をしたか: `inits/mozc.init.el` に `my/deactivate-input-method-command`（`interactive` ラッパー）を追加し、`<muhenkan>` の global / `mozc-mode-map` / `anything-map` を同コマンドへ切り替えた。確認として `commandp` が `t`、キー束縛が同コマンドになっていることを確認した。
- なぜそうしたか: この環境では `deactivate-input-method` が関数としては存在するがコマンドではないため、キー実行時に落ちる問題を回避するため。
- 未完了の事項: 実GUI環境での体感確認（通常入力時・isearch時の `muhenkan`）は未実施。
- 次にやるとよいこと: 実運用で `muhenkan` が安定して英数モードへ戻るかを確認し、まだ不安定なら `henkan/muhenkan` のイベント名差分（`eisu-toggle` 等）を追加で拾う。

- 何をしたか: `elisp/dired-explorer/dired-explorer.el` をリファクタし、`lexical-binding` 有効化、検索トリガーキーの `defcustom` 化（`dired-explorer-isearch-trigger-keys`）、判定ヘルパー追加（`dired-explorer--trigger-char-p`）、`dired-explorer-do-isearch` の変数名整理と安全化（`regx` 未設定時の next/prev 抑止、`unread-command-events` は `cons` で追加）を行った。あわせて `dired-mac-alias-path` のカッコ不整合を修正し、`batch-byte-compile` が通ることを確認した。
- なぜそうしたか: 挙動を変えずに可読性と保守性を上げ、手修正時に混入した構文崩れを取り除いて、将来の調整（modifier 前提キー化など）を安全に進められる土台にするため。
- 未完了の事項: トリガーキーを runtime で変更した場合に既存キー定義を再生成する API は未実装。
- 次にやるとよいこと: 必要なら `dired-explorer-refresh-keymap` のような再適用関数を追加し、`customize` 変更を即時反映できるようにする。

- 何をしたか: `elisp/dired-explorer/dired-explorer.el` を追加リファクタし、(1) `dired-explorer-isearch-trigger-keys` の `defcustom` に `:set` を追加してキー変更時にキーマップを再生成するようにした、(2) 未使用だった `defvar`（`dired-explorer-mode` / `dired-explorer-mode-hook` / `dired-mode-old-local-map` / `dired-explorer-isearch-returnkey` / `dired-explorer-isearch-word`）を整理した、(3) isearch の直前入力状態をグローバル変数ではなくローカル変数 `last-word` で扱うようにした。
- なぜそうしたか: カスタマイズ反映の即時性を上げ、不要なグローバル状態を減らして副作用リスクを下げるため。
- 未完了の事項: `defcustom` の `:group` は現状 `dired` のままで、専用 `defgroup dired-explorer` は未作成。
- 次にやるとよいこと: 設定項目が増える見込みがある場合は `defgroup dired-explorer` を作り、`M-x customize-group` からの発見性を上げる。

- 何をしたか: `elisp/dired-explorer/dired-explorer.el` の文字コードを `EUC-JP` から `UTF-8` に変換し、1行目ヘッダへ `coding: utf-8` を明示した。
- なぜそうしたか: MELPA 上の description/commentary 表示で日本語が文字化けしていたため。
- 未完了の事項: MELPA 側の表示反映待ち（定期ビルド更新待ち）。
- 次にやるとよいこと: この変更を commit/push して、MELPA 更新後に https://melpa.org/#/dired-explorer で表示を再確認する。

- 何をしたか: `elisp/focus-on-editable-buffers` 配下を改善し、`focus-on-editable-buffers.el` と `anything-focus-on-editable-buffers.el` に `lexical-binding` ヘッダと `tested on Emacs 30.2` を追加。`focus-on-editable-buffers.el` は `add-to-list` のローカル変数利用を `push` + `nreverse` に置換し、`delete-window` advice 関数をトップレベル定義に整理。`anything-focus-on-editable-buffers.el` は `require 'anything` をソフト化し、未導入時は `user-error` を返すようにした。
- なぜそうしたか: `anything` 未導入環境でのロード失敗を防ぎつつ、古いヘッダ情報と byte-compile warning を減らすため。
- 未完了の事項: `anything` 系はパッケージ自体が古く、将来的には `helm`/`consult` 系への移行判断が必要。
- 次にやるとよいこと: 必要なら `anything-focus-on-editable-buffers.el` を optional モジュールとして明示し、通常起動経路からは切り離す。

- 何をしたか: `init.el` と `inits/util.init.el` を修正し、起動時の最後のバッファ復元で TRAMP 先が不達でも起動が止まらないようにした。具体的には `my-hist-restore-remote-files`（既定 nil）を追加し、リモート履歴は既定でスキップ、ローカル/リモートとも復元エラーは `condition-case` で握ってメッセージのみ出すようにした。あわせて `init.el` の `byte-compile-warnings` 全抑制（nil）を削除して、重要 warning の可視性を戻した。
- なぜそうしたか: 実際に init ロード時に TRAMP 接続失敗で起動が停止するケースがあり、復元処理は失敗しても起動継続すべきため。また warning 全抑制は不具合発見を遅らせるため。
- 未完了の事項: `my-hist-restore-remote-files` を `t` にした場合は接続状況に応じて起動遅延が再発する可能性がある。
- 次にやるとよいこと: リモート履歴復元を使いたい場合は「起動完了後の idle timer で非同期に復元」へ移行すると、起動体験を維持しやすい。

- 何をしたか: `init.el` に warning 運用を追加し、`byte-compile-warnings nil` の全面抑制ではなく `warning-suppress-types` で `bytecomp` のみ抑制する設定へ変更した。あわせて `my/byte-compile-user-elisp` を追加し、`~/.emacs.d/inits` と `~/.emacs.d/elisp` だけを再コンパイルできるようにした。
- なぜそうしたか: `anything` / `foreign-regexp` など第三者パッケージ由来の膨大な bytecomp 警告を日常起動から切り離しつつ、自作elispの品質確認は継続するため。
- 未完了の事項: 第三者パッケージの旧マクロ (`case` / `labels`) 自体は未修正のまま。
- 次にやるとよいこと: 必要時だけ `M-x my/byte-compile-user-elisp` を実行し、自作elispの warning を定期点検する。

- 何をしたか: `inits/fun-startup.init.el` を新規追加し、起動時 `*Messages*` に `Package cl is deprecated` が含まれる場合のみ、ランダムな英語メッセージを1つ表示する処理を追加した。`init.el` から `require 'fun-startup.init` で読み込むようにした。
- なぜそうしたか: 旧依存パッケージ由来の `cl` 警告は当面残るため、起動時の認知負荷を下げるため。
- 未完了の事項: `cl` deprecated warning 自体は抑制しておらず、表示文言を上書きして体感を改善する対応。
- 次にやるとよいこと: 必要ならメッセージ配列 `my/fun-startup-messages` を好みで追加・差し替えする。

- 何をしたか: `elisp/dired-explorer` をリリース整理し、`Version` を `0.7` に更新、`Change Log` に 0.7 項目を追記、ヘッダの `Package-Requires` を `((emacs "24.3"))` へ更新、互換表記を `tested on Emacs 30.2` に変更した。あわせてコメント英語を読みやすく整えた。`0.7` タグを作成して `origin` へ push した。
- なぜそうしたか: MELPA 掲載情報の整合性と可読性を上げ、今回のリファクタ結果を正式リリースとして固定するため。
- 未完了の事項: MELPA 側の反映は定期ビルド待ちでタイムラグがある。
- 次にやるとよいこと: MELPA 反映後に `https://melpa.org/#/dired-explorer` で表示（version/commentary）を確認する。

- 何をしたか: `elisp/focus-on-editable-buffers` 側で、ユーザー更新の `Version: 0.2` を含む修正をコミット・push した（Anything 依存の optional 化、ヘッダ更新、lexical-binding 追加、warning 低減を含む）。
- なぜそうしたか: 既存ワークフローを維持しながら、Anything 未導入環境で壊れない実装に寄せるため。
- 未完了の事項: `anything-focus-on-editable-buffers.el` の version ヘッダは `0.1` のまま。
- 次にやるとよいこと: 必要ならサブモジュール側 version も `0.2` へ揃える。

- 何をしたか: `inits/keyboard.init.el` に `my/revert-buffer-safe` を追加し、`<f5>` へ割り当てた。未保存変更があるときのみ確認し、実際にリロードしたときはミニバッファへ `Reverted buffer: ...` を表示するようにした。
- なぜそうしたか: `revert-buffer` の利用頻度増加に対して、安全性（誤破棄防止）と操作フィードバックを両立するため。
- 未完了の事項: なし。
- 次にやるとよいこと: 必要なら `revert-buffer-with-coding-system` など派生操作も同じキー運用に統一する。

- 何をしたか: `inits/mozc.init.el` にマウスイベントの明示バインドを追加し、`mozc-mode` 有効時でも `down/drag/mouse-1` と `double/triple click` の通常選択挙動を維持するようにした。実機で「全角入力有効中の単語ダブルクリック選択」が復旧したことを確認した。
- なぜそうしたか: Mozc 有効時だけマウス選択系の体感が劣化していたため。
- 未完了の事項: 他のボタン（middle/right）や環境固有マウスイベントまでは未調整。
- 次にやるとよいこと: 必要なら `mouse-2` / `mouse-3` 系の期待挙動も同様に明示化する。

- 何をしたか: `inits/editing.init.el` に `(setq-default indent-tabs-mode nil)` を追加して既定のインデントをスペースに変更し、`inits/modes.init.el` の `js-mode-hook` でも `(setq indent-tabs-mode nil)` へ変更して JS を含めてスペース寄せに統一した。
- なぜそうしたか: 既定をスペースにしたい要望があり、加えてモダンな JS の実運用（Prettier 既定など）でもスペースが主流なため。
- 未完了の事項: 既存バッファには自動反映されないため、必要ならバッファ再オープンまたは mode 再起動が必要。
- 次にやるとよいこと: プロジェクト単位でタブ運用が必要な場合は `.editorconfig` か `dir-locals.el` で局所上書きする。
