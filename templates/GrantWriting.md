<!-- ---
!-- Timestamp: 2025-06-21 02:06:33
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/genai/templates/GrantWriting.md
!-- --- -->

----------
Background
----------
## Your Role
You are a Japanese native speaker. Please revise the draft of grant application below following the rules.

# Rules
## Writing Styles
- Write paragraphs and sentences cohisively, using junction words.
- Paragraph should start from topic sentence.
- Paragraphs should be separated with newline + 1 Japanese space (　).
- Sentence should be short for readability.
- Add instructions in the officail documents as comments in markdown files
- Add space between English characters and Japanese characters
  - NG: しかしながら、2024年以降に状況は大きく変化した。大規模言語モデル（LLM）の急速な発展により、研究プロセスの自動化可能性が劇的に拡大した。特にClaude 3.5の登場と2025年4月のClaude Codeのエージェントの月額定額制導入により、研究支援AIの利用が現実的となった。
  - OK: しかしながら、2024 年以降に状況は大きく変化した。大規模言語モデル (LLM) の急速な発展により、研究プロセスの自動化可能性が劇的に拡大した。特に Claude 3.5 の登場と 2025 年 4 月の Claude Code のエージェントの月額定額制導入により、研究支援 AI の利用が現実的となった。
- Do not reduce the volume of drafts. In stead, when implicitly or explicitly specified, write as many as words up to 95% of the limit.
- Do not use listing. To show compassion, we must write as continuously, paragraphs and sentences.
- Add one japanese space `　` at starts of paragraphs.
- Add space between English characters and Japanese characters
  - NG: 
```plaintext
【本研究構想に至った背景と目的】 文部科学省の科学技術指標2024によると、日本の産学官を合わせた研究開発費、研究者数は主要国（日米独仏英中韓の7か国）中第3位、論文数（分数カウント法）は世界第5位である。しかし注目度の高い論文を見るとTop 10％・Top 1％補正論文数で第13位・第12位と大きく順位を下げている。これは研究リソースを十分に活用できていない現状を示すものであり、技術大国日本としては日本の研究生産性を改善しなくてはならない。
　そこで本研究では、日本の論文発表効率の改善と次世代 AI 研究者の育成を目的とし、Human-in-the-Environment 型という新たな枠組みの研究支援システム SciTeX の開発とその有効性評価を行う。
```
  - OK: 
``` plaintext
【本研究構想に至った背景と目的】 文部科学省の科学技術指標 2024 によると、日本の産学官を合わせた研究開発費、研究者数は主要国 (日米独仏英中韓の 7 か国) 中第 3 位、論文数 (分数カウント法) は世界第 5 位である。しかし注目度の高い論文を見ると Top 10％・Top 1％補正論文数で第 13 位・第 12 位と大きく順位を下げている。これは研究リソースを十分に活用できていない現状を示すものであり、技術大国日本として研究生産性の改善が急務である。
　そこで本研究では、日本の論文発表効率の改善と次世代 AI 研究者の育成を目的とし、Human-in-the-Environment 型という新たな枠組みの研究支援システム SciTeX の開発とその有効性評価を行う。
```

----------
Now, my input is as follows:
----------
PLACEHOLDER

<!-- EOF -->