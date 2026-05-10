#import "@preview/numbly:0.1.0": numbly
#import "@preview/tablem:0.3.0": tablem, three-line-table
#import "@preview/mitex:0.2.7": *
#import "@preview/cmarker:0.1.8": render as cmarker-render
#import "@preview/theorion:0.6.0": *
#import cosmos.fancy: *
#let md = cmarker-render.with(math: mitex)

#let default-font = (
  main: "Sabon LT Pro",
  mono: "Maple Mono NF",
  cjk: "Noto Serif CJK SC",
  emph-cjk: "KaiTi",
  math: "New Computer Modern Math",
  math-cjk: "Noto Serif CJK SC",
  heading-cjk: "Noto Sans CJK SC",
)

// Definitions for math
#let prox = math.op("prox")
#let proj = math.op("proj")
#let argmax = math.op("argmax", limits: true)
#let argmin = math.op("argmin", limits: true)


/// 模板的核心类，规范了文档的格式。
/// - media (string): 媒体类型，可选值为 `"screen"` 和 `"print"`。默认为 `"print"`。
/// - size (length): 字体大小。默认为 `11pt`。
/// - screen-size (length): 屏幕字体大小。默认为 `11pt`。
/// - title (string): 文档的标题。
/// - subtitle (string): 文档的副标题。
/// - author (string): 作者。
/// - subject (string): 课程名。
/// - date (datetime): 时间。
/// - font (object): 字体。默认为 `default-font`。如果你想使用不同的字体，可以传入一个字典，包含 `main`、`mono`、`cjk`、`emph-cjk`、`math`、`math-cjk` 和 `heading-cjk` 字段。
/// - lang (string): 语言。默认为 `zh`。
/// - region (string): 地区。默认为 `cn`。
/// -> content
#let ori(
  media: "print",
  size: 12pt,
  screen-size: 12pt,
  title: none,
  subtitle: none,
  author: none,
  subject: none,
  date: none,
  font: default-font,
  lang: "zh",
  region: "cn",
  first-line-indent: (amount: 0pt, all: false),
  maketitle: true,
  makeoutline: false,
  outline-depth: 2,
  body,
) = {
  assert(media == "screen" or media == "print", message: "media must be 'screen' or 'print'")

  let page-margin = if media == "screen" { (x: 35pt, y: 35pt) } else { auto }
  let text-size = if media == "screen" { screen-size } else { size }

  let font = default-font + font

  /// 设置字体。
  set text(
    font: ((name: font.main, covers: "latin-in-cjk"), font.cjk),
    size: text-size,
    lang: lang,
    region: region,
  )
  show smartquote: set text(font: font.main)
  show emph: set text(font: ((name: font.main, covers: "latin-in-cjk"), font.emph-cjk))
  show raw: set text(font: ((name: font.mono, covers: "latin-in-cjk"), font.cjk))
  show math.equation: it => {
    set text(font: font.math)
    show regex("\p{script=Han}"): set text(font: font.math-cjk)
    it
  }

  /// 设置段落样式。
  set par(
    justify: true,
    first-line-indent: if first-line-indent == auto {
      (amount: 2em, all: true)
    } else {
      first-line-indent
    },
  )

  /// 设置标题样式。
  set heading(numbering: "1.1 ")
  show heading: it => {
    show h.where(amount: 0.3em): none
    it
  }
  show heading: set block(spacing: 1.2em)
  show heading: set text(
    font: ((name: font.main, covers: "latin-in-cjk"), font.heading-cjk),
  )

  /// 设置代码块样式。
  show raw.where(block: false): body => box(
    fill: rgb("#f0f0f0"),
    inset: (x: 3pt, y: 0pt),
    outset: (x: 0pt, y: 3pt),
    radius: 2pt,
    {
      set par(justify: false)
      body
    },
  )
  show raw.where(block: true): body => block(
    width: 100%,
    fill: rgb("#f0f0f0"),
    outset: (x: 0pt, y: 4pt),
    inset: (x: 8pt, y: 4pt),
    radius: 4pt,
    {
      set par(justify: false)
      body
    },
  )

  /// 设置链接样式。
  show link: it => {
    if type(it.dest) == str {
      set text(fill: blue)
      it
    } else {
      it
    }
  }

  /// 设置 figure 样式。
  show figure: set block(breakable: true)
  show figure.caption: set text(size: 0.9em, weight: "bold")
  show figure.where(kind: table): set figure.caption(position: top)

  /// 设置列表样式。
  set list(indent: 6pt)
  set enum(indent: 6pt)
  set enum(numbering: numbly("{1:1}.", "{2:1})", "{3:a}."), full: true)

  /// 设置引用样式。
  set bibliography(style: "ieee")

  /// 基础设置。
  set document(title: title, author: if type(author) == str { author } else { () }, date: date)

  /// 重置页面计数器。
  counter(page).update(1)

  /// 设置页面。
  set page(
    header: {
      set text(0.9em)
      stack(
        spacing: 0.2em,
        grid(
          columns: (1fr, auto, 1fr),
          align(left, author), align(center, title), align(right, subject),
        ),
        v(0.3em),
        line(length: 100%, stroke: 1pt),
        v(.15em),
        line(length: 100%, stroke: .5pt),
      )
      // reset footnote counter
      counter(footnote).update(0)
    },
    numbering: "1",
    margin: page-margin,
  )

  /// 标题
  if maketitle {
    align(center + top)[
      // 标题
      #text(
        size: 1.7em,
        font: ((name: font.main, covers: "latin-in-cjk"), font.heading-cjk),
        weight: "bold",
        title,
      )
      #v(1em, weak: true)

      // 副标题
      #align(
        right + top,
        text(
          size: 1.4em,
          font: ((name: font.main, covers: "latin-in-cjk"), font.heading-cjk),
          weight: "bold",
          subtitle,
        ),
      )
      #v(1em, weak: true)

      // 作者
      #text(
        size: 1.25em,
        font: ((name: font.main, covers: "latin-in-cjk"), font.heading-cjk),
        weight: "bold",
        author,
      )
    ]
  }

  /// 目录。
  if makeoutline {
    show heading: align.with(center)
    show outline.entry: set block(spacing: 1.2em)

    outline(depth: outline-depth, indent: 2em)
  }

  /// 设置定理环境。
  show: show-theorion

  body
}
