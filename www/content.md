RTF mark-up resources
---------------------

- [Rich Text Format (RTF) Version 1.5 Specification](http://www.biblioscape.com/rtf15_spec.htm)

- [Try pandoc!](https://pandoc.org/try/) for seeing how [Pandoc](https://pandoc.org/) outputs RTF from other formats

- [Wikipedia's Rich Text Format](https://en.wikipedia.org/wiki/Rich_Text_Format) page 
  has background information and links to various RTF-related tools


Frequently asked questions
--------------------------

### Why would I use `rtfSweave`?

Most people would rather use Rmarkdown but if you (a) have Microsoft
Word, (b) you need to generate a Word document, (c) would like a lot
of control over the document formatting, and (d) do not mind fiddling
a little with RTF, then maybe this is a reason to use `rtfSweave`.

The trade-offs include the following:

- RTF markup is not clean and elegant like Markdown

- There is only one output format available: RTF. You don't get
  multiple formats "for free" as with Rmarkdown

<!-- ### What do you mean "a lot of control over the document formatting"? -->

<!-- Sometimes it is nice to specify where a pagebreak goes (use `\page`), -->

### Should I save RTF file to a native Word format like `*.docx`?

If you have Microsoft Word, yes. It is important to save the RTF file
as `*.docx` (or `*.doc`) _before_ making any edits to the RTF
file. The reason is that if you edit the RTF and save the file (as
RTF) the file size balloons (often to 100 megabytes or more) because
Word is inserting an enormous number of its own RTF commands. The easy
way to avoid this is to save the RTF file as `*.docx` and _then_ edit
it.

### What's the worst part about using RTF?

It's very easy to have unbalanced curly brackets in the RTF file and
this usually means the file cannot be opened. And sometimes it's hard
to find the problem.

### Why is some of my RTF file not showing up?

An RTF file opens with `{\rtf1` and ends as soon as that opening curly
bracket is closed with a `}`. If there is content after that closing curly bracket
it won't show up. 





