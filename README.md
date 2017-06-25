# ELSE
Emacs Language Sensitive Editor
ELSE is a minor mode for Emacs, designed to work with any major mode. The aim is to provide a seamless mechanism to generate language constructs easily and intuitively with a minimum of keystrokes and interference with the user. It's primary application is a minor mode for any programming language (major mode) but it can be used for any editing task that involves generation of repetitive text/blocks of text e.g. there is a template file for Texinfo mode. All that is required to use ELSE with a major mode is a template file defining the textual constructs that define typical textual constructs used by that mode i.e. when creating a node in a Texinfo file, it would be advantageous to generate the node entry as well as a choice to the user for a chapter/section/subsection heading - both at the same time. A template entry that would achieve this (for a chapter node) is:

```
DEFINE PLACEHOLDER CHAPTER-NODE
    /LANGUAGE="Texinfo"
    /NOAUTO_SUBSTITUTE
    /DESCRIPTION="Generate a chapter node"
    /DUPLICATION=CONTEXT_DEPENDENT
    /SEPARATOR=""
    /TYPE=NONTERMINAL
    
    "\@node {node-text},,"
    "\@chapter {node-text}"
    "[cindex]"
    "{text}"

END DEFINE
```

To generate a chapter node entry in the buffer, the user could type "chap<C-x / e>" (note: this is a key sequence mandated by the Emacs guide for new commands, I have the command bound to F3 for single key-press) and "point" would be (automatically) positioned within the "placeholder" {node-text}. The result would be:

```
@node {node-text},,
@chapter {node-text}
[cindex]
{text}
```

The user could then just start typing the text for the node name - ELSE will automatically delete the "{node-name}" text and replace it with the typed text. Note: as the user is typing in the first "node-text" placeholder, the same text is being repeated in the "node-text" placeholder in the chapter line. For example, if the user typed "First chapter" then the text would look like this:

```
@node First chapter,,
@chapter First chapter
[cindex]
{text}
```

The user could then navigate (else-next - C-x / n) to the next placeholder ([cindex]) and choose to either "expand" it (if they desired an content index) using the command else-expand (C-x / e) or delete it using else-kill (C-x / k). If the placeholder is deleted, then ELSE will delete the entire placeholder text, notice the line is blank and then delete the blank line i.e. clean-up after itself.

Placeholders are "persistent" over edit session and visible - probably the biggest advantage to using ELSE over other template systems, which use invisible markers that disappear at the most inappropriate times or just aren't there at all when the edit session is restarted.
