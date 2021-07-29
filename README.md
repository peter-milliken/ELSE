# ELSE
Emacs Language Sensitive Editor

Emacs 26.1: Version 26.1 of Emacs does not write readable Lisp Objects, so the fast load file does not work as described in the manual. Bug-02 fixes this issue by causing ELSE to catch the error thrown by Emacs on the Lisp Object read failure and re-directs ELSE to compile the templates from the original source files instead. This is all transparent to the user.

ELSE now works with ivy and the Emacs built-in completing-read functions for menu display and selection. The default is (still) to use the popup package, but users can use ivy and completing-read (respectively) by including either of the following lines AFTER the "(require 'else-mode)" statement in their config file:
ivy: (require 'else-ivy)
completing-read: (require 'else-completing-read)

If there is some other completion package the use would like to use, they need to write a menu interface function (see else-default-display-menu in else-mode.el or else-display-ivy-menu in else-ivy.el for examples) and there is a custom variable (else-alternate-menu-picker) which can be set to redirect ELSE to the function to use to prepare and display the menu of seletions.

This is ELSE v2 and works with Emacs 25+. ELSE v1 was developed and used under Emacs 18/19 - 24 but with the advent of Emacs 25 it became 'broken' and was certainly showing its age. This version of ELSE is a complete rewrite and, whilst it is functionally equivalent to v1, it contains significent changes to the template files that were used in v1. The single biggest change is that TOKENs no longer exist i.e. everything is now defined as a PLACEHOLDER and ELSE now treats abbreviated text like it used to treat TOKENs i.e. f, fo and for can now all be 'expanded' to have a for-loop template placed in the buffer at point. Depending on the number of characters being expanded, the user may be presented with a popup menu to accept a possible completion.

ELSE is a minor mode for Emacs, designed to work with any major mode. The aim is to provide a seamless mechanism to generate language constructs easily and intuitively with a minimum of keystrokes and interference with the user. It's primary application is a minor mode for any programming language (major mode) but it can be used for any editing task that involves generation of repetitive text/blocks of text e.g. there is a template file for Texinfo mode. 

All that is required to use ELSE with a major mode is a template file defining the textual constructs that define typical textual constructs used by that mode i.e. when creating a node in a Texinfo file, it would be advantageous to generate the node entry as well as a choice to the user for a chapter/section/subsection heading - both at the same time. A template entry that would achieve this (for a chapter node) is:

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

To generate a chapter node entry in the buffer, the user could type "chapC-x / e" (note: this is a key sequence mandated by the Emacs guide for new commands, I have the command bound to F3 for single key-press) and "point" would be (automatically) positioned within the "placeholder" {node-text}. The result would be:

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

Just to provide a further "flavour" of what a coding construct might look like, here is the placeholder definition for the Python "with" statement:
```
DEFINE PLACEHOLDER WITH_STMT 
    /LANGUAGE="Python" 
    /NOAUTO_SUBSTITUTE 
    /DESCRIPTION=""
    /DUPLICATION=CONTEXT_DEPENDENT 
    /SEPARATOR="" 
    /TYPE=NONTERMINAL 

    "with {expression} [as {target}]:"
    "  {statement}..."

END DEFINE
```
The user would type "withC-x / e" to get this:
```
   with {expression} [as {target}]:
     {statement}...
```
After supplying and "expression", the user would navigate to the "as {target}" placeholder and either expand it or kill it. If killed, the buffer would look like this:
```
   with some_expression:
     {statement}...
```
Note that ELSE "cleaned up" after the kill command by moving the ":" character hard up against the expression - so there is no extraneous actions required and then moved point automatically to the "{statement}" placeholder.
