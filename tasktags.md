# 2018-May-05, Sat

I decided to to start a separate `task-tags` project,
but not in the `myemacs` project.
I'll later create a Javascript/HTML code to display reports.
It won't belong to the `myemacs` project.

## `task-start` and `task-stop` functions

```
commit 3fb8746d296ed2082ced06839629da470e65816a (HEAD -> master)
Author: Ilya Silvestrov <tellary@gmail.com>
Date:   Sat May 5 18:32:53 2018 -0700

    Created `task-start` and `task-stop`
```

Created [tasktags](https://github.com/tellary/tasktags)
GitHub repository and pushed.

## Create the simplest possible time report

I decided to start from
"newtask: 20180505.1: Covert into Toggl CSV format"
so that I can use Toggl for reporting.
Toggl will remain my fall-back time
tracking option while I'm experimenting with tags
and it isn't mature enough.

## Create keymaps

I'll need to support the following functions:

1. `task-start`,
2. `task-stop`,
3. `newtask`
4. `newtag`.

I'll probably need more commands.

I'll need a prefix key, because I'm already running out
of key strokes for the commands I'm actively using.

I'll use `M-4` as a prefix key.

Looking at `markdown-mode-map` definition in ` markdown-mode.el`
as an example.

`make-sparse-keymap` is explained
[here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Keymaps.html#Creating-Keymaps)

## 20180505.1: Covert into Toggl CSV format

# 2018-May-05, Sat

## Create keymap

```
commit 372fb7ddffe0691cfb34b2cb362b4b0374fd30d4 (HEAD -> master)
Author: Ilya Silvestrov <tellary@gmail.com>
Date:   Sun May 6 12:22:53 2018 -0700

    Created `task-tags-mode`
    
    - Bound keys for starting and stopping tasks
    - Hooked the `task-tags-mode` into `markdown-mode`
```

It would be nice to 
"newtask: 20180506.1: Detect current task state on start/stop"
so that I don't start or stop a task that's already
started or stopped.

## 20180505.1: Covert into Toggl CSV format

## Change task tags to be HTML elements

This prevents them to be shown in the resulting
HTML and PDF documents after `pandoc` conversion.

For example:

```
<task-start t="20180506 12:42:56 -0700"/>
<task-stop t="20180506 12:42:58 -0700"/>
```
