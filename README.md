alynx-emacs-config
==================

Alynx's Emacs configuration files.
----------------------------------

![Screenshot](./screenshot.png)

I don't suggest others to use my configuration files since I only care about what I need, this is just for syncing my files.

But if you really want to try, first backup your configuration files.

```
$ mv ~/.emacs ~/.emacs.backup
$ mv ~/.emacs.d ~/.emacs.d.backup
```

Then clone it.

```
$ git clone https://github.com/AlynxZhou/alynx-emacs-config.git ~/.emacs.d
```

And run Emacs.

I am using Emacs 28 with pgtk and native-comp from <https://aur.archlinux.org/packages/emacs-gcc-wayland-devel-bin>, if you are not using the latest version, this configuration may not work.
